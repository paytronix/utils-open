//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.utility

import net.liftweb.json.JsonAST.{JNothing, JObject, JValue, render}
import net.liftweb.json.Printer.compact
import org.slf4j.LoggerFactory
import scalaz.Lens

import com.paytronix.utils.internal.scm.common.{ConfigurationEngine, KeyPath, Node, NodeContents, Watcher, WatchEvent, WatchIdentifier}
import com.paytronix.utils.internal.scm.common.Filter.filter
import com.paytronix.utils.interchange.{Coder, Coding, json}
import com.paytronix.utils.scala.log.resultLoggerOps
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, optionOps}

/**
 * Mixin for objects which maintain a decoded copy of a single configuration node, using Interchange.
 *
 * This trait attempts to keep a decoded copy of the configuration, and watches the node for further updates to the configured data. When the watch triggers,
 * it will attempt to load and decode the updated node contents.
 *
 * Determining what particular changes were made at each update is left to the particular use case, but this trait will not report on any watch trigger
 * that doesn't yield a change to the used portion of the node contents
 */
trait SingleConfigurable extends Watcher {
    protected implicit val logger = LoggerFactory.getLogger(getClass)

    protected def locateConfigEngine: Result[ConfigurationEngine]

    trait Dependent {
        def dependencyReconfigured(dependency: SingleConfigurable.this.type): Unit = ()
        def dependencyUnconfigured(dependency: SingleConfigurable.this.type): Unit = ()
        def dependencyLostConfiguration(dependency: SingleConfigurable.this.type): Unit = ()
    }

    private class DependentWatchIdentifier extends WatchIdentifier

    /** The type of the decoded object */
    protected type Configured

    /** Where in the configuration store to get the configuration data */
    protected def node: Node

    /** The Interchange coder to use when decoding the configuration data to produce a `Configured` */
    protected def coderResult: Result[Coder[Configured]]

    /** Whether the configured data is a subset (`Some`) of the configuration node, or the entire node (`None`). */
    protected def nodeSubset: Option[KeyPath] = None

    /** Some default configuration to use in the case where the configuration could not be found in storage */
    protected def defaultUnconfigureResult: UnconfigureResult = FailReconfiguration

    /** Whether becoming unconfigured is considered an error or not */
    protected def optional: Boolean = false

    /**
     * Some qualifying text to identify this configurable among others.
     * Defaults to the class name.
     * Don't bother to include the node, it should be included in all log messages
     */
    protected def logPrefix: String = getClass.getName

    /**
     * Called when the configuration for the node goes missing or was missing at first time load.
     *
     * Can yield `ReconfigureWithDefault(configured)` which is treated as a configuration to use instead of becoming unconfigured.
     * In that case, `reconfigure` will be called with the new configuration (that is, the default one).
     * Can yield `BecomeUnconfigured` to become unconfigured without an error (e.g. configuration is optional)
     * Can yield `FailReconfiguration` to become unconfigured with an error
     *
     * The default implementation of `unconfigure` yields the value of `defaultUnconfigureResult`
     */
    protected def unconfigure(previous: Option[ConfiguredState]): UnconfigureResult =
        defaultUnconfigureResult

    sealed abstract class UnconfigureResult
    final case class ReconfigureWithDefault(default: Configured) extends UnconfigureResult
    case object BecomeUnconfigured extends UnconfigureResult
    case object FailReconfiguration extends UnconfigureResult

    /** Called when the configuration data for the node could not be decoded or loaded */
    protected def failedReconfigure(previous: Option[ConfiguredState], currentJValue: Option[JValue], failed: Failed): Unit = ()

    /** Called when the configuration data changes or is loaded for the first time */
    protected def reconfigure(previous: Option[ConfiguredState], current: ConfiguredState): Unit = ()

    /** Contains the encoded `JValue` and decoded object */
    final case class ConfiguredState(jvalue: JValue, obj: Configured)

    /** Yield the last result of (re)loading the configuration data */
    def lastLoadResult: Result[Unit] =
        _lastLoadResult

    /**
     * Yield the current in-use state of the configuration data.
     * May not reflect the configuration store, if the data in the store could not be loaded or decoded (see `lastLoadResult`)
     */
    def currentState: Result[ConfiguredState] =
        _current.map(Okay.apply) getOrElse _lastLoadResult.flatMap(_ => Failed("current is None, so lastLoadResult should be Failed"))

    /** Return <tt>true</tt> if configuration settings are stored in the SCM system, whether or not this object has successfully used them or used a default */
    def hasConfiguration: Boolean =
        _hasConfiguration

    /**
     * Yield the current in-use state of the configuration data or throw an exception if it's not available.
     * May not reflect the configuration store, if the data in the store could not be loaded or decoded (see `lastLoadResult`)
     */
    def getCurrentState(): ConfiguredState =
        currentState.orThrow

    /**
     * Yield the current in-use decoded configuration object.
     * May not reflect the configuration store, if the data in the store could not be loaded or decoded (see `lastLoadResult`)
     */
    def currentConfiguration: Result[Configured] =
        currentState.map(_.obj)

    /**
     * Yield the current in-use decoded configuration object or throw an exception if it's not available.
     * May not reflect the configuration store, if the data in the store could not be loaded or decoded (see `lastLoadResult`)
     */
    def getCurrentConfiguration(): Configured =
        currentConfiguration.orThrow

    /** Add a dependent to this `SingleConfigurable` to get called whenever this gets updated */
    def addDependent(dependent: Dependent): WatchIdentifier = synchronized {
        val ident = new DependentWatchIdentifier()
        _dependents :+= (ident -> dependent)
        ident
    }

    /** Remove a previously added dependent */
    def removeDependent(ident: WatchIdentifier): Unit = synchronized {
        _dependents = _dependents.filterNot(_._1 == ident)
    }

    private var _lastLoadResult: Result[Unit] = Failed("never loaded")
    private var _watchIdentifier: Option[WatchIdentifier] = None
    private var _current: Option[ConfiguredState] = None
    private var _hasConfiguration: Boolean = false
    private var _dependents: Seq[(WatchIdentifier, Dependent)] = Vector.empty
    private var _starts = 0

    private def triggerDependents(f: Dependent => this.type => Unit): Unit =
        _dependents.foreach { case (_, dependent) =>
            try {
                f(dependent)(this)
            } catch { case t: Throwable =>
                logger.warn(logPrefix + ": Failed to notify dependent " + dependent + " about change to " + this + ":", t)
            }
        }

    protected def getSubset(contents: JValue): JValue =
        nodeSubset.getOrElse(Nil).foldLeft(contents)(_ \ _)

    def watchTriggered(event: WatchEvent): Unit =
        reload().logError("failed to reconfigure from " + node)

    /** Reload the configuration data without affecting the watch state */
    protected def reload(): Result[Unit] = synchronized {
        val contentsResult = for {
            configEngine <- locateConfigEngine
            contents <- configEngine.fetchNode(node) | ("couldn't fetch configuration from " + node)
        } yield contents

        // need me some semantic editor combinators!
        val finalResult = contentsResult.map(_.flatMap(contents => Some(getSubset(contents)).filter(_ != JNothing))) match {
            case Okay(Some(newJValue)) =>
                _hasConfiguration = true
                lazy val renderedNewJValue = if (newJValue == JNothing) "<nothing>" else compact(render(newJValue))
                if (_current.map(_.jvalue == newJValue) getOrElse false) {
                    logger.info(logPrefix + ": Not reconfiguring since relevant portion of " + node + " has not actually changed")
                    Okay(())
                } else
                    (
                        coderResult.orElse("can't decode configuration")
                        .flatMap(_.decode(newJValue).orElse("failed to decode " + renderedNewJValue + " from " + node))
                    ) match {
                        case Okay(configured) =>
                            val previous = _current
                            val current = ConfiguredState(newJValue, configured)
                            _current = Some(current)
                            logger.info(logPrefix + ": Reconfiguring with " + configured)
                            reconfigure(previous, current)
                            triggerDependents(_.dependencyReconfigured)
                            Okay(())
                        case failed@FailedG(_, _) =>
                            failedReconfigure(_current, Some(newJValue), failed)
                            failed.orElse("failed to decode new configuration " + renderedNewJValue).unit
                    }

            case Okay(None) =>
                _hasConfiguration = false
                val previous = _current
                unconfigure(previous) match {
                    case ReconfigureWithDefault(default) =>
                        coderResult.orElse("can't encode configuration")
                            .flatMap(_.encode(default).orElse("failed to encode default configuration"))
                            .map(ConfiguredState(_, default))
                            .orElse("Tried to configure with default (since no configuration found at " + node + ") but failed to encode that default configuration")
                            .sideEffect { current =>
                                val previous = _current
                                _current = Some(current)
                                logger.info(logPrefix + ": Configuration for " + node + " not found, using default configuration: " + current.obj)
                                reconfigure(previous, current)
                                triggerDependents(_.dependencyReconfigured)
                            }
                            .unit

                    case BecomeUnconfigured =>
                        _current = None
                        logger.debug(logPrefix + ": Configuration for " + node + " not found, becoming unconfigured")
                        triggerDependents(_.dependencyUnconfigured)
                        Okay(())

                    case FailReconfiguration =>
                        _current = None
                        logger.error(logPrefix + ": Configuration for " + node + " not found, becoming unconfigured")
                        triggerDependents(_.dependencyLostConfiguration)
                        Failed("no configuration in storage at " + node + " and no default configuration to use")
                }

            case failed@FailedG(_, _) =>
                failed.logError(logPrefix + ": failed to load new configuration from " + node)
                failedReconfigure(_current, None, failed)
                failed.unit
        }

        _lastLoadResult = finalResult

        finalResult.logWarn(logPrefix + ": Failed to reload configuration from " + node)
    }

    /** Store a new copy of the configured object in the configuration store by encoding it, replacing the current configured state */
    protected def update(obj: Configured, auditInformation: NodeContents, atNode: Node = node): Result[Unit] =
        for {
            configEngine <- locateConfigEngine
            configCoder <- coderResult | ("can't encode configuration")
            newJValue <- configCoder.encode(obj) | ("failed to encode configuration " + obj)
            storedOk <- nodeSubset match {
                case Some(path) =>
                    val upserts = List(path -> newJValue)
                    lazy val renderedUpserts = path.mkString("/") + " -> " + (if (newJValue == JNothing) "<nothing>" else compact(render(newJValue)))
                    configEngine.updateNode(atNode, upserts, Nil, auditInformation) | ("failed to update " + node + " in configuration storage with " + renderedUpserts)
                case None =>
                    lazy val rendered = if (newJValue == JNothing) "<nothing>" else compact(render(newJValue))
                    configEngine.replaceNode(atNode, newJValue.asInstanceOf[JObject], auditInformation) | ("failed to replace " + node + " in configuration storage with " + rendered)
            }
            reloadedOk <- reload()
        } yield ()

    /**
     * Start watching the underlying configuration node for changes and load it for the first time.
     * To ensure timely resource collection, `stop()` should be called when finished.
     * Note that subsequent calls to `start()` have no effect unless a corresponding number of calls to `stop()` have been made.
     */
    def start(): Result[Unit] = synchronized {
        _starts match {
            case 0 =>
                _starts = 1
                _start()
            case n =>
                _starts = n + 1
                Okay(())
        }
    }

    /**
     * Clear any connection context and stop watching the configuration node for changes
     * Note that if more than one call to `start()` has been made, then `stop()` will not have any effect until a corresponding number of `stop()` calls have been made
     */
    def stop(): Unit = synchronized {
        _starts match {
            case 0 =>
                logger.warn("stop() called too many times", new RuntimeException("call trace").fillInStackTrace())
            case 1 =>
                _stop()
                _starts = 0
            case n =>
                _starts = n - 1
        }
    }

    /** Do the actual work of starting up the configurable. Only called in proper pairs with `_stop()` */
    protected def _start(): Result[Unit] =
        for {
            configEngine <- locateConfigEngine
            _ = { _watchIdentifier = Some(configEngine.watch(this, filter(node))) }
            _ <- reload()
        } yield ()

    /** Do the actual work of stopping the configurable. Only called in proper pairs with `_start()` */
    protected def _stop(): Unit = {
        _watchIdentifier.foreach(ident => locateConfigEngine.foreach(_.unwatch(ident)))
        _current = None
        _lastLoadResult = Failed("stopped")
    }
}

/** Mixin for configuring from a portion of a configuration node */
trait SingleConfigurableAtPath extends SingleConfigurable {
    protected def keyPath: KeyPath

    override protected lazy val nodeSubset = if (keyPath.isEmpty) None else Some(keyPath)
}

/** Mixin for managing some active resource(s) that depend on the underlying configuration, such as database connections */
trait AttachedActiveResource extends SingleConfigurable {
    type Resource

    /** Called to activate (connect, acquire, whatever) the associated resource when this configurable initializes or is reconfigured */
    protected def activateResource(configuration: Configured, previous: Result[Resource]): Result[Resource]

    /** Called to deactivated (disconnect, whatever) the associated resource when there is a currently active resource and a reconfiguration is occurring or `stop()` is called */
    protected def deactivateResource(resource: Resource, reconfiguring: Boolean): Unit = ()

    private var _lastResourceResult: Result[Unit] = Failed("not yet configured")
    private var _resource: Option[Resource] = None

    override protected def reconfigure(previous: Option[ConfiguredState], current: ConfiguredState): Unit =
        refreshResource().logError(logPrefix + ": Failed to connect after reconfiguration:")

    /** (Re)connect to / refresh the associated resource, whatever it is */
    def refreshResource(): Result[Unit] = synchronized {
        currentConfiguration.flatMap { config =>
            val previous = resource
            try {
                previous.foreach(deactivateResource(_, true))
            } catch { case e: Exception =>
                logger.error("Failed to deactivate previous resource:", e)
            }
            activateResource(config, resource)
        }.sideEffect { r => _resource = Some(r) }
        .unit
        .pass { _lastResourceResult = _ }
    }

    /** The current associated resource (e.g. database connection */
    def resource: Result[Resource] =
        _resource.map(Okay.apply) getOrElse _lastResourceResult.flatMap(_ => Failed("_resource is None, so lastResourceResult should be Failed"))

    override protected def _start(): Result[Unit] =
        super._start() then refreshResource()

    override protected def _stop(): Unit = {
        super._stop()
        _resource.foreach(deactivateResource(_, false))
        _lastResourceResult = Failed("stopped")
        _resource = None
    }
}

