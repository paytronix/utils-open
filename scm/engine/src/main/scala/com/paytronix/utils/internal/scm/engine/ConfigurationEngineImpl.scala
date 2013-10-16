//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.engine

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.FutureTaskRunner
import scala.concurrent.ops.spawn
import scala.ref.WeakReference

import net.liftweb.json.JsonAST.{JField, JObject, JString, JValue, render}
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.Printer.{compact, pretty}
import org.joda.time.format.ISODateTimeFormat
import org.slf4j.{Logger, LoggerFactory}

import com.paytronix.utils.internal.scm.common.{
    CacheEntry, CachingConfigurationEngine, ConfigurationReadEngine, ConfigurationReadWriteEngine, ConfigurationWatchEngine,
    AspectName, KeyPath, Node, NodeContents, aspect,
    Filter, AspectFilter, ContextFilter, NodeFilter, Unfiltered,
    InheritanceRule, InheritanceRules,
    Watcher, WatchEvent, WatchIdentifier, AllInvalidated, AspectInvalidated, NodeInvalidated
}
import com.paytronix.utils.internal.scm.common.context.{Path, ~, /, id, root}
import com.paytronix.utils.internal.scm.common.context.Segment.stringOps
import com.paytronix.utils.internal.scm.utility.audit
import com.paytronix.utils.interchange.Coding
import com.paytronix.utils.scala.cache.LFUCache
import com.paytronix.utils.scala.concurrent.{NoUpdate, Update, atomicTransaction, atomicUpdate}
import com.paytronix.utils.scala.log.resultLoggerOps
import com.paytronix.utils.scala.result.{FailedG, Okay, Result, iterableResultOps, optionOps, tryCatch}

import Filter.filter

final class WatchIdentifierImpl extends WatchIdentifier

final case class Watch(identifier: WatchIdentifierImpl, filter: Filter, watcher: WeakReference[Watcher])

object ConfigurationEngineImpl {
    val InheritanceRulesAspect = "configuration.InheritanceRules"
}

import ConfigurationEngineImpl.InheritanceRulesAspect

object ConfigurationReadEngineImpl {
    def apply(storage: ConfigurationReadStorage): ConfigurationReadEngineImpl =
        new ConfigurationReadEngineImpl {
            def locateStorage = Okay(storage)
        }
}

trait ConfigurationReadEngineImpl extends ConfigurationReadEngine {
    protected def logger: Logger = LoggerFactory.getLogger(getClass)
    protected def locateStorage: Result[ConfigurationReadStorage]

    // these explicit override hooks seem a little gross, oh well, it's nice to be able to choose caching and watching a la carte
    protected def cookedCacheGet(n: Node): Option[NodeContents] = None
    protected def cookedCacheStore(n: Node, c: NodeContents): Unit = ()
    protected def rawCacheGet(n: Node): Option[NodeContents] = None
    protected def rawCacheStore(n: Node, c: NodeContents): Unit = ()
    protected def rawCacheStoreMany(pairs: Iterable[(Node, NodeContents)]): Unit = ()
    protected def clearCachedAspect(aspect: AspectName): Unit = ()
    protected def clearCachedNodes(nodes: Iterable[Node]): Unit = ()
    protected def clearAllCached(): Unit = ()
    protected def triggerAspectWatches(aspect: AspectName): Unit = ()
    protected def triggerNodeWatches(nodes: Iterable[Node]): Unit = ()
    protected def triggerAllWatches(): Unit = ()

    protected implicit def implicitLogger = logger

    val inheritanceRulesCache: AtomicReference[Map[AspectName, InheritanceRules]] = new AtomicReference(Map.empty)

    reloadInheritanceRules().logError("Failed to load inheritance rules for first time, stored settings will not be used. This might be okay, if each service correctly registers the rules")

    protected def invalidateAspect(aspect: AspectName): Unit = {
        clearCachedAspect(aspect)
        triggerAspectWatches(aspect)
    }

    def invalidate(nodes: Iterable[Node]): Unit = {
        clearCachedNodes(nodes)
        triggerNodeWatches(nodes)
    }

    def invalidateAll(): Unit = {
        clearAllCached()
        triggerAllWatches()
    }

    private def applyInheritanceRules(aspect: AspectName, chain: Seq[NodeContents]): Result[Option[NodeContents]] =
        inheritanceRulesCache.get.getOrElse(aspect, InheritanceRules.empty).apply(chain)

    def enumerateNodesUsingFilter(filter: Filter): Result[Seq[Node]] =
        locateStorage.flatMap(_.enumerateNodesUsingFilter(filter))

    def fetchNode(node: Node): Result[Option[NodeContents]] =
        cookedCacheGet(node) match {
            case s@Some(precached) =>
                Okay(s)
            case None =>
                fetchRawNodes(node.andAncestors)
                    .flatMap(raw => applyInheritanceRules(node.aspect, raw.flatten))
                    .sideEffect(_.foreach(cookedCacheStore(node, _)))
        }

    def fetchNodes(nodes: Iterable[Node]): Result[Seq[Option[NodeContents]]] = {
        var anyMissing = false

        val preliminary = nodes.map { n =>
            cookedCacheGet(n) match {
                case Some(precached) =>
                    n -> Some(precached)

                case None =>
                    anyMissing = true
                    n -> None
            }
        }.toSeq

        if (!anyMissing)
            Okay(preliminary.map(_._2))
        else {
            val requirements = Map(preliminary.collect { case (n, None) => n -> n.andAncestors }: _*)
            val nodesToFetch = requirements.values.flatten.toSeq.distinct
            fetchRawNodes(nodesToFetch)
                .map { fetched => Map((nodesToFetch zip fetched).collect { case (n, Some(c)) => (n, c) }: _*) }
                .flatMap { fetched =>
                    preliminary mapResult {
                        case (_, precached@Some(_)) => Okay(precached)
                        case (n, _) =>
                            applyInheritanceRules(n.aspect, requirements(n).map(fetched.get).toSeq.flatten)
                                .sideEffect { _.foreach { c => cookedCacheStore(n, c) } }
                    }
                }
        }
    }

    def fetchNodesUsingFilter(filter: Filter): Result[Seq[(Node, NodeContents)]] =
        enumerateNodesUsingFilter(filter).flatMap { nodes =>
            fetchNodes(nodes) map { nodes zip _ } map { _ collect { case (n, Some(c)) => n -> c } }
        }

    def fetchRawNode(node: Node): Result[Option[NodeContents]] =
        rawCacheGet(node) match {
            case s@Some(precached) =>
                Okay(s)
            case None =>
                locateStorage.flatMap(_.fetchNode(node))
                    .sideEffect(_.foreach(rawCacheStore(node, _)))
        }

    def fetchRawNodes(nodes: Iterable[Node]): Result[Seq[Option[NodeContents]]] = {
        var anyMissing = false

        val preliminary = nodes.map { n =>
            rawCacheGet(n) match {
                case Some(precached) =>
                    n -> Some(precached)

                case None =>
                    anyMissing = true
                    n -> None
            }
        }.toSeq

        if (!anyMissing)
            Okay(preliminary.map(_._2))
        else {
            val missing = preliminary.collect { case (n, None) => n }

            locateStorage
                .flatMap(_.fetchNodes(missing))
                .map(fetched => Map(missing.zip(fetched).collect { case (n, Some(c)) => n -> c }: _*))
                .sideEffect(rawCacheStoreMany)
                .map { fetched =>
                    preliminary map {
                        case (_, s@Some(_)) => s
                        case (node, _)      => fetched.get(node)
                    }
                }
        }
    }

    def fetchRawNodesUsingFilter(filter: Filter): Result[Seq[(Node, NodeContents)]] =
        enumerateNodesUsingFilter(filter).flatMap { nodes =>
            fetchRawNodes(nodes) map { nodes zip _ } map { _ collect { case (n, Some(c)) => n -> c } }
        }

    def fetchInheritanceRules(): Result[Map[AspectName, InheritanceRules]] =
        Okay(inheritanceRulesCache.get)

    def fetchInheritanceRulesForAspect(aspect: AspectName): Result[InheritanceRules] =
        Okay(inheritanceRulesCache.get.getOrElse(aspect, InheritanceRules.empty))

    def reloadInheritanceRules(): Result[Unit] =
        for {
            coding <- InheritanceRules.coding
            fetched <- fetchNodesUsingFilter(filter(root/id("metadata")).aspect(InheritanceRulesAspect).descendantsOnly)
            decoded <- fetched.mapResult {
                case (root / id("metadata") / "aspect" ~ aspectName aspect _, c) =>
                    coding.decode(c) map { rules => Some(aspectName -> rules) }
                case _ =>
                    Okay(None)
            }.map(_.flatten)
        } yield inheritanceRulesCache.set(Map(decoded: _*))

    def reloadInheritanceRulesForAspect(aspectName: AspectName): Result[Unit] =
        atomicTransaction(inheritanceRulesCache) { (cache, _: Option[Result[Unit]]) =>
            {
                val node = root / id("metadata") / "aspect" ~ aspectName aspect InheritanceRulesAspect

                for {
                    coding <- InheritanceRules.coding
                    fetched <- fetchNode(node)
                    decoded <- fetched.mapResult { c => coding.decode(c) }
                } yield decoded
            } match {
                case Okay(Some(rules)) =>
                    Update(cache + (aspectName -> rules), Okay(()))
                case Okay(None) =>
                    NoUpdate(Okay(()))
                case failed@FailedG(_, _) =>
                    NoUpdate(failed)
            }
        }
}

trait ConfigurationWatchEngineImpl extends ConfigurationReadEngineImpl with ConfigurationWatchEngine {
    val watchRunner: FutureTaskRunner = {
        val numCores = Runtime.getRuntime.availableProcessors
        val keepAliveTime = 60000L
        val workQueue = new LinkedBlockingQueue[Runnable]
        val exec = new ThreadPoolExecutor(numCores,
                                          numCores,
                                          keepAliveTime,
                                          TimeUnit.MILLISECONDS,
                                          workQueue,
                                          new ThreadPoolExecutor.CallerRunsPolicy())
        scala.concurrent.JavaConversions.asTaskRunner(exec)
    }

    watch(inheritanceRuleUpdater, filter(root / id("metadata")).aspect(InheritanceRulesAspect).withDescendants)

    def endWatchSupport(): Unit =
        watchRunner.shutdown()

    val watches: AtomicReference[Vector[Watch]] = new AtomicReference(Vector.empty)

    private lazy val inheritanceRuleUpdater = new Watcher {
        def watchTriggered(event: WatchEvent): Unit =
            event match {
                case NodeInvalidated(root/id("metadata")/("aspect" ~ aspectName) aspect InheritanceRules.Aspect) =>
                    reloadInheritanceRulesForAspect(aspectName).logWarn("Failed to load updated inheritance rules")
                case AspectInvalidated(InheritanceRules.Aspect) =>
                    reloadInheritanceRules().logWarn("Failed to load updated inheritance rules")
                case _ => ()
            }
    }

    private def triggerWatch(event: WatchEvent, watch: Watch): Unit =
        watch.watcher.get match {
            case Some(watcher) =>
                spawn {
                    try {
                        watcher.watchTriggered(event)
                    } catch { case e: Exception =>
                        logger.warn("Failed to callback watch " + watch + " for " + event + " due to exception:", e)
                    }
                }(watchRunner)
            case None =>
                logger.info("Callback for watch " + watch.identifier + " with filter " + watch.filter + " has been garbage collected, cancelling the watch")
                unwatch(watch.identifier)
        }

    override protected def triggerAspectWatches(aspect: AspectName): Unit = {
        val event = AspectInvalidated(aspect)

        watches.get.foreach {
            case watch@Watch(_, filter, _) if filter(aspect) =>
                triggerWatch(event, watch)
            case _ => ()
        }
    }

    override protected def triggerNodeWatches(nodes: Iterable[Node]): Unit =
        nodes.foreach { node =>
            val event = NodeInvalidated(node)
            watches.get.foreach {
                case watch@Watch(_, filter, _) if filter(node) =>
                    triggerWatch(event, watch)
                case _ => ()
            }
        }

    override protected def triggerAllWatches(): Unit =
        watches.get.foreach { triggerWatch(AllInvalidated, _) }

    def watch(watcher: Watcher, filter: Filter): WatchIdentifier = {
        val ident = new WatchIdentifierImpl
        atomicUpdate(watches)(_ :+ Watch(ident, filter, new WeakReference(watcher)))
        ident
    }

    def unwatch(identifier: WatchIdentifier): Unit =
        atomicUpdate(watches)(_.filterNot(_.identifier == identifier))
}

trait CachingConfigurationEngineImpl extends ConfigurationReadEngineImpl with CachingConfigurationEngine {
    protected def rawCacheMaxSize: Int
    protected def rawCacheEvictFraction: Double
    protected def cookedCacheMaxSize: Int
    protected def cookedCacheEvictFraction: Double

    val rawCache:    LFUCache[Node, NodeContents] = new LFUCache(rawCacheMaxSize, rawCacheEvictFraction)
    val cookedCache: LFUCache[Node, NodeContents] = new LFUCache(cookedCacheMaxSize, cookedCacheEvictFraction)

    override protected def invalidateAspect(aspect: AspectName): Unit = {
        rawCache.remove(rawCache.byKey.entryMap.keys.filter(_.aspect == aspect))
        cookedCache.remove(cookedCache.byKey.entryMap.keys.filter(_.aspect == aspect))
        super.invalidateAspect(aspect)
    }

    override def invalidate(nodes: Iterable[Node]): Unit = {
        rawCache.remove(nodes)
        cookedCache.remove(cookedCache.byKey.entryMap.keys.filter(node => nodes.exists(_ isEqualToOrAncestorOf node)))
        super.invalidate(nodes)
    }

    override def invalidateAll(): Unit = {
        rawCache.clear()
        cookedCache.clear()
        super.invalidateAll()
    }

    override protected def cookedCacheGet(n: Node): Option[NodeContents] = cookedCache.byKey.get(n)
    override protected def cookedCacheStore(n: Node, c: NodeContents): Unit = cookedCache.store(n, c)
    override protected def rawCacheGet(n: Node): Option[NodeContents] = rawCache.byKey.get(n)
    override protected def rawCacheStore(n: Node, c: NodeContents): Unit = rawCache.store(n, c)
    override protected def rawCacheStoreMany(pairs: Iterable[(Node, NodeContents)]): Unit = rawCache.store(pairs)

    def clearRawCache(): Unit = {
        rawCache.clear()
        clearCookedCache()
    }

    def clearCookedCache(): Unit =
        cookedCache.clear()

    def getRawCacheSize(): Int =
        rawCache.size

    def getCookedCacheSize(): Int =
        cookedCache.size

    def dumpRawCache(): Seq[CacheEntry] =
        rawCache.byKey.entryMap.values.map { e => CacheEntry(e.key, e.value, e.accessCount, e.lastAccess) }.toSeq

    def dumpCookedCache(): Seq[CacheEntry] =
        cookedCache.byKey.entryMap.values.map { e => CacheEntry(e.key, e.value, e.accessCount, e.lastAccess) }.toSeq
}

trait ConfigurationReadWriteEngineImpl extends CachingConfigurationEngineImpl with ConfigurationReadWriteEngine {
    protected def locateStorage: Result[ConfigurationReadWriteStorage]

    def updateNode(node: Node, upserts: Seq[(KeyPath, JValue)], deletes: Seq[KeyPath], auditInfo: NodeContents): Result[NodeContents] =
        locateStorage.flatMap(_.updateNode(node, upserts, deletes, auditInfo)).sideEffect(_ => invalidate(node :: Nil))

    def replaceNode(node: Node, newContents: NodeContents, auditInfo: NodeContents): Result[NodeContents] =
        locateStorage.flatMap(_.replaceNode(node, newContents, auditInfo)).map(_ => newContents).sideEffect(_ => invalidate(node :: Nil))

    def deleteNode(node: Node, auditInfo: NodeContents): Result[Unit] =
        locateStorage.flatMap(_.deleteNode(node, auditInfo)).sideEffect(_ => invalidate(node :: Nil))

    def updateInheritanceRulesForAspect(aspectName: AspectName, rules: InheritanceRules): Result[Unit] =
        for {
            rulesCoding <- InheritanceRules.coding
            encodedRules <- rulesCoding.encode(rules).asA[JObject]
            updatedOk <- replaceNode(root/id("metadata")/("aspect" ~ aspectName) aspect InheritanceRulesAspect, encodedRules, audit.internal("updateInheritanceRulesForAspect"))
        } yield invalidateAspect(aspectName)
}
