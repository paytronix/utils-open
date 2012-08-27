//
// Copyright 2011 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.standalone

import java.io.File
import scala.collection.JavaConverters.asScalaSetConverter
import scala.util.control.Exception.catching

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import com.typesafe.config.{Config, ConfigException, ConfigFactory, ConfigParseOptions, ConfigObject, ConfigValue, ConfigValueType}
import org.slf4j.LoggerFactory

import com.paytronix.utils.internal.java.{BuildVersion, ClassUtils}
import com.paytronix.utils.scala.log.resultLoggerOps
import com.paytronix.utils.scala.resource.withResource
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, ResultG, tryCatch}
import com.paytronix.utils.validation.base.{Validated, ValidationError, validationErrorsToString}

/** Base trait of standalone tools which provides loading and setup of Configgy */
trait StandaloneTool {
    private var _config: Option[Config] = None

    /** Get the raw (unresolved) top-level configuration */
    def rawConfig = _config getOrElse sys.error("config not yet loaded")

    /** Get the top-level configuration */
    lazy val config = rawConfig.resolve

    /** Name of the config file located in the configuration directory given on the command line, e.g. `loader`. Various extensions will be tried (.conf, .json, etc.) */
    protected def configFileName: String

    /** Main entry point of a standalone tool which initializes configgy and then triggers each onLoad handler */
    def main(args: Array[String]): Unit =
        args match {
            case Array(configPath) if new File(configPath).isDirectory =>
                try {
                    val options = ConfigParseOptions.defaults
                        .setAllowMissing(false)
                        .setClassLoader(getClass.getClassLoader)
                    val fn = configPath + File.separator + configFileName
                    _config = Some(ConfigFactory.parseFileAnySyntax(new File(fn), options))
                } catch { case e: Exception =>
                    System.err.println("Failed to load configuration:")
                    e.printStackTrace(System.err)
                    System.exit(1)
                }

                val logFn = configPath + File.separator + "logback.xml"
                try {
                    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
                    val configurator = new JoranConfigurator()
                    configurator.setContext(loggerContext)
                    loggerContext.reset()
                    configurator.doConfigure(logFn)
                } catch { case e: Exception =>
                    // No use using a logger.
                    System.err.println("Failed to initialize Logback system from \"" + logFn + "\" due to exception:")
                    e.printStackTrace(System.err)
                    System.exit(1)
                }

                implicit val log = LoggerFactory.getLogger(getClass)

                // merely instantiating this class will cause an info-level log message to be output, so don't bother doing anything with the instance
                new BuildVersion(getClass, ClassUtils.getUnqualifiedClassName(getClass) + ".class")

                if (log.isDebugEnabled) {
                    log.debug(getClass.getName + " tool starting with configuration:")
                    for (entry <- config.entrySet.asScala.toSeq.sortBy(_.getKey)) log.debug(entry.getKey + " = " + entry.getValue.unwrapped)
                }

                def runAdvice(fs: List[(String, Advice)]): Result[Unit] =
                    tryCatch.result {
                        fs match {
                            case (source, f) :: rest => {
                                log.debug("Setting up: " + source)
                                f(() => runAdvice(rest))
                            }

                            case Nil => {
                                log.debug("Running...")
                                run()
                            }
                        }
                    }

                runAdvice(advice.reverse).logError("Failed:")

            case Array(configPath) =>
                System.err.println("usage: " + getClass.getName + " <config directory>")
                System.err.println("  " + configPath + " was given but was not a directory")

            case _ =>
                System.err.println("usage: " + getClass.getName + " <config directory>")
        }

    type Advice = (() => Result[Unit]) => Result[Unit]

    /** Keeps around the list of advice */
    private var advice: List[(String, Advice)] = Nil

    /** Little helper function that grabs information about the caller's stack frame */
    private def callerInfo: String =
        new Exception().fillInStackTrace.getStackTrace.toList.drop(1).dropWhile(_.getMethodName == "advise").headOption match {
            case Some(stackElement) => stackElement.toString
            case _ => "<unknown caller>"
        }

    /** Helper function used during parameter loading advice to set and then clear some parameters */
    protected def withParameters[A](setter: Option[A] => Unit)(load: => Validated[A]): Advice =
        k => load match {
            case Right(params) =>
                setter(Some(params))
                val result = k()
                setter(None)
                Okay(())

            case Left(errors) =>
                FailedG("", errors)
        }

    /**
     * Register advice to run around the main body of the tool.
     *
     * <pre>
     *     advise {
     *         f => initialize; f(); finalize
     *     }
     * </pre>
     */
    protected def advise(f: Advice): Unit = advice = (callerInfo, f) :: advice

    /** Override with the main work of the tool */
    def run(): Result[Unit]
}

object StandaloneTool {
    implicit def validatedToResult[A](in: Validated[A]): Result[A] =
        in.fold(errors => Failed("Failed to start due to validation errors:\n" + validationErrorsToString(errors)), Okay.apply)

    implicit def formatFailedValidationError[A](in: ResultG[List[ValidationError], A]): Result[A] =
        in | { case FailedG(_, errors) => Failed("Failed to start due to validation errors:\n" + validationErrorsToString(errors)) }

    implicit def configOps(in: Config): ConfigOps = ConfigOps(in)

    final case class ConfigOps(config: Config) {
        def getStringOption(k: String): Result[Option[String]] =
            tryCatch.value {
                try Some(config.getString(k)) catch { case _: ConfigException.Missing => None }
            }

        def getConfigOption(k: String): Result[Option[Config]] =
            tryCatch.value {
                try Some(config.getConfig(k)) catch { case _: ConfigException.Missing => None }
            }
    }
}