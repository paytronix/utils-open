//
// Copyright 2011 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.standalone

import scala.util.control.Exception.catching
import net.lag.configgy.{ConfigMap, Configgy}
import net.lag.logging.Logger
import com.paytronix.utils.internal.java.{BuildVersion, ClassUtils}
import com.paytronix.utils.scala.resource.withResource
import com.paytronix.utils.validation.base.{Validated, ValidationError, validationErrorsToString}

/** Base trait of standalone tools which provides loading and setup of Configgy */
trait StandaloneTool {
    /** Main entry point of a standalone tool which initializes configgy and then triggers each onLoad handler */
    def main(args: Array[String]): Unit =
        args match {
            case Array(configPath) =>
                try {
                    Configgy.configure(configPath)
                } catch {
                    case e: Exception =>
                        // Configgy emits its own exceptions, no need to duplicate that effort
                        // e.printStackTrace(System.err)
                        System.exit(1)
                }

                val log = Logger.get(getClass.getName)

                // merely instantiating this class will cause an info-level log message to be output, so don't bother doing anything with the instance
                new BuildVersion(getClass, ClassUtils.getUnqualifiedClassName(getClass) + ".class")

                def runAdvice(fs: List[(String, Advice)]): Either[Throwable, Unit] =
                    catching(classOf[Exception]).either {
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
                    }.right.flatMap(x => x)

                runAdvice(advice.reverse).left.foreach { t =>
                    t.toString.split('\n').foreach(log.fatal(_))
                    t.getStackTrace.foreach(ste => log.fatal(" at " + ste))
                }

            case _ => System.err.println("usage: " + getClass.getName + " <config file>")
        }


    /** Get the top-level configuration. Usually just the global configgy */
    def configMap: ConfigMap = Configgy.config

    type Advice = (() => Either[Throwable, Unit]) => Either[Throwable, Unit]

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
        f => load match {
            case Right(params) =>
                setter(Some(params))
                val result = f()
                setter(None)

            case Left(errors) =>
                Left(new RuntimeException(validationErrorsToString(errors)))
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

    implicit def unitToEitherThrowable(in: Unit): Either[Throwable, Unit] = Right(())
    implicit def eitherStringToEitherThrowable[A](in: Either[String, A]): Either[Throwable, A] = in.left.map(new RuntimeException(_))
    implicit def eitherValidationErrorsToEitherThrowable[A](in: Either[List[ValidationError], A]): Either[Throwable, A] =
        in.left.map(errors => new RuntimeException("Failed to start due to validation errors:\n" + validationErrorsToString(errors)))

    /** Override with the main work of the tool */
    def run(): Either[Throwable, Unit]
}
