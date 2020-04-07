//
// Copyright 2009-2020 Paytronix Systems, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

package com.paytronix.utils.lift

import scala.language.implicitConversions

import net.liftweb.common.{Box, Empty, Failure, Full}
import org.slf4j.Logger

package object log {
    /** Implicitly convert loggers to extended loggers, which integrate better with Lift and Scala */
    implicit def loggerBoxOps(l: Logger): Slf4jLoggerExtension = new Slf4jLoggerExtension(l)

    /** Log a message along with any information available from a box describing the failure using the given log adapter */
    def logBox[A, B](logMessage: String => Unit, logWithThrowable: (String, Throwable) => Unit, msg: String, cause: Box[B]) {
        def emitMessage(msg: String, exBox: Box[Throwable]) = exBox match {
            case Full(ex) => logWithThrowable(msg, ex)
            case _        => logMessage(msg)
        }

        def logNestedCause(nestedCause: Box[Failure]) = nestedCause match {
            case Full(x) => logBox(logMessage, logWithThrowable, "Caused by", x)
            case _       => ()
        }

        cause match {
            case Full(Failure(failMsg, ex, nestedCause)) =>
                emitMessage(msg + ": " + failMsg, ex)
                logNestedCause(nestedCause)

            case Full(_) => logMessage(msg + ": Cause present, but does not contain a Failure: " + cause)
            case Empty   => logMessage(msg + ": No known cause (is Empty)")
            case Failure(failMsg, ex, nestedCause) =>
                // This branch is probably never visited, but I include it for completeness
                emitMessage(msg + ": " + failMsg, ex)
                logNestedCause(nestedCause)
        }
    }

    /** Flyout of Slf4j's Logger that provides useful methods to integrate better with Lift and Scala */
    class Slf4jLoggerExtension(logger: Logger) {
        def traceBox[A](msg: String, cause: Box[A]) = logBox(logger.trace(_), logger.trace(_, _), msg, cause)
        def debugBox[A](msg: String, cause: Box[A]) = logBox(logger.debug(_), logger.debug(_, _), msg, cause)
        def infoBox [A](msg: String, cause: Box[A]) = logBox(logger.info(_),  logger.info(_, _),  msg, cause)
        def warnBox [A](msg: String, cause: Box[A]) = logBox(logger.warn(_),  logger.warn(_, _),  msg, cause)
        def errorBox[A](msg: String, cause: Box[A]) = logBox(logger.error(_), logger.error(_, _), msg, cause)
    }
}
