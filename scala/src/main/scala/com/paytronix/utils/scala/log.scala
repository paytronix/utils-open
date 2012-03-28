//
// Copyright 2009-2012 Paytronix Systems, Inc.
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

package com.paytronix.utils.scala

import org.slf4j.Logger

import result.{FailedG, Okay, ResultG}

/**
 * Helpers for logging
 */
object log {
    /** Enrich loggers to allow logging ResultGs as causes */
    implicit def loggerResultOps(l: Logger): LoggerResultOps =
        new LoggerResultOps(l)

    /** Enrich ResultGs to allow logging as part of a chain of result operations */
    implicit def resultLoggerOps[R <: ResultG[_, _]](r: R): ResultLoggerOps[R] =
        new ResultLoggerOps(r)

    /** Log a message along with any information available from a box describing the failure using the given log adapter */
    def logResult(log: String => Unit, logWithThrowable: (String, Throwable) => Unit)(msg: String, cause: ResultG[_, _]) {
        cause match {
            case FailedG(throwable, parameter) =>
                logWithThrowable(msg + ": failed with " + parameter + ": ", throwable)

            case ok: Okay[_] =>
                log(msg + ": no cause (instead got " + ok + ")")
        }
    }

    /** Enrichment of SLF4J's Logger that provides methods to log Failed Results as causes */
    class LoggerResultOps(logger: Logger) {
        val traceResult = logResult(logger.trace(_), logger.trace(_: String, _: Throwable)) _
        val debugResult = logResult(logger.debug(_), logger.debug(_: String, _: Throwable)) _
        val infoResult  = logResult(logger.info(_),  logger.info (_: String, _: Throwable)) _
        val warnResult  = logResult(logger.warn(_),  logger.warn (_: String, _: Throwable)) _
        val errorResult = logResult(logger.error(_), logger.error(_: String, _: Throwable)) _
    }

    /** Enrichment of ResultG that allows logging as part of a chain of result operations */
    class ResultLoggerOps[R <: ResultG[_, _]](result: R) {
        /** If not Okay then log the failure at trace level */
        def logTrace(message: String = "")(implicit logger: Logger): R = {
            result match {
                case FailedG(throwable, _) => logger.trace(if (message == "") "Failed with:" else message, throwable)
                case _ => ()
            }
            result
        }

        /** If not Okay then log the failure at debug level */
        def logDebug(message: String = "")(implicit logger: Logger): R = {
            result match {
                case FailedG(throwable, _) => logger.debug(if (message == "") "Failed with:" else message, throwable)
                case _ => ()
            }
            result
        }

        /** If not Okay then log the failure at info level */
        def logInfo(message: String = "")(implicit logger: Logger): R = {
            result match {
                case FailedG(throwable, _) => logger.info(if (message == "") "Failed with:" else message, throwable)
                case _ => ()
            }
            result
        }

        /** If not Okay then log the failure at warning level */
        def logWarn(message: String = "")(implicit logger: Logger): R = {
            result match {
                case FailedG(throwable, _) => logger.warn(if (message == "") "Failed with:" else message, throwable)
                case _ => ()
            }
            result
        }

        /** If not Okay then log the failure at error level */
        def logError(message: String = "")(implicit logger: Logger): R = {
            result match {
                case FailedG(throwable, _) => logger.error(if (message == "") "Failed with:" else message, throwable)
                case _ => ()
            }
            result
        }
    }
}
