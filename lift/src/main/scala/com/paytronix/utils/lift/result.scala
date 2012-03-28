//
// Copyright 2012 Paytronix Systems, Inc.
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

import net.liftweb.common.{Box, Empty, Failure, Full}
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, ResultG}

object result {
    /** Enrich ResultGs with a .toBox method */
    implicit def resultBoxOps[A](in: ResultG[_, A]): ResultBoxOps[A] =
        ResultBoxOps(in)

    /** Enrichment of ResultG with a .toBox method */
    final case class ResultBoxOps[A](result: ResultG[_, A]) {
        /** Yields Full if Okay, Failure otherwise */
        def toBox: Box[A] =
            result match {
                case Okay(res) =>
                    Full(res)

                case FailedG(throwable, ()) =>
                    Failure(Option(throwable.getMessage) getOrElse throwable.toString, Full(throwable), Empty)

                case FailedG(throwable, param) =>
                    Failure(Option(throwable.getMessage) getOrElse throwable.toString, Full(throwable), Empty) ~> param
            }
    }

    /** Enrich Boxes with a .toResult method */
    implicit def boxOps[A](in: Box[A]): BoxOps[A] = new BoxOps(in)

    /** Enrichment of Box with a .toResult method */
    class BoxOps[A](in: Box[A]) {
        /** Convert Full to Okay, Failure to Failed, and Empty to Failed("box was empty") */
        def toResult: Result[A] =
            in match {
                case Full(a) => Okay(a)
                case Empty   => Failed("box was empty")
                case failure => Failed(box.failureToException(failure))
            }
    }
}
