//
// Copyright 2012-2020 Paytronix Systems, Inc.
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

package com.paytronix.utils.validation

import scalaz.{Failure, Success}
import scalaz.NonEmptyList.nels
import scalaz.Validation.success

import com.paytronix.utils.scala.result.{FailedG, Okay, ResultG}

import base.{Validated, ValidationError, missingValueError}

object result {
    /** Apply some validation to the value inside a `Result`/`ResultG` */
    def result[E, A, B](f: A => Validated[B]): ResultG[E, A] => Validated[ResultG[E, B]] = {
        case Okay(v)         => f(v).map(Okay.apply)
        case f@FailedG(_, _) => Success(f)
    }

    /** Require that a result be Okay */
    def okay[E, A]: ResultG[E, A] => Validated[A] =
        okayE[E, A, A](_ => missingValueError)(success)

    /** Require that a result be Okay and apply a function to it */
    def okay[E, A, B](f: A => Validated[B]): ResultG[E, A] => Validated[B] =
        okayE[E, A, B](_ => missingValueError)(f)

    /** Require that a result be Okay applying a validation function when it is and specifying a particular error message when it is not. */
    def okayE[E, A, B](error: FailedG[E] => ValidationError)(f: A => Validated[B]): ResultG[E, A] => Validated[B] = {
        case Okay(v)         => f(v)
        case f@FailedG(_, _) => Failure(nels(error(f)))
    }
}
