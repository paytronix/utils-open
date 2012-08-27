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

package com.paytronix.utils.validation

import com.paytronix.utils.scala.result.{FailedG, Okay, ResultG}

import base.{ValidationError, ValidationFunction, missingValueError}

object result {
    /** Apply some validation to the value inside a `Result`/`ResultG` */
    def result[E, A, B](f: ValidationFunction[A, B]): ValidationFunction[ResultG[E, A], ResultG[E, B]] = {
        case Okay(v) => f(v).right.map(Okay.apply)
        case f@FailedG(_, _) => Right(f)
    }

    /** Require that a result be Okay */
    def okay[E, A]: ValidationFunction[ResultG[E, A], A] = {
        case Okay(v) => Right(v)
        case f@FailedG(_, _) => Left(ValidationError(f.message) :: Nil)
    }

    /** Require that a result be Okay and apply a function to it */
    def okay[E, A, B](f: ValidationFunction[A, B]): ValidationFunction[ResultG[E, A], B] = {
        case Okay(v) => f(v)
        case f@FailedG(_, _) => Left(ValidationError(f.message) :: Nil)
    }

    /** Require that a result be Okay applying a validation function when it is and specifying a particular error message when it is not. */
    def okay[E, A, B](error: ValidationError)(f: ValidationFunction[A, B]): ValidationFunction[ResultG[E, A], B] = {
        case Okay(v) => f(v)
        case f@FailedG(_, _) => Left(error :: Nil)
    }
}
