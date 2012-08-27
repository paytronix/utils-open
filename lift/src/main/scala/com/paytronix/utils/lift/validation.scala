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
import com.paytronix.utils.validation.base.{ValidationError, ValidationFunction, missingValueError}

object validation {
    /** Apply some validation to the value inside a Box. */
    def boxed[A, B](f: ValidationFunction[A, B]): ValidationFunction[Box[A], Box[B]] = (in: Box[A]) => in match {
        case Full(v)      => f(v).right.map(Full.apply)
        case Empty        => Right(Empty)
        case (f: Failure) => Right(f)
    }

    /** Require that a boxed value be present. */
    def full[A]: ValidationFunction[Box[A], A] = {
        case Full(v) => Right(v)
        // FIXME? deal with failure some other way? maybe ParamFailure carrying ValidationError should be unwrapped
        case _ => Left(missingValueError :: Nil)
    }

    /** Require that a boxed value be present and apply a validation value to the function */
    def full[A, B](f: ValidationFunction[A, B]): ValidationFunction[Box[A], B] = {
        case Full(v) => f(v)
        // FIXME? deal with failure some other way? maybe ParamFailure carrying ValidationError should be unwrapped
        case _ => Left(missingValueError :: Nil)
    }

    /** Require that a boxed value be present applying a validation function when it is and specifying a particular error message when it is not. */
    def full[A, B](error: ValidationError)(f: ValidationFunction[A, B]): ValidationFunction[Box[A], B] = {
            case Full(v) => f(v)
            // FIXME? deal with failure some other way? maybe ParamFailure carrying ValidationError should be unwrapped
            case _ => Left(error :: Nil)
        }
}

