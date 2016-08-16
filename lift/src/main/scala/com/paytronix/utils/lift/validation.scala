//
// Copyright 2012-2014 Paytronix Systems, Inc.
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
import com.paytronix.utils.validation.base.{Validated, ValidationError, missingValueError, failure, success}

object validation {
    /** Apply some validation to the value inside a Box. */
    def boxed[A, B](f: A => Validated[B]): Box[A] => Validated[Box[B]] = (in: Box[A]) => in match {
        case Full(v)      => f(v).map(Full.apply)
        case Empty        => success(Empty)
        case (f: Failure) => success(f)
    }

    /** Require that a boxed value be present. */
    def full[A]: Box[A] => Validated[A] = {
        case Full(v) => success(v)
        // FIXME? deal with failure some other way? maybe ParamFailure carrying ValidationError should be unwrapped
        case _ => failure(missingValueError)
    }

    /** Require that a boxed value be present and apply a validation value to the function */
    def full[A, B](f: A => Validated[B]): Box[A] => Validated[B] = {
        case Full(v) => f(v)
        // FIXME? deal with failure some other way? maybe ParamFailure carrying ValidationError should be unwrapped
        case _ => failure(missingValueError)
    }

    /** Require that a boxed value be present applying a validation function when it is and specifying a particular error message when it is not. */
    def full[A, B](error: ValidationError)(f: A => Validated[B]): Box[A] => Validated[B] = {
            case Full(v) => f(v)
            // FIXME? deal with failure some other way? maybe ParamFailure carrying ValidationError should be unwrapped
            case _ => failure(error)
        }
}

