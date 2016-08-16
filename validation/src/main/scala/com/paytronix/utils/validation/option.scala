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

package com.paytronix.utils.validation

import base.{Validated, ValidationError, failure, missingValueError, nonMissingValueError, success}

object option {
    /** Apply some validation to the value inside an Option. */
    def optional[A, B](f: A => Validated[B]): Option[A] => Validated[Option[B]] = {
        case Some(a) => f(a).map(Some.apply)
        case None    => success(None)
    }

    /** Require that an optional value be present. */
    def some[A, B]: Option[A] => Validated[A] =
        someE(missingValueError)(success)

    /** Require that an optional value be present and apply a validation function to the value. */
    def some[A, B](f: A => Validated[B]): Option[A] => Validated[B] =
        someE(missingValueError)(f)

    /** Require that an optional value be present and apply a validation function to the value. */
    def someE[A, B](error: ValidationError)(f: A => Validated[B]): Option[A] => Validated[B] = {
        case Some(v) => f(v)
        case None    => failure(error)
    }

    /** Require that an optional value not be present. */
    val none: Option[Any] => Validated[Unit] =
        noneE(nonMissingValueError)

    /** Require that an optional value not be present. */
    def noneE(error: ValidationError): Option[Any] => Validated[Unit] = {
        case Some(v) => failure(error)
        case None    => success(())
    }

    /** Either unwrap the `Option` or substitute some default value. Similar to `getOrElse` */
    def defaultsTo[A](substitute: A): Option[A] => Validated[A] = {
        case Some(v) => success(v)
        case None    => success(substitute)
    }
}

