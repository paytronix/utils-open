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

import base.{ValidationError, ValidationFunction, missingValueError, nonMissingValueError}

object option {
    /** Apply some validation to the value inside an Option. */
    def optional[A, B](f: ValidationFunction[A, B]): ValidationFunction[Option[A], Option[B]] = (in: Option[A]) => in match {
        case Some(v) => f(v).right.map(Some.apply)
        case None => Right(None)
    }

    /** Require that an optional value be present. */
    def some[A, B]: ValidationFunction[Option[A], A] = {
        case Some(a) => Right(a)
        case None => Left(missingValueError :: Nil)
    }

    /** Require that an optional value be present and apply a validation function to the value. */
    def some[A, B](f: ValidationFunction[A, B]): ValidationFunction[Option[A], B] = {
        case Some(v) => f(v)
        case None => Left(missingValueError :: Nil)
    }

    /** Require that an optional value be present and apply a validation function to the value, specifying a particular error message if not. */
    def some[A, B](error: ValidationError)(f: ValidationFunction[A, B]): ValidationFunction[Option[A], B] = {
        case Some(v) => f(v)
        case None => Left(error :: Nil)
    }

    /** Require that an optional value not be present. */
    val none: ValidationFunction[Option[Any], Unit] = none(nonMissingValueError)

    /** Require that an optional value not be present. */
    def none(error: ValidationError): ValidationFunction[Option[Any], Unit] = _ match {
        case Some(v) => Left(error :: Nil)
        case None => Right(())
    }
}

