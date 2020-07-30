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

import scala.reflect.{ClassTag, classTag}

import scalaz.{Failure, Success}
import scalaz.NonEmptyList.nels

import base.{Validated, ValidationError}

object reflection {
    import string.nonBlank

    val invalidClassName = ValidationError("invalid_class_name", "invalid class name")

    def lookupError(e: Exception): ValidationError = ValidationError("unknown_error", "error while looking up class: " + e.toString)

    /** Assert that a String is nonblank and refers to a loadable class */
    def className[A: ClassTag](classLoader: ClassLoader): String => Validated[Class[_ <: A]] =
        classNameE[A](invalidClassName, lookupError)(classLoader)

    /** Assert that a String is nonblank and refers to a loadable class */
    def classNameE[A: ClassTag](unknownClassError: ValidationError, lookupError: Exception => ValidationError)(classLoader: ClassLoader): String => Validated[Class[_ <: A]] =
        nonBlank and { s =>
            try Success(Class.forName(s, true, classLoader).asSubclass(classTag[A].runtimeClass.asInstanceOf[Class[A]]))
            catch {
                case e: ClassNotFoundException => Failure(nels(unknownClassError))
                case e: Exception              => Failure(nels(lookupError(e)))
            }
        }
}
