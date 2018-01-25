//
// Copyright 2014 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange.base

import scala.reflect.runtime.universe.{TypeTag, typeTag}

import com.paytronix.utils.scala.result.{FailedG, Result, tryCatchValue}

object enum {
    def enumerationInstance[A <: Enumeration: TypeTag]: Result[A] =
        tryCatchValue {
            typeTag[A].mirror
                .reflectModule(typeTag[A].tpe.termSymbol.asModule)
                .instance.asInstanceOf[A]
        }

    def enumerationValueFromString[A <: Enumeration](enum: A, in: String, out: Receiver[A#Value]): CoderResult[Unit] =
        enum.values.find(_.toString == in) match {
            case Some(e) => out(e)
            case None    => FailedG(s""""${in}" is not a valid enumeration value""", CoderFailure.terminal)
        }
}
