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

import scala.collection.breakOut

import base.{Validated, ValidationError, failure, success}

object enumeration {
    def invalidEnumerationError(incorrect: String): ValidationError =
        ValidationError("invalid_enumeration", s"""value "$incorrect" is not an allowed option for this field""")

    /**
     * Apply a partial function to the input only if it is defined, yielding the result of the application.
     * Fails validation if the partial function isn't defined.
     */
    def definedIn[A, B](pf: PartialFunction[A, B]): A => Validated[B] =
        definedInE[A, B](a => invalidEnumerationError(String.valueOf(a)))(pf)

    /**
     * Apply a partial function to the input only if it is defined, yielding the result of the application.
     * Fails validation if the partial function isn't defined.
     */
    def definedInE[A, B](error: A => ValidationError)(pf: PartialFunction[A, B]): A => Validated[B] =
        in => if (pf.isDefinedAt(in)) success(pf(in)) else failure(error(in))

    /**
     * Assert that the value is a member of the given enumeration, and yield the enumeration value if successful.
     * NOTE: Scala's inference of the type of singletons is heinously awful at best.
     * If you get wonky errors like type mismatch MyEnum#Value versus MyEnum.Value, then explicitly pass the enumeration type:
     *    valueOf[MyEnum.type](MyEnum)
     */
    def valueOf[A <: Enumeration](enum: A): String => Validated[A#Value] =
        valueOfE[A](invalidEnumerationError)(enum)

    /**
     * Assert that the value is a member of the given enumeration, and yield the enumeration value if successful.
     * NOTE: Scala's inference of the type of singletons is heinously awful at best.
     * If you get wonky errors like type mismatch MyEnum#Value versus MyEnum.Value, then explicitly pass the enumeration type:
     *    valueOf[MyEnum.type](MyEnum)
     */
    def valueOfE[A <: Enumeration](error: String => ValidationError)(enum: A): String => Validated[A#Value] =
        definedInE(error)(enumeration(enum))

    /** Assert that the value is a member of the given enumeration, and yield the enumeration value if successful. */
    def valueOfJavaEnum[A <: Enum[A]](enumClass: Class[A]): String => Validated[A] =
        valueOfJavaEnumE(invalidEnumerationError)(enumClass)

    /** Assert that the value is a member of the given enumeration, and yield the enumeration value if successful. */
    def valueOfJavaEnumE[A <: Enum[A]](error: String => ValidationError)(enumClass: Class[A]): String => Validated[A] =
        definedInE(error)(enumeration(enumClass))

    /**
     * Helper implict which converts any Enumeration into a PartialFunction from String.
     */
    def enumeration[A <: Enumeration](enum: A): PartialFunction[String, A#Value] =
        enum.values.map(v => v.toString -> v)(breakOut): Map[String, A#Value]

    /**
     * Helper implict which converts any Java enum into a PartialFunction from String.
     */
    def enumeration[A <: Enum[A]](enumClass: Class[A]): PartialFunction[String, A] = {
        // bah.
        val meth = enumClass.getMethod("values")
        Map(meth.invoke(null).asInstanceOf[Array[A]].map(v => v.name -> v): _*)
    }
}
