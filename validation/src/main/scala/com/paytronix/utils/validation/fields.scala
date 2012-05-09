//
// Copyright 2010-2012 Paytronix Systems, Inc.
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

import values.{Validated, ValidationError, ValidationFunction}

/**
 * Build on validations for single values and HLists by allowing validations of fields, which can be composed together to form validations
 * of series of fields in a way that collects all errors, is type safe and compile time checked.
 *
 * Individual field validations are represented by instances of the FieldValidation class, which pairs a field name along with some Validated[A]
 * for the value of the field.
 *
 * Using value validations, you can validate a series of fields like this:
 *   validate (
 *       field("firstField", firstFieldValue is (nonEmpty and noLongerThan(3))) ::
 *       field("secondField", secondFieldValue is (anInt and greaterThan(0) and lessThan(10))) ::
 *       HNil
 *   ) match {
 *       case Left(errors) => // do something with FieldValidationErrors
 *       case Right(validatedFirstField :: validatedSecondField :: HNil) =>
 *           // do something with the valid inputs
 *   }
 *
 * Because field validations can be composed arbitrarily with :&:, you can do things such as validate some fields, then use those validated
 * values to validate more fields. For example, state/province depends on what country is selected, so you could validate country and then
 * state/province:
 *   validate (
 *       field("country", countryFieldValue is (...)) ::
 *       field("otherField", otherFieldValue is (...)) ::
 *       ...
 *       HNil
 *   ).right.flatMap {
 *       case country :: rest =>
 *           validate(field("stateProvince", stateProvinceValue is validStateOrProvinceFor(country)) :: rest)
 *   } match {
 *       case Left(errors) => // do something with errors
 *       case Right(stateProvince :: country :: otherField :: HNil) => ...
 *   }
 */
object fields {
    /** Attach a field name to any errors in a Validated value */
    def field[A](name: String, result: Validated[A]): Validated[A] =
        result match {
            case Left(errors) => Left(errors.map(_.nest(name)))
            case Right(value) => Right(value)
        }

    /** Wrap a ValidationFunction such that any errors it yields will have a field name added */
    def field[A, B](name: String, func: ValidationFunction[A, B]): ValidationFunction[A, B] =
        in => field(name, func(in))

    /** Implicitly extend a String with the "in" method for writing field validations as "field" in fieldContainer is nonBlank() */
    implicit def fieldNameOps(in: String): FieldNameOps = FieldNameOps(in)

    /** Extended operations on Strings that represent field names */
    final case class FieldNameOps(name: String) {
        /** Extract the value by name from the field container so it can be validated using "is" */
        def in[A](container: String => A): FieldValueOps[A] = FieldValueOps(name, container(name))

        /** Extract the value by name from the field container so it can be validated using "is" */
        def from[A](container: String => A): FieldValueOps[A] = FieldValueOps(name, container(name))
    }

    /** Container of a field name and its value to allow for "field" in container is validation... */
    final case class FieldValueOps[A](name: String, value: A) {
        /** Validate the value of the field, as validate(name, value is validation) */
        def is[B](validation: ValidationFunction[A, B]): Validated[B] = field(name, validation(value))
    }
}
