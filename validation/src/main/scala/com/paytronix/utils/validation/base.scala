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

import shapeless.{::, HList, HNil, Poly2, RightFolder}

/**
 * Basic primitives for composing validations of values that can transform the value as it's being validated (e.g. instead of just testing a
 * value is numeric, convert it to an int as well).
 *
 * The fundamental type being used is `Validated[A]`, which is a type alias for `Either[List[ValidationError], A]` where the Left side indicates
 * the value did not pass validation and contains one or more errors for the value, and the Right side indicates successful validation.
 *
 * Functions which take some value and produce a validated one are called `ValidationFunction[A, B]` with `A` being the input type (e.g. `String`) and
 * `B` being the output type (e.g. `Int`). Most validation functions have the same type, and do not perform any particular modification of the value.
 * For example, a function that validates the length of a string would be of type `ValidationFunction[String, String]`.
 *
 * Such functions can be composed together using the convenience operator `and`, for example:
 * {{{
 *     nonBlank() and int() and greaterThan(0)
 * }}}
 *
 * Where `nonEmpty` validates a string is not empty, `int` validates a string is a valid integer and converts it to an `Int`, and `greaterThan` tests
 * that an `Int` is not less than or equal to a particular value.
 *
 * Validations can be applied to values via the `is` operator (provided by the implicit `valueOps`):
 * {{{
 *     Some("foobar") is (nonBlank() and noLongerThan(3)) == Right("foobar")
 * }}}
 */
object base {
    /** The type of a validated value. `Left(errors)` indicates the value did not pass validation, and `Right(v)` indicates it did pass. */
    type Validated[+A] = Either[List[ValidationError], A]

    /** The type of functions that validate values of type `A`, yielding possibly modified values of type `B` */
    type ValidationFunction[-A, +B] = A => Validated[B]

    /**
     * Type of validation errors, which contain some error code, potentially some arguments for formatting the error code, and the
     * formatted version.
     */
    trait ValidationError {
        /** Path to the possibly-nested error. `Nil` for a scalar value. See the `field` function in [[com.paytronix.utils.validation.fields]]  */
        val location: List[String]

        /** Make a new `ValidationError` with the given segment prepended to the location, indicating it is for a nested value */
        def nest(segment: String): ValidationError = {
            val outer = this
            new ValidationError {
                val location = segment :: outer.location
                val code = outer.code
                val text = outer.text
                val invalidInput = outer.invalidInput
            }
        }

        /** Some regular code string, e.g. "null_field" indicating what type of error */
        val code: String

        /** A human readable default equivalent in english */
        def text: String

        /** A textual representation of the invalid input value, or `None` if the input value is too complex to display in text */
        def invalidInput: Option[String]

        /** Supply the textual representation of the invalid input value */
        def withInvalidInput(s: String): ValidationError = {
            val outer = this
            new ValidationError {
                val location = outer.location
                val code = outer.code
                val text = outer.text
                override val invalidInput = Some(s)
            }
        }

        /** Default `toString` makes things nicer */
        override def toString: String = (
            (location match {
                case Nil => ""
                case _ => "At " + location.mkString(".") + ": "
            }) + code + ": " + text
        )
    }

    object ValidationError {
        /** Create a validation error with no code value */
        def apply(_message: String): ValidationError = new ValidationError {
            val location = Nil
            val code = _message
            val text = _message
            val invalidInput = None
        }

        /** Create a validation error with no formattable parameters */
        def apply(_code: String, _message: String): ValidationError = new ValidationError {
            val location = Nil
            val code = _code
            val text = _message
            val invalidInput = None
        }

        /** Create a validation error with some formatting parameters using `String.format` */
        def apply(_code: String, _format: String, args: Any*): ValidationError = new ValidationError {
            val location = Nil
            val code = _code
            val text = _format.format(args: _*)
            val invalidInput = None
        }

        /** Extract the code and text from a `ValidationError` */
        def unapply(in: ValidationError): Option[(String, String)] = Some((in.code, in.text))
    }

    /**
     * Combine a series of `Validated` values from a `HList` into a single `Validated` value containing either the validated
     * values in a `HList`, or all the validation errors encountered.
     *
     * For example:
     * {{{
     *     validate (
     *         field("foo", "foo" is nonBlank()) ::
     *         field("bar", 1 is positive()) ::
     *         HNil
     *     )
     *     == Right("foo" :: 1 :: HNil): Validated[String :: Int :: HNil]
     *
     *     validate (
     *         field("foo", "foo" is nonBlank()) ::
     *         field("bar", -1 is positive()) ::
     *         HNil
     *     )
     *     == Left(List(ValidationError("At bar: invalid_negative_or_zero: positive value required"))
     * }}}
     */
    def validate[L <: HList](in: L)(implicit folder: RightFolder[L, Validated[HNil], combineValidated.type]): folder.Out =
        in.foldRight(Right(HNil): Validated[HNil])(combineValidated)

    object combineValidated extends Poly2 {
        implicit def caseValidatedValidated[A, B <: HList] = at[Validated[A], Validated[B]] {
            (l, r) => (l, r) match {
                case (Left(newErrors), Left(existingErrors) ) => Left(newErrors ++ existingErrors): Validated[A :: B]
                case (Left(newErrors), _                    ) => Left(newErrors): Validated[A :: B]
                case (_,               Left(existingErrors) ) => Left(existingErrors): Validated[A :: B]
                case (Right(newValue), Right(existingValues)) => Right(newValue :: existingValues): Validated[A :: B]
            }
        }
    }

    /** Attach a field name to any errors in a `Validated` value */
    def field[A](name: String, result: Validated[A]): Validated[A] =
        result match {
            case Left(errors) => Left(errors.map(_.nest(name)))
            case Right(value) => Right(value)
        }

    /** Wrap a `ValidationFunction` such that any errors it yields will have a field name added */
    def field[A, B](name: String, func: ValidationFunction[A, B]): ValidationFunction[A, B] =
        in => field(name, func(in))

    /** Convert a list of `ValidationError`s to a map by field name */
    def validationErrorsToMap(in: List[ValidationError]): Map[List[String], List[ValidationError]] =
        in.foldLeft(Map.empty[List[String], List[ValidationError]].withDefaultValue(Nil))((m, ve) => m + (ve.location -> (ve :: m(ve.location))))

    /**
     * Convert a list of `ValidationError`s to error text, suitable for display to a console.
     *
     * Default formatting is like:
     * {{{
     *   field 1: error 1
     *            error 2
     *   field 2: error 1
     *   field three: error 1
     *                error 2
     * }}}
     *
     * but can be customized:
     *
     *   `groupSeparator` goes between each group. if bundled is true then there is a group per field. if `false`, then per error. defaults to "\n"
     *   `fieldSeparator` goes between the field and the error text. defaults to ": "
     *   `errorSeparator` goes between each error. defaults to "\n"
     *   `locationSeparator` goes between each component of the field location.
     *   `invalidInputPrefix` goes before the invalid input, if `withInvalidInputs` is `true`.
     *   `invalidInputSuffix` goes after the invalid input, if `withInvalidInputs` is `true`.
     *   `indented` controls whether an automatic indent before each error after the first for a given field is inserted, and only works when bundled is `true` (the default)
     *   `bundled` indicates whether multiple errors for a single field will be bundled together under a single field heading. defaults to `true`
     *   `withInvalidInputs` indicates whether the original input value (if available) will be added to the end of each error message. defaults to `true`
     *
     * Another example that emits a single line output:
     * {{{
     *   validationErrorsToString(..., groupSeparator=". ", fieldSeparator=": ", errorSeparator="; ", indented=false) + "."
     *   == "field 1: error 1; error 2. field 2: error1. field three: error 1; error 2."
     * }}}
     */
    def validationErrorsToString (
        in: List[ValidationError],
        groupSeparator: String = "\n",
        fieldSeparator: String = ": ",
        errorSeparator: String = "\n",
        locationSeparator: String = ".",
        invalidInputPrefix: String = " (invalid input was: ",
        invalidInputSuffix: String = ")",
        indented: Boolean = true,
        bundled: Boolean = true,
        withInvalidInputs: Boolean = true
    ): String = {
        val m = validationErrorsToMap(in)

        (
            for ((key, keyString) <- m.keys.toSeq.map(k => (k, k.mkString(locationSeparator))).sortBy(_._2))
            yield {
                val errors = m(key).map { error =>
                    error.text + (error.invalidInput match {
                        case Some(s) if withInvalidInputs => invalidInputPrefix + s + invalidInputSuffix
                        case _ => ""
                    })
                }

                val prefix = if (keyString != "") keyString + fieldSeparator else ""

                if (bundled) {
                    prefix + errors.mkString(errorSeparator + (if (indented) (" " * prefix.length) else ""))
                } else {
                    errors.map(prefix + _).mkString(errorSeparator)
                }
            }
        ).mkString(groupSeparator)
    }

    /** Validation error for missing values */
    val missingValueError = ValidationError("null_field", "value is required")

    /** Validation error for values that are present but shouldn't be */
    val nonMissingValueError = ValidationError("non_null_field", "value should not be present")

    /** The ValidationError given when no more specific error is available */
    val generalError = ValidationError("invalid_value", "value is invalid")

    /**
     * Apply some validation but if it succeeds ignore the result and use the input.
     * That is, just assert the condition, but do not use the conversion of some validation
     */
    def onlyAssert[A](f: ValidationFunction[A, _]): ValidationFunction[A, A] =
        in => f(in).right.map(_ => in)

    /** Apply a validation only if a boolean is true */
    def when[A](condition: Boolean)(f: ValidationFunction[A, A]): ValidationFunction[A, A] =
        in => if (condition) f(in)
              else Right(in)

    /** Extend a `ValidationFunction` with the `and` combinator, which is provided by the wrapper `ValidationFunctionOps` */
    implicit def validationFunctionOps[A, B](f: ValidationFunction[A, B]): ValidationFunctionOps[A, B] =
        ValidationFunctionOps(f)

    /** Extension of `ValidationFunction` that provides composition via `and` */
    final case class ValidationFunctionOps[A, B](f: ValidationFunction[A, B]) extends ValidationFunction[A, B] {
        def apply(a: A): Validated[B] = f(a)

        /** Compose a validation function with another, from left to right */
        def and[C](rhs: ValidationFunction[B, C]): ValidationFunction[A, C] =
            in => f(in).right.flatMap(rhs)
    }

    /** Implicitly extend any value with an `is` operator that can be used to apply a validation function to the value */
    implicit def valueOps[A](in: A): ValueOps[A] = ValueOps(in)

    /** `ValueOps` wraps some value and provides additional operators for validation */
    final case class ValueOps[A](a: A) {
        /** `value is (validationFunction)` applies the given validation(s) to the value */
        def is[B](f: ValidationFunction[A, B]): Validated[B] = f(a)

        /** alias for `is` that works better for plurals */
        def are[B](f: ValidationFunction[A, B]): Validated[B] = f(a)
    }

     /** Implicitly extend a `String` with the `in` operator for writing field validations, e.g. `"field" in fieldContainer is nonBlank()` */
    implicit def fieldNameOps(in: String): FieldNameOps = FieldNameOps(in)

    /** Extended operations on `String`s that represent field names */
    final case class FieldNameOps(name: String) {
        /** Extract the value by name from the field container so it can be validated using `is` */
        def in[A](container: String => A): FieldValueOps[A] = FieldValueOps(name, container(name))

        /** Extract the value by name from the field container so it can be validated using `is` */
        def from[A](container: String => A): FieldValueOps[A] = FieldValueOps(name, container(name))
    }

    /** Container of a field name and its value to allow for `"field" in container is validation...` */
    final case class FieldValueOps[A](name: String, value: A) {
        /** Validate the value of the field, as `field(name, value is validation)` */
        def is[B](validation: ValidationFunction[A, B]): Validated[B] = field(name, validation(value))
    }
}

