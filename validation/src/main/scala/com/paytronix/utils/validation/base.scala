//
// Copyright 2010-2014 Paytronix Systems, Inc.
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

import scalaz.{@@, NonEmptyList, Tag, Failure, Success, ValidationNel}
import scalaz.Id.Id
import scalaz.Kleisli.kleisli
import scalaz.Tags.First
import scalaz.std.option.optionFirst
import scalaz.std.string.stringInstance
import scalaz.syntax.foldable.ToFoldableOps /* .foldLeft, .intercalate */
import shapeless.{::, HList, HNil, Poly2}
import shapeless.ops.hlist.RightFolder

import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, ResultG}

import NonEmptyList.nels

/**
 * Basic primitives for composing validations of values that can transform the value as it's being validated (e.g. instead of just testing a
 * value is numeric, convert it to an int as well).
 *
 * The fundamental type being used is `Validated[A]`, which is a type alias for `Either[List[ValidationError], A]` where the Left side indicates
 * the value did not pass validation and contains one or more errors for the value, and the Right side indicates successful validation.
 *
 * Functions which take some value and produce a validated one are called `A => Validated[B]` with `A` being the input type (e.g. `String`) and
 * `B` being the output type (e.g. `Int`). Most validation functions have the same type, and do not perform any particular modification of the value.
 * For example, a function that validates the length of a string would be of type `String => Validated[String]`.
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
    type Validated[+A] = ValidationNel[ValidationError, A]

    /** Type of field paths, represented by a list of field names. Empty list indicates a scalar value */
    type ErrorPath = List[String]

    /**
     * Type of validation errors, which contain some error code, potentially some arguments for formatting the error code, and the
     * formatted version.
     */
    final case class ValidationError (
        /** Some regular code string, e.g. "null_field" indicating what type of error */
        code: String,
        /** A human readable default equivalent in english */
        text: String,
        /** Path to the possibly-nested error. `Nil` for a scalar value. See the `field` function in [[com.paytronix.utils.validation.fields]]  */
        location: ErrorPath = Nil,
        /** A textual representation of the invalid input value, or `None` if the input value is too complex to display in text */
        invalidInput: Option[String] = None
    ) {
        /** Make a new `ValidationError` with the given segment prepended to the location, indicating it is for a nested value */
        def nest(segment: String): ValidationError =
            copy(location = segment :: location)

        /** Supply the textual representation of the invalid input value */
        def withInvalidInput(s: String): ValidationError =
            copy(invalidInput = Some(s))

        override def toString: String = (
            (location match {
                case Nil => ""
                case _ => "At " + location.mkString("/") + ": "
            }) + code + ": " + text
        )
    }

    object ValidationError {
        /** Create a validation error with no code value */
        def apply(message: String): ValidationError =
            ValidationError(message, message)
    }

    /** Make a `Success` with a value */
    def success[A](value: A): Validated[A] =
        Success(value)

    /** Make a `Failure` with a `NonEmptyList` of the given errors */
    def failure(error: ValidationError, errors: ValidationError*): Validated[Nothing] =
        Failure(nels(error, errors: _*))

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
     *     == Success("foo" :: 1 :: HNil): Validated[String :: Int :: HNil]
     *
     *     validate (
     *         field("foo", "foo" is nonBlank()) ::
     *         field("bar", -1 is positive()) ::
     *         HNil
     *     )
     *     == Failure(NonEmptyList(ValidationError("At bar: invalid_negative_or_zero: positive value required"))
     * }}}
     */
    def validate[L <: HList](in: L)(implicit folder: RightFolder[L, Validated[HNil], combineValidated.type]): folder.Out =
        in.foldRight(Success(HNil): Validated[HNil])(combineValidated)

    object combineValidated extends Poly2 {
        implicit def caseValidatedValidated[A, B <: HList] = at[Validated[A], Validated[B]] { (l, r) =>
            identity[Validated[A :: B]] {
                (l, r) match {
                    case (Failure(newErrors), Failure(existingErrors)) => Failure(newErrors append existingErrors)
                    case (Failure(newErrors), _                      ) => Failure(newErrors)
                    case (_,                  Failure(existingErrors)) => Failure(existingErrors)
                    case (Success(newValue),  Success(existingValues)) => Success(newValue :: existingValues)
                }
            }
        }
    }

    /** Attach a field name to any errors in a `Validated` value */
    def field[A](name: String, result: Validated[A]): Validated[A] =
        result.leftMap(_.map(_.nest(name)))

    /** Attach a field name to any errors in a `Validated` value */
    def field[A, B](name: String, func: A => Validated[B]): A => Validated[B] =
        in => field(name, func(in))

    /** Wrap a validation function `A => Validated[B]` such that any errors it yields will have a field name added */
    def field[A, B >: A, C](name: String, container: String => A, func: B => Validated[C]): Validated[C] =
        field(name, func(container(name)))

    type ValidationErrorMap = Map[ErrorPath, NonEmptyList[ValidationError]]

    /** Convert a list of `ValidationError`s to a map by field name */
    def validationErrorsToMap(in: NonEmptyList[ValidationError]): ValidationErrorMap =
        in.foldLeft[ValidationErrorMap](Map.empty) { (m, ve) =>
            val errors = m.get(ve.location) match {
                case Some(errors) => ve <:: errors
                case None         => nels(ve)
            }
            m + (ve.location -> errors)
        }

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
        in: NonEmptyList[ValidationError],
        groupSeparator: String = "\n",
        fieldSeparator: String = ": ",
        errorSeparator: String = "\n",
        locationSeparator: String = "/",
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
                    prefix + errors.intercalate(errorSeparator + (if (indented) (" " * prefix.length) else ""))
                } else {
                    errors.map(prefix + _).intercalate(errorSeparator)
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
    def onlyAssert[A](f: A => Validated[_]): A => Validated[A] =
        in => f(in).map(_ => in)

    /** Apply a validation only if a boolean is true */
    def when[A](condition: Boolean)(f: A => Validated[A]): A => Validated[A] =
        in => if (condition) f(in)
              else Success(in)

    /** Apply some predicate function, failing validation if the predicate fails */
    def predicateE[A](error: ValidationError)(p: A => Boolean): A => Validated[A] =
        a => if (p(a)) Success(a) else Failure(nels(error))

    /** Accept the value with the first validation function `A => Validated[B]` which doesn't fail */
    def any[A, B](fs: NonEmptyList[A => Validated[B]]) =
        anyE(generalError)(fs)

    /** Accept the value with the first validation function `A => Validated[B]` which doesn't fail */
    def anyE[A, B](error: ValidationError)(fs: NonEmptyList[A => Validated[B]]): A => Validated[B] = {
        def unFirst[A](in: A @@ First): A = Tag.unsubst[A, Id, First](in)
        a => unFirst(fs.foldMap(f => First(f(a).toOption))) match {
            case Some(result) => Success(result)
            case None         => Failure(nels(error))
        }
    }

    /** Extend a validation kleisli `A => Validated[B]` with the `and` combinator, equivalent to reversed kleisli composition */
    implicit class validationFunctionOps[A, B](f: A => Validated[B]) {
        /** Compose a validation function with another, from left to right */
        def and[C](g: B => Validated[C]): A => Validated[C] =
            a => f(a).flatMap(g)

        /** Map the output value of the validation function */
        def map[C](g: B => C): A => Validated[C] =
            a => f(a).map(g)
    }

    /** Extend a `Validated[A]` with the `and` combinator */
    implicit class validatedOps[A](lhs: Validated[A]) {
        /** Apply a `ValidationFunction` to a `Validated` value */
        def and[B](rhs: A => Validated[B]): Validated[B] =
            lhs.flatMap(rhs)

        /** Convert the `Validated` to a `ResultG` containing the same information */
        def toResultG: ResultG[NonEmptyList[ValidationError], A] =
            lhs match {
                case Success(a)      => Okay(a)
                case Failure(errors) => FailedG("validation failed", errors)
            }

        /** Convert the `Validated` to a simple `Result` with all validation errors packed into the message */
        def toResult: Result[A] =
            lhs match {
                case Success(a) => Okay(a)
                case Failure(errors) => Failed(validationErrorsToString(errors))
            }

        /** Yield the `Success` value or throw an exception with the validation errors as the message */
        def orThrow: A =
            toResult.orThrow
    }

    /** Implicitly extend any value with an `is` operator that can be used to apply a validation function to the value */
    implicit class valueOps[A](lhs: A) {
        /** `value is (validationFunction)` applies the given validation(s) to the value */
        def is[B](f: A => Validated[B]): Validated[B] = f(lhs)

        /** alias for `is` that works better for plurals */
        def are[B](f: A => Validated[B]): Validated[B] = is(f)
    }
}

