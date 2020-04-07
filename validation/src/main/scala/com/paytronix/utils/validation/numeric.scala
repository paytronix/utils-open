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

import cats.kernel.Order

import base.{Validated, ValidationError, failure, missingValueError, predicateE, success}
import ordered.{tooSmallError, tooLargeError}

object numeric {
    val nonPositiveError                  = ValidationError("invalid_negative_or_zero", "positive value required")
    val nonNegativeError                  = ValidationError("invalid_positive_or_zero", "negative value required")
    val negativeError                     = ValidationError("invalid_negative", "non-negative value required")
    val positiveError                     = ValidationError("invalid_positive", "non-positive value required")

    private def toOrder[A](implicit o: Numeric[A]): Order[A] = Order.fromOrdering(o)

    /** Assert that some value is ordered greater than some value (> x) */
    def greaterThan[A: Numeric](minimum: A): A => Validated[A] =
        ordered.greaterThan[A](minimum)(toOrder)

    /** Assert that some value is ordered greater than some value (> x) */
    def greaterThanE[A: Numeric](error: A => ValidationError)(minimum: A): A => Validated[A] =
        ordered.greaterThanE[A](error)(minimum)(toOrder)

    /** Assert that some value is not ordered less than some value (>= x) */
    def noLessThan[A: Numeric](minimum: A): A => Validated[A] =
        ordered.noLessThan[A](minimum)(toOrder)

    /** Assert that some value is not ordered less than some value (>= x) */
    def noLessThanE[A: Numeric](error: A => ValidationError)(minimum: A): A => Validated[A] =
        ordered.noLessThanE[A](error)(minimum)(toOrder)

    /** Assert that some value is ordered lesser than some value (< x) */
    def lessThan[A: Numeric](maximum: A): A => Validated[A] =
        ordered.lessThan[A](maximum)(toOrder)

    /** Assert that some value is ordered lesser than some value (< x) */
    def lessThanE[A: Numeric](error: A => ValidationError)(maximum: A): A => Validated[A] =
        ordered.lessThanE[A](error)(maximum)(toOrder)

    /** Assert that some value is not ordered lesser than some value (<= x) */
    def noGreaterThan[A: Numeric](maximum: A): A => Validated[A] =
        ordered.noGreaterThan[A](maximum)(toOrder)

    /** Assert that some value is not ordered lesser than some value (<= x) */
    def noGreaterThanE[A: Numeric](error: A => ValidationError)(maximum: A): A => Validated[A] =
        ordered.noGreaterThanE[A](error)(maximum)(toOrder)

    /** Assert that some number is positive (> 0) */
    def positive[A: Numeric]: A => Validated[A] =
        positiveE(nonPositiveError)

    /** Assert that some number is positive (> 0) */
    def positiveE[A: Numeric](error: ValidationError): A => Validated[A] =
        predicateE(error)(a => implicitly[Numeric[A]].signum(a) > 0)

    /** Assert that some number is negative (< 0) */
    def negative[A: Numeric]: A => Validated[A] =
        negativeE(nonNegativeError)

    /** Assert that some number is negative (< 0) */
    def negativeE[A: Numeric](error: ValidationError): A => Validated[A] =
        predicateE(error)(a => implicitly[Numeric[A]].signum(a) < 0)

    /** Assert that some number is non-negative (>= 0) */
    def nonNegative[A: Numeric]: A => Validated[A] =
        nonNegativeE(negativeError)

    /** Assert that some number is non-negative (>= 0) */
    def nonNegativeE[A: Numeric](error: ValidationError): A => Validated[A] =
        predicateE(error)(a => implicitly[Numeric[A]].signum(a) >= 0)

    /** Assert that some number is non-positive (<= 0) */
    def nonPositive[A: Numeric]: A => Validated[A] =
        nonPositiveE(nonNegativeError)

    /** Assert that some number is non-positive (<= 0) */
    def nonPositiveE[A: Numeric](error: ValidationError): A => Validated[A] =
        predicateE(error)(a => implicitly[Numeric[A]].signum(a) <= 0)

    import string.{nonBlankE, numericWithSignE, numericWithSignAndDecimalE, nonNumericError}

    /** Assert that some string is a well formatted number, converting it using some parsing function */
    def numberE[A] (
        missingValue: ValidationError,
        malformatted: ValidationError,
        underflow: A => ValidationError,
        overflow: A => ValidationError
    )(
        parse: String => A, // expected to throw NumberFormatException on overflow and underflow
        minValue: A,
        maxValue: A
    ): String => Validated[A] = nonBlankE(missingValue) and numericWithSignE(malformatted) and { s =>
        try success(parse(s)) catch { case _: NumberFormatException =>
            failure(if (s.startsWith("-")) underflow(minValue) else overflow(maxValue))
        }
    }

    /** Assert that some string is a well formatted decimal number, converting it */
    def numberWithDecimalE[A] (
        missingValue: ValidationError,
        malformatted: ValidationError,
        underflow: A => ValidationError,
        overflow: A => ValidationError
    )(
        parse: String => A, // expected to throw NumberFormatException on overflow and underflow
        minValue: A,
        maxValue: A
    ): String => Validated[A] = nonBlankE(missingValue) and numericWithSignAndDecimalE(malformatted) and { s =>
        try success(parse(s)) catch { case _: NumberFormatException =>
            failure(if (s.startsWith("-")) underflow(minValue) else overflow(maxValue))
        }
    }


    import java.lang.{Byte => JLByte, Short => JLShort, Integer => JLInteger, Long => JLLong, Float => JLFloat, Double => JLDouble}

    /** Assert that a string is a well formatted byte and convert it */
    val byte: String => Validated[Byte] =
        byteE(missingValueError, nonNumericError, tooSmallError, tooLargeError)

    /** Assert that a string is a well formatted byte and convert it */
    def byteE(missingValue: ValidationError, malformatted: ValidationError, underflow: Byte => ValidationError, overflow: Byte => ValidationError): String => Validated[Byte] =
        numberE(missingValue, malformatted, underflow, overflow)(s => JLByte.parseByte(s): JLByte, JLByte.MIN_VALUE, JLByte.MAX_VALUE)

    /** Assert that a string is a well formatted short and convert it */
    val short: String => Validated[Short] =
        shortE(missingValueError, nonNumericError, tooSmallError, tooLargeError)

    /** Assert that a string is a well formatted short and convert it */
    def shortE(missingValue: ValidationError, malformatted: ValidationError, underflow: Short => ValidationError, overflow: Short => ValidationError): String => Validated[Short] =
        numberE(missingValue, malformatted, underflow, overflow)(s => JLShort.parseShort(s): JLShort, JLShort.MIN_VALUE, JLShort.MAX_VALUE)

    /** Assert that a string is a well formatted int and convert it */
    val int: String => Validated[Int] =
        intE(missingValueError, nonNumericError, tooSmallError, tooLargeError)

    /** Assert that a string is a well formatted int and convert it */
    def intE(missingValue: ValidationError, malformatted: ValidationError, underflow: Int => ValidationError, overflow: Int => ValidationError): String => Validated[Int] =
        numberE(missingValue, malformatted, underflow, overflow)(s => JLInteger.parseInt(s): JLInteger, JLInteger.MIN_VALUE, JLInteger.MAX_VALUE)

    /** Assert that a string is a well formatted long and convert it */
    val long: String => Validated[Long] =
        longE(missingValueError, nonNumericError, tooSmallError, tooLargeError)

    /** Assert that a string is a well formatted long and convert it */
    def longE(missingValue: ValidationError, malformatted: ValidationError, underflow: Long => ValidationError, overflow: Long => ValidationError): String => Validated[Long] =
        numberE(missingValue, malformatted, underflow, overflow)(s => JLLong.parseLong(s): JLLong, JLLong.MIN_VALUE, JLLong.MAX_VALUE)

    /** Assert that a string is a well formatted float and convert it */
    val float: String => Validated[Float] =
        floatE(missingValueError, nonNumericError, tooSmallError, tooLargeError)

    /** Assert that a string is a well formatted float and convert it */
    def floatE(missingValue: ValidationError, malformatted: ValidationError, underflow: Float => ValidationError, overflow: Float => ValidationError): String => Validated[Float] =
        numberWithDecimalE(missingValue, malformatted, underflow, overflow)(s => JLFloat.parseFloat(s): JLFloat, JLFloat.MIN_VALUE, JLFloat.MAX_VALUE)

    /** Assert that a string is a well formatted double and convert it */
    val double: String => Validated[Double] =
        doubleE(missingValueError, nonNumericError, tooSmallError, tooLargeError)

    /** Assert that a string is a well formatted double and convert it */
    def doubleE(missingValue: ValidationError, malformatted: ValidationError, underflow: Double => ValidationError, overflow: Double => ValidationError): String => Validated[Double] =
        numberWithDecimalE(missingValue, malformatted, underflow, overflow)(s => JLDouble.parseDouble(s): JLDouble, JLDouble.MIN_VALUE, JLDouble.MAX_VALUE)

    /** Assert that a string is a well formatted integer value */
    val bigInt: String => Validated[BigInt] =
        bigIntE(missingValueError, nonNumericError)

    /** Assert that a string is a well formatted integer value */
    def bigIntE(missingValue: ValidationError, malformatted: ValidationError): String => Validated[BigInt] =
        nonBlankE(missingValue) and numericWithSignE(malformatted) and { s =>
            try success(BigInt(s)) catch { case _: NumberFormatException => failure(malformatted) }
        }

    /** Assert that a string is a well formatted decimal value */
    val bigDecimal: String => Validated[BigDecimal] =
        bigDecimalE(missingValueError, nonNumericError)

    /** Assert that a string is a well formatted decimal value */
    def bigDecimalE(missingValue: ValidationError, malformatted: ValidationError): String => Validated[BigDecimal] =
        nonBlankE(missingValue) and numericWithSignAndDecimalE(malformatted) and { s =>
            try success(BigDecimal(s)) catch { case _: NumberFormatException => failure(malformatted) }
        }
}
