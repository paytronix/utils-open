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

import scala.util.control.Exception.catching

import base.{ValidationError, ValidationFunction, missingValueError}

object numeric {
    val nonPositiveError                  = ValidationError("invalid_negative_or_zero", "positive value required")
    val nonNegativeError                  = ValidationError("invalid_positive_or_zero", "negative value required")
    val negativeError                     = ValidationError("invalid_negative", "non-negative value required")
    val positiveError                     = ValidationError("invalid_positive", "non-positive value required")
    def tooSmallErrorExclusive(s: String) = ValidationError("underflow", "must greater than %s", s)
    def tooSmallError(s: String)          = ValidationError("underflow", "must greater than or equal to %s", s)
    def tooLargeErrorExclusive(s: String) = ValidationError("overflow", "must less than %s", s)
    def tooLargeError(s: String)          = ValidationError("overflow", "must less than or equal to %s", s)

    /** Assert that some number is greater than some value (> x) */
    def greaterThan[A](minimum: A, error: ValidationError = null)(implicit num: Numeric[A]): ValidationFunction[A, A] = in =>
        if (num.gt(in, minimum))   Right(in)
        else if (error == null)    Left(tooSmallErrorExclusive(minimum.toString) :: Nil)
        else                       Left(error :: Nil)

    /** Assert that some number is not less than some value (>= x) */
    def noLessThan[A](minimum: A, error: ValidationError = null)(implicit num: Numeric[A]): ValidationFunction[A, A] = in =>
        if (num.gteq(in, minimum)) Right(in)
        else if (error == null)    Left(tooSmallError(minimum.toString) :: Nil)
        else                       Left(error :: Nil)

    /** Assert that some number is greater than some value (< x) */
    def lessThan[A](maximum: A, error: ValidationError = null)(implicit num: Numeric[A]): ValidationFunction[A, A] = in =>
        if (num.lt(in, maximum))   Right(in)
        else if (error == null)    Left(tooSmallErrorExclusive(maximum.toString) :: Nil)
        else                       Left(error :: Nil)

    /** Assert that some number is not less than some value (<= x) */
    def noGreaterThan[A](maximum: A, error: ValidationError = null)(implicit num: Numeric[A]): ValidationFunction[A, A] = in =>
        if (num.lteq(in, maximum)) Right(in)
        else if (error == null)    Left(tooSmallError(maximum.toString) :: Nil)
        else                       Left(error :: Nil)

    /** Assert that some number is positive (> 0) */
    def positive[A](error: ValidationError = nonPositiveError)(implicit num: Numeric[A]): ValidationFunction[A, A] =
        in => if (num.signum(in) > 0) Right(in)
              else Left(error :: Nil)

    /** Assert that some number is negative (< 0) */
    def negative[A](error: ValidationError = nonNegativeError)(implicit num: Numeric[A]): ValidationFunction[A, A] =
        in => if (num.signum(in) < 0) Right(in)
              else Left(error :: Nil)

    /** Assert that some number is non-negative (>= 0) */
    def nonNegative[A](error: ValidationError = negativeError)(implicit num: Numeric[A]): ValidationFunction[A, A] =
        in => if (num.signum(in) >= 0) Right(in)
              else Left(error :: Nil)

    /** Assert that some number is non-positive (<= 0) */
    def nonPositive[A](error: ValidationError = nonNegativeError)(implicit num: Numeric[A]): ValidationFunction[A, A] =
        in => if (num.signum(in) <= 0) Right(in)
              else Left(error :: Nil)

    import string.{nonBlank, numericWithSign, numericWithSignAndDecimal, nonNumericError}

    /** Assert that some string is a well formatted number, converting it */
    def number[A] (
        parse: String => A, // expected to throw NumberFormatException on overflow and underflow
        minValue: A,
        maxValue: A,
        missingValue: ValidationError,
        malformatted: ValidationError,
        underflow: ValidationError,
        overflow: ValidationError
    ): ValidationFunction[String, A] = nonBlank() and numericWithSign(malformatted) and (
        in => catching(classOf[NumberFormatException]).either(parse(in)).left.map(_ => {
            if (in.startsWith("-")) {
                if (underflow != null) underflow else tooSmallError(minValue.toString)
            } else {
                if (overflow  != null) overflow  else tooLargeError(maxValue.toString)
            }
        } :: Nil)
    )

    /** Assert that some string is a well formatted decimal number, converting it */
    def numberWithDecimal[A] (
        parse: String => A, // expected to throw NumberFormatException on overflow and underflow
        minValue: A,
        maxValue: A,
        missingValue: ValidationError,
        malformatted: ValidationError,
        underflow: ValidationError,
        overflow: ValidationError
    ): ValidationFunction[String, A] = nonBlank() and numericWithSignAndDecimal(malformatted) and (
        in => catching(classOf[NumberFormatException]).either(parse(in)).left.map(_ => {
            if (in.startsWith("-")) {
                if (underflow != null) underflow else tooSmallError(minValue.toString)
            } else {
                if (overflow  != null) overflow  else tooLargeError(maxValue.toString)
            }
        } :: Nil)
    )

    import java.lang.{Byte => JLByte, Short => JLShort, Integer => JLInteger, Long => JLLong, Float => JLFloat, Double => JLDouble}

    /** Assert that a string is a well formatted byte and convert it */
    def byte (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Byte] =
        number(s => JLByte.parseByte(s): JLByte, JLByte.MIN_VALUE, JLByte.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted short and convert it */
    def short (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Short] =
        number(s => JLShort.parseShort(s): JLShort, JLShort.MIN_VALUE, JLShort.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted int and convert it */
    def int (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Int] =
        number(s => JLInteger.parseInt(s): JLInteger, JLInteger.MIN_VALUE, JLInteger.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted long and convert it */
    def long (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Long] =
        number(s => JLLong.parseLong(s): JLLong, JLLong.MIN_VALUE, JLLong.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted float and convert it */
    def float (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Float] =
        numberWithDecimal(s => JLFloat.parseFloat(s): JLFloat, JLFloat.MIN_VALUE, JLFloat.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted double and convert it */
    def double (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError,
        underflow: ValidationError = null,
        overflow: ValidationError = null
    ): ValidationFunction[String, Double] =
        numberWithDecimal(s => JLDouble.parseDouble(s): JLDouble, JLDouble.MIN_VALUE, JLDouble.MAX_VALUE, missingValue, malformatted, underflow, overflow)

    /** Assert that a string is a well formatted integer value */
    def bigInt (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError
    ): ValidationFunction[String, BigInt] = nonBlank() and numericWithSignAndDecimal(malformatted) and (
        in => catching(classOf[NumberFormatException]).either(BigInt(in)).left.map(_ => malformatted :: Nil)
    )

    /** Assert that a string is a well formatted decimal value */
    def bigDecimal (
        missingValue: ValidationError = missingValueError,
        malformatted: ValidationError = nonNumericError
    ): ValidationFunction[String, BigDecimal] = nonBlank() and numericWithSignAndDecimal(malformatted) and (
        in => catching(classOf[NumberFormatException]).either(BigDecimal(in)).left.map(_ => malformatted :: Nil)
    )
}
