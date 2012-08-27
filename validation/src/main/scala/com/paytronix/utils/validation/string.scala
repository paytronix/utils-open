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

import scala.util.matching.Regex

import base.{ValidationError, ValidationFunction, missingValueError}

object string {
    val ValidDomainChars = "a-zA-Z0-9"
    // This is somewhat complicated because we're allowed to have dashes, but not on either end
    val ValidDomainLabel = "[" + ValidDomainChars + "]+(?:[-]+[" + ValidDomainChars + "]+)*"
    val ValidDomainPart = ValidDomainLabel + "(?:[.]" + ValidDomainLabel + ")+"
    // We do not allow quoted-string local part values!
    val ValidLocalChars = ValidDomainChars + "!#$%&'*+\\-/=?^_`{|}~"
    val ValidLocalPart  = "[" + ValidLocalChars + "]+(?:[.][" + ValidLocalChars + "]+)*"
    val ValidEmail = (ValidLocalPart + "@" + ValidDomainPart).r

    def tooShortError(i: Int) = ValidationError("too_short", "must have at least %d character(s)", i)
    def tooLongError(i: Int)  = ValidationError("too_long", "must have no more than %d character(s)", i)
    val patternMatchError     = ValidationError("invalid_format", "not formatted correctly")
    val numericError          = ValidationError("invalid_numeric", "must not be entirely numeric")
    val nonNumericError       = ValidationError("invalid_non_numeric", "must be numeric")
    val nonIntegralError      = ValidationError("invalid_non_integral", "must be an integral value")
    val invalidEmailError     = ValidationError("invalid_email", "invalid email address")

    /** Assert that some string is not empty */
    def nonEmpty(error: ValidationError = missingValueError): ValidationFunction[String, String] =
        in => if (!in.isEmpty) Right(in)
              else Left(error :: Nil)

    /** Trim the string and then assert it is nonempty */
    def nonBlank(error: ValidationError = missingValueError): ValidationFunction[String, String] =
        in => in.trim match {
            case "" => Left(error :: Nil)
            case s => Right(s)
        }

    /** Trim the string and make Some(string) out of it if nonblank, None otherwise */
    val optionalString: ValidationFunction[String, Option[String]] =
        in => Right(in.trim match {
            case "" => None
            case s => Some(s)
        })

    /** Assert that some string is at least some size */
    def noShorterThan(i: Int, error: ValidationError = null): ValidationFunction[String, String] =
        in => if (in.size >= i) Right(in)
              else Left((if (error != null) error else tooShortError(i)) :: Nil)

    /** Assert that some string is no longer than some size */
    def noLongerThan(i: Int, error: ValidationError = null): ValidationFunction[String, String] =
        in => if (in.size <= i) Right(in)
              else Left((if (error != null) error else tooLongError(i)) :: Nil)

    /** Assert that a string matches some pattern Regex */
    def matches(pattern: Regex, message: ValidationError = patternMatchError): ValidationFunction[String, String] =
        in => if (pattern.unapplySeq(in).isDefined) Right(in)
              else Left(message :: Nil)

    /** Assert that a string does not match some pattern Regex */
    def doesNotMatch(pattern: Regex, message: ValidationError = patternMatchError): ValidationFunction[String, String] =
        in => if (!pattern.unapplySeq(in).isDefined) Right(in)
              else Left(message :: Nil)

    /** Assert that the string is comprised only of digits */
    def numeric(error: ValidationError = nonNumericError) = matches("\\d+".r, error)

    /** Assert that the string is comprised only of digits with an optional leading sign */
    def numericWithSign(error: ValidationError = nonIntegralError) = matches("[+-]?\\d+".r, error)

    /** Assert that the string is comprised only of digits with an optional leading sign and optional fractional part after a decimal */
    def numericWithSignAndDecimal(error: ValidationError = nonNumericError) = matches("[+-]?\\d+(?:\\.\\d+)?".r, error)

    /** Assert that the string is not comprised only of digits */
    def nonNumeric(error: ValidationError = numericError) = doesNotMatch("\\d+".r, error)

    /** Assert that the string conforms to the valid email address format */
    def validEmailAddress(message: ValidationError = invalidEmailError): ValidationFunction[String, String] = string.matches(ValidEmail, invalidEmailError)

    /** Return a boolean indicating whether the string conforms to the valid email address format */
    def isValidEmailAddress(email: String): Boolean = validEmailAddress()(email).isRight
}

