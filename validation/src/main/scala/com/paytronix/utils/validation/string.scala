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

import scala.util.matching.Regex

import base.{Validated, ValidationError, failure, missingValueError, predicateE, success}

object string {
    val ValidDomainChars = "a-zA-Z0-9"
    // This is somewhat complicated because we're allowed to have dashes, but not on either end
    val ValidDomainLabel = "[" + ValidDomainChars + "]+(?:[-]+[" + ValidDomainChars + "]+)*"
    val ValidDomainPart  = ValidDomainLabel + "(?:[.]" + ValidDomainLabel + ")+"
    // We do not allow quoted-string local part values!
    val ValidLocalChars  = ValidDomainChars + "!#$%&'*+\\-/=?^_`{|}~"
    val ValidLocalPart   = "[" + ValidLocalChars + "]+(?:[.][" + ValidLocalChars + "]+)*"
    val ValidEmail       = (ValidLocalPart + "@" + ValidDomainPart).r

    def tooShortError(i: Int) = ValidationError("too_short", s"must have at least $i character(s)")
    def tooLongError(i: Int)  = ValidationError("too_long", s"must have no more than $i character(s)")
    val patternMatchError     = ValidationError("invalid_format", "not formatted correctly")
    val numericError          = ValidationError("invalid_numeric", "must not be entirely numeric")
    val nonNumericError       = ValidationError("invalid_non_numeric", "must be numeric")
    val nonIntegralError      = ValidationError("invalid_non_integral", "must be an integral value")
    val invalidEmailError     = ValidationError("invalid_email", "invalid email address")

    /** Assert that some string is not empty */
    val nonEmpty: String => Validated[String] =
        nonEmptyE(missingValueError)

    /** Assert that some string is not empty */
    def nonEmptyE(error: ValidationError): String => Validated[String] =
        predicateE(error)(!_.isEmpty)

    /** Trim the string and then assert it is nonempty */
    val nonBlank: String => Validated[String] =
        nonBlankE(missingValueError)

    /** Trim the string and then assert it is nonempty */
    def nonBlankE(error: ValidationError): String => Validated[String] =
        in => in.trim match {
            case "" => failure(error)
            case s  => success(s)
        }

    /** Trim the string and make Some(string) out of it if nonblank, None otherwise */
    val optionalString: String => Validated[Option[String]] =
        in => success(in.trim match {
            case "" => None
            case s => Some(s)
        })

    /** Assert that some string is at least some size */
    val noShorterThan: Int => String => Validated[String] =
        noShorterThanE(tooShortError)

    /** Assert that some string is at least some size */
    def noShorterThanE(error: Int => ValidationError)(limit: Int): String => Validated[String] =
        predicateE(error(limit))(_.size >= limit)

    /** Assert that some string is no longer than some size */
    val noLongerThan: Int => String => Validated[String] =
        noLongerThanE(tooLongError)

    /** Assert that some string is no longer than some size */
    def noLongerThanE(error: Int => ValidationError)(limit: Int): String => Validated[String] =
        predicateE(error(limit))(_.size <= limit)

    /** Assert that a string matches some pattern Regex */
    val matches: Regex => String => Validated[String] =
        matchesE(patternMatchError)

    /** Assert that a string matches some pattern Regex */
    def matchesE(error: ValidationError)(pattern: Regex): String => Validated[String] =
        predicateE(error)(s => pattern.unapplySeq(s).isDefined)

    /** Assert that a string does not match some pattern Regex */
    val doesNotMatch: Regex => String => Validated[String] =
        doesNotMatchE(patternMatchError)

    /** Assert that a string does not match some pattern Regex */
    def doesNotMatchE(error: ValidationError)(pattern: Regex): String => Validated[String] =
        predicateE(error)(s => !pattern.unapplySeq(s).isDefined)

    /** Assert that the string is comprised only of digits */
    val numeric: String => Validated[String] =
        numericE(nonNumericError)

    /** Assert that the string is comprised only of digits */
    def numericE(error: ValidationError) =
        matchesE(error)("\\d+".r)

    /** Assert that the string is comprised only of digits with an optional leading sign */
    val numericWithSign: String => Validated[String] =
        numericWithSignE(nonNumericError)

    /** Assert that the string is comprised only of digits with an optional leading sign */
    def numericWithSignE(error: ValidationError): String => Validated[String] =
        matchesE(error)("[+-]?\\d+".r)

    /** Assert that the string is comprised only of digits with an optional leading sign and optional fractional part after a decimal */
    val numericWithSignAndDecimal: String => Validated[String] =
        numericWithSignAndDecimalE(nonNumericError)

    /** Assert that the string is comprised only of digits with an optional leading sign and optional fractional part after a decimal */
    def numericWithSignAndDecimalE(error: ValidationError): String => Validated[String] =
        matchesE(error)("[+-]?\\d+(?:\\.\\d+)?".r)

    /** Assert that the string is not comprised only of digits */
    val nonNumeric: String => Validated[String] =
        nonNumericE(numericError)

    /** Assert that the string is not comprised only of digits */
    def nonNumericE(error: ValidationError): String => Validated[String] =
        doesNotMatchE(error)("\\d+".r)

    /** Assert that the string conforms to the valid email address format */
    val validEmailAddress: String => Validated[String] =
        validEmailAddressE(invalidEmailError)

    /** Assert that the string conforms to the valid email address format */
    def validEmailAddressE(message: ValidationError): String => Validated[String] =
        matchesE(message)(ValidEmail)

    /** Return a boolean indicating whether the string conforms to the valid email address format */
    def isValidEmailAddress(email: String): Boolean =
        validEmailAddress(email).isValid
}

