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

import java.util.regex.PatternSyntaxException
import scala.util.matching.Regex

import scalaz.{Failure, Success}
import scalaz.NonEmptyList.nels

import base.{Validated, ValidationError, missingValueError}
import string.nonBlankE

object regex {
    import string.nonBlank

    /** Assert that a String is nonblank and parse it as a regular expression */
    val pattern: String => Validated[Regex] =
        patternE(missingValueError, t => ValidationError("invalid_regex", Option(t.getMessage) getOrElse t.toString))

    /** Assert that a String is nonblank and parse it as a regular expression */
    def patternE(missingValue: ValidationError, parseError: PatternSyntaxException => ValidationError): String => Validated[Regex] =
        nonBlankE(missingValue) and { s =>
            try Success(s.r) catch { case e: PatternSyntaxException => Failure(nels(parseError(e))) }
        }
}

