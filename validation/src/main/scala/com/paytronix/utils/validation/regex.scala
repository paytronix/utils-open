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

import java.util.regex.PatternSyntaxException
import scala.util.control.Exception.catching
import scala.util.matching.Regex

import base.{ValidationError, ValidationFunction, missingValueError}

object regex {
    import string.nonBlank

    /** Assert that a String is nonblank and parse it as a regular expression */
    def pattern(missingValue: ValidationError = missingValueError): ValidationFunction[String, Regex] =
        nonBlank() and (
            in => catching(classOf[PatternSyntaxException]).either(in.r).left.map(t => ValidationError("invalid_regex", Option(t.getMessage) getOrElse t.toString) :: Nil)
        )
}

