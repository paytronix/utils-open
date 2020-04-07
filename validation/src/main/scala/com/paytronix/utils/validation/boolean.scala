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

import base.{Validated, ValidationError, failure, success}

object boolean {
    val invalidBooleanError = ValidationError("invalid_boolean", "invalid boolean (expected true/false, yes/no, on/off, or a number)")

    /**
     * Convert a string to a boolean, appropriate for parsing HTML form input.
     * Any nonempty string that is not "false", "no" or "off" is treated as true
     */
    lazy val boolean = booleanE(invalidBooleanError)

    /**
     * Convert a string to a boolean, appropriate for parsing HTML form input.
     * Any nonempty string that is not "false", "no" or "off" is treated as true
     */
    def booleanE(error: ValidationError): String => Validated[Boolean] =
        _.toLowerCase.trim match {
            case ""|"false"|"no"|"off" => success(false)
            case "true"|"yes"|"on"     => success(true)
            case s =>
                try success(Integer.parseInt(s) != 0) catch { case _: NumberFormatException => failure(error) }
        }
}

