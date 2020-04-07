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

import java.net.{URI, URISyntaxException}

import base.{Validated, ValidationError, failure, missingValueError, success}

object uri {
    import string.nonBlankE

    /** Assert that a String is nonblank and convert it to a URI */
    val uri: String => Validated[URI] =
        uriE(missingValueError, t => ValidationError(Option(t.getMessage) getOrElse t.toString))

    /** Assert that a String is nonblank and convert it to a URI */
    def uriE(missingValue: ValidationError, parseError: URISyntaxException => ValidationError): String => Validated[URI] =
        nonBlankE(missingValue) and { s =>
            try success(new URI(s)) catch { case t: URISyntaxException => failure(parseError(t)) }
        }
}

