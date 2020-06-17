//
// Copyright 2020 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange.format.string

import java.time.ZoneId

import com.paytronix.utils.interchange.base.{Receiver, terminal}
import com.paytronix.utils.scala.result.tryCatchResultG

object javatime extends javatime

trait javatime {
    implicit object zoneIdStringCoder extends StringCoder[ZoneId] {
        object encode extends StringEncoder[ZoneId] {
            def run(in: ZoneId, out: Receiver[String]) = out(in.toString)
        }

        object decode extends StringDecoder[ZoneId] {
            def run(in: String, out: Receiver[ZoneId]) = tryCatchResultG(terminal)(out(ZoneId.of(in)))
        }
    }
}