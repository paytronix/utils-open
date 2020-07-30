//
// Copyright 2018-2020 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange.base.datetime

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZonedDateTime, ZoneOffset}
import org.joda.time.{
    DateTime          => JodaDateTime,
    DateTimeZone      => JodaDateTimeZone,
    LocalDate         => JodaLocalDate,
    LocalDateTime     => JodaLocalDateTime,
    LocalTime         => JodaLocalTime
}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.scala.result.Okay

import com.paytronix.utils.interchange.base.datetime.arbitraries._

class jodaDatesTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        Joda Time bijections should round trip
            DateTime           $e1
            LocalDate          $e2
            LocalDateTime      $e3
            LocalTime          $e4
    """

    val roundTripDateTime      = joda.dateTimeBijection      <=< joda.dateTimeBijection.flip
    val roundTripLocalDate     = joda.localDateBijection     <=< joda.localDateBijection.flip
    val roundTripLocalDateTime = joda.localDateTimeBijection <=< joda.localDateTimeBijection.flip
    val roundTripLocalTime     = joda.localTimeBijection     <=< joda.localTimeBijection.flip

    def e1 = prop { (zdt: ZonedDateTime) => roundTripDateTime.from(zdt) ==== Okay(zdt) }
    def e2 = prop { (ld: LocalDate)      => roundTripLocalDate.from(ld) ==== Okay(ld) }
    def e3 = prop { (ldt: LocalDateTime) => roundTripLocalDateTime.from(ldt) ==== Okay(ldt) }
    def e4 = prop { (lt: LocalTime)      => roundTripLocalTime.from(lt) ==== Okay(lt) }
}
