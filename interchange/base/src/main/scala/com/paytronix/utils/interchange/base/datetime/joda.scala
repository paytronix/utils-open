//
// Copyright 2018 Paytronix Systems, Inc.
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

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZonedDateTime, ZoneOffset, ZoneId}
import org.joda.time.{
    DateTime          => JodaDateTime,
    DateTimeZone      => JodaDateTimeZone,
    LocalDate         => JodaLocalDate,
    LocalDateTime     => JodaLocalDateTime,
    LocalTime         => JodaLocalTime
}
import scalaz.BijectionT

import com.paytronix.utils.scala.result.{Okay, Result, tryCatchValue}

import BijectionT.bijection

/** Conversions between Joda Time and Java Time types */
object joda {
    val dateTimeBijection: BijectionT[Result, Result, JodaDateTime, ZonedDateTime] =
        bijection (
            (jdt: JodaDateTime)  => tryCatchValue(ZonedDateTime.ofInstant(Instant.ofEpochMilli(jdt.getMillis), ZoneId.of(jdt.getZone.getID))): Result[ZonedDateTime],
            (zdt: ZonedDateTime) => tryCatchValue(new JodaDateTime(zdt.toInstant.toEpochMilli, JodaDateTimeZone.forID(zdt.getZone.getId))): Result[JodaDateTime]
        )
    val localDateBijection: BijectionT[Result, Result, JodaLocalDate, LocalDate] =
        bijection (
            (jld: JodaLocalDate) => Okay(LocalDateTime.ofInstant(Instant.ofEpochMilli(jld.toDateTimeAtStartOfDay(JodaDateTimeZone.UTC).getMillis), ZoneOffset.UTC).toLocalDate): Result[LocalDate],
            (ld: LocalDate)      => Okay(new JodaLocalDate(ld.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli, JodaDateTimeZone.UTC)): Result[JodaLocalDate]
        )
    val localDateTimeBijection: BijectionT[Result, Result, JodaLocalDateTime, LocalDateTime] =
        bijection (
            (jldt: JodaLocalDateTime) => Okay(LocalDateTime.ofInstant(Instant.ofEpochMilli(jldt.toDateTime(JodaDateTimeZone.UTC).getMillis), ZoneOffset.UTC)): Result[LocalDateTime],
            (ldt: LocalDateTime)      => Okay(new JodaLocalDateTime(ldt.toInstant(ZoneOffset.UTC).toEpochMilli, JodaDateTimeZone.UTC)): Result[JodaLocalDateTime]
        )
    val localTimeBijection: BijectionT[Result, Result, JodaLocalTime, LocalTime] =
        bijection (
            (jlt: JodaLocalTime) => Okay(LocalTime.ofNanoOfDay(jlt.getMillisOfDay * 1000000L)): Result[LocalTime],
            (lt: LocalTime) => Okay(new JodaLocalTime(lt.toNanoOfDay / 1000000)): Result[JodaLocalTime]
        )
}

// MATT make unit tests for this... especially around the stupid railroad time in 1883 or whatever the eff
// MATT make unit tests for this... especially around the stupid railroad time in 1883 or whatever the eff
// MATT make unit tests for this... especially around the stupid railroad time in 1883 or whatever the eff
// MATT make unit tests for this... especially around the stupid railroad time in 1883 or whatever the eff
// MATT make unit tests for this... especially around the stupid railroad time in 1883 or whatever the eff
// MATT make unit tests for this... especially around the stupid railroad time in 1883 or whatever the eff
// MATT make unit tests for this... especially around the stupid railroad time in 1883 or whatever the eff
// MATT make unit tests for this... especially around the stupid railroad time in 1883 or whatever the eff