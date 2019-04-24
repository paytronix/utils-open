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

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZonedDateTime, ZoneOffset}
import org.joda.time.{
    DateTime          => JodaDateTime,
    DateTimeZone      => JodaDateTimeZone,
    LocalDate         => JodaLocalDate,
    LocalDateTime     => JodaLocalDateTime,
    LocalTime         => JodaLocalTime
}
import scalaz.BijectionT

import com.paytronix.utils.interchange.base.datetime.{
    long    => javaLong,
    iso8601 => javaIso8601,
    classic => javaClassic
}
import com.paytronix.utils.scala.result.{Okay, Result, tryCatchValue}

import BijectionT.bijection

/** Conversions between Joda Time and Java Time types */
object joda {
    // Conversions between time offset representations. Always use the offset rather than the zone name, as they don't always match 1:1
    private def jodaDateTimeZone(dtz: ZoneOffset): JodaDateTimeZone = JodaDateTimeZone.forOffsetMillis(dtz.getTotalSeconds * 1000)
    private def javaZoneOffset(jdtz: JodaDateTimeZone, instantMillis: long): ZoneOffset = ZoneOffset.ofTotalSeconds(jdtz.getOffset(instantMillis) / 1000)

    val dateTimeBijection: BijectionT[Result, Result, JodaDateTime, ZonedDateTime] =
        bijection (
            (jdt: JodaDateTime)  => tryCatchValue(ZonedDateTime.ofInstant(Instant.ofEpochMilli(jdt.getMillis), javaZoneOffset(jdt.getZone, jdt.getMillis))): Result[ZonedDateTime],
            (zdt: ZonedDateTime) => tryCatchValue(new JodaDateTime(zdt.toInstant.toEpochMilli, jodaDateTimeZone(zdt.getOffset))): Result[JodaDateTime]
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
            // Java Time treats all LocalTime values having no offset, but Joda treats constructors args as being adjusted for local time zone unless specified
            (jlt: JodaLocalTime) => Okay(LocalTime.ofNanoOfDay(jlt.getMillisOfDay * 1000000L)): Result[LocalTime],
            (lt: LocalTime)      => Okay(new JodaLocalTime(lt.toNanoOfDay / 1000000, JodaDateTimeZone.UTC)): Result[JodaLocalTime]
        )

    object long {
        val dateTimeBijection:      BijectionT[Result, Result, JodaDateTime, Long]      = javaLong.zonedDateTimeBijection <=< joda.dateTimeBijection
        val localDateBijection:     BijectionT[Result, Result, JodaLocalDate, Long]     = javaLong.localDateBijection     <=< joda.localDateBijection
        val localDateTimeBijection: BijectionT[Result, Result, JodaLocalDateTime, Long] = javaLong.localDateTimeBijection <=< joda.localDateTimeBijection
        val localTimeBijection:     BijectionT[Result, Result, JodaLocalTime, Long]     = javaLong.localTimeBijection     <=< joda.localTimeBijection
    }

    object iso8601 {
        val dateTimeBijection:      BijectionT[Result, Result, JodaDateTime, String]      = javaIso8601.zonedDateTimeBijection <=< joda.dateTimeBijection
        val localDateBijection:     BijectionT[Result, Result, JodaLocalDate, String]     = javaIso8601.localDateBijection     <=< joda.localDateBijection
        val localDateTimeBijection: BijectionT[Result, Result, JodaLocalDateTime, String] = javaIso8601.localDateTimeBijection <=< joda.localDateTimeBijection
        val localTimeBijection:     BijectionT[Result, Result, JodaLocalTime, String]     = javaIso8601.localTimeBijection     <=< joda.localTimeBijection
    }

    object classic {
        val dateTimeBijection:      BijectionT[Result, Result, JodaDateTime, String]      = javaClassic.zonedDateTimeBijection <=< joda.dateTimeBijection
        val localDateBijection:     BijectionT[Result, Result, JodaLocalDate, String]     = javaClassic.localDateBijection     <=< joda.localDateBijection
        val localDateTimeBijection: BijectionT[Result, Result, JodaLocalDateTime, String] = javaClassic.localDateTimeBijection <=< joda.localDateTimeBijection
        val localTimeBijection:     BijectionT[Result, Result, JodaLocalTime, String]     = javaClassic.localTimeBijection     <=< joda.localTimeBijection
    }

}
