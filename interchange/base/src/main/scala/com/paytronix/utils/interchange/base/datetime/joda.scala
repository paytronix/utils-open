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

import com.paytronix.utils.interchange.base.TypeConverter
import com.paytronix.utils.interchange.base.datetime.{
    long    => javaLong,
    iso8601 => javaIso8601,
    classic => javaClassic
}
import com.paytronix.utils.scala.result.{Okay, Result, tryCatchValue}

/** Conversions between Joda Time and Java Time types */
object joda {
    // Conversions between time offset representations. Always use the offset rather than the zone name, as they don't always match 1:1
    private def jodaDateTimeZone(dtz: ZoneOffset): JodaDateTimeZone = JodaDateTimeZone.forOffsetMillis(dtz.getTotalSeconds * 1000)
    private def javaZoneOffset(jdtz: JodaDateTimeZone, instantMillis: Long): ZoneOffset = ZoneOffset.ofTotalSeconds(jdtz.getOffset(instantMillis) / 1000)

    val dateTimeConverter: TypeConverter[JodaDateTime, ZonedDateTime] =
        TypeConverter(
            (jdt: JodaDateTime)  => tryCatchValue(ZonedDateTime.ofInstant(Instant.ofEpochMilli(jdt.getMillis), javaZoneOffset(jdt.getZone, jdt.getMillis))): Result[ZonedDateTime],
            (zdt: ZonedDateTime) => tryCatchValue(new JodaDateTime(zdt.toInstant.toEpochMilli, jodaDateTimeZone(zdt.getOffset))): Result[JodaDateTime]
        )
    val localDateConverter: TypeConverter[JodaLocalDate, LocalDate] =
        TypeConverter(
            (jld: JodaLocalDate) => Okay(LocalDateTime.ofInstant(Instant.ofEpochMilli(jld.toDateTimeAtStartOfDay(JodaDateTimeZone.UTC).getMillis), ZoneOffset.UTC).toLocalDate): Result[LocalDate],
            (ld: LocalDate)      => Okay(new JodaLocalDate(ld.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli, JodaDateTimeZone.UTC)): Result[JodaLocalDate]
        )
    val localDateTimeConverter: TypeConverter[JodaLocalDateTime, LocalDateTime] =
        TypeConverter(
            (jldt: JodaLocalDateTime) => Okay(LocalDateTime.ofInstant(Instant.ofEpochMilli(jldt.toDateTime(JodaDateTimeZone.UTC).getMillis), ZoneOffset.UTC)): Result[LocalDateTime],
            (ldt: LocalDateTime)      => Okay(new JodaLocalDateTime(ldt.toInstant(ZoneOffset.UTC).toEpochMilli, JodaDateTimeZone.UTC)): Result[JodaLocalDateTime]
        )
    val localTimeConverter: TypeConverter[JodaLocalTime, LocalTime] =
        TypeConverter(
            // Java Time treats all LocalTime values having no offset, but Joda treats constructors args as being adjusted for local time zone unless specified
            (jlt: JodaLocalTime) => Okay(LocalTime.ofNanoOfDay(jlt.getMillisOfDay * 1000000L)): Result[LocalTime],
            (lt: LocalTime)      => Okay(new JodaLocalTime(lt.toNanoOfDay / 1000000, JodaDateTimeZone.UTC)): Result[JodaLocalTime]
        )

    object long {
        val dateTimeConverter:      TypeConverter[JodaDateTime, Long]      = joda.dateTimeConverter.compose(javaLong.zonedDateTimeConverter)
        val localDateConverter:     TypeConverter[JodaLocalDate, Long]     = joda.localDateConverter.compose(javaLong.localDateConverter)
        val localDateTimeConverter: TypeConverter[JodaLocalDateTime, Long] = joda.localDateTimeConverter.compose(javaLong.localDateTimeConverter)
        val localTimeConverter:     TypeConverter[JodaLocalTime, Long]     = joda.localTimeConverter.compose(javaLong.localTimeConverter)
    }

    object iso8601 {
        val dateTimeConverter:      TypeConverter[JodaDateTime, String]      = joda.dateTimeConverter.compose(javaIso8601.zonedDateTimeConverter)
        val localDateConverter:     TypeConverter[JodaLocalDate, String]     = joda.localDateConverter.compose(javaIso8601.localDateConverter)
        val localDateTimeConverter: TypeConverter[JodaLocalDateTime, String] = joda.localDateTimeConverter.compose(javaIso8601.localDateTimeConverter)
        val localTimeConverter:     TypeConverter[JodaLocalTime, String]     = joda.localTimeConverter.compose(javaIso8601.localTimeConverter)
    }

    object classic {
        val dateTimeConverter:      TypeConverter[JodaDateTime, String]      = joda.dateTimeConverter.compose(javaClassic.zonedDateTimeConverter)
        val localDateConverter:     TypeConverter[JodaLocalDate, String]     = joda.localDateConverter.compose(javaClassic.localDateConverter)
        val localDateTimeConverter: TypeConverter[JodaLocalDateTime, String] = joda.localDateTimeConverter.compose(javaClassic.localDateTimeConverter)
        val localTimeConverter:     TypeConverter[JodaLocalTime, String]     = joda.localTimeConverter.compose(javaClassic.localTimeConverter)
    }

}
