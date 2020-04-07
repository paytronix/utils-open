//
// Copyright 2014-2020 Paytronix Systems, Inc.
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

import cats.data.NonEmptyList
import java.sql.{Date => SqlDate, Time => SqlTime, Timestamp => SqlTimestamp}
import java.util.{Date => UtilDate}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetTime, ZonedDateTime, ZoneOffset, ZoneId}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField

import com.paytronix.utils.interchange.base.TypeConverter
import com.paytronix.utils.scala.result.{Failed, Okay, Result, firstOrLastG, tryCatchValue}

object constants {
    val epoch = LocalDate.ofEpochDay(0L)
}

object DateTimeFormatters {
    /** Create a DateTimeFormatter with a range of fractional seconds, because the string-based formatting does not allow for variable-length fractions */
    def withOptionalFractionalSecondRange(dateTimePattern: String, offsetPattern: Option[String] = None): DateTimeFormatter = {
        val builder = new DateTimeFormatterBuilder
        builder.appendPattern(dateTimePattern)
        builder.appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true)
        offsetPattern.foreach(p => builder.appendPattern(p))
        builder.toFormatter
    }
}

object javaDates {
    val javaDateConverter: TypeConverter[UtilDate, ZonedDateTime] = TypeConverter(
        (ud: UtilDate)       => Okay(ZonedDateTime.ofInstant(ud.toInstant, ZoneOffset.UTC)): Result[ZonedDateTime],
        (zdt: ZonedDateTime) => Okay(UtilDate.from(zdt.toInstant)): Result[UtilDate]
    )
    val javaSqlDateConverter: TypeConverter[SqlDate, LocalDate] = TypeConverter(
        (sd: SqlDate)   => Okay(sd.toLocalDate): Result[LocalDate],
        (ld: LocalDate) => Okay(SqlDate.valueOf(ld)): Result[SqlDate]
    )
    val javaSqlTimeConverter: TypeConverter[SqlTime, LocalTime] = TypeConverter(
        (st: SqlTime)   => Okay(st.toLocalTime): Result[LocalTime],
        (lt: LocalTime) => Okay(SqlTime.valueOf(lt)): Result[SqlTime]
    )
    val javaSqlTimestampConverter: TypeConverter[SqlTimestamp, ZonedDateTime] = TypeConverter(
        (sts: SqlTimestamp) => Okay(ZonedDateTime.ofInstant(sts.toInstant, ZoneOffset.UTC)): Result[ZonedDateTime],
        (zdt: ZonedDateTime) => Okay(SqlTimestamp.from(zdt.toInstant)): Result[SqlTimestamp]
    )
}

class DateTimeStringConversions (
    dateTimeFormatters:      NonEmptyList[DateTimeFormatter],
    localDateFormatters:     NonEmptyList[DateTimeFormatter],
    localDateTimeFormatters: NonEmptyList[DateTimeFormatter],
    localTimeFormatters:     NonEmptyList[DateTimeFormatter],
    offsetTimeFormatters:    NonEmptyList[DateTimeFormatter]
) {
    private def fail(encodeFormatter: DateTimeFormatter): Failed =
        Failed("incorrectly formatted date -- expected format like  " + ZonedDateTime.now.format(encodeFormatter))

    val zonedDateTimeConverter: TypeConverter[ZonedDateTime, String] = TypeConverter(
        (zdt: ZonedDateTime) => tryCatchValue(zdt.format(dateTimeFormatters.head)): Result[String],
        (s: String)          => firstOrLastG(fail(dateTimeFormatters.head), dateTimeFormatters) { formatter =>
            tryCatchValue(ZonedDateTime.parse(s, formatter))
        }: Result[ZonedDateTime]
    )

    val localDateConverter: TypeConverter[LocalDate, String] = TypeConverter(
        (ld: LocalDate) => tryCatchValue(ld.format(localDateFormatters.head)): Result[String],
        (s: String)     => firstOrLastG(fail(localDateFormatters.head), localDateFormatters) { formatter =>
            tryCatchValue(LocalDate.parse(s, formatter))
        }: Result[LocalDate]
    )

    val localDateTimeConverter: TypeConverter[LocalDateTime, String] = TypeConverter(
        (ldt: LocalDateTime) => tryCatchValue(ldt.format(localDateTimeFormatters.head)): Result[String],
        (s: String)          => firstOrLastG(fail(localDateTimeFormatters.head), localDateTimeFormatters) { formatter =>
            tryCatchValue(LocalDateTime.parse(s, formatter))
        }: Result[LocalDateTime]
    )

    val localTimeConverter: TypeConverter[LocalTime, String] = TypeConverter(
        (lt: LocalTime) => tryCatchValue(lt.format(localTimeFormatters.head)): Result[String],
        (s: String)     => firstOrLastG(fail(localTimeFormatters.head), localTimeFormatters) { formatter =>
            tryCatchValue(LocalTime.parse(s, formatter))
        }: Result[LocalTime]
    )

    val offsetTimeConverter: TypeConverter[OffsetTime, String] = TypeConverter(
        (ot: OffsetTime) => tryCatchValue(ot.format(offsetTimeFormatters.head)): Result[String],
        (s: String)      => firstOrLastG(fail(offsetTimeFormatters.head), offsetTimeFormatters) { formatter =>
            tryCatchValue(OffsetTime.parse(s, formatter))
        }: Result[OffsetTime]
    )

    val javaDateConverter: TypeConverter[UtilDate, String] = javaDates.javaDateConverter.compose(zonedDateTimeConverter)

    val javaSqlDateConverter: TypeConverter[SqlDate, String] = javaDates.javaSqlDateConverter.compose(localDateConverter)

    val javaSqlTimeConverter: TypeConverter[SqlTime, String] = javaDates.javaSqlTimeConverter.compose(localTimeConverter)

    val javaSqlTimestampConverter: TypeConverter[SqlTimestamp, String] = javaDates.javaSqlTimestampConverter.compose(zonedDateTimeConverter)
}

object long {
    val zonedDateTimeConverter: TypeConverter[ZonedDateTime, Long] = TypeConverter(
        (zdt: ZonedDateTime) => Okay(zdt.toInstant.toEpochMilli): Result[Long],
        (l: Long) => tryCatchValue(ZonedDateTime.ofInstant(Instant.ofEpochMilli(l), ZoneId.systemDefault)): Result[ZonedDateTime]
    )

    val localDateConverter: TypeConverter[LocalDate, Long] = TypeConverter(
        (ld: LocalDate) => Okay(ld.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli): Result[Long],
        (l: Long) => tryCatchValue(LocalDateTime.ofInstant(Instant.ofEpochMilli(l), ZoneOffset.UTC).toLocalDate): Result[LocalDate]
    )

    val localDateTimeConverter: TypeConverter[LocalDateTime, Long] =
        TypeConverter(
            (ldt: LocalDateTime) => Okay(ldt.toInstant(ZoneOffset.UTC).toEpochMilli): Result[Long],
            (l: Long) => tryCatchValue(LocalDateTime.ofInstant(Instant.ofEpochMilli(l), ZoneOffset.UTC)): Result[LocalDateTime]
        )

    val localTimeConverter: TypeConverter[LocalTime, Long] = TypeConverter(
        (lt: LocalTime) => Okay(lt.toNanoOfDay / 1000000L): Result[Long],
        (l: Long) => tryCatchValue(LocalTime.ofNanoOfDay(l * 1000000L)): Result[LocalTime]
    )

    val javaDateConverter: TypeConverter[UtilDate, Long] = javaDates.javaDateConverter.compose(zonedDateTimeConverter)

    val javaSqlDateConverter: TypeConverter[SqlDate, Long] = javaDates.javaSqlDateConverter.compose(localDateConverter)

    val javaSqlTimeConverter: TypeConverter[SqlTime, Long] = javaDates.javaSqlTimeConverter.compose(localTimeConverter)

    val javaSqlTimestampConverter: TypeConverter[SqlTimestamp, Long] = javaDates.javaSqlTimestampConverter.compose(zonedDateTimeConverter)
}


object iso8601 extends DateTimeStringConversions (
    dateTimeFormatters = NonEmptyList.of(
        // ISO_OFFSET_DATETIME doesn't print seconds or millis at all if they're zero, but the old Joda Time code did so we need this for backwards compatibility when rendering datetimes
        DateTimeFormatter.ofPattern("uuuu-MM-dd'T'HH:mm:ss.SSSXXX"),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm[:ss]", Some("[ ]XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm[:ss]", Some("[ ]XX")), // This is required to allow offsets without a colon, e.g. +0100
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmm[ss]", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmm[ss]", Some("[ ]XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]", Some("[ ]XXX"))
    ),
    localDateFormatters = NonEmptyList.of(
        DateTimeFormatter.ISO_DATE,      // Seems to be equivalent to uuuu-MM-dd
        DateTimeFormatter.BASIC_ISO_DATE // Seems to be equivalent to uuuuMMdd
    ),
    localDateTimeFormatters = NonEmptyList.of(
        // ISO_LOCAL_DATE_TIME doesn't print millis at all if they're zero, but the old Joda Time code did so we need this for backwards compatibility when rendering datetimes
        DateTimeFormatter.ofPattern("uuuu-MM-dd'T'HH:mm:ss.SSS"),
        DateTimeFormatter.ISO_LOCAL_DATE_TIME, // Seems to be equivalent to uuuu-MM-dd'T'HH:mm[:ss][.SSS]
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmm[ss]")
    ),
    localTimeFormatters = NonEmptyList.of(
        // ISO_LOCAL_TIME doesn't print millis at all if they're zero, but the old Joda Time code did so we need this for backwards compatibility when rendering datetimes
        DateTimeFormatter.ofPattern("HH:mm:ss.SSS"),
        DateTimeFormatter.ISO_LOCAL_TIME, // Seems to be equivalent to HH:mm[:ss][.SSS]
        DateTimeFormatters.withOptionalFractionalSecondRange("HHmm[ss]")
    ),
    offsetTimeFormatters = NonEmptyList.of(
        DateTimeFormatter.ISO_OFFSET_TIME
    )
)

object classic extends DateTimeStringConversions (
    dateTimeFormatters = NonEmptyList.of(
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss xx"),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]", Some("[ ]XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm[:ss]", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm[:ss]", Some("[ ]XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E MMM dd HH:mm[:ss]", Some(" XX uuuu")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E MMM dd HH:mm[:ss]", Some(" XXX uuuu")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E, dd MMM yy HH:mm[:ss]", Some(" XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E, dd MMM yy HH:mm[:ss]", Some(" XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd HHmm[ss]", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd HHmm[ss]", Some("[ ]XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmm[ss]", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmm[ss]", Some("[ ]XXX")),
        // For some terrible reason our documentation says timezone is optional so we'll fall back to this
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]").withZone(ZoneOffset.UTC),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]").withZone(ZoneOffset.UTC)
    ),
    localDateFormatters = NonEmptyList.of(
        DateTimeFormatter.ISO_DATE,      // Seems to be equivalent to uuuu-MM-dd
        DateTimeFormatter.BASIC_ISO_DATE, // Seems to be equivalent to uuuuMMdd
        DateTimeFormatter.ofPattern("E MMM dd uuuu"),
        DateTimeFormatter.ofPattern("E, dd MMM uu")
    ),
    localDateTimeFormatters = NonEmptyList.of(
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss"),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm[:ss]"),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm[:ss]"),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmm[ss]"),
        (new DateTimeFormatterBuilder).appendPattern("E MMM dd HH:mm[:ss]").appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true).appendPattern(" uuuu").toFormatter,
        DateTimeFormatters.withOptionalFractionalSecondRange("E, dd MMM uu HH:mm[:ss]")
    ),
    localTimeFormatters = NonEmptyList.of(
        DateTimeFormatters.withOptionalFractionalSecondRange("HH:mm:ss"),
        DateTimeFormatters.withOptionalFractionalSecondRange("HH:mm[:ss]"),
        DateTimeFormatters.withOptionalFractionalSecondRange("HHmm[ss]")
    ),
    offsetTimeFormatters = NonEmptyList.of(
        DateTimeFormatter.ISO_OFFSET_TIME
    )
)

object sqlServer extends DateTimeStringConversions (
    dateTimeFormatters = NonEmptyList.of(
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSS")
    ),
    localDateFormatters = NonEmptyList.of(
        DateTimeFormatter.ofPattern("uuuu-MM-dd")
    ),
    localDateTimeFormatters = NonEmptyList.of(
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSS")
    ),
    localTimeFormatters = NonEmptyList.of(
        DateTimeFormatter.ofPattern("HH:mm:ss.SSS")
    ),
    offsetTimeFormatters = NonEmptyList.of(
        DateTimeFormatter.ISO_OFFSET_TIME
    )
)
