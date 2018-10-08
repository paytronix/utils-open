//
// Copyright 2014-2018 Paytronix Systems, Inc.
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

import java.sql.{Date => SqlDate, Time => SqlTime, Timestamp => SqlTimestamp}
import java.util.{Date => UtilDate}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZonedDateTime, ZoneOffset, ZoneId}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import scalaz.{BijectionT, NonEmptyList}

import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, firstOrLastG, tryCatchValue}

import BijectionT.bijection

object constants {
    val epoch = LocalDate.ofEpochDay(0L)
}

object DateTimeFormatters {
    /** Create a DateTimeFormatter with a range of fractional seconds, because the string-based formatting does not allow for variable-length fractions */
    def withOptionalFractionalSecondRange(dateTimePattern: String, offsetPattern: Option[String] = None): DateTimeFormatter = {
        val dtf = new DateTimeFormatterBuilder
        dtf.appendPattern(dateTimePattern)
        dtf.appendFraction(ChronoField.MICRO_OF_SECOND, 0, 9, true)
        offsetPattern.foreach(p => dtf.appendPattern(p))
        dtf.toFormatter
    }
}

object javaDates {
    val javaDateBijection: BijectionT[Result, Result, UtilDate, ZonedDateTime] =
        bijection (
            (ud: UtilDate)       => Okay(ZonedDateTime.ofInstant(ud.toInstant, ZoneOffset.UTC)): Result[ZonedDateTime],
            (zdt: ZonedDateTime) => Okay(UtilDate.from(zdt.toInstant)): Result[UtilDate]
        )
    val javaSqlDateBijection: BijectionT[Result, Result, SqlDate, LocalDate] =
        bijection (
            (sd: SqlDate)   => Okay(sd.toLocalDate): Result[LocalDate],
            (ld: LocalDate) => Okay(SqlDate.valueOf(ld)): Result[SqlDate]
        )
    val javaSqlTimeBijection: BijectionT[Result, Result, SqlTime, LocalTime] =
        bijection (
            (st: SqlTime)   => Okay(st.toLocalTime): Result[LocalTime],
            (lt: LocalTime) => Okay(SqlTime.valueOf(lt)): Result[SqlTime]
        )
    val javaSqlTimestampBijection: BijectionT[Result, Result, SqlTimestamp, ZonedDateTime] =
        bijection (
            (sts: SqlTimestamp) => Okay(ZonedDateTime.ofInstant(sts.toInstant, ZoneOffset.UTC)): Result[ZonedDateTime],
            (zdt: ZonedDateTime) => Okay(SqlTimestamp.from(zdt.toInstant)): Result[SqlTimestamp]
        )
}

class DateTimeStringBijections (
    dateTimeFormatters:      NonEmptyList[DateTimeFormatter],
    localDateFormatters:     NonEmptyList[DateTimeFormatter],
    localDateTimeFormatters: NonEmptyList[DateTimeFormatter],
    localTimeFormatters:     NonEmptyList[DateTimeFormatter]
) {
    private def fail(encodeFormatter: DateTimeFormatter): Failed =
        Failed("incorrectly formatted date -- expected format like  " + ZonedDateTime.now.format(encodeFormatter))

    val zonedDateTimeBijection: BijectionT[Result, Result, ZonedDateTime, String] =
        bijection (
            zdt => tryCatchValue(zdt.format(dateTimeFormatters.head)): Result[String],
            s => firstOrLastG(fail(dateTimeFormatters.head), dateTimeFormatters) { formatter =>
                tryCatchValue(ZonedDateTime.parse(s, formatter))
            }: Result[ZonedDateTime]
        )

    val localDateBijection: BijectionT[Result, Result, LocalDate, String] =
        bijection (
            ld => tryCatchValue(ld.format(localDateFormatters.head)): Result[String],
            s => firstOrLastG(fail(localDateFormatters.head), localDateFormatters) { formatter =>
                tryCatchValue(LocalDate.parse(s, formatter))
            }: Result[LocalDate]
        )

    val localDateTimeBijection: BijectionT[Result, Result, LocalDateTime, String] =
        bijection (
            ldt => tryCatchValue(ldt.format(localDateTimeFormatters.head)): Result[String],
            s => firstOrLastG(fail(localDateTimeFormatters.head), localDateTimeFormatters) { formatter =>
                tryCatchValue(LocalDateTime.parse(s, formatter))
            }: Result[LocalDateTime]
        )

    val localTimeBijection: BijectionT[Result, Result, LocalTime, String] =
        bijection (
            lt => tryCatchValue(lt.format(localTimeFormatters.head)): Result[String],
            s => firstOrLastG(fail(localTimeFormatters.head), localTimeFormatters) { formatter =>
                tryCatchValue(LocalTime.parse(s, formatter))
            }: Result[LocalTime]
        )

    val javaDateBijection: BijectionT[Result, Result, UtilDate, String] =
        zonedDateTimeBijection <=< javaDates.javaDateBijection

    val javaSqlDateBijection: BijectionT[Result, Result, SqlDate, String] =
        localDateBijection <=< javaDates.javaSqlDateBijection

    val javaSqlTimeBijection: BijectionT[Result, Result, SqlTime, String] =
        localTimeBijection <=< javaDates.javaSqlTimeBijection

    val javaSqlTimestampBijection: BijectionT[Result, Result, SqlTimestamp, String] =
        zonedDateTimeBijection <=< javaDates.javaSqlTimestampBijection
}

object long {
    val zonedDateTimeBijection: BijectionT[Result, Result, ZonedDateTime, Long] =
        bijection (
            (zdt: ZonedDateTime) => Okay(zdt.toInstant.toEpochMilli): Result[Long],
            (l: Long) => tryCatchValue(ZonedDateTime.ofInstant(Instant.ofEpochMilli(l), ZoneId.systemDefault)): Result[ZonedDateTime]
        )

    val localDateBijection: BijectionT[Result, Result, LocalDate, Long] =
        bijection (
            (ld: LocalDate) => Okay(ld.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli): Result[Long],
            (l: Long) => tryCatchValue(LocalDateTime.ofInstant(Instant.ofEpochMilli(l), ZoneOffset.UTC).toLocalDate): Result[LocalDate]
        )

    val localDateTimeBijection: BijectionT[Result, Result, LocalDateTime, Long] =
        bijection (
            (ldt: LocalDateTime) => Okay(ldt.toInstant(ZoneOffset.UTC).toEpochMilli): Result[Long],
            (l: Long) => tryCatchValue(LocalDateTime.ofInstant(Instant.ofEpochMilli(l), ZoneOffset.UTC)): Result[LocalDateTime]
        )

    val localTimeBijection: BijectionT[Result, Result, LocalTime, Long] =
        bijection (
            (lt: LocalTime) => Okay(lt.toNanoOfDay / 1000000L): Result[Long],
            (l: Long) => tryCatchValue(LocalTime.ofNanoOfDay(l * 1000000L)): Result[LocalTime]
        )

    val javaDateBijection: BijectionT[Result, Result, UtilDate, Long] =
        zonedDateTimeBijection <=< javaDates.javaDateBijection

    val javaSqlDateBijection: BijectionT[Result, Result, SqlDate, Long] =
        localDateBijection <=< javaDates.javaSqlDateBijection

    val javaSqlTimeBijection: BijectionT[Result, Result, SqlTime, Long] =
        localTimeBijection <=< javaDates.javaSqlTimeBijection

    val javaSqlTimestampBijection: BijectionT[Result, Result, SqlTimestamp, Long] =
        zonedDateTimeBijection <=< javaDates.javaSqlTimestampBijection
}


object iso8601 extends DateTimeStringBijections (
    dateTimeFormatters = NonEmptyList (
        // ISO_OFFSET_DATETIME doesn't print seconds or millis at all if they're zero, but the old Joda Time code did so we need this for backwards compatibility when rendering datetimes
        DateTimeFormatter.ofPattern("uuuu-MM-dd'T'HH:mm:ss.SSSXXX"),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm:ss", Some("[ ]XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm:ss", Some("[ ]XX")), // This is required to allow offsets without a colon, e.g. +0100
        DateTimeFormatter.ISO_OFFSET_DATE_TIME, // Seems to be equivalent to: uuuu-MM-dd'T'HH:mm[:ss][.SSS]XXX
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmmss", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmmss", Some("[ ]XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm:ss", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm:ss", Some("[ ]XXX"))
    ),
    localDateFormatters = NonEmptyList (
        DateTimeFormatter.ISO_DATE,      // Seems to be equivalent to uuuu-MM-dd
        DateTimeFormatter.BASIC_ISO_DATE // Seems to be equivalent to uuuuMMdd
    ),
    localDateTimeFormatters = NonEmptyList (
        // ISO_LOCAL_DATE_TIME doesn't print millis at all if they're zero, but the old Joda Time code did so we need this for backwards compatibility when rendering datetimes
        DateTimeFormatter.ofPattern("uuuu-MM-dd'T'HH:mm:ss.SSS"),
        DateTimeFormatter.ISO_LOCAL_DATE_TIME, // Seems to be equivalent to uuuu-MM-dd'T'HH:mm[:ss][.SSS]
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmmss")
    ),
    localTimeFormatters = NonEmptyList (
        // ISO_LOCAL_TIME doesn't print millis at all if they're zero, but the old Joda Time code did so we need this for backwards compatibility when rendering datetimes
        DateTimeFormatter.ofPattern("HH:mm:ss.SSS"),
        DateTimeFormatter.ISO_LOCAL_TIME,
        DateTimeFormatters.withOptionalFractionalSecondRange("HHmmss")
    )
)

object classic extends DateTimeStringBijections (
    dateTimeFormatters = NonEmptyList (
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm:ss", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm:ss", Some("[ ]XXX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E MMM dd HH:mm:ss", Some(" X uuuu")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E, dd MMM yy HH:mm:ss", Some(" XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("E, dd MMM yy HH:mm:ss", Some(" XXX")),
        DateTimeFormatter.ISO_OFFSET_DATE_TIME,
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmmss", Some("[ ]XX")),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuuMMdd'T'HHmmss", Some("[ ]XXX")),
        // For some terrible reason our documentation says timezone is optional so we'll fall back to this
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm:ss").withZone(ZoneOffset.UTC),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd HH:mm:ss").withZone(ZoneOffset.UTC),
        DateTimeFormatters.withOptionalFractionalSecondRange("uuuu-MM-dd'T'HH:mm:ss", Some("[ ]XX"))
    ),
    localDateFormatters = NonEmptyList (
        DateTimeFormatter.ofPattern("uuuu-MM-dd"),
        DateTimeFormatter.ofPattern("E MMM dd uuuu"),
        DateTimeFormatter.ofPattern("E, dd MMM uu")
    ),
    localDateTimeFormatters = NonEmptyList (
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss"),
        DateTimeFormatter.ofPattern("E MMM dd HH:mm:ss uuuu"),
        DateTimeFormatter.ofPattern("E, dd MMM uu HH:mm:ss")
    ),
    localTimeFormatters = NonEmptyList (
        DateTimeFormatters.withOptionalFractionalSecondRange("HH:mm:ss")
    )
)

object sqlServer extends DateTimeStringBijections (
    dateTimeFormatters = NonEmptyList (
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSS")
    ),
    localDateFormatters = NonEmptyList (
        DateTimeFormatter.ofPattern("uuuu-MM-dd")
    ),
    localDateTimeFormatters = NonEmptyList (
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSS")
    ),
    localTimeFormatters = NonEmptyList (
        DateTimeFormatter.ofPattern("HH:mm:ss.SSS")
    )
)
