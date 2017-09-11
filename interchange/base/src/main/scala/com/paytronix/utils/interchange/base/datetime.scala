//
// Copyright 2014 Paytronix Systems, Inc.
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

import java.sql.{Date => JavaSqlDate, Time => JavaSqlTime, Timestamp => JavaSqlTimestamp}
import java.util.{Date => JavaDate}

import org.joda.time.{DateTime, DateTimeZone, LocalDate, LocalDateTime, LocalTime}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter, ISODateTimeFormat}
import scalaz.{BijectionT, NonEmptyList}

import com.paytronix.utils.interchange.base.terminal
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, firstOrLastG, tryCatchValue}

import BijectionT.bijection

object constants {
    val epoch = new LocalDate(1970, 1, 1)
}

object javaDates {
    val javaDateBijection: BijectionT[Result, Result, JavaDate, DateTime] =
        bijection (
            (jud: JavaDate) => Okay(new DateTime(jud)): Result[DateTime],
            (dt: DateTime) => Okay(dt.toDate): Result[JavaDate]
        )
    val javaSqlDateBijection: BijectionT[Result, Result, JavaSqlDate, LocalDate] =
        bijection (
            (jsd: JavaSqlDate) => Okay(new LocalDate(jsd)): Result[LocalDate],
            (ld: LocalDate) => Okay(new JavaSqlDate(ld.toDateTimeAtStartOfDay().getMillis)): Result[JavaSqlDate]
        )
    val javaSqlTimeBijection: BijectionT[Result, Result, JavaSqlTime, LocalTime] =
        bijection (
            (jst: JavaSqlTime) => Okay(new LocalTime(jst)): Result[LocalTime],
            (lt: LocalTime) => Okay(new JavaSqlTime(constants.epoch.toDateTime(lt).getMillis)): Result[JavaSqlTime] // gross
        )
    val javaSqlTimestampBijection: BijectionT[Result, Result, JavaSqlTimestamp, DateTime] =
        bijection (
            (jsts: JavaSqlTimestamp) => Okay(new DateTime(jsts)): Result[DateTime],
            (dt: DateTime) => Okay(new JavaSqlTimestamp(dt.getMillis)): Result[JavaSqlTimestamp]
        )
}

class DateTimeStringBijections (
    dateTimeFormatters:      NonEmptyList[DateTimeFormatter],
    localDateFormatters:     NonEmptyList[DateTimeFormatter],
    localDateTimeFormatters: NonEmptyList[DateTimeFormatter],
    localTimeFormatters:     NonEmptyList[DateTimeFormatter]
) {
    private def fail(encodeFormatter: DateTimeFormatter): Failed =
        Failed("incorrectly formatted date -- expected format like  " + encodeFormatter.print(new DateTime()))

    val dateTimeBijection: BijectionT[Result, Result, DateTime, String] =
        bijection (
            dt => tryCatchValue(dateTimeFormatters.head.print(dt)): Result[String],
            s => firstOrLastG(fail(dateTimeFormatters.head), dateTimeFormatters) { formatter =>
                tryCatchValue(formatter.parseDateTime(s))
            }: Result[DateTime]
        )

    val localDateBijection: BijectionT[Result, Result, LocalDate, String] =
        bijection (
            ld => tryCatchValue(localDateFormatters.head.print(ld)): Result[String],
            s => firstOrLastG(fail(localDateFormatters.head), localDateFormatters) { formatter =>
                tryCatchValue(formatter.parseLocalDate(s))
            }: Result[LocalDate]
        )

    val localDateTimeBijection: BijectionT[Result, Result, LocalDateTime, String] =
        bijection (
            ldt => tryCatchValue(localDateTimeFormatters.head.print(ldt)): Result[String],
            s => firstOrLastG(fail(localDateTimeFormatters.head), localDateTimeFormatters) { formatter =>
                tryCatchValue(formatter.parseLocalDateTime(s))
            }: Result[LocalDateTime]
        )

    val localTimeBijection: BijectionT[Result, Result, LocalTime, String] =
        bijection (
            lt => tryCatchValue(localTimeFormatters.head.print(lt)): Result[String],
            s => firstOrLastG(fail(localTimeFormatters.head), localTimeFormatters) { formatter =>
                tryCatchValue(formatter.parseLocalTime(s))
            }: Result[LocalTime]
        )

    val javaDateBijection: BijectionT[Result, Result, JavaDate, String] =
        dateTimeBijection <=< javaDates.javaDateBijection

    val javaSqlDateBijection: BijectionT[Result, Result, JavaSqlDate, String] =
        localDateBijection <=< javaDates.javaSqlDateBijection

    val javaSqlTimeBijection: BijectionT[Result, Result, JavaSqlTime, String] =
        localTimeBijection <=< javaDates.javaSqlTimeBijection

    val javaSqlTimestampBijection: BijectionT[Result, Result, JavaSqlTimestamp, String] =
        dateTimeBijection <=< javaDates.javaSqlTimestampBijection
}

object long {
    val dateTimeBijection: BijectionT[Result, Result, DateTime, Long] =
        bijection (
            (dt: DateTime) => Okay(dt.getMillis): Result[Long],
            (l: Long) => tryCatchValue(new DateTime(l)): Result[DateTime]
        )

    val localDateBijection: BijectionT[Result, Result, LocalDate, Long] =
        bijection (
            (ld: LocalDate) => Okay(ld.toDateTimeAtStartOfDay(DateTimeZone.UTC).getMillis): Result[Long],
            (l: Long) => tryCatchValue(new DateTime(l, DateTimeZone.UTC).toLocalDate): Result[LocalDate]
        )

    val localDateTimeBijection: BijectionT[Result, Result, LocalDateTime, Long] =
        bijection (
            (ldt: LocalDateTime) => Okay(ldt.toDateTime(DateTimeZone.UTC).getMillis): Result[Long],
            (l: Long) => tryCatchValue(new DateTime(l, DateTimeZone.UTC).toLocalDateTime): Result[LocalDateTime]
        )

    val localTimeBijection: BijectionT[Result, Result, LocalTime, Long] =
        bijection (
            (lt: LocalTime) => Okay(lt.toDateTime(constants.epoch.toDateTimeAtStartOfDay).getMillis): Result[Long],
            (l: Long) => tryCatchValue(new DateTime(l).toLocalTime): Result[LocalTime]
        )

    val javaDateBijection: BijectionT[Result, Result, JavaDate, Long] =
        dateTimeBijection <=< javaDates.javaDateBijection

    val javaSqlDateBijection: BijectionT[Result, Result, JavaSqlDate, Long] =
        localDateBijection <=< javaDates.javaSqlDateBijection

    val javaSqlTimeBijection: BijectionT[Result, Result, JavaSqlTime, Long] =
        localTimeBijection <=< javaDates.javaSqlTimeBijection

    val javaSqlTimestampBijection: BijectionT[Result, Result, JavaSqlTimestamp, Long] =
        dateTimeBijection <=< javaDates.javaSqlTimestampBijection
}

object iso8601 extends DateTimeStringBijections (
    dateTimeFormatters = NonEmptyList (
        ISODateTimeFormat.dateTime.withOffsetParsed,
        ISODateTimeFormat.basicDateTime.withOffsetParsed,
        ISODateTimeFormat.dateTimeNoMillis.withOffsetParsed,
        ISODateTimeFormat.basicDateTimeNoMillis.withOffsetParsed,
        DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss.SSS Z"),
        DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss Z")
    ),
    localDateFormatters = NonEmptyList (
        DateTimeFormat.forPattern("yyyy-MM-dd"),
        DateTimeFormat.forPattern("yyyyMMdd")
    ),
    localDateTimeFormatters = NonEmptyList (
        DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSS"),
        DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss"),
        DateTimeFormat.forPattern("yyyyMMdd'T'HHmmss.SSS"),
        DateTimeFormat.forPattern("yyyyMMdd'T'HHmmss")
    ),
    localTimeFormatters = NonEmptyList (
        DateTimeFormat.forPattern("HH:mm:ss.SSS"),
        DateTimeFormat.forPattern("HH:mm:ss"),
        DateTimeFormat.forPattern("HHmmss.SSS"),
        DateTimeFormat.forPattern("HHmmss")
    )
)

object classic extends DateTimeStringBijections (
    dateTimeFormatters = NonEmptyList (
        DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss Z").withOffsetParsed,
        DateTimeFormat.forPattern("E MMM dd HH:mm:ss Z yyyy").withOffsetParsed,
        DateTimeFormat.forPattern("E, dd MMM yy HH:mm:ss Z").withOffsetParsed,
        ISODateTimeFormat.dateTime.withOffsetParsed,
        ISODateTimeFormat.basicDateTime.withOffsetParsed,
        ISODateTimeFormat.dateTimeNoMillis.withOffsetParsed,
        ISODateTimeFormat.basicDateTimeNoMillis.withOffsetParsed
    ),
    localDateFormatters = NonEmptyList (
        DateTimeFormat.forPattern("yyyy-MM-dd"),
        DateTimeFormat.forPattern("E MMM dd yyyy"),
        DateTimeFormat.forPattern("E, dd MMM yy")
    ),
    localDateTimeFormatters = NonEmptyList (
        DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss"),
        DateTimeFormat.forPattern("E MMM dd HH:mm:ss yyyy"),
        DateTimeFormat.forPattern("E, dd MMM yy HH:mm:ss"),
        ISODateTimeFormat.localDateOptionalTimeParser // FIXME not ideal due to optional time
    ),
    localTimeFormatters = NonEmptyList (
        DateTimeFormat.forPattern("HH:mm:ss.SSS"),
        DateTimeFormat.forPattern("HH:mm:ss"),
        ISODateTimeFormat.localTimeParser
    )
)

object sqlServer extends DateTimeStringBijections (
    dateTimeFormatters = NonEmptyList (
        DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss.SSS")
    ),
    localDateFormatters = NonEmptyList (
        DateTimeFormat.forPattern("yyyy-MM-dd")
    ),
    localDateTimeFormatters = NonEmptyList (
        DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss.SSS")
    ),
    localTimeFormatters = NonEmptyList (
        DateTimeFormat.forPattern("HH:mm:ss.SSS")
    )
)
