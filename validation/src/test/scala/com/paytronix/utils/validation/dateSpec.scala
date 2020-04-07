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

package com.paytronix.utils.validation

import cats.data.NonEmptyList
import java.util.Calendar
import org.joda.time.{DateTime, DateTimeZone, LocalDate, LocalDateTime, LocalTime, Period}
import org.joda.time.format.{ISODateTimeFormat, ISOPeriodFormat, PeriodFormat}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import base.valueOps
import date._

object dateSpec {
    def reasonablePeriod(l: Long): Boolean =
        l >= (Int.MinValue: Long) * 2L && l <= (Int.MaxValue: Long) * 2L

    // 1900 to avoid annoying transitions between gregorian and julian calendars as well as other things like the introduction of Standard Railway Time in 1883
    // that show up when converting between Joda and java.util.Date that I don't want to deal with right now
    val minTimestamp = new DateTime("1900-01-01T00:00:00.000Z").getMillis
    // 8099 because java.sql.Date doesn't like dates beyond that year
    val maxTimestamp = new DateTime("8099-12-31T23:59:59.999Z").getMillis

    implicit val arbitraryDateTime:      Arbitrary[DateTime]      = Arbitrary { Gen.choose(minTimestamp, maxTimestamp).map { new DateTime(_) } }
    implicit val arbitraryLocalDate:     Arbitrary[LocalDate]     = Arbitrary { arbitraryDateTime.arbitrary.map(_.toLocalDate) }
    implicit val arbitraryLocalDateTime: Arbitrary[LocalDateTime] = Arbitrary { arbitraryDateTime.arbitrary.map(_.toLocalDateTime) }
    implicit val arbitraryLocalTime:     Arbitrary[LocalTime]     = Arbitrary { arbitraryDateTime.arbitrary.map(_.toLocalTime) }
    implicit val arbitraryPeriod:        Arbitrary[Period]        = Arbitrary { Gen.choose((Int.MinValue: Long) * 2L, (Int.MaxValue: Long) * 2L).map { new Period(_) } }

    def calendarField(field: Int): java.util.Date => Int = { d =>
        val cal = Calendar.getInstance
        cal.setLenient(false)
        cal.setTime(d)
        cal.get(field)
    }

}

import dateSpec._

class dateTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s"""Date validation
        `dateTimeWithFormat` $dateTimeWithFormat1
        `dateTimeWithFormats` (a, b)(a) $dateTimeWithFormats1
        `dateTimeWithFormats` (b, a)(a) $dateTimeWithFormats2
        `dateTimeWithFormats` (b, a)(b) $dateTimeWithFormats3
        `localDateWithFormat` $localDateWithFormat1
        `localDateWithFormats` (a, b)(a) $localDateWithFormats1
        `localDateWithFormats` (b, a)(a) $localDateWithFormats2
        `localDateWithFormats` (b, a)(b) $localDateWithFormats3
        `localDateTimeWithFormat` $localDateTimeWithFormat1
        `localDateTimeWithFormats` (a, b)(a) $localDateTimeWithFormats1
        `localDateTimeWithFormats` (b, a)(a) $localDateTimeWithFormats2
        `localDateTimeWithFormats` (b, a)(b) $localDateTimeWithFormats3
        `localTimeWithFormat` $localTimeWithFormat1
        `localTimeWithFormats` (a, b)(a) $localTimeWithFormats1
        `localTimeWithFormats` (b, a)(a) $localTimeWithFormats2
        `localTimeWithFormats` (b, a)(b) $localTimeWithFormats3
        `periodWithFormat` $periodWithFormat1
        `periodWithFormats` (a, b)(a) $periodWithFormats1
        `periodWithFormats` (b, a)(a) $periodWithFormats2
        `periodWithFormats` (b, a)(b) $periodWithFormats3
    """

    def dateTimeWithFormat1       = prop { (dt:  DateTime     ) => (dt.toString(ISODateTimeFormat.dateTime)      is dateTimeWithFormat(ISODateTimeFormat.dateTime)) ==== base.success(dt) }
    def dateTimeWithFormats1      = prop { (dt:  DateTime     ) => (dt.toString(ISODateTimeFormat.dateTime)      is dateTimeWithFormats(NonEmptyList.of(ISODateTimeFormat.dateTime, ISODateTimeFormat.basicDateTime))) ==== base.success(dt) }
    def dateTimeWithFormats2      = prop { (dt:  DateTime     ) => (dt.toString(ISODateTimeFormat.dateTime)      is dateTimeWithFormats(NonEmptyList.of(ISODateTimeFormat.basicDateTime, ISODateTimeFormat.dateTime))) ==== base.success(dt) }
    def dateTimeWithFormats3      = prop { (dt:  DateTime     ) => (dt.toString(ISODateTimeFormat.basicDateTime) is dateTimeWithFormats(NonEmptyList.of(ISODateTimeFormat.basicDateTime, ISODateTimeFormat.dateTime))) ==== base.success(dt) }
    def localDateWithFormat1      = prop { (ld:  LocalDate    ) => (ld.toString("yyyy-MM-dd")                    is localDateWithFormat("yyyy-MM-dd")) ==== base.success(ld) }
    def localDateWithFormats1     = prop { (ld:  LocalDate    ) => (ld.toString("yyyy-MM-dd")                    is localDateWithFormats(NonEmptyList.of("yyyy-MM-dd", "yyyyMMdd"))) ==== base.success(ld) }
    def localDateWithFormats2     = prop { (ld:  LocalDate    ) => (ld.toString("yyyy-MM-dd")                    is localDateWithFormats(NonEmptyList.of("yyyyMMdd", "yyyy-MM-dd"))) ==== base.success(ld) }
    def localDateWithFormats3     = prop { (ld:  LocalDate    ) => (ld.toString("yyyyMMdd")                      is localDateWithFormats(NonEmptyList.of("yyyyMMdd", "yyyy-MM-dd"))) ==== base.success(ld) }
    def localDateTimeWithFormat1  = prop { (ldt: LocalDateTime) => (ldt.toString("yyyy-MM-dd'T'HH:mm:ss.SSS")    is localDateTimeWithFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")) ==== base.success(ldt) }
    def localDateTimeWithFormats1 = prop { (ldt: LocalDateTime) => (ldt.toString("yyyy-MM-dd'T'HH:mm:ss.SSS")    is localDateTimeWithFormats(NonEmptyList.of("yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyyMMdd'T'HHmmss.SSS"))) ==== base.success(ldt) }
    def localDateTimeWithFormats2 = prop { (ldt: LocalDateTime) => (ldt.toString("yyyy-MM-dd'T'HH:mm:ss.SSS")    is localDateTimeWithFormats(NonEmptyList.of("yyyyMMdd'T'HHmmss.SSS", "yyyy-MM-dd'T'HH:mm:ss.SSS"))) ==== base.success(ldt) }
    def localDateTimeWithFormats3 = prop { (ldt: LocalDateTime) => (ldt.toString("yyyyMMdd'T'HHmmss.SSS")        is localDateTimeWithFormats(NonEmptyList.of("yyyyMMdd'T'HHmmss.SSS", "yyyy-MM-dd'T'HH:mm:ss.SSS"))) ==== base.success(ldt) }
    def localTimeWithFormat1      = prop { (lt:  LocalTime    ) => (lt.toString("HH:mm:ss.SSS")                  is localTimeWithFormat("HH:mm:ss.SSS")) ==== base.success(lt) }
    def localTimeWithFormats1     = prop { (lt:  LocalTime    ) => (lt.toString("HH:mm:ss.SSS")                  is localTimeWithFormats(NonEmptyList.of("HH:mm:ss.SSS", "HHmmss.SSS"))) ==== base.success(lt) }
    def localTimeWithFormats2     = prop { (lt:  LocalTime    ) => (lt.toString("HH:mm:ss.SSS")                  is localTimeWithFormats(NonEmptyList.of("HHmmss.SSS", "HH:mm:ss.SSS"))) ==== base.success(lt) }
    def localTimeWithFormats3     = prop { (lt:  LocalTime    ) => (lt.toString("HHmmss.SSS")                    is localTimeWithFormats(NonEmptyList.of("HHmmss.SSS", "HH:mm:ss.SSS"))) ==== base.success(lt) }
    def periodWithFormat1         = prop { (p:   Period       ) => (p.toString(ISOPeriodFormat.standard)         is periodWithFormat(ISOPeriodFormat.standard)) ==== base.success(p) }
    def periodWithFormats1        = prop { (p:   Period       ) => (p.toString(ISOPeriodFormat.standard)         is periodWithFormats(NonEmptyList.of(ISOPeriodFormat.standard, PeriodFormat.wordBased))) ==== base.success(p) }
    def periodWithFormats2        = prop { (p:   Period       ) => (p.toString(ISOPeriodFormat.standard)         is periodWithFormats(NonEmptyList.of(PeriodFormat.wordBased, ISOPeriodFormat.standard))) ==== base.success(p) }
    def periodWithFormats3        = prop { (p:   Period       ) => (p.toString(PeriodFormat.wordBased)           is periodWithFormats(NonEmptyList.of(PeriodFormat.wordBased, ISOPeriodFormat.standard))) ==== base.success(p) }
}

class javaDateTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""Java Date validation
        `javaUtilDate` should work like the example         $javaUtilDate1
        `localJavaUtilDate` should work like the example    $localJavaUtilDate1
        `javaSqlDate` should work like the example          $javaSqlDate1
        `javaSqlTime` should work like the example          $javaSqlTime1
        `javaSqlTimestamp` should work like the example     $javaSqlTimestamp1
    """

    def javaUtilDate1 = {
        val f = {
            import base.validationFunctionOps
            isoDateTime and javaUtilDate
        }

        prop { (dt: DateTime) =>
            val res = ISODateTimeFormat.dateTime.print(dt) is f
            (res.map(_.getTime) ==== base.success(dt.getMillis)).updateMessage { s =>
                s"expected datetime $dt, validation result $res: $s"
            }
        }
    }

    def localJavaUtilDate1 = {
        val f = {
            import base.validationFunctionOps
            isoishLocalDateTime and localJavaUtilDate
        }

        prop { (dt: DateTime) =>
            val res = ISODateTimeFormat.dateHourMinuteSecondMillis.withZone(DateTimeZone.getDefault).print(dt) is f
            (res.map(_.getTime) ==== base.success(dt.getMillis)).updateMessage { s =>
                s"expected datetime $dt, validation result $res: $s"
            }
        }
    }

    def javaSqlDate1 = {
        val f = {
            import base.validationFunctionOps
            isoDate and javaSqlDate
        }
        prop { (dt: DateTime) =>
            val res = ISODateTimeFormat.date.print(dt) is f

            def failed(which: String): String => String = s =>
                s"expected datetime $dt, validation result $res, but $which didn't match: $s"

            (res.map(calendarField(Calendar.YEAR)) ==== base.success(dt.getYear)).updateMessage(failed("year")) and
            (res.map(calendarField(Calendar.MONTH)).map(_+1) ==== base.success(dt.getMonthOfYear)).updateMessage(failed("month")) and
            (res.map(calendarField(Calendar.DAY_OF_MONTH)) ==== base.success(dt.getDayOfMonth)).updateMessage(failed("day"))
        }
    }

    def javaSqlTime1 = {
        val f = {
            import base.validationFunctionOps
            isoTime and javaSqlTime
        }
        prop { (dt: LocalTime) =>
            val res = ISODateTimeFormat.time.print(dt) is f

            def failed(which: String): String => String = s =>
                s"expected datetime $dt, validation result $res, but $which didn't match: $s"

            (res.map(calendarField(Calendar.HOUR_OF_DAY)) ==== base.success(dt.getHourOfDay)).updateMessage(failed("hour")) and
            (res.map(calendarField(Calendar.MINUTE)) ==== base.success(dt.getMinuteOfHour)).updateMessage(failed("minute")) and
            (res.map(calendarField(Calendar.SECOND)) ==== base.success(dt.getSecondOfMinute)).updateMessage(failed("second"))
        }
    }

    def javaSqlTimestamp1 = {
        val f = {
            import base.validationFunctionOps
            isoDateTime and javaSqlTimestamp
        }
        prop { (dt: DateTime) =>
            val res = ISODateTimeFormat.dateTime.print(dt) is f
            (res.map(_.getTime) ==== base.success(dt.getMillis)).updateMessage { s =>
                s"expected datetime $dt, validation result $res: $s"
            }
        }
    }
}
