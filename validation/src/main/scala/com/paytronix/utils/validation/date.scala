//
// Copyright 2012-2014 Paytronix Systems, Inc.
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

import scala.language.implicitConversions

import java.util.TimeZone
import org.joda.time.{DateTime, DateTimeZone, LocalDate, LocalDateTime, LocalTime, Period, ReadableInstant}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter, ISODateTimeFormat, PeriodFormatter}
import scalaz.NonEmptyList

import base.{Validated, ValidationError, anyE, failure, predicateE, success}
import NonEmptyList.nels

object date {
    val invalidDateFormatError = ValidationError("invalid_date", "invalid date") // FIXME? explain what is expected?
    val invalidTimeFormatError = ValidationError("invalid_time", "invalid time") // FIXME? explain what is expected?
    val invalidPeriodFormatError = ValidationError("invalid_period", "invalid period") // FIXME? explain what is expected?

    /**
     * Validation function which maps a Joda `ReadableInstant` (e.g. `DateTime`) to a `java.util.Date`
     * Example: `isoDateTime and javaUtilDate`
     */
    val javaUtilDate: ReadableInstant => Validated[java.util.Date] =
        in => success(new java.util.Date(in.getMillis))

    /**
     * Validation function which maps a Joda `LocalDateTime` to a `java.util.Date` in the default time zone
     * Example: `isoishLocalDateTime and localJavaUtilDate`
     */
    val localJavaUtilDate: LocalDateTime => Validated[java.util.Date] =
        in => success(in.toDate(TimeZone.getDefault))

    /**
     * Validation function which maps a Joda `LocalDate` to a `java.sql.Date` in the default time zone
     * Example: `isoDate and javaSqlDate`
     */
    val javaSqlDate: LocalDate => Validated[java.sql.Date] =
        in => success(new java.sql.Date(in.toDate().getTime))

    /**
     * Validation function which maps a Joda `LocalTime` to a `java.sql.Time`
     * Example: `isoTime and javaSqlTime`
     */
    val javaSqlTime: LocalTime => Validated[java.sql.Time] =
        in => success(new java.sql.Time(new LocalDate(1970, 1, 1).toDateTime(in).getMillis))

    /**
     * Validation function which maps a Joda `ReadableInstant` (e.g. `DateTime`) to a `java.sql.Timestamp`
     * Example: `isoDateTime and javaSqlTimestamp`
     */
    val javaSqlTimestamp: DateTime => Validated[java.sql.Timestamp] =
        in => success(new java.sql.Timestamp(in.getMillis))

    /** Magnet for specifying `DateTimeFormatter`s either as strings to be passed to `DateTimeFormat.forPattern` or some specific formatter */
    final case class DateTimeFormatterMagnet(formatter: DateTimeFormatter)
    object DateTimeFormatterMagnet {
        implicit def fromString(s: String): DateTimeFormatterMagnet = DateTimeFormatterMagnet(DateTimeFormat.forPattern(s))
        implicit def fromDateTimeFormatter(dtf: DateTimeFormatter): DateTimeFormatterMagnet = DateTimeFormatterMagnet(dtf)
        implicit def fromStringNEL(ss: NonEmptyList[String]): NonEmptyList[DateTimeFormatterMagnet] = ss.map(fromString)
        implicit def fromDateTimeFormatterNEL(dtfs: NonEmptyList[DateTimeFormatter]): NonEmptyList[DateTimeFormatterMagnet] = dtfs.map(fromDateTimeFormatter)
    }

    /** Parse a string as a `DateTime` with a particular format */
    val dateTimeWithFormat: DateTimeFormatterMagnet => String => Validated[DateTime] =
        dateTimeWithFormatE(_ => invalidDateFormatError)

    /** Parse a string as a `DateTime` with a particular format */
    def dateTimeWithFormatE(error: Exception => ValidationError)(format: DateTimeFormatterMagnet): String => Validated[DateTime] =
        in => try success(format.formatter.parseDateTime(in)) catch { case e: Exception => failure(error(e)) }

    /** Parse a string as a `DateTime` in one of several formats */
    val dateTimeWithFormats: NonEmptyList[DateTimeFormatterMagnet] => String => Validated[DateTime] =
        dateTimeWithFormatsE(invalidDateFormatError)

    /** Parse a string as a `DateTime` in one of several formats */
    def dateTimeWithFormatsE(error: ValidationError)(formats: NonEmptyList[DateTimeFormatterMagnet]): String => Validated[DateTime] =
        anyE(error)(formats map dateTimeWithFormat)


    /** Parse a string as a `LocalDate` with a particular format */
    val localDateWithFormat: DateTimeFormatterMagnet => String => Validated[LocalDate] =
        localDateWithFormatE(_ => invalidDateFormatError)

    /** Parse a string as a `LocalDate` with a particular format */
    def localDateWithFormatE(error: Exception => ValidationError)(format: DateTimeFormatterMagnet): String => Validated[LocalDate] =
        in => try success(format.formatter.parseLocalDate(in)) catch { case e: Exception => failure(error(e)) }

    /** Parse a string as a `LocalDate` in one of several formats */
    val localDateWithFormats: NonEmptyList[DateTimeFormatterMagnet] => String => Validated[LocalDate] =
        localDateWithFormatsE(invalidDateFormatError)

    /** Parse a string as a `LocalDate` in one of several formats */
    def localDateWithFormatsE(error: ValidationError)(formats: NonEmptyList[DateTimeFormatterMagnet]): String => Validated[LocalDate] =
        anyE(error)(formats map localDateWithFormat)


    /** Parse a string as a `LocalDateTime` with a particular format */
    val localDateTimeWithFormat: DateTimeFormatterMagnet => String => Validated[LocalDateTime] =
        localDateTimeWithFormatE(_ => invalidDateFormatError)

    /** Parse a string as a `LocalDateTime` with a particular format */
    def localDateTimeWithFormatE(error: Exception => ValidationError)(format: DateTimeFormatterMagnet): String => Validated[LocalDateTime] =
        in => try success(format.formatter.parseLocalDateTime(in)) catch { case e: Exception => failure(error(e)) }

    /** Parse a string as a `LocalDateTime` in one of several formats */
    val localDateTimeWithFormats: NonEmptyList[DateTimeFormatterMagnet] => String => Validated[LocalDateTime] =
        localDateTimeWithFormatsE(invalidDateFormatError)

    /** Parse a string as a `LocalDateTime` in one of several formats */
    def localDateTimeWithFormatsE(error: ValidationError)(formats: NonEmptyList[DateTimeFormatterMagnet]): String => Validated[LocalDateTime] =
        anyE(error)(formats map localDateTimeWithFormat)


    /** Parse a string as a `LocalTime` with a particular format */
    val localTimeWithFormat: DateTimeFormatterMagnet => String => Validated[LocalTime] =
        localTimeWithFormatE(_ => invalidTimeFormatError)

    /** Parse a string as a `LocalDateTime` with a particular format */
    def localTimeWithFormatE(error: Exception => ValidationError)(format: DateTimeFormatterMagnet): String => Validated[LocalTime] =
        in => try success(format.formatter.parseLocalTime(in)) catch { case e: Exception => failure(error(e)) }

    /** Parse a string as a `LocalTime` in one of several formats */
    val localTimeWithFormats: NonEmptyList[DateTimeFormatterMagnet] => String => Validated[LocalTime] =
        localTimeWithFormatsE(invalidTimeFormatError)

    /** Parse a string as a `LocalTime` in one of several formats */
    def localTimeWithFormatsE(error: ValidationError)(formats: NonEmptyList[DateTimeFormatterMagnet]): String => Validated[LocalTime] =
        anyE(error)(formats map localTimeWithFormat)


    /** Parse a string as a `Period` with a particular formatter */
    val periodWithFormat: PeriodFormatter => String => Validated[Period] =
        periodWithFormatE(_ => invalidPeriodFormatError)

    /** Parse a string as a `Period` with a particular formatter */
    def periodWithFormatE(error: Exception => ValidationError)(formatter: PeriodFormatter): String => Validated[Period] =
        in => try success(formatter.parsePeriod(in)) catch { case e: Exception => failure(error(e)) }

    /** Parse a string as a `Period` in one of several formats */
    val periodWithFormats: NonEmptyList[PeriodFormatter] => String => Validated[Period] =
        periodWithFormatsE(invalidPeriodFormatError)

    /** Parse a string as a `Period` in one of several formats */
    def periodWithFormatsE(error: ValidationError)(formats: NonEmptyList[PeriodFormatter]): String => Validated[Period] =
        anyE(error)(formats map periodWithFormat)


    /** Parse a string as a `DateTime` using ISO8601 formats */
    val isoDateTime: String => Validated[DateTime] =
        isoDateTimeE(invalidDateFormatError)

    /** Parse a string as a `DateTime` using ISO8601 formats */
    def isoDateTimeE(error: ValidationError): String => Validated[DateTime] =
        dateTimeWithFormatsE(error)(nels(ISODateTimeFormat.dateTime, ISODateTimeFormat.dateTimeNoMillis))

    /** Parse a string as a `DateTime` using ISO8601-ish formats (ISO8601 but without TZ suffix) */
    val isoishLocalDateTime: String => Validated[LocalDateTime] =
        isoishLocalDateTimeE(invalidDateFormatError)

    /** Parse a string as a `LocalDateTime` using the ISO8601-ish formats `yyyy-MM-dd'T'HH:mm:ss`, `yyyy-MM-dd'T'HH:mm:ss.SSS` */
    def isoishLocalDateTimeE(error: ValidationError): String => Validated[LocalDateTime] =
        localDateTimeWithFormatsE(error)(nels("yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyy-MM-dd'T'HH:mm:ss"))

    /** Parse a string as a `LocalDate` using the ISO8601 format `yyyy-mm-dd` */
    val isoDate: String => Validated[LocalDate] =
        isoDateE(invalidDateFormatError)

    /** Parse a string as a `LocalDate` using the ISO8601 format `yyyy-mm-dd` */
    def isoDateE(error: ValidationError): String => Validated[LocalDate] =
        localDateWithFormatE(_ => error)("yyyy-MM-dd")

    /** Parse a string as a `LocalTime` using the ISO8601 format `hh:mm:ss.SSS` or `hh:mm:ss` */
    val isoTime: String => Validated[LocalTime] =
        isoTimeE(invalidDateFormatError)

    /** Parse a string as a `LocalTime` using the ISO8601 format `hh:mm:ss.SSS` or `hh:mm:ss` */
    def isoTimeE(error: ValidationError): String => Validated[LocalTime] =
        localTimeWithFormatsE(error)(nels(ISODateTimeFormat.time, ISODateTimeFormat.timeNoMillis))

    def mustBeBeforeError   (limit: ReadableInstant) = ValidationError("invalid_date", "date must be before " + limit)
    def mustNotBeBeforeError(limit: ReadableInstant) = ValidationError("invalid_date", "date must not be before " + limit)
    def mustBeAfterError    (limit: ReadableInstant) = ValidationError("invalid_date", "date must be after " + limit)
    def mustNotBeAfterError (limit: ReadableInstant) = ValidationError("invalid_date", "date must not be after " + limit)
    val tooFarInPastError   = ValidationError("invalid_date", "date is too far in the past")
    val pastDateError       = ValidationError("invalid_date", "date cannot be in the future")
    val futureDateError     = ValidationError("invalid_date", "date cannot be in the past")
    val tooFarInFutureError = ValidationError("invalid_date", "date is too far in the future")

    /** Assert that some instant in time is before some limit */
    def before[A <: ReadableInstant](limit: ReadableInstant): A => Validated[A] =
        beforeE[A](mustBeBeforeError)(limit)

    /** Assert that some instant in time is before some limit */
    def beforeE[A <: ReadableInstant](error: ReadableInstant => ValidationError)(limit: ReadableInstant): A => Validated[A] =
        predicateE(error(limit))(_.isBefore(limit))

    /** Assert that some instant in time is not before some limit */
    def notBefore[A <: ReadableInstant](limit: ReadableInstant): A => Validated[A] =
        notBeforeE[A](mustNotBeBeforeError)(limit)

    /** Assert that some instant in time is not before some limit */
    def notBeforeE[A <: ReadableInstant](error: ReadableInstant => ValidationError)(limit: ReadableInstant): A => Validated[A] =
        predicateE(error(limit))(!_.isBefore(limit))

    /** Assert that some instant in time is after some limit */
    def after[A <: ReadableInstant](limit: ReadableInstant): A => Validated[A] =
        afterE[A](mustBeAfterError)(limit)

    /** Assert that some instant in time is after some limit */
    def afterE[A <: ReadableInstant](error: ReadableInstant => ValidationError)(limit: ReadableInstant): A => Validated[A] =
        predicateE(error(limit))(_.isAfter(limit))

    /** Assert that some instant in time is not after some limit */
    def notAfter[A <: ReadableInstant](limit: ReadableInstant): A => Validated[A] =
        notAfterE[A](mustNotBeAfterError)(limit)

    /** Assert that some instant in time is not after some limit */
    def notAfterE[A <: ReadableInstant](error: ReadableInstant => ValidationError)(limit: ReadableInstant): A => Validated[A] =
        predicateE(error(limit))(!_.isAfter(limit))


    private val sqlServerEpoch: DateTime = new DateTime(1753, 1, 1, 0, 0, 0, DateTimeZone.UTC)

    /** Assert that some date is not before the SQL Server database epoch (1753-01-01) */
    def afterSqlServerEpoch[A <: ReadableInstant]: A => Validated[A] =
        notBefore(sqlServerEpoch)

    /** Assert that some date is not before the SQL Server database epoch (1753-01-01) */
    def afterSqlServerEpochE[A <: ReadableInstant](error: ReadableInstant => ValidationError): A => Validated[A] =
        notBeforeE(error)(sqlServerEpoch)

    /** Assert that some date is in the past */
    def beforeNow[A <: ReadableInstant]: A => Validated[A] =
        beforeNowE[A](_ => pastDateError)

    /** Assert that some date is in the past */
    def beforeNowE[A <: ReadableInstant](error: ReadableInstant => ValidationError): A => Validated[A] =
        in =>  // delay until the value is provided rather than currying since we want to mean really now
            beforeE(error)(new DateTime())(in)

    /** Assert that some date is in the future */
    def afterNow[A <: ReadableInstant]: A => Validated[A] =
        afterNowE[A](_ => futureDateError)

    /** Assert that some date is in the future */
    def afterNowE[A <: ReadableInstant](error: ReadableInstant => ValidationError): A => Validated[A] =
        in =>  // delay until the value is provided rather than currying since we want to mean really now
            afterE(error)(new DateTime())(in)

    /** Assert that some date is in the future */
    def beforeDaysInFuture[A <: ReadableInstant](i: Int): A => Validated[A] =
        beforeDaysInFutureE[A](_ => tooFarInFutureError)(i)

    /** Assert that some date is in the future */
    def beforeDaysInFutureE[A <: ReadableInstant](error: ReadableInstant => ValidationError)(i: Int): A => Validated[A] =
        in => { // delay until the value is provided rather than currying since we want to mean really now
            val limit = new DateTime().dayOfMonth.addToCopy(i)
            notAfterE[A](error)(limit)(in)
        }
}
