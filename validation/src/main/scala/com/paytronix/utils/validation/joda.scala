//
// Copyright 2013 Paytronix Systems, Inc.
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

import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime, ReadableInstant}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter, ISODateTimeFormat}

import scala.util.control.Exception.catching

import base.{ValidationError, ValidationFunction}

object joda {
    private val databaseEpoch: Long = -6847786800000L // 1753-01-01 00:00:00 UTC

    val invalidDateFormatError = ValidationError("invalid_date", "invalid date") // FIXME? explain what is expected?

    final case class DateTimeFormatterMagnet(formatter: DateTimeFormatter)
    object DateTimeFormatterMagnet {
        implicit def fromString(s: String): DateTimeFormatterMagnet = DateTimeFormatterMagnet(DateTimeFormat.forPattern(s))
        implicit def fromDateTimeFormatter(dtf: DateTimeFormatter): DateTimeFormatterMagnet = DateTimeFormatterMagnet(dtf)
    }

    def dateTimeWithFormat(format: DateTimeFormatterMagnet, error: ValidationError = invalidDateFormatError): ValidationFunction[String, DateTime] = in =>
        catching(classOf[Exception]).either(format.formatter.parseDateTime(in)).left.map(_ => error :: Nil)
    def localDateWithFormat(format: DateTimeFormatterMagnet, error: ValidationError = invalidDateFormatError): ValidationFunction[String, LocalDate] = in =>
        catching(classOf[Exception]).either(format.formatter.parseLocalDate(in)).left.map(_ => error :: Nil)
    def localDateTimeWithFormat(format: DateTimeFormatterMagnet, error: ValidationError = invalidDateFormatError): ValidationFunction[String, LocalDateTime] = in =>
        catching(classOf[Exception]).either(format.formatter.parseLocalDateTime(in)).left.map(_ => error :: Nil)
    def localTimeWithFormat(format: DateTimeFormatterMagnet, error: ValidationError = invalidDateFormatError): ValidationFunction[String, LocalTime] = in =>
        catching(classOf[Exception]).either(format.formatter.parseLocalTime(in)).left.map(_ => error :: Nil)

    def dateTimeWithFormats(formats: Seq[DateTimeFormatterMagnet], error: ValidationError = invalidDateFormatError): ValidationFunction[String, DateTime] =
        in => formats.view.map { format => catching(classOf[Exception]).either(format.formatter.parseDateTime(in)) }.find { _.isRight } match {
            case Some(Right(date)) => Right(date)
            case _                 => Left(error :: Nil)
        }
    def localDateWithFormats(formats: Seq[DateTimeFormatterMagnet], error: ValidationError = invalidDateFormatError): ValidationFunction[String, LocalDate] =
        in => formats.view.map { format => catching(classOf[Exception]).either(format.formatter.parseLocalDate(in)) }.find { _.isRight } match {
            case Some(Right(date)) => Right(date)
            case _                 => Left(error :: Nil)
        }
    def localDateTimeWithFormats(formats: Seq[DateTimeFormatterMagnet], error: ValidationError = invalidDateFormatError): ValidationFunction[String, LocalDateTime] =
        in => formats.view.map { format => catching(classOf[Exception]).either(format.formatter.parseLocalDateTime(in)) }.find { _.isRight } match {
            case Some(Right(date)) => Right(date)
            case _                 => Left(error :: Nil)
        }
    def localTimeWithFormats(formats: Seq[DateTimeFormatterMagnet], error: ValidationError = invalidDateFormatError): ValidationFunction[String, LocalTime] =
        in => formats.view.map { format => catching(classOf[Exception]).either(format.formatter.parseLocalTime(in)) }.find { _.isRight } match {
            case Some(Right(date)) => Right(date)
            case _                 => Left(error :: Nil)
        }

    def isoDateTime(error: ValidationError = invalidDateFormatError): ValidationFunction[String, DateTime] =
        dateTimeWithFormats(Seq(ISODateTimeFormat.dateTime, ISODateTimeFormat.dateTimeNoMillis))

    def isoDate(error: ValidationError = invalidDateFormatError): ValidationFunction[String, LocalDate] =
        localDateWithFormat("yyyy-MM-dd")

    val tooFarInPastError = ValidationError("invalid_date", "date is too far in the past")
    val pastDateError = ValidationError("invalid_date", "date cannot be in the future")
    val futureDateError = ValidationError("invalid_date", "date cannot be in the past")
    val tooFarInFutureError = ValidationError("invalid_date", "date is too far in the future")

    /** Assert that some date is after the SQL Server database epoch (1753-01-01) */
    def afterSqlServerEpoch[A <: ReadableInstant](error: ValidationError = tooFarInPastError): ValidationFunction[A, A] =
        in => if (in.getMillis >= databaseEpoch) Right(in)
              else Left(error :: Nil)

    /** Assert that some date is in the past */
    def beforeNow[A <: ReadableInstant](error: ValidationError = pastDateError): ValidationFunction[A, A] =
        in => if (new DateTime().isAfter(in)) Right(in)
              else Left(error :: Nil)

    /** Assert that some date is in the future */
    def afterNow[A <: ReadableInstant](error: ValidationError = futureDateError): ValidationFunction[A, A] =
        in => if (new DateTime().isBefore(in)) Right(in)
              else Left(error :: Nil)

    /** Assert that some date is in the future */
    def beforeDaysInFuture[A <: ReadableInstant](i: Int, error: ValidationError = tooFarInFutureError): ValidationFunction[A, A] =
        in => if (new DateTime().dayOfMonth.addToCopy(1).isAfter(in)) Right(in) else Left(error :: Nil)
}
