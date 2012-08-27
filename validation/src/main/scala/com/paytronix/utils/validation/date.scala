//
// Copyright 2012 Paytronix Systems, Inc.
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

import java.text.SimpleDateFormat
import scala.util.control.Exception.catching

import base.{ValidationError, ValidationFunction}

object date {
    private val databaseEpoch: Long = -6847786800000L // 1753-01-01 00:00:00 UTC

    val invalidDateFormatError = ValidationError("invalid_date", "invalid date") // FIXME? explain what is expected?

    private def dateFormat(format: String): SimpleDateFormat = {
        val df = new SimpleDateFormat(format)
        df.setLenient(false) // really? lenient is the default? I hate Java.
        df
    }

    def dateWithFormat(format: String, error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.util.Date] = in =>
        catching(classOf[Exception]).either(dateFormat(format).parse(in)).left.map(_ => error :: Nil)

    def dateWithFormats(formats: Seq[String], error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.util.Date] =
        in => formats.view map { format => catching(classOf[Exception]).either(dateFormat(format).parse(in)) } find { _.isRight } match {
            case Some(Right(date)) => Right(date)
            case _                 => Left(error :: Nil)
        }

    def isoDateTime(error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.util.Date] =
        dateWithFormats("yyyy-MM-dd'T'HH:mm:ss.SSS" :: "yyyy-MM-dd'T'HH:mm:ss" :: Nil)

    def isoDate(error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.util.Date] =
        dateWithFormat("yyyy-MM-dd")

    def sqlDatetime(error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.sql.Timestamp] =
        isoDateTime() and { d => Right(new java.sql.Timestamp(d.getTime)) }

    def sqlDate(error: ValidationError = invalidDateFormatError): ValidationFunction[String, java.sql.Date] =
        isoDate() and { d => Right(new java.sql.Date(d.getTime)) }

    val tooFarInPastError = ValidationError("invalid_date", "date is too far in the past")
    val pastDateError = ValidationError("invalid_date", "date cannot be in the future")
    val futureDateError = ValidationError("invalid_date", "date cannot be in the past")
    val tooFarInFutureError = ValidationError("invalid_date", "date is too far in the future")

    /** Assert that some date is after the SQL Server database epoch (1753-01-01) */
    def afterSqlServerEpoch[A <: java.util.Date](error: ValidationError = tooFarInPastError): ValidationFunction[A, A] =
        in => if (in.getTime >= databaseEpoch) Right(in)
              else Left(error :: Nil)

    /** Assert that some date is in the past */
    def beforeNow[A <: java.util.Date](error: ValidationError = pastDateError): ValidationFunction[A, A] =
        in => if (new java.util.Date().after(in)) Right(in)
              else Left(error :: Nil)

    /** Assert that some date is in the future */
    def afterNow[A <: java.util.Date](error: ValidationError = futureDateError): ValidationFunction[A, A] =
        in => if (new java.util.Date().before(in)) Right(in)
              else Left(error :: Nil)

    /** Assert that some date is in the future */
    def beforeDaysInFuture[A <: java.util.Date](i: Int, error: ValidationError = tooFarInFutureError): ValidationFunction[A, A] =
        in => {
                val cal = java.util.Calendar.getInstance
                println(i)
                cal.add(java.util.Calendar.DAY_OF_MONTH, i)
                println(cal.getTime)
                println(in)
                println(cal.after(in))
                if (cal.getTime.after(in)) Right(in)
                else Left(error :: Nil)
        }

}
