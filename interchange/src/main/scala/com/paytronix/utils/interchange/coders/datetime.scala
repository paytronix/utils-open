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

package com.paytronix.utils.interchange

import java.text.SimpleDateFormat
import java.util.{Date => JavaDate}

import net.liftweb.json.JsonAST.{JNothing, JNull, JString, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}
import org.joda.time.{DateTime, Duration, LocalDate, LocalDateTime, LocalTime}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter, ISODateTimeFormat}
import org.slf4j.LoggerFactory

import com.paytronix.utils.scala.log.resultLoggerOps
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, firstOrLastG, parameter, tryCatch}

/** Abstract superclass of coders that code ReadableDateTime or some subclass using DateTimeFormatter */
abstract class DateTimeCoderBase[T] extends StringSafeCoder[T] {
    import ComposableCoder.{CoderResult, FailedPath}

    protected val additionalFormatters: List[DateTimeFormatter] = Nil

    val defaultFormatter: DateTimeFormatter
    lazy val formatters: List[DateTimeFormatter] = defaultFormatter :: additionalFormatters

    protected def fromDateTime(in: DateTime): T
    protected def toDateTime(in: T): DateTime
    protected def parseDate(in: String, formatter: DateTimeFormatter): CoderResult[T]
    protected def formatDate(in: T): CoderResult[String] =
        tryCatch.valueG(parameter(Nil))(formatters.head.print(toDateTime(in)))

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T): CoderResult[JValue] =
        encodeString(classLoader, in).map(JString.apply)

    def decodeString(classLoader: ClassLoader, in: String) =
        firstOrLastG (
            FailedG("incorrectly formatted date -- expected format like  " + formatDate(fromDateTime(new DateTime())), Nil: FailedPath),
            formatters
        )(parseDate(in, _))

    def encodeString(classLoader: ClassLoader, in: T) =
        formatDate(in)

    lazy val avroSchema = (Schema.create(Schema.Type.LONG), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.valueG(parameter(Nil))(fromDateTime(new DateTime(in.readLong())))

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        Okay(out.writeLong(toDateTime(in).getMillis))

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        Okay(jsonNodeFactory.numberNode(toDateTime(in).getMillis))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case dt: DateTime      => Okay(fromDateTime(dt))
            case d: java.util.Date => Okay(fromDateTime(new DateTime(d)))
            case s: String         => decodeString(classLoader, s)
            case null              => FailedG("required but missing", Nil)
            case _                 => FailedG("not a date", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        Okay(toDateTime(in).toDate)
}

object DateTimeCoder extends DateTimeCoderBase[DateTime] {
    val mostSpecificClass = classOf[DateTime]

    // Formatter used for encoding
    val defaultFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss Z")

    override val additionalFormatters = {
        // Strings representing some additional time formats allowed for decoding
        val additionalFormatStrings = List(
            "E MMM dd HH:mm:ss Z yyyy",
            "E, dd MMM yy HH:mm:ss Z"
        )

        // Everything above, plus ISO8601 is allowed (with or without milliseconds)
        additionalFormatStrings.map(DateTimeFormat.forPattern) :+ ISODateTimeFormat.dateTime :+ ISODateTimeFormat.dateTimeNoMillis
    }

    protected def parseDate(in: String, formatter: DateTimeFormatter) =
        tryCatch.valueG(parameter(Nil))(formatter.parseDateTime(in))
    protected def fromDateTime(in: DateTime) = in
    protected def toDateTime(in: DateTime) = in

    override def toString = "DateTimeCoder"
}

object LocalDateCoder extends DateTimeCoderBase[LocalDate] {
    val mostSpecificClass = classOf[LocalDate]

    // Formatter used for encoding
    val defaultFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")

    override val additionalFormatters = {
        // Strings representing some additional time formats allowed for decoding
        val additionalFormatStrings = List(
            "E MMM dd yyyy",
            "E, dd MMM yy"
        )

        // Everything above, plus http://joda-time.sourceforge.net/apidocs/org/joda/time/format/ISODateTimeFormat.html#localDateParser()
        additionalFormatStrings.map(DateTimeFormat.forPattern) :+ ISODateTimeFormat.localDateParser
    }

    protected def parseDate(in: String, formatter: DateTimeFormatter) =
        tryCatch.valueG(parameter(Nil))(formatter.parseLocalDate(in))
    protected def fromDateTime(in: DateTime) = in.toLocalDate
    protected def toDateTime(in: LocalDate) = in.toDateTimeAtStartOfDay

    override def toString = "LocalDateCoder"
}

object LocalDateTimeCoder extends DateTimeCoderBase[LocalDateTime] {
    val mostSpecificClass = classOf[LocalDateTime]

    // Formatter used for encoding
    val defaultFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")

    override val additionalFormatters = {
        // Strings representing some additional time formats allowed for decoding
        val additionalFormatStrings = List("E MMM dd HH:mm:ss yyyy", "E, dd MMM yy HH:mm:ss")

        // Everything above, plus http://joda-time.sourceforge.net/apidocs/org/joda/time/format/ISODateTimeFormat.html#localDateOptionalTimeParser()
        additionalFormatStrings.map(DateTimeFormat.forPattern) :+ ISODateTimeFormat.localDateOptionalTimeParser // FIXME not the best choice
    }

    protected def parseDate(in: String, formatter: DateTimeFormatter) =
        tryCatch.valueG(parameter(Nil))(formatter.parseLocalDateTime(in))
    protected def fromDateTime(in: DateTime) = in.toLocalDateTime
    protected def toDateTime(in: LocalDateTime) = in.toDateTime

    override def toString = "LocalDateTimeCoder"
}

object LocalTimeCoder extends DateTimeCoderBase[LocalTime] {
    val mostSpecificClass = classOf[LocalTime]

    // Formatter used for encoding
    val defaultFormatter = DateTimeFormat.forPattern("HH:mm:ss.SSS")

    // Additional formatters allowed for decoding
    override val additionalFormatters = DateTimeFormat.forPattern("HH:mm:ss") :: ISODateTimeFormat.localTimeParser :: Nil

    protected def parseDate(in: String, formatter: DateTimeFormatter) =
        tryCatch.valueG(parameter(Nil))(formatter.parseLocalTime(in))
    protected def fromDateTime(in: DateTime) = in.toLocalTime
    protected def toDateTime(in: LocalTime) = in.toDateTimeToday

    override def toString = "LocalTimeCoder"
}

/** Map a Java Date to a JString */
object JavaDateCoder extends DateTimeCoderBase[JavaDate] {
    val mostSpecificClass = classOf[JavaDate]

    // Formatter used for encoding
    val defaultFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss Z")

    override val additionalFormatters = {
        // Strings representing some additional time formats allowed for decoding
        val additionalFormatStrings = List(
            "E MMM dd HH:mm:ss Z yyyy",
            "E, dd MMM yy HH:mm:ss Z"
        )

        // Everything above, plus ISO8601 is allowed (with or without milliseconds)
        additionalFormatStrings.map(DateTimeFormat.forPattern) :+ ISODateTimeFormat.dateTime :+ ISODateTimeFormat.dateTimeNoMillis
    }

    protected def parseDate(in: String, formatter: DateTimeFormatter) =
        tryCatch.valueG(parameter(Nil))(formatter.parseDateTime(in).toDate)
    protected def fromDateTime(in: DateTime) = in.toDate
    protected def toDateTime(in: JavaDate) = new DateTime(in)

    override def toString = "JavaDateCoder"
}

/** Map a Java SQL Date to a JString */
object JavaSqlDateCoder extends DateTimeCoderBase[java.sql.Date] {
    val mostSpecificClass = classOf[java.sql.Date]

    // Formatter used for encoding
    val defaultFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")

    override val additionalFormatters = {
        // Strings representing some additional time formats allowed for decoding
        val additionalFormatStrings = List(
            "E MMM dd yyyy",
            "E, dd MMM yy"
        )

        // Everything above, plus http://joda-time.sourceforge.net/apidocs/org/joda/time/format/ISODateTimeFormat.html#localDateParser()
        additionalFormatStrings.map(DateTimeFormat.forPattern) :+ ISODateTimeFormat.localDateParser
    }

    protected def parseDate(in: String, formatter: DateTimeFormatter) =
        tryCatch.valueG(parameter(Nil))(new java.sql.Date(formatter.parseLocalDate(in).toDate.getTime))
    protected def fromDateTime(in: DateTime) = new java.sql.Date(in.getMillis)
    protected def toDateTime(in: java.sql.Date) = new DateTime(in)

    override def toString = "JavaSqlDateCoder"
}

/** Map a Java SQL Timestamp to a JString */
object JavaSqlTimestampCoder extends DateTimeCoderBase[java.sql.Timestamp] {
    val mostSpecificClass = classOf[java.sql.Timestamp]

    // Formatter used for encoding
    val defaultFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss.SSS")

    // Additional formatters allowed for decoding
    override val additionalFormatters = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss") :: ISODateTimeFormat.localDateOptionalTimeParser :: Nil // FIXME not the best choice

    protected def parseDate(in: String, formatter: DateTimeFormatter) =
        tryCatch.valueG(parameter(Nil))(new java.sql.Timestamp(formatter.parseLocalDateTime(in).toDateTime.getMillis))
    protected def fromDateTime(in: DateTime) = new java.sql.Timestamp(in.getMillis)
    protected def toDateTime(in: java.sql.Timestamp) = new DateTime(in)

    override def toString = "JavaSqlTimestampCoder"
}

object DurationCoder extends StringSafeCoder[Duration] {
    val mostSpecificClass = classOf[Duration]

    def decode(classLoader: ClassLoader, in: JValue) =
        LongCoder.decode(classLoader, in) map { l => new Duration(l) }

    def encode(classLoader: ClassLoader, in: Duration) =
        LongCoder.encode(classLoader, in.getMillis)

    def decodeString(classLoader: ClassLoader, in: String) =
        LongCoder.decodeString(classLoader, in) map { l => new Duration(l) }

    def encodeString(classLoader: ClassLoader, in: Duration) =
        LongCoder.encodeString(classLoader, in.getMillis)

    def avroSchema =
        LongCoder.avroSchema

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        LongCoder.decodeAvro(classLoader, in) map { l => new Duration(l) }

    def encodeAvro(classLoader: ClassLoader, in: Duration, out: Encoder) =
        LongCoder.encodeAvro(classLoader, in.getMillis, out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Duration) =
        LongCoder.encodeAvroDefaultJson(classLoader, in.getMillis)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        LongCoder.decodeMongoDB(classLoader, in) map { l => new Duration(l) }

    def encodeMongoDB(classLoader: ClassLoader, in: Duration) =
        LongCoder.encodeMongoDB(classLoader, in.getMillis)

    override def toString = "DurationCoder"
}
