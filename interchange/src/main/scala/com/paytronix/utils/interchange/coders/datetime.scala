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
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.slf4j.LoggerFactory

import com.paytronix.utils.scala.log.resultLoggerOps
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, firstOrLastG, parameter, tryCatch}

/** Abstract superclass of coders that code JavaDate or some subclass using SimpleDateFormat */
abstract class DateCoder[T <: JavaDate] extends StringSafeCoder[T] {
    implicit private val logger = LoggerFactory.getLogger(getClass)

    import ComposableCoder.CoderResult
    protected val additionalFormats: List[String] = Nil

    val defaultFormatString: String
    lazy val formats: List[String] = defaultFormatString :: additionalFormats

    protected def createFromDate(in: JavaDate): CoderResult[T] = createFromMillisSinceEpoch(in.getTime)
    protected def createFromMillisSinceEpoch(in: Long): CoderResult[T]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T) =
        formatDate(in).map(JString.apply)

    def decodeString(classLoader: ClassLoader, in: String) =
        for {
            parsed <- firstOrLastG (
                FailedG("incorrectly formatted date -- expected format like  " + formatDate(new JavaDate()).getOrElse("SYSTEM ERROR"), Nil),
                formats
            )(parseDate(in, _))
            instance <- createFromDate(parsed)
        } yield instance

    def encodeString(classLoader: ClassLoader, in: T) =
        formatDate(in)

    private def parseDate(in: String, fmtStr: String) =
        tryCatch.value {
            val dateFormat = new SimpleDateFormat(fmtStr)
            dateFormat.setLenient(false)
            dateFormat.parse(in)
        } | parameter(Nil)

    private def formatDate(in: JavaDate) =
        tryCatch.value(new SimpleDateFormat(defaultFormatString).format(in)).logError("Failed to format date: " + String.valueOf(in)) | parameter(Nil)

    lazy val avroSchema = (Schema.create(Schema.Type.LONG), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        createFromMillisSinceEpoch(in.readLong())

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) = {
        out.writeLong(in.getTime)
        Okay(())
    }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        Okay(jsonNodeFactory.numberNode(in.getTime))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case d: JavaDate => createFromDate(d)
            case s: String   => decodeString(classLoader, s)
            case null        => FailedG("required but missing", Nil)
            case _           => FailedG("not a date", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        Okay(in)
}

/** Map a Java Date to a JString */
object JavaDateCoder extends DateCoder[JavaDate] {
    val mostSpecificClass = classOf[JavaDate]

    val defaultFormatString = "yyyy-MM-dd HH:mm:ss Z"
    override val additionalFormats = List("E MMM dd HH:mm:ss Z yyyy", "E, dd MMM yy HH:mm:ss Z")

    override protected def createFromDate(in: JavaDate) =
        Okay(in)

    protected def createFromMillisSinceEpoch(in: Long) =
        Okay(new JavaDate(in))

    override def toString = "JavaDateCoder"
}

/** Map a Java SQL Date to a JString */
object JavaSqlDateCoder extends DateCoder[java.sql.Date] {
    val mostSpecificClass = classOf[java.sql.Date]

    val defaultFormatString = "yyyy-MM-dd"
    override val additionalFormats = List("E MMM dd yyyy", "E, dd MMM yy")

    protected def createFromMillisSinceEpoch(in: Long) =
        Okay(new java.sql.Date(in))

    override def toString = "JavaSqlDateCoder"
}

/** Map a Java SQL Timestamp to a JString */
object JavaSqlTimestampCoder extends DateCoder[java.sql.Timestamp] {
    val mostSpecificClass = classOf[java.sql.Timestamp]

    val defaultFormatString = "yyyy-MM-dd hh:mm:ss.SSS"
    override val additionalFormats = List("yyyy-MM-dd hh:mm:ss")

    protected def createFromMillisSinceEpoch(in: Long) =
        Okay(new java.sql.Timestamp(in))

    override def toString = "JavaSqlTimestampCoder"
}

/** Abstract superclass of coders that code ReadableDateTime or some subclass using DateTimeFormatter */
abstract class JodaDateTimeCoder[T] extends StringSafeCoder[T] {
    import ComposableCoder.{CoderResult, FailedPath}

    protected val additionalFormats: List[String] = Nil

    val defaultFormatString: String
    lazy val formats: List[String] = defaultFormatString :: additionalFormats
    lazy val formatters: List[DateTimeFormatter] = formats map DateTimeFormat.forPattern

    protected def fromDateTime(in: DateTime): T
    protected def toDateTime(in: T): DateTime

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T) =
        formatDate(toDateTime(in)).map(JString.apply)

    def decodeString(classLoader: ClassLoader, in: String) =
        firstOrLastG (
            FailedG("incorrectly formatted date -- expected format like  " + formatDate(new DateTime()), Nil: FailedPath),
            formatters
        )(parseDate(in, _)) map fromDateTime

    def encodeString(classLoader: ClassLoader, in: T) =
        formatDate(toDateTime(in))

    private def parseDate(in: String, formatter: DateTimeFormatter): CoderResult[DateTime] =
        tryCatch.valueG(parameter(Nil))(formatter.parseDateTime(in))

    private def formatDate(in: DateTime): CoderResult[String] =
        tryCatch.valueG(parameter(Nil))(formatters.head.print(in))

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

object DateTimeCoder extends JodaDateTimeCoder[DateTime] {
    val mostSpecificClass = classOf[DateTime]

    val defaultFormatString = "yyyy-MM-dd HH:mm:ss Z"
    override val additionalFormats = List("E MMM dd HH:mm:ss Z yyyy", "E, dd MMM yy HH:mm:ss Z", "yyyy-MM-dd'T'HH:mm:ss.SSSZ")

    protected def fromDateTime(in: DateTime) = in
    protected def toDateTime(in: DateTime) = in
}

object LocalDateCoder extends JodaDateTimeCoder[LocalDate] {
    val mostSpecificClass = classOf[LocalDate]

    val defaultFormatString = "yyyy-MM-dd"
    override val additionalFormats = List("E MMM dd yyyy", "E, dd MMM yy")

    protected def fromDateTime(in: DateTime) = in.toLocalDate
    protected def toDateTime(in: LocalDate) = in.toDateTimeAtStartOfDay
}

object LocalDateTimeCoder extends JodaDateTimeCoder[LocalDateTime] {
    val mostSpecificClass = classOf[LocalDateTime]

    val defaultFormatString = "yyyy-MM-dd HH:mm:ss"
    override val additionalFormats = List("E MMM dd HH:mm:ss yyyy", "E, dd MMM yy HH:mm:ss")

    protected def fromDateTime(in: DateTime) = in.toLocalDateTime
    protected def toDateTime(in: LocalDateTime) = in.toDateTime
}

object LocalTimeCoder extends JodaDateTimeCoder[LocalTime] {
    val mostSpecificClass = classOf[LocalTime]

    val defaultFormatString = "yyyy-MM-dd hh:mm:ss.SSS"
    override val additionalFormats = List("yyyy-MM-dd hh:mm:ss")

    protected def fromDateTime(in: DateTime) = in.toLocalTime
    protected def toDateTime(in: LocalTime) = in.toDateTimeToday
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
