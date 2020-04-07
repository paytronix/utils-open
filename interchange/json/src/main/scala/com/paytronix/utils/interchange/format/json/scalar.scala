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

package com.paytronix.utils.interchange.format.json

import com.fasterxml.jackson.core.{JsonParseException, JsonToken}
import java.lang.{Boolean => JavaBoolean, Integer => JavaInteger, Long => JavaLong}
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
import java.nio.ByteBuffer
import java.sql.{Date => JavaSqlDate, Time => JavaSqlTime, Timestamp => JavaSqlTimestamp}
import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, OffsetTime, ZonedDateTime}
import java.util.{Arrays, Date => JavaDate}
import scala.annotation.{Annotation, StaticAnnotation}
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.language.experimental.macros
import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe.{TypeTag, typeTag}

import com.paytronix.utils.interchange.base.{CoderFailure, Receiver, TypeConverter, atTerminal, datetime, terminal}
import com.paytronix.utils.interchange.base.enum.{enumerationInstance, enumerationValueFromString}
import com.paytronix.utils.interchange.format.string.{StringDecoder, StringEncoder}
import com.paytronix.utils.interchange.format.string.coders._
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, tryCatchValue, tryCatchValueG, tryCatchResultG, tryCatchingResultG}

object scalar extends scalar

trait scalar extends scalarLPI {
    /** JsonCoder for Unit. Does not emit any value */
    implicit object unitJsonCoder extends JsonCoder[Unit] {
        object encode extends JsonEncoder[Unit] {
            val mightBeNull = true
            val codesAsObject = false

            def run(in: Unit, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeNothingOrNull()
                    Okay.unit
                }
        }

        object decode extends JsonDecoder[Unit] {
            val mightBeNull = true
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[Unit]) =
                out(())
        }
    }

    /** JsonCoder for Boolean. Encodes as a JSON boolean */
    implicit object booleanJsonCoder extends JsonCoder[Boolean] {
        object encode extends JsonEncoder[Boolean] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: Boolean, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeBoolean(in)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[Boolean] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[Boolean]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL   => in.unexpectedMissingValue
                        case JsonToken.VALUE_TRUE   => out(true)
                        case JsonToken.VALUE_FALSE  => out(false)
                        case JsonToken.VALUE_STRING => booleanStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case _                      => in.unexpectedToken("a boolean, \"true\", or \"false\"")
                    }
                }
        }
    }

    /** `JsonCoder` for Java Boolean. Encodes as a JSON boolean */
    implicit lazy val javaBooleanJsonCoder = booleanJsonCoder.mapWithConverter(TypeConverter(
        (i: JavaBoolean) => tryCatchValue(i: Boolean): Result[Boolean],
        (i: Boolean) => Okay(boolean2Boolean(i)): Result[JavaBoolean]
    ))

    /** JsonCoder for Byte. Encodes as a JSON number */
    implicit object byteJsonCoder extends JsonCoder[Byte] {
        object encode extends JsonEncoder[Byte] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: Byte, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeNumber(in)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[Byte] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[Byte]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => byteStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case JsonToken.VALUE_NUMBER_INT =>
                            try out(in.byteValue) catch {
                                case _: JsonParseException => FailedG("number out of bounds", in.terminal)
                                case e: Exception          => FailedG(e, in.terminal)
                            }
                        case _ => in.unexpectedToken("an integer")
                    }
                }
        }
    }

    /** JsonCoder for Short. Encodes as a JSON number */
    implicit object shortJsonCoder extends JsonCoder[Short] {
        object encode extends JsonEncoder[Short] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: Short, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeNumber(in)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[Short] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[Short]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => shortStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case JsonToken.VALUE_NUMBER_INT =>
                            try out(in.shortValue) catch {
                                case _: JsonParseException => FailedG("number out of bounds", in.terminal)
                                case e: Exception          => FailedG(e, in.terminal)
                            }
                        case _ => in.unexpectedToken("an integer")
                    }
                }
        }
    }

    /** JsonCoder for Int. Encodes as a JSON number */
    implicit object intJsonCoder extends JsonCoder[Int] {
        object encode extends JsonEncoder[Int] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: Int, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeNumber(in)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[Int] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[Int]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => intStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case JsonToken.VALUE_NUMBER_INT =>
                            try out(in.intValue) catch {
                                case _: JsonParseException => FailedG("number out of bounds", in.terminal)
                                case e: Exception          => FailedG(e, in.terminal)
                            }
                        case _ => in.unexpectedToken("an integer")
                    }
                }
        }
    }

    /** `JsonCoder` for Java Integer. Encodes as a JSON number */
    implicit lazy val javaIntegerJsonCoder = intJsonCoder.mapWithConverter(TypeConverter(
        (i: JavaInteger) => tryCatchValue(i.toInt): Result[Int],
        (i: Int) => Okay(int2Integer(i)): Result[JavaInteger]
    ))

    /** JsonCoder for Long. Encodes as a JSON number */
    implicit object longJsonCoder extends JsonCoder[Long] {
        object encode extends JsonEncoder[Long] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: Long, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeNumber(in)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[Long] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[Long]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => longStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case JsonToken.VALUE_NUMBER_INT =>
                            try out(in.longValue) catch {
                                case _: JsonParseException => FailedG("number out of bounds", in.terminal)
                                case e: Exception          => FailedG(e, in.terminal)
                            }
                        case _ => in.unexpectedToken("an integer")
                    }
                }
        }
    }

    /** `JsonCoder` for Java Long. Encodes as a JSON number */
    implicit lazy val javaLongJsonCoder = longJsonCoder.mapWithConverter(TypeConverter(
        (i: JavaLong) => tryCatchValue(i.toLong): Result[Long],
        (i: Long) => Okay(long2Long(i)): Result[JavaLong]
    ))

    /** JsonCoder for Float. Encodes as a JSON number */
    implicit object floatJsonCoder extends JsonCoder[Float] {
        object encode extends JsonEncoder[Float] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: Float, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeNumber(in)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[Float] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[Float]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => floatStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case JsonToken.VALUE_NUMBER_INT|JsonToken.VALUE_NUMBER_FLOAT =>
                            try out(in.floatValue) catch {
                                case _: JsonParseException => FailedG("number out of bounds", in.terminal)
                                case e: Exception          => FailedG(e, in.terminal)
                            }
                        case _ => in.unexpectedToken("a number")
                    }
                }
        }
    }

    /** JsonCoder for Double. Encodes as a JSON number */
    implicit object doubleJsonCoder extends JsonCoder[Double] {
        object encode extends JsonEncoder[Double] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: Double, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeNumber(in)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[Double] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[Double]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => doubleStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case JsonToken.VALUE_NUMBER_INT|JsonToken.VALUE_NUMBER_FLOAT =>
                            try out(in.doubleValue) catch {
                                case _: JsonParseException => FailedG("number out of bounds", in.terminal)
                                case e: Exception          => FailedG(e, in.terminal)
                            }
                        case _ => in.unexpectedToken("a number")
                    }
                }
        }
    }

    /** JsonCoder for `java.math.BigInteger`. Encodes as a JSON string (to avoid issues with overflow) */
    implicit object javaBigIntegerJsonCoder extends JsonCoder[java.math.BigInteger] {
        object encode extends JsonEncoder[java.math.BigInteger] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: java.math.BigInteger, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeNumber(in)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[java.math.BigInteger] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[java.math.BigInteger]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => javaBigIntegerStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case JsonToken.VALUE_NUMBER_INT =>
                            try out(in.bigIntegerValue) catch {
                                case _: JsonParseException => FailedG("number out of bounds", in.terminal)
                                case e: Exception          => FailedG(e, in.terminal)
                            }
                        case _ => in.unexpectedToken("a string or integer")
                    }
                }
        }
    }

    /** `JsonCoder` for `scala.math.BigInt`. Encodes as a JSON string (to avoid issues with overflow) */
    implicit lazy val scalaBigIntJsonCoder = javaBigIntegerJsonCoder.mapWithConverter(TypeConverter(
        (bi: BigInt) => Okay(bi.bigInteger): Result[JavaBigInteger],
        (bi: JavaBigInteger) => Okay(BigInt(bi)): Result[BigInt]
    ))

    /** JsonCoder for `java.math.BigDecimal`. Encodes as a JSON string (to avoid issues with precision of IEEE754) */
    implicit object javaBigDecimalJsonCoder extends JsonCoder[java.math.BigDecimal] {
        object encode extends JsonEncoder[java.math.BigDecimal] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: java.math.BigDecimal, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    if (in.scale < 0 || in.scale > 50)
                        out.writeString(in.toString)
                    else
                        out.writeString(in.toPlainString)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[java.math.BigDecimal] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[java.math.BigDecimal]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => javaBigDecimalStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case JsonToken.VALUE_NUMBER_INT =>
                            try out(new java.math.BigDecimal(in.bigIntegerValue)) catch {
                                case _: JsonParseException => FailedG("number out of bounds", in.terminal)
                                case e: Exception          => FailedG(e, in.terminal)
                            }
                        case JsonToken.VALUE_NUMBER_FLOAT =>
                            try out(in.bigDecimalValue) catch {
                                case _: JsonParseException => FailedG("number out of bounds", in.terminal)
                                case e: Exception          => FailedG(e, in.terminal)
                            }
                        case _ => in.unexpectedToken("a string or number")
                    }
                }
        }
    }

    /** `JsonCoder` for `scala.math.BigDecimal`. Encodes as a JSON string (to avoid issues with overflow) */
    implicit lazy val scalaBigDecimalJsonCoder = javaBigDecimalJsonCoder.mapWithConverter(TypeConverter(
        (bd: BigDecimal) => Okay(bd.bigDecimal): Result[JavaBigDecimal],
        (bd: JavaBigDecimal) => Okay(BigDecimal(bd)): Result[BigDecimal]
    ))

    /** JsonCoder for `Char`. Encodes as a JSON string of one character */
    implicit object charJsonCoder extends JsonCoder[Char] {
        object encode extends JsonEncoder[Char] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: Char, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeString(in.toString)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[Char] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[Char]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => charStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case _ => in.unexpectedToken("a string with one character")
                    }
                }
        }
    }

    /** JsonCoder for `String`. Encodes as a JSON string */
    implicit object stringJsonCoder extends JsonCoder[String] {
        object encode extends JsonEncoder[String] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: String, out: InterchangeJsonGenerator) =
                tryCatchResultG(terminal) {
                    out.writeString(in)
                    Okay.unit
                }
        }
        object decode extends JsonDecoder[String] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[String]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => out(in.stringValue)
                        case _ => in.unexpectedToken("a string")
                    }
                }
        }
    }

    /** JsonCoder for `java.nio.ByteBuffer`. Encodes as a JSON string */
    implicit object byteBufferJsonCoder extends JsonCoder[ByteBuffer] {
        object encode extends JsonEncoder[ByteBuffer] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: ByteBuffer, out: InterchangeJsonGenerator) = {
                val r = new Receiver[String]
                byteBufferStringCoder.encode.run(in, r) >> tryCatchResultG(terminal) {
                    out.writeString(r.value)
                    Okay.unit
                }
            }
        }
        object decode extends JsonDecoder[ByteBuffer] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[ByteBuffer]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => byteBufferStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case _ => in.unexpectedToken("a string with base64 formatted content")
                    }
                }
        }
    }

    /** JsonCoder for `Array[Byte]`. Encodes as a JSON string */
    implicit object byteArrayJsonCoder extends JsonCoder[Array[Byte]] {
        object encode extends JsonEncoder[Array[Byte]] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: Array[Byte], out: InterchangeJsonGenerator) = {
                val r = new Receiver[String]
                byteArrayStringCoder.encode.run(in, r) >> tryCatchResultG(terminal) {
                    out.writeString(r.value)
                    Okay.unit
                }
            }
        }
        object decode extends JsonDecoder[Array[Byte]] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[Array[Byte]]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL       => in.unexpectedMissingValue
                        case JsonToken.VALUE_STRING     => byteArrayStringCoder.decode.run(in.stringValue, out) orElse in.noteSourceLocation
                        case _ => in.unexpectedToken("a string with base64 formatted content")
                    }
                }
        }
    }

    /** JsonCoder for Java enumerations. Encodes as a JSON string with the text equivalent of each enum value */
    implicit def javaEnumJsonCoder[A <: Enum[A]: ClassTag]: JsonCoder[A] = {
        val enumClass = classTag[A].runtimeClass.asInstanceOf[Class[A]]
        val enumValues = enumClass.getMethod("values").invoke(null).asInstanceOf[Array[A]]

        class javaEnumJsonCoder extends JsonCoder[A] {
            object encode extends JsonEncoder[A] {
                val mightBeNull = false
                val codesAsObject = false

                def run(in: A, out: InterchangeJsonGenerator) =
                    tryCatchResultG(terminal) {
                        out.writeString(in.toString)
                        Okay.unit
                    }
            }
            object decode extends JsonDecoder[A] {
                val mightBeNull = false
                val codesAsObject = false

                def run(in: InterchangeJsonParser, out: Receiver[A]) =
                    in.requireValue >> {
                        in.currentToken match {
                            case JsonToken.VALUE_NULL   => in.unexpectedMissingValue
                            case JsonToken.VALUE_STRING =>
                                tryCatchingResultG(classOf[IllegalArgumentException])(in.terminal) {
                                    out(Enum.valueOf(enumClass, in.stringValue))
                                } | s""""${in.stringValue}" is not a valid enumeration value (valid values: ${enumValues.map(_.toString).mkString(", ")})"""
                            case _ => in.unexpectedToken(s"a string with one of the following values: ${enumValues.map(_.toString).mkString(", ")}")
                        }
                    }
            }
        }
        new javaEnumJsonCoder
    }

    /** JsonCoder for Scala `Enumeration`s. Encodes as a JSON string with the text equivalent of each enum value */
    implicit def scalaEnumJsonCoder[A <: Enumeration: TypeTag]: JsonCoder[A#Value] = {
        val enumeration = atTerminal(enumerationInstance[A]).orThrow
        val enumValues: Vector[A#Value] = Vector.empty ++ enumeration.values

        class scalaEnumJsonCoder extends JsonCoder[A#Value] {
            object encode extends JsonEncoder[A#Value] {
                val mightBeNull = false
                val codesAsObject = false

                def run(in: A#Value, out: InterchangeJsonGenerator) =
                    tryCatchResultG(terminal) {
                        out.writeString(in.toString)
                        Okay.unit
                    }
            }
            object decode extends JsonDecoder[A#Value] {
                val mightBeNull = false
                val codesAsObject = false

                def run(in: InterchangeJsonParser, out: Receiver[A#Value]) =
                    in.requireValue >> {
                        in.currentToken match {
                            case JsonToken.VALUE_NULL   => in.unexpectedMissingValue
                            case JsonToken.VALUE_STRING =>
                                tryCatchValueG(in.terminal)(in.stringValue) >>= { s =>
                                    enumeration.values.find(_.toString == s) match {
                                        case Some(a) => out(a)
                                        case None =>
                                            val msg = s""""${in.stringValue}" is not a valid enumeration value (valid values: ${enumValues.map(_.toString).mkString(", ")})"""
                                            FailedG(msg, in.terminal)
                                    }
                                }
                            case _ => in.unexpectedToken(s"a string with one of the following values: ${enumValues.map(_.toString).mkString(", ")}")
                        }
                    }
            }
        }
        new scalaEnumJsonCoder
    }

    implicit val zonedDateTimeJsonCoderIso8601    = dateAsIso8601.zonedDateTimeJsonCoder
    implicit val localDateJsonCoderIso8601        = dateAsIso8601.localDateJsonCoder
    implicit val localDateTimeJsonCoderIso8601    = dateAsIso8601.localDateTimeJsonCoder
    implicit val localTimeJsonCoderIso8601        = dateAsIso8601.localTimeJsonCoder
    implicit val offsetTimeJsonCoderIso8601       = dateAsIso8601.offsetTimeJsonCoder
    implicit val javaDateJsonCoderIso8601         = dateAsIso8601.javaDateJsonCoder
    implicit val javaSqlDateJsonCoderIso8601      = dateAsIso8601.javaSqlDateJsonCoder
    implicit val javaSqlTimeJsonCoderIso8601      = dateAsIso8601.javaSqlTimeJsonCoder
    implicit val javaSqlTimestampJsonCoderIso8601 = dateAsIso8601.javaSqlTimestampJsonCoder

    object dateAsIso8601 {
        implicit val zonedDateTimeJsonCoder    : JsonCoder[ZonedDateTime]    = stringJsonCoder.mapWithConverter(datetime.iso8601.zonedDateTimeConverter)
        implicit val localDateJsonCoder        : JsonCoder[LocalDate]        = stringJsonCoder.mapWithConverter(datetime.iso8601.localDateConverter)
        implicit val localDateTimeJsonCoder    : JsonCoder[LocalDateTime]    = stringJsonCoder.mapWithConverter(datetime.iso8601.localDateTimeConverter)
        implicit val localTimeJsonCoder        : JsonCoder[LocalTime]        = stringJsonCoder.mapWithConverter(datetime.iso8601.localTimeConverter)
        implicit val offsetTimeJsonCoder       : JsonCoder[OffsetTime]       = stringJsonCoder.mapWithConverter(datetime.iso8601.offsetTimeConverter)
        implicit val javaDateJsonCoder         : JsonCoder[JavaDate]         = stringJsonCoder.mapWithConverter(datetime.iso8601.javaDateConverter)
        implicit val javaSqlDateJsonCoder      : JsonCoder[JavaSqlDate]      = stringJsonCoder.mapWithConverter(datetime.iso8601.javaSqlDateConverter)
        implicit val javaSqlTimeJsonCoder      : JsonCoder[JavaSqlTime]      = stringJsonCoder.mapWithConverter(datetime.iso8601.javaSqlTimeConverter)
        implicit val javaSqlTimestampJsonCoder : JsonCoder[JavaSqlTimestamp] = stringJsonCoder.mapWithConverter(datetime.iso8601.javaSqlTimestampConverter)
    }

    object dateAsClassic {
        implicit val zonedDateTimeJsonCoder    : JsonCoder[ZonedDateTime]    = stringJsonCoder.mapWithConverter(datetime.classic.zonedDateTimeConverter)
        implicit val localDateJsonCoder        : JsonCoder[LocalDate]        = stringJsonCoder.mapWithConverter(datetime.classic.localDateConverter)
        implicit val localDateTimeJsonCoder    : JsonCoder[LocalDateTime]    = stringJsonCoder.mapWithConverter(datetime.classic.localDateTimeConverter)
        implicit val localTimeJsonCoder        : JsonCoder[LocalTime]        = stringJsonCoder.mapWithConverter(datetime.classic.localTimeConverter)
        implicit val javaDateJsonCoder         : JsonCoder[JavaDate]         = stringJsonCoder.mapWithConverter(datetime.classic.javaDateConverter)
        implicit val javaSqlDateJsonCoder      : JsonCoder[JavaSqlDate]      = stringJsonCoder.mapWithConverter(datetime.classic.javaSqlDateConverter)
        implicit val javaSqlTimeJsonCoder      : JsonCoder[JavaSqlTime]      = stringJsonCoder.mapWithConverter(datetime.classic.javaSqlTimeConverter)
        implicit val javaSqlTimestampJsonCoder : JsonCoder[JavaSqlTimestamp] = stringJsonCoder.mapWithConverter(datetime.classic.javaSqlTimestampConverter)
    }

    object dateAsSqlServer {
        implicit val zonedDateTimeJsonCoder    : JsonCoder[ZonedDateTime]    = stringJsonCoder.mapWithConverter(datetime.sqlServer.zonedDateTimeConverter)
        implicit val localDateJsonCoder        : JsonCoder[LocalDate]        = stringJsonCoder.mapWithConverter(datetime.sqlServer.localDateConverter)
        implicit val localDateTimeJsonCoder    : JsonCoder[LocalDateTime]    = stringJsonCoder.mapWithConverter(datetime.sqlServer.localDateTimeConverter)
        implicit val localTimeJsonCoder        : JsonCoder[LocalTime]        = stringJsonCoder.mapWithConverter(datetime.sqlServer.localTimeConverter)
        implicit val javaDateJsonCoder         : JsonCoder[JavaDate]         = stringJsonCoder.mapWithConverter(datetime.sqlServer.javaDateConverter)
        implicit val javaSqlDateJsonCoder      : JsonCoder[JavaSqlDate]      = stringJsonCoder.mapWithConverter(datetime.sqlServer.javaSqlDateConverter)
        implicit val javaSqlTimeJsonCoder      : JsonCoder[JavaSqlTime]      = stringJsonCoder.mapWithConverter(datetime.sqlServer.javaSqlTimeConverter)
        implicit val javaSqlTimestampJsonCoder : JsonCoder[JavaSqlTimestamp] = stringJsonCoder.mapWithConverter(datetime.sqlServer.javaSqlTimestampConverter)
    }

    /** `JsonCoder` for `org.joda.time.Duration`. Encodes as a JSON number */
    implicit lazy val durationJsonCoder = longJsonCoder.mapWithConverter(TypeConverter(
        (d: Duration) => Okay(d.toMillis): Result[Long],
        (d: Long)     => Okay(Duration.ofMillis(d)): Result[Duration]
    ))
}

trait scalarLPI { self: scalar =>
    implicit def stringCoderAsJsonCoder[A](implicit stringEncoder: StringEncoder[A], stringDecoder: StringDecoder[A]): JsonCoder[A] =
        stringJsonCoder.mapWithConverter(TypeConverter(
            (a: A)      => stringEncoder(a).mapFailure { _ => () }: Result[String],
            (s: String) => stringDecoder(s).mapFailure { _ => () }: Result[A]
        ))
}


