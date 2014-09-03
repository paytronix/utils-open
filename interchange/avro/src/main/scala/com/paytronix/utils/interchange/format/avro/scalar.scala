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

package com.paytronix.utils.interchange.format.avro

import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
import java.nio.ByteBuffer
import java.sql.{Date => JavaSqlDate, Time => JavaSqlTime, Timestamp => JavaSqlTimestamp}
import java.util.{Arrays, Date => JavaDate}
import scala.annotation.{Annotation, StaticAnnotation}
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.language.experimental.macros
import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe.{TypeTag, typeTag}

import org.apache.avro.{Schema, io}
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}
import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import scalaz.BijectionT.bijection

import com.paytronix.utils.interchange.base.{CoderFailure, Receiver, atTerminal, datetime, terminal}
import com.paytronix.utils.interchange.base.enum.{enumerationInstance, enumerationValueFromString}
import com.paytronix.utils.interchange.format.string.{StringDecoder, StringEncoder}
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, tryCatchValue, tryCatchValueG, tryCatchResultG}

import utils.nameAndNamespaceFromClass

/** `AvroCoder`s for scalar types such as `int`, `String`, and so on. */
object scalar extends scalar

/** `AvroCoder`s for scalar types such as `int`, `String`, and so on. */
trait scalar extends scalarLPI {
    /** `AvroCoder` for `Unit`. Encodes as an empty record of type scala.Unit. */
    implicit object unitAvroCoder extends AvroCoder[Unit] {
        trait UnitEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = {
                val r = Schema.createRecord("Unit", "", "scala", false)
                r.setFields(Arrays.asList())
                r
            }
            val defaultJson = Some(jsonNodeFactory.objectNode)
        }
        object encode extends AvroEncoder[Unit] with UnitEncoderOrDecoder {
            def encodeDefaultJson(in: Unit) = Okay(jsonNodeFactory.objectNode)
            def run(in: Unit, out: io.Encoder) = tryCatchResultG(terminal) { Okay.unit }
        }
        object decode extends AvroDecoder[Unit] with UnitEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[Unit]) = tryCatchResultG(terminal) { val _ = in.readFieldOrder(); out(()) }
        }
    }

    /** `AvroCoder` for `Boolean`. Encodes as an Avro boolean */
    implicit object booleanAvroCoder extends AvroCoder[Boolean] {
        trait BooleanEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.create(Schema.Type.BOOLEAN)
            val defaultJson = None
        }
        object encode extends AvroEncoder[Boolean] with BooleanEncoderOrDecoder {
            def encodeDefaultJson(in: Boolean) = Okay(jsonNodeFactory.booleanNode(in))
            def run(in: Boolean, out: io.Encoder) = tryCatchResultG(terminal) { out.writeBoolean(in); Okay.unit }
        }
        object decode extends AvroDecoder[Boolean] with BooleanEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[Boolean]) = tryCatchResultG(terminal) { out(in.readBoolean()) }
        }
    }

    /** `AvroCoder` for `Byte`. Encodes as an Avro fixed of size 1 */
    implicit object byteAvroCoderFixed extends AvroCoder[Byte] {
        trait ByteEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.createFixed("byte", "", "", 1)
            val defaultJson = None
        }
        object encode extends AvroEncoder[Byte] with ByteEncoderOrDecoder {
            def encodeDefaultJson(in: Byte) = Okay(jsonNodeFactory.textNode(new String(Array(in), "ISO-8859-1")))
            def run(in: Byte, out: io.Encoder) = tryCatchResultG(terminal) {
                val bytes = Array.ofDim[Byte](1)
                bytes(0) = in
                out.writeFixed(bytes, 0, 1)
                Okay.unit
            }
        }
        object decode extends AvroDecoder[Byte] with ByteEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[Byte]) = tryCatchResultG(terminal) {
                val bytes = Array.ofDim[Byte](1)
                in.readFixed(bytes, 0, 1)
                out(bytes(0))
            }
        }
    }

    /** `AvroCoder` for `Short`. Encodes as an Avro fixed of size 2 */
    implicit object shortAvroCoderFixed extends AvroCoder[Short] {
        trait ShortEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.createFixed("char", "", "", 2)
            val defaultJson = None
        }
        object encode extends AvroEncoder[Short] with ShortEncoderOrDecoder {
            private def encodeBytes(in: Short) = {
                val bytes = Array.ofDim[Byte](2)
                bytes(0) = ((in >>> 8) & 0xff).asInstanceOf[Byte]
                bytes(1) = ((in >>> 0) & 0xff).asInstanceOf[Byte]
                bytes
            }
            def encodeDefaultJson(in: Short) = Okay(jsonNodeFactory.textNode(new String(encodeBytes(in), "ISO-8859-1")))
            def run(in: Short, out: io.Encoder) = tryCatchResultG(terminal) {
                out.writeFixed(encodeBytes(in), 0, 2)
                Okay.unit
            }
        }
        object decode extends AvroDecoder[Short] with ShortEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[Short]) = tryCatchResultG(terminal) {
                val bytes = Array.ofDim[Byte](2)
                in.readFixed(bytes, 0, 2)
                out((
                    ((bytes(0) << 8) & 0xff00) |
                    ( bytes(1)       & 0x00ff)
                ).asInstanceOf[Short])
            }
        }
    }

    /** `AvroCoder` for `Int`. Encodes as an Avro int (zig-zag encoding) */
    implicit object intAvroCoder extends AvroCoder[Int] {
        trait IntEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.create(Schema.Type.INT)
            val defaultJson = None
        }
        object encode extends AvroEncoder[Int] with IntEncoderOrDecoder {
            def encodeDefaultJson(in: Int) = Okay(jsonNodeFactory.numberNode(in))
            def run(in: Int, out: io.Encoder) = tryCatchResultG(terminal) {
                out.writeInt(in)
                Okay.unit
            }
        }
        object decode extends AvroDecoder[Int] with IntEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[Int]) = tryCatchResultG(terminal) {
                out(in.readInt())
            }
        }
    }

    /** `AvroCoder` for `Long`. Encodes as an Avro long (zig-zag encoding) */
    implicit object longAvroCoder extends AvroCoder[Long] {
        trait LongEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.create(Schema.Type.LONG)
            val defaultJson = None
        }
        object encode extends AvroEncoder[Long] with LongEncoderOrDecoder {
            def encodeDefaultJson(in: Long) = Okay(jsonNodeFactory.numberNode(in))
            def run(in: Long, out: io.Encoder) = tryCatchResultG(terminal) {
                out.writeLong(in)
                Okay.unit
            }
        }
        object decode extends AvroDecoder[Long] with LongEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[Long]) = tryCatchResultG(terminal) {
                out(in.readLong())
            }
        }
    }

    /** `AvroCoder` for `Float`. Encodes as an Avro float */
    implicit object floatAvroCoder extends AvroCoder[Float] {
        trait FloatEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.create(Schema.Type.FLOAT)
            val defaultJson = None
        }
        object encode extends AvroEncoder[Float] with FloatEncoderOrDecoder {
            def encodeDefaultJson(in: Float) = Okay(jsonNodeFactory.numberNode(in))
            def run(in: Float, out: io.Encoder) = tryCatchResultG(terminal) {
                out.writeFloat(in)
                Okay.unit
            }
        }
        object decode extends AvroDecoder[Float] with FloatEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[Float]) = tryCatchResultG(terminal) {
                out(in.readFloat())
            }
        }
    }

    /** `AvroCoder` for `Double`. Encodes as an Avro double */
    implicit object doubleAvroCoder extends AvroCoder[Double] {
        trait DoubleEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.create(Schema.Type.DOUBLE)
            val defaultJson = None
        }
        object encode extends AvroEncoder[Double] with DoubleEncoderOrDecoder {
            def encodeDefaultJson(in: Double) = Okay(jsonNodeFactory.numberNode(in))
            def run(in: Double, out: io.Encoder) = tryCatchResultG(terminal) {
                out.writeDouble(in)
                Okay.unit
            }
        }
        object decode extends AvroDecoder[Double] with DoubleEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[Double]) = tryCatchResultG(terminal) {
                out(in.readDouble())
            }
        }
    }

    /** `AvroCoder` for `java.math.BigInteger`. Encodes as an Avro byte array using `.toByteArray` of `BigInteger` */
    implicit object javaBigIntegerAvroCoderBytes extends AvroCoder[JavaBigInteger] {
        trait JavaBigIntegerEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.create(Schema.Type.BYTES)
            val defaultJson = None
        }
        object encode extends AvroEncoder[JavaBigInteger] with JavaBigIntegerEncoderOrDecoder {
            def encodeDefaultJson(in: JavaBigInteger) = tryCatchValueG(terminal) {
                jsonNodeFactory.textNode(new String(in.toByteArray, "ISO-8859-1"))
            }
            def run(in: JavaBigInteger, out: io.Encoder) = tryCatchResultG(terminal) {
                out.writeBytes(ByteBuffer.wrap(in.toByteArray))
                Okay.unit
            }
        }

        object decode extends AvroDecoder[JavaBigInteger] with JavaBigIntegerEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[JavaBigInteger]) = tryCatchResultG(terminal) {
                val byteBuffer = in.readBytes(null)
                if (byteBuffer.remaining < 1)
                    FailedG("insufficient number of bytes to represent a BigInteger - got " +
                            byteBuffer.remaining + " but need at least 1", CoderFailure.terminal)
                else {
                    val bytes = Array.ofDim[Byte](byteBuffer.remaining)
                    byteBuffer.get(bytes)
                    out(new JavaBigInteger(bytes))
                }
            }
        }
    }

    /** `AvroCoder` for `scala.math.BigInt`. Encodes as an Avro byte array using `.toByteArray` of `BigInteger` */
    implicit lazy val scalaBigIntAvroCoderBytes = javaBigIntegerAvroCoderBytes.mapBijection(bijection (
        (bi: BigInt) => Okay(bi.bigInteger): Result[JavaBigInteger],
        (bi: JavaBigInteger) => Okay(BigInt(bi)): Result[BigInt]
    ))


    /** `AvroCoder` for `java.math.BigDecimal`. Encodes as an Avro byte array with the scale as a 4-byte integer followed by the unscaled digits using `.toByteArray` */
    implicit object javaBigDecimalAvroCoderBytes extends AvroCoder[JavaBigDecimal] {
        trait JavaBigDecimalEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.create(Schema.Type.BYTES)
            val defaultJson = None
        }
        object encode extends AvroEncoder[JavaBigDecimal] with JavaBigDecimalEncoderOrDecoder {
            private def encodeByteBuffer(in: JavaBigDecimal): ByteBuffer = {
                val bytes = in.unscaledValue.toByteArray
                val byteBuffer = ByteBuffer.allocate(bytes.length + 4)
                byteBuffer.putInt(in.scale)
                byteBuffer.put(bytes).rewind
                byteBuffer
            }
            def encodeDefaultJson(in: JavaBigDecimal) =
                tryCatchValueG(terminal)(jsonNodeFactory.textNode(new String(encodeByteBuffer(in).array, "ISO-8859-1")))
            def run(in: JavaBigDecimal, out: io.Encoder) = tryCatchResultG(terminal) {
                out.writeBytes(encodeByteBuffer(in))
                Okay.unit
            }
        }
        object decode extends AvroDecoder[JavaBigDecimal] with JavaBigDecimalEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[JavaBigDecimal]) = tryCatchResultG(terminal) {
                val byteBuffer = in.readBytes(null)
                if (byteBuffer.remaining < 5)
                    FailedG("insufficient number of bytes to represent a BigDecimal - got " +
                            byteBuffer.remaining + " but need at least 5", CoderFailure.terminal)
                else {
                    val scale = byteBuffer.getInt
                    val bytes = Array.ofDim[Byte](byteBuffer.remaining)
                    byteBuffer.get(bytes)
                    out(new JavaBigDecimal(new JavaBigInteger(bytes), scale))
                }
            }
        }
    }

    /** `AvroCoder` for `scala.math.BigDecimal`. Encodes as an Avro byte array with the scale as a 4-byte integer followed by the unscaled digits using `.toByteArray` */
    implicit lazy val scalaBigDecimalAvroCoderBytes = javaBigDecimalAvroCoderBytes.mapBijection(bijection (
        (bd: BigDecimal) => Okay(bd.bigDecimal): Result[JavaBigDecimal],
        (bd: JavaBigDecimal) => Okay(BigDecimal(bd)): Result[BigDecimal]
    ))

    /** `AvroCoder` for `Char`. Encodes as an Avro fixed of length 2 */
    implicit object charAvroCoderFixed extends AvroCoder[Char] {
        trait CharEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.createFixed("char", "", "", 2)
            val defaultJson = None
        }
        object encode extends AvroEncoder[Char] with CharEncoderOrDecoder {
            private def encodeBytes(in: Char) = {
                val bytes = Array.ofDim[Byte](2)
                bytes(0) = ((in >>> 8) & 0xff).asInstanceOf[Byte]
                bytes(1) = ((in >>> 0) & 0xff).asInstanceOf[Byte]
                bytes
            }

            def encodeDefaultJson(in: Char) = Okay(jsonNodeFactory.textNode(new String(encodeBytes(in), "ISO-8859-1")))
            def run(in: Char, out: io.Encoder) = tryCatchResultG(terminal) {
                out.writeFixed(encodeBytes(in), 0, 2)
                Okay.unit
            }
        }
        object decode extends AvroDecoder[Char] with CharEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[Char]) = tryCatchResultG(terminal) {
                val bytes = Array.ofDim[Byte](2)
                in.readFixed(bytes, 0, 2)
                out((((bytes(0) << 8) & 0xff00)| ((bytes(1) << 0) & 0x00ff)).asInstanceOf[Char])
            }
        }
    }

    /** `AvroCoder` for `String`. Encodes as an Avro string: length as a zig-zag encoded integer followed by UTF-8 bytes */
    implicit object stringAvroCoder extends AvroCoder[String] {
        trait StringEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.create(Schema.Type.STRING)
            val defaultJson = None
        }
        object encode extends AvroEncoder[String] with StringEncoderOrDecoder {
            def encodeDefaultJson(in: String) = Okay(jsonNodeFactory.textNode(in))
            def run(in: String, out: io.Encoder) =
                if (in == null) FailedG("cannot encode null string", CoderFailure.terminal)
                else tryCatchResultG(terminal) {
                    out.writeString(in)
                    Okay.unit
                }
        }
        object decode extends AvroDecoder[String] with StringEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[String]) = tryCatchResultG(terminal) {
                out(in.readString(null).toString)
            }
        }
    }

    /** `AvroCoder` for `java.nio.ByteByffer`. Encodes as an Avro byte array */
    implicit object byteBufferAvroCoder extends AvroCoder[ByteBuffer] {
        trait ByteBufferEncoderOrDecoder extends AvroEncoderOrDecoder {
            val schema = Schema.create(Schema.Type.BYTES)
            val defaultJson = None
        }
        object encode extends AvroEncoder[ByteBuffer] with ByteBufferEncoderOrDecoder {
            def encodeDefaultJson(in: ByteBuffer) = tryCatchValueG(terminal) {
                val bytes = Array.ofDim[Byte](in.remaining)
                in.mark()
                in.get(bytes)
                in.reset()
                jsonNodeFactory.textNode(new String(bytes, "ISO-8859-1"))
            }

            def run(in: ByteBuffer, out: io.Encoder) = tryCatchResultG(terminal) {
                out.writeBytes(in)
                Okay.unit
            }
        }
        object decode extends AvroDecoder[ByteBuffer] with ByteBufferEncoderOrDecoder {
            def run(in: io.ResolvingDecoder, out: Receiver[ByteBuffer]) =
                tryCatchResultG(terminal) {
                    out(in.readBytes(null))
                }
        }
    }

    /** `AvroCoder` for `Array[Byte]`. Encodes as an Avro byte array */
    implicit lazy val byteArrayAvroCoder: AvroCoder[Array[Byte]] =
        byteBufferAvroCoder.mapBijection(bijection (
            (ba: Array[Byte]) => Okay(ByteBuffer.wrap(ba)),
            (bb: ByteBuffer) => Okay(bb.array)
        ))

    /** `AvroCoder` for Java enumerations. Encodes as an Avro enumeration */
    implicit def javaEnumAvroCoder[A <: Enum[A]: ClassTag]: AvroCoder[A] = {
        def toSanitizedString(in: A): String = in.toString.replaceAll("[^_a-zA-Z0-9]", "")
        val enumClass = classTag[A].runtimeClass.asInstanceOf[Class[A]]
        val enumValues: Vector[A] = (Vector.empty ++ enumClass.getMethod("values").invoke(null).asInstanceOf[Array[A]]).sortBy(toSanitizedString)
        val enumsByOrdinal: Map[Int, A] = Map(enumValues.zipWithIndex.map(_.swap): _*)
        val ordinalsByEnum: Map[A, Int] = Map(enumValues.zipWithIndex: _*)

        class javaEnumAvroCoder extends AvroCoder[A] {
            trait JavaEnumEncoderOrDecoder extends AvroEncoderOrDecoder {
                val schema = {
                    val (namespace, name) = nameAndNamespaceFromClass(enumClass)
                    Schema.createEnum(name, "", namespace, enumValues.map(toSanitizedString).asJava)
                }
                val defaultJson = None
            }
            object encode extends AvroEncoder[A] with JavaEnumEncoderOrDecoder {
                def encodeDefaultJson(in: A) = Okay(jsonNodeFactory.textNode(in.toString))
                def run(in: A, out: io.Encoder) = tryCatchResultG(terminal) {
                    out.writeEnum(ordinalsByEnum(in))
                    Okay.unit
                }
            }
            object decode extends AvroDecoder[A] with JavaEnumEncoderOrDecoder {
                def run(in: io.ResolvingDecoder, out: Receiver[A]) = tryCatchResultG(terminal) {
                    in.readEnum() match {
                        case i if i < 0                  => FailedG("read negative enum index " + i + " from Avro", CoderFailure.terminal)
                        case i if i >= enumValues.length => FailedG("read overflow enum index " + i + " from Avro", CoderFailure.terminal)
                        case i                           => out(enumsByOrdinal(i))
                    }
                }
            }
        }
        new javaEnumAvroCoder
    }

    /** `AvroCoder` for Scala `Enumeration`s. Encodes as an Avro enumeration */
    implicit def scalaEnumAvroCoder[A <: Enumeration: TypeTag]: AvroCoder[A#Value] = {
        def toSanitizedString(in: A#Value): String = in.toString.replaceAll("[^_a-zA-Z0-9]", "")
        val enumeration = atTerminal(enumerationInstance[A]).orThrow
        val enumValues: Vector[A#Value] = (Vector.empty ++ enumeration.values).sortBy(toSanitizedString)
        val enumsByOrdinal: Map[Int, A#Value] = Map(enumValues.zipWithIndex.map(_.swap): _*)
        val ordinalsByEnum: Map[A#Value, Int] = Map(enumValues.zipWithIndex: _*)

        class scalaEnumAvroCoder extends AvroCoder[A#Value] {
            trait ScalaEnumEncoderOrDecoder extends AvroEncoderOrDecoder {
                val schema = {
                    val (namespace, name) = nameAndNamespaceFromClass(enumeration.getClass)
                    Schema.createEnum(name, "", namespace, enumValues.map(toSanitizedString).asJava)
                }
                val defaultJson = None
            }
            object encode extends AvroEncoder[A#Value] with ScalaEnumEncoderOrDecoder {
                def encodeDefaultJson(in: A#Value) = Okay(jsonNodeFactory.textNode(in.toString.replaceAll("[^_a-zA-Z0-9]", "")))
                def run(in: A#Value, out: io.Encoder) = tryCatchResultG(terminal) {
                    out.writeEnum(ordinalsByEnum(in))
                    Okay.unit
                }
            }
            object decode extends AvroDecoder[A#Value] with ScalaEnumEncoderOrDecoder {
                def run(in: io.ResolvingDecoder, out: Receiver[A#Value]) = tryCatchResultG(terminal) {
                    in.readEnum() match {
                        case i if i < 0                  => FailedG("read negative enum index " + i + " from Avro", CoderFailure.terminal)
                        case i if i >= enumValues.length => FailedG("read overflow enum index " + i + " from Avro", CoderFailure.terminal)
                        case i                           => out(enumsByOrdinal(i))
                    }
                }
            }
        }
        new scalaEnumAvroCoder
    }

    implicit lazy val dateTimeAvroCoderLong         = dateAsLong.dateTimeAvroCoder
    implicit lazy val localDateAvroCoderLong        = dateAsLong.localDateAvroCoder
    implicit lazy val localDateTimeAvroCoderLong    = dateAsLong.localDateTimeAvroCoder
    implicit lazy val localTimeAvroCoderLong        = dateAsLong.localTimeAvroCoder
    implicit lazy val javaDateAvroCoderLong         = dateAsLong.javaDateAvroCoder
    implicit lazy val javaSqlDateAvroCoderLong      = dateAsLong.javaSqlDateAvroCoder
    implicit lazy val javaSqlTimeAvroCoderLong      = dateAsLong.javaSqlTimeAvroCoder
    implicit lazy val javaSqlTimestampAvroCoderLong = dateAsLong.javaSqlTimestampAvroCoder

    object dateAsLong {
        implicit lazy val dateTimeAvroCoder         = longAvroCoder.mapBijection(datetime.long.dateTimeBijection)
        implicit lazy val localDateAvroCoder        = longAvroCoder.mapBijection(datetime.long.localDateBijection)
        implicit lazy val localDateTimeAvroCoder    = longAvroCoder.mapBijection(datetime.long.localDateTimeBijection)
        implicit lazy val localTimeAvroCoder        = longAvroCoder.mapBijection(datetime.long.localTimeBijection)
        implicit lazy val javaDateAvroCoder         = longAvroCoder.mapBijection(datetime.long.javaDateBijection)
        implicit lazy val javaSqlDateAvroCoder      = longAvroCoder.mapBijection(datetime.long.javaSqlDateBijection)
        implicit lazy val javaSqlTimeAvroCoder      = longAvroCoder.mapBijection(datetime.long.javaSqlTimeBijection)
        implicit lazy val javaSqlTimestampAvroCoder = longAvroCoder.mapBijection(datetime.long.javaSqlTimestampBijection)
    }

    object dateAsIso8601String {
        implicit lazy val dateTimeAvroCoder         : AvroCoder[DateTime]         = stringAvroCoder.mapBijection(datetime.iso8601.dateTimeBijection)
        implicit lazy val localDateAvroCoder        : AvroCoder[LocalDate]        = stringAvroCoder.mapBijection(datetime.iso8601.localDateBijection)
        implicit lazy val localDateTimeAvroCoder    : AvroCoder[LocalDateTime]    = stringAvroCoder.mapBijection(datetime.iso8601.localDateTimeBijection)
        implicit lazy val localTimeAvroCoder        : AvroCoder[LocalTime]        = stringAvroCoder.mapBijection(datetime.iso8601.localTimeBijection)
        implicit lazy val javaDateAvroCoder         : AvroCoder[JavaDate]         = stringAvroCoder.mapBijection(datetime.iso8601.javaDateBijection)
        implicit lazy val javaSqlDateAvroCoder      : AvroCoder[JavaSqlDate]      = stringAvroCoder.mapBijection(datetime.iso8601.javaSqlDateBijection)
        implicit lazy val javaSqlTimeAvroCoder      : AvroCoder[JavaSqlTime]      = stringAvroCoder.mapBijection(datetime.iso8601.javaSqlTimeBijection)
        implicit lazy val javaSqlTimestampAvroCoder : AvroCoder[JavaSqlTimestamp] = stringAvroCoder.mapBijection(datetime.iso8601.javaSqlTimestampBijection)
    }

    object dateAsClassicString {
        implicit lazy val dateTimeAvroCoder         : AvroCoder[DateTime]         = stringAvroCoder.mapBijection(datetime.classic.dateTimeBijection)
        implicit lazy val localDateAvroCoder        : AvroCoder[LocalDate]        = stringAvroCoder.mapBijection(datetime.classic.localDateBijection)
        implicit lazy val localDateTimeAvroCoder    : AvroCoder[LocalDateTime]    = stringAvroCoder.mapBijection(datetime.classic.localDateTimeBijection)
        implicit lazy val localTimeAvroCoder        : AvroCoder[LocalTime]        = stringAvroCoder.mapBijection(datetime.classic.localTimeBijection)
        implicit lazy val javaDateAvroCoder         : AvroCoder[JavaDate]         = stringAvroCoder.mapBijection(datetime.classic.javaDateBijection)
        implicit lazy val javaSqlDateAvroCoder      : AvroCoder[JavaSqlDate]      = stringAvroCoder.mapBijection(datetime.classic.javaSqlDateBijection)
        implicit lazy val javaSqlTimeAvroCoder      : AvroCoder[JavaSqlTime]      = stringAvroCoder.mapBijection(datetime.classic.javaSqlTimeBijection)
        implicit lazy val javaSqlTimestampAvroCoder : AvroCoder[JavaSqlTimestamp] = stringAvroCoder.mapBijection(datetime.classic.javaSqlTimestampBijection)
    }

    object dateAsSqlServerString {
        implicit lazy val dateTimeAvroCoder         : AvroCoder[DateTime]         = stringAvroCoder.mapBijection(datetime.sqlServer.dateTimeBijection)
        implicit lazy val localDateAvroCoder        : AvroCoder[LocalDate]        = stringAvroCoder.mapBijection(datetime.sqlServer.localDateBijection)
        implicit lazy val localDateTimeAvroCoder    : AvroCoder[LocalDateTime]    = stringAvroCoder.mapBijection(datetime.sqlServer.localDateTimeBijection)
        implicit lazy val localTimeAvroCoder        : AvroCoder[LocalTime]        = stringAvroCoder.mapBijection(datetime.sqlServer.localTimeBijection)
        implicit lazy val javaDateAvroCoder         : AvroCoder[JavaDate]         = stringAvroCoder.mapBijection(datetime.sqlServer.javaDateBijection)
        implicit lazy val javaSqlDateAvroCoder      : AvroCoder[JavaSqlDate]      = stringAvroCoder.mapBijection(datetime.sqlServer.javaSqlDateBijection)
        implicit lazy val javaSqlTimeAvroCoder      : AvroCoder[JavaSqlTime]      = stringAvroCoder.mapBijection(datetime.sqlServer.javaSqlTimeBijection)
        implicit lazy val javaSqlTimestampAvroCoder : AvroCoder[JavaSqlTimestamp] = stringAvroCoder.mapBijection(datetime.sqlServer.javaSqlTimestampBijection)
    }
}

/** Lower priority implicit `AvroCoder`s that would conflict with the primary ones in `scalar` */
trait scalarLPI extends scalarLPI2 { self: scalar =>
    /** `AvroCoder` for `Byte` which encodes as an Avro integer instead of fixed. Can increase the space required but is more compatible with other tools */
    implicit lazy val byteAvroCoderInt = intAvroCoder.mapBijection(bijection (
        (b: Byte) => Okay(b: Int): Result[Int],
        (i: Int) => {
            if (i >= (Byte.MinValue: Int) || i <= (Byte.MaxValue: Int)) Okay(i.asInstanceOf[Byte])
            else Failed(s"expected an integer between ${Byte.MinValue} and ${Byte.MaxValue} but got $i")
        }: Result[Byte]
    ))

    /** `AvroCoder` for `Short` which encodes as an Avro integer instead of fixed. More compatible with other tools, but variable storage space.  */
    implicit lazy val shortAvroCoderInt = intAvroCoder.mapBijection(bijection (
        (s: Short) => Okay(s): Result[Int],
        (i: Int) => {
            if (i >= (Short.MinValue: Int) || i <= (Short.MaxValue: Int)) Okay(i.asInstanceOf[Short])
            else Failed(s"expected an integer between ${Short.MinValue} and ${Short.MaxValue} but got $i")
        }: Result[Short]
    ))

    /** `AvroCoder` for `Char` which encodes as an Avro string instead of fixed. More compatible with other tools, but increased storage space.  */
    implicit lazy val charAvroCoderString = stringAvroCoder.mapBijection(bijection (
        (c: Char) => Okay(new String(Array(c))): Result[String],
        (s: String) => (s.size match {
            case 1 => Okay(s.charAt(0))
            case other => Failed("expected a string with one character, got " + s.size + " characters")
        }): Result[Char]
    ))

    /** `AvroCoder` for `java.math.BigInteger` which encodes as an Avro string instead of bytes. More compatible with other tools, but increased storage space.  */
    implicit lazy val javaBigIntegerAvroCoderString = stringAvroCoder.mapBijection(bijection (
        (bi: JavaBigInteger) => Okay(bi.toString): Result[String],
        (s: String) => tryCatchValue(new JavaBigInteger(s)): Result[JavaBigInteger]
    ))

    /** `AvroCoder` for `scala.math.BigInt` which encodes as an Avro string instead of bytes. More compatible with other tools, but increased storage space.  */
    implicit lazy val scalaBigIntAvroCoderString = javaBigIntegerAvroCoderString.mapBijection(bijection (
        (bi: BigInt) => Okay(bi.bigInteger): Result[JavaBigInteger],
        (bi: JavaBigInteger) => Okay(BigInt(bi)): Result[BigInt]
    ))

    /** `AvroCoder` for `java.math.BigDecimal` which encodes as an Avro string instead of bytes. More compatible with other tools, but increased storage space.  */
    implicit lazy val javaBigDecimalAvroCoderString = stringAvroCoder.mapBijection(bijection (
        (bd: JavaBigDecimal) => Okay(bd.toString): Result[String],
        (s: String) => tryCatchValue(new JavaBigDecimal(s)): Result[JavaBigDecimal]
    ))

    /** `AvroCoder` for `scala.math.BigDecimal` which encodes as an Avro string instead of bytes. More compatible with other tools, but increased storage space.  */
    implicit lazy val scalaBigDecimalAvroCoderString = javaBigDecimalAvroCoderString.mapBijection(bijection (
        (bd: BigDecimal) => Okay(bd.bigDecimal): Result[JavaBigDecimal],
        (bd: JavaBigDecimal) => Okay(BigDecimal(bd)): Result[BigDecimal]
    ))
}

trait scalarLPI2 { self: scalar =>
    implicit def stringCoderAsAvroCoder[A](implicit stringEncoder: StringEncoder[A], stringDecoder: StringDecoder[A]): AvroCoder[A] =
        stringAvroCoder.mapBijection(bijection (
            (a: A)      => stringEncoder(a).mapFailure { _ => () }: Result[String],
            (s: String) => stringDecoder(s).mapFailure { _ => () }: Result[A]
        ))
}


