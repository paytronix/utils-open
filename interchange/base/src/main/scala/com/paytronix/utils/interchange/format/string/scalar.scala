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

package com.paytronix.utils.interchange.format.string

import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
import java.nio.ByteBuffer
import java.sql.{Date => JavaSqlDate, Time => JavaSqlTime, Timestamp => JavaSqlTimestamp}
import java.util.{Date => JavaDate}
import javax.xml.bind.DatatypeConverter
import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe.{TypeTag, typeTag}

import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import scalaz.BijectionT.bijection

import com.paytronix.utils.interchange.base.{CoderFailure, Receiver, atTerminal, datetime, terminal}
import com.paytronix.utils.interchange.base.enum.{enumerationInstance, enumerationValueFromString}
import com.paytronix.utils.scala.result.{FailedG, Okay, Result, parameter, tryCatchResultG, tryCatchingResultG}

object scalar extends scalar

trait scalar {
    implicit object unitStringCoder extends StringCoder[Unit] {
        object encode extends StringEncoder[Unit] {
            def run(in: Unit, out: Receiver[String]) = out("")
        }

        object decode extends StringDecoder[Unit] {
            def run(in: String, out: Receiver[Unit]) = out(())
        }
    }

    implicit object booleanStringCoder extends StringCoder[Boolean] {
        object encode extends StringEncoder[Boolean] {
            def run(in: Boolean, out: Receiver[String]) = tryCatchResultG(terminal)(out(in.toString))
        }

        object decode extends StringDecoder[Boolean] {
            def run(in: String, out: Receiver[Boolean]) =
                in.toLowerCase match {
                    case "true"  => out(true)
                    case "false" => out(false)
                    case _       => FailedG("not \"true\" or \"false\"", CoderFailure.terminal)
                }
        }
    }

    implicit object byteStringCoder extends StringCoder[Byte] {
        object encode extends StringEncoder[Byte] {
            def run(in: Byte, out: Receiver[String]) = tryCatchResultG(terminal)(out(in.toString))
        }

        object decode extends StringDecoder[Byte] {
            def run(in: String, out: Receiver[Byte]) = tryCatchResultG(terminal)(out(java.lang.Byte.parseByte(in)))
        }
    }

    implicit object shortStringCoder extends StringCoder[Short] {
        object encode extends StringEncoder[Short] {
            def run(in: Short, out: Receiver[String]) = tryCatchResultG(terminal)(out(in.toString))
        }

        object decode extends StringDecoder[Short] {
            def run(in: String, out: Receiver[Short]) = tryCatchResultG(terminal)(out(java.lang.Short.parseShort(in)))
        }
    }

    implicit object intStringCoder extends StringCoder[Int] {
        object encode extends StringEncoder[Int] {
            def run(in: Int, out: Receiver[String]) = tryCatchResultG(terminal)(out(in.toString))
        }

        object decode extends StringDecoder[Int] {
            def run(in: String, out: Receiver[Int]) = tryCatchResultG(terminal)(out(java.lang.Integer.parseInt(in)))
        }
    }

    implicit object longStringCoder extends StringCoder[Long] {
        object encode extends StringEncoder[Long] {
            def run(in: Long, out: Receiver[String]) = tryCatchResultG(terminal)(out(in.toString))
        }
        object decode extends StringDecoder[Long] {
            def run(in: String, out: Receiver[Long]) = tryCatchResultG(terminal)(out(java.lang.Long.parseLong(in)))
        }
    }

    implicit object floatStringCoder extends StringCoder[Float] {
        object encode extends StringEncoder[Float] {
            def run(in: Float, out: Receiver[String]) = tryCatchResultG(terminal)(out(in.toString))
        }

        object decode extends StringDecoder[Float] {
            def run(in: String, out: Receiver[Float]) = tryCatchResultG(terminal)(out(java.lang.Float.parseFloat(in)))
        }
    }

    implicit object doubleStringCoder extends StringCoder[Double] {
        object encode extends StringEncoder[Double] {
            def run(in: Double, out: Receiver[String]) = tryCatchResultG(terminal)(out(in.toString))
        }

        object decode extends StringDecoder[Double] {
            def run(in: String, out: Receiver[Double]) = tryCatchResultG(terminal)(out(java.lang.Double.parseDouble(in)))
        }
    }

    implicit object javaBigIntegerStringCoder extends StringCoder[JavaBigInteger] {
        object encode extends StringEncoder[JavaBigInteger] {
            def run(in: JavaBigInteger, out: Receiver[String]) = tryCatchResultG(terminal)(out(in.toString))
        }

        object decode extends StringDecoder[JavaBigInteger] {
            def run(in: String, out: Receiver[JavaBigInteger]) = tryCatchResultG(terminal)(out(new JavaBigInteger(in)))
        }
    }

    implicit val scalaBigIntStringCoder = javaBigIntegerStringCoder.mapBijection(bijection (
        (bi: BigInt) => Okay(bi.bigInteger): Result[JavaBigInteger],
        (bi: JavaBigInteger) => Okay(BigInt(bi)): Result[BigInt]
    ))

    implicit object javaBigDecimalStringCoder extends StringCoder[JavaBigDecimal] {
        object encode extends StringEncoder[JavaBigDecimal] {
            def run(in: JavaBigDecimal, out: Receiver[String]) = out(in.toString)
        }
        object decode extends StringDecoder[JavaBigDecimal] {
            def run(in: String, out: Receiver[JavaBigDecimal]) = tryCatchResultG(terminal)(out(new JavaBigDecimal(in)))
        }
    }

    implicit val scalaBigDecimalStringCoder = javaBigDecimalStringCoder.mapBijection(bijection (
        (bd: BigDecimal) => Okay(bd.bigDecimal): Result[JavaBigDecimal],
        (bd: JavaBigDecimal) => Okay(BigDecimal(bd)): Result[BigDecimal]
    ))

    implicit object charStringCoder extends StringCoder[Char] {
        object encode extends StringEncoder[Char] {
            def run(in: Char, out: Receiver[String]) = tryCatchResultG(terminal)(out(in.toString))
        }

        object decode extends StringDecoder[Char] {
            def run(in: String, out: Receiver[Char]) =
                if (in.length == 1) out(in.charAt(0))
                else FailedG("expected a string with exactly one character in it", CoderFailure.terminal)
        }
    }

    implicit object stringCoder extends StringCoder[String] {
        object encode extends StringEncoder[String] {
            def run(in: String, out: Receiver[String]) = out(in)
        }
        object decode extends StringDecoder[String] {
            def run(in: String, out: Receiver[String]) = out(in)
        }
    }

    implicit object byteBufferStringCoder extends StringCoder[ByteBuffer] {
        object encode extends StringEncoder[ByteBuffer] {
            def run(in: ByteBuffer, out: Receiver[String]) =
                tryCatchResultG(terminal) {
                    if (in.hasArray && in.capacity == in.limit && in.position == 0)
                        out(DatatypeConverter.printBase64Binary(in.array))
                    else {
                        // FIXME not the most efficient, better to chop it up into blocks divisible by 3 (base64 uses 3-grams)
                        val a = Array.ofDim[Byte](in.remaining)
                        in.get(a)
                        out(DatatypeConverter.printBase64Binary(a))
                    }
                }
        }
        object decode extends StringDecoder[ByteBuffer] {
            def run(in: String, out: Receiver[ByteBuffer]) =
                tryCatchResultG(terminal) {
                    out(ByteBuffer.wrap(DatatypeConverter.parseBase64Binary(in)))
                }
        }
    }

    implicit val byteArrayStringCoder: StringCoder[Array[Byte]] =
        byteBufferStringCoder.mapBijection(bijection (
            (ba: Array[Byte]) => Okay(ByteBuffer.wrap(ba)),
            (bb: ByteBuffer) => Okay(bb.array)
        ))

    implicit def javaEnumStringCoder[A <: Enum[A]: ClassTag]: StringCoder[A] = {
        object encode extends StringEncoder[A] {
            def run(in: A, out: Receiver[String]) = out(in.toString)
        }
        object decode extends StringDecoder[A] {
            def run(in: String, out: Receiver[A]) = {
                val errorMessage = FailedG(s""""$in" is not a valid enumeration value""", CoderFailure.terminal)
                tryCatchingResultG(classOf[IllegalArgumentException])(errorMessage) {
                    out(Enum.valueOf(classTag[A].runtimeClass.asInstanceOf[Class[A]], in))
                }
            }
        }
        StringCoder.make(encode, decode)
    }

    implicit def scalaEnumStringCoder[A <: Enumeration: TypeTag]: StringCoder[A#Value] = {
        val enumeration = atTerminal(enumerationInstance[A]).orThrow

        object encode extends StringEncoder[A#Value] {
            def run(in: A#Value, out: Receiver[String]) = out(in.toString)
        }
        object decode extends StringDecoder[A#Value] {
            def run(in: String, out: Receiver[A#Value]) =
                enumerationValueFromString(enumeration, in, out)
        }
        StringCoder.make(encode, decode)
    }

    implicit val dateTimeStringCoderIso8601         = dateAsIso8601.dateTimeStringCoder
    implicit val localDateStringCoderIso8601        = dateAsIso8601.localDateStringCoder
    implicit val localDateTimeStringCoderIso8601    = dateAsIso8601.localDateTimeStringCoder
    implicit val localTimeStringCoderIso8601        = dateAsIso8601.localTimeStringCoder
    implicit val javaDateStringCoderIso8601         = dateAsIso8601.javaDateStringCoder
    implicit val javaSqlDateStringCoderIso8601      = dateAsIso8601.javaSqlDateStringCoder
    implicit val javaSqlTimeStringCoderIso8601      = dateAsIso8601.javaSqlTimeStringCoder
    implicit val javaSqlTimestampStringCoderIso8601 = dateAsIso8601.javaSqlTimestampStringCoder

    object dateAsIso8601 {
        implicit val dateTimeStringCoder         : StringCoder[DateTime]         = stringCoder.mapBijection(datetime.iso8601.dateTimeBijection)
        implicit val localDateStringCoder        : StringCoder[LocalDate]        = stringCoder.mapBijection(datetime.iso8601.localDateBijection)
        implicit val localDateTimeStringCoder    : StringCoder[LocalDateTime]    = stringCoder.mapBijection(datetime.iso8601.localDateTimeBijection)
        implicit val localTimeStringCoder        : StringCoder[LocalTime]        = stringCoder.mapBijection(datetime.iso8601.localTimeBijection)
        implicit val javaDateStringCoder         : StringCoder[JavaDate]         = stringCoder.mapBijection(datetime.iso8601.javaDateBijection)
        implicit val javaSqlDateStringCoder      : StringCoder[JavaSqlDate]      = stringCoder.mapBijection(datetime.iso8601.javaSqlDateBijection)
        implicit val javaSqlTimeStringCoder      : StringCoder[JavaSqlTime]      = stringCoder.mapBijection(datetime.iso8601.javaSqlTimeBijection)
        implicit val javaSqlTimestampStringCoder : StringCoder[JavaSqlTimestamp] = stringCoder.mapBijection(datetime.iso8601.javaSqlTimestampBijection)
    }

    object dateAsClassic {
        implicit val dateTimeStringCoder         : StringCoder[DateTime]         = stringCoder.mapBijection(datetime.classic.dateTimeBijection)
        implicit val localDateStringCoder        : StringCoder[LocalDate]        = stringCoder.mapBijection(datetime.classic.localDateBijection)
        implicit val localDateTimeStringCoder    : StringCoder[LocalDateTime]    = stringCoder.mapBijection(datetime.classic.localDateTimeBijection)
        implicit val localTimeStringCoder        : StringCoder[LocalTime]        = stringCoder.mapBijection(datetime.classic.localTimeBijection)
        implicit val javaDateStringCoder         : StringCoder[JavaDate]         = stringCoder.mapBijection(datetime.classic.javaDateBijection)
        implicit val javaSqlDateStringCoder      : StringCoder[JavaSqlDate]      = stringCoder.mapBijection(datetime.classic.javaSqlDateBijection)
        implicit val javaSqlTimeStringCoder      : StringCoder[JavaSqlTime]      = stringCoder.mapBijection(datetime.classic.javaSqlTimeBijection)
        implicit val javaSqlTimestampStringCoder : StringCoder[JavaSqlTimestamp] = stringCoder.mapBijection(datetime.classic.javaSqlTimestampBijection)
    }

    object dateAsSqlServer {
        implicit val dateTimeStringCoder         : StringCoder[DateTime]         = stringCoder.mapBijection(datetime.sqlServer.dateTimeBijection)
        implicit val localDateStringCoder        : StringCoder[LocalDate]        = stringCoder.mapBijection(datetime.sqlServer.localDateBijection)
        implicit val localDateTimeStringCoder    : StringCoder[LocalDateTime]    = stringCoder.mapBijection(datetime.sqlServer.localDateTimeBijection)
        implicit val localTimeStringCoder        : StringCoder[LocalTime]        = stringCoder.mapBijection(datetime.sqlServer.localTimeBijection)
        implicit val javaDateStringCoder         : StringCoder[JavaDate]         = stringCoder.mapBijection(datetime.sqlServer.javaDateBijection)
        implicit val javaSqlDateStringCoder      : StringCoder[JavaSqlDate]      = stringCoder.mapBijection(datetime.sqlServer.javaSqlDateBijection)
        implicit val javaSqlTimeStringCoder      : StringCoder[JavaSqlTime]      = stringCoder.mapBijection(datetime.sqlServer.javaSqlTimeBijection)
        implicit val javaSqlTimestampStringCoder : StringCoder[JavaSqlTimestamp] = stringCoder.mapBijection(datetime.sqlServer.javaSqlTimestampBijection)
    }
}
