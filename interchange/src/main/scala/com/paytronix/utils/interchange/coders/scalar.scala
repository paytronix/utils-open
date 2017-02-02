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

import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
import java.nio.ByteBuffer

import net.liftweb.json.JsonAST.{JArray, JBool, JDouble, JInt, JNothing, JNull, JObject, JString, JValue, render}
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.Printer.compact
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.result.{FailedG, Okay, parameter, tryCatch}

/** Identity coder (just capture/emit some JValue) */
object JValueCoder extends ComposableCoder[JValue] {
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[JValue]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JNothing => FailedG("expected a value", Nil)
            case _        => Okay(in)
        }
    def encode(classLoader: ClassLoader, in: JValue) =
        Okay(in)

    def decodeString(classLoader: ClassLoader, in: String) =
        catchingCoderException(Okay(parse("[" + in + "]"))).flatMap {
            case JArray(List(v)) => Okay(v)
            case other => FailedG("expected an array with one element, not " + other, Nil)
        }

    def encodeString(classLoader: ClassLoader, in: JValue) =
        catchingCoderException(Okay(compact(render(in))))

    lazy val avroSchema = (Schema.create(Schema.Type.STRING), None)
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException(Okay(in.readString(null).toString)).flatMap(decodeString(classLoader, _))
    def encodeAvro(classLoader: ClassLoader, in: JValue, out: Encoder) =
        encodeString(classLoader, in).flatMap(s => catchingCoderException(Okay(out.writeString(s))))
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: JValue) =
        encodeString(classLoader, in).flatMap(s => catchingCoderException(Okay(jsonNodeFactory.textNode(s))))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (in == null) Okay(JNothing)
        else decodeString(classLoader, in.toString)

    def encodeMongoDB(classLoader: ClassLoader, in: JValue) =
        encodeString(classLoader, in)

    override def toString = "JValueCoder"
}

object JObjectCoder extends ComposableCoder[JObject] {
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[JObject]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case jo: JObject => Okay(jo)
            case _ => FailedG("expected an object", Nil)
        }

    def encode(classLoader: ClassLoader, in: JObject) =
        Okay(in)

    def decodeString(classLoader: ClassLoader, in: String) =
        JValueCoder.decodeString(classLoader, in) flatMap { decode(classLoader, _) }

    def encodeString(classLoader: ClassLoader, in: JValue) =
        JValueCoder.encodeString(classLoader, in)

    lazy val avroSchema = JValueCoder.avroSchema
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        JValueCoder.decodeAvro(classLoader, in) flatMap { decode(classLoader, _) }
    def encodeAvro(classLoader: ClassLoader, in: JObject, out: Encoder) =
        JValueCoder.encodeAvro(classLoader, in, out)
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: JObject) =
        JValueCoder.encodeAvroDefaultJson(classLoader, in)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        JValueCoder.decodeMongoDB(classLoader, in) flatMap { decode(classLoader, _) }

    def encodeMongoDB(classLoader: ClassLoader, in: JObject) =
        JValueCoder.encodeMongoDB(classLoader, in)

    override def toString = "JObjectCoder"
}

/** Unit coder, which codes the Unit */
object UnitCoder extends StringSafeCoder[Unit] {
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[Unit]

    def decode(classLoader: ClassLoader, in: JValue) = Okay(())
    def encode(classLoader: ClassLoader, in: Unit) = Okay(JNothing)

    def decodeString(classLoader: ClassLoader, in: String) = Okay(())
    def encodeString(classLoader: ClassLoader, in: Unit) = Okay("")

    lazy val avroSchema = (Schema.create(Schema.Type.NULL), Some(jsonNodeFactory.nullNode))
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = catchingCoderException(Okay(in.readNull()))
    def encodeAvro(classLoader: ClassLoader, in: Unit, out: Encoder) = catchingCoderException(Okay(out.writeNull()))
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Unit) = Okay(jsonNodeFactory.nullNode)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) = if (in == null) Okay(()) else FailedG("input is not null", Nil)
    def encodeMongoDB(classLoader: ClassLoader, in: Unit) = Okay(null)

    override def toString = "UnitCoder"
}


/** Map a Java BigDecimal to a JString */
object JavaBigDecimalCoder extends StringSafeCoder[JavaBigDecimal] {
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[JavaBigDecimal]

    def decode(classLoader: ClassLoader, in: JValue) = in match {
        case JString(s)     => decodeString(classLoader, s)
        case JDouble(d)     => decodeString(classLoader, d.toString)
        case JInt(bi)       => decodeString(classLoader, bi.toString)
        case JNothing|JNull => FailedG("required but missing", Nil)
        case _              => FailedG("not a string", Nil)
    }

    def encode(classLoader: ClassLoader, in: JavaBigDecimal) = Okay(JString(in.toString))

    def decodeString(classLoader: ClassLoader, in: String) =
        catchingCoderException(Okay(new JavaBigDecimal(in)))

    def encodeString(classLoader: ClassLoader, in: JavaBigDecimal) =
        Okay(in.toString)

    lazy val avroSchema = (Schema.create(Schema.Type.BYTES), None)
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            val byteBuffer = in.readBytes(null)
            if (byteBuffer.remaining < 5)
                FailedG("insufficient number of bytes to represent a BigDecimal - got " +
                        byteBuffer.remaining + " but need at least 5", Nil)
            else {
                val scale = byteBuffer.getInt
                val bytes = Array.ofDim[Byte](byteBuffer.remaining)
                byteBuffer.get(bytes)
                Okay(new JavaBigDecimal(new JavaBigInteger(bytes), scale))
            }
        }

    private def encodeByteBuffer(in: JavaBigDecimal): ByteBuffer = {
        val bytes = in.unscaledValue.toByteArray
        val byteBuffer = ByteBuffer.allocate(bytes.length + 4)
        byteBuffer.putInt(in.scale)
        byteBuffer.put(bytes).rewind
        byteBuffer
    }

    def encodeAvro(classLoader: ClassLoader, in: JavaBigDecimal, out: Encoder) =
        catchingCoderException {
            out.writeBytes(encodeByteBuffer(in))
            Okay(())
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: JavaBigDecimal) =
        catchingCoderException {
            Okay(jsonNodeFactory.textNode(new String(encodeByteBuffer(in).array, "ISO-8859-1")))
        }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (in == null) FailedG("required but missing", Nil)
        else decodeString(classLoader, in.toString)

    def encodeMongoDB(classLoader: ClassLoader, in: JavaBigDecimal) =
        encodeString(classLoader, in)

    override def toString = "JavaBigDecimalCoder"
}

/** Map a Scala BigDecimal to a JString */
object ScalaBigDecimalCoder extends StringSafeCoder[scala.math.BigDecimal] {
    val mostSpecificClass = classOf[scala.math.BigDecimal]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JDouble(d)     => decodeString(classLoader, d.toString)
            case JInt(bi)       => decodeString(classLoader, bi.toString)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: scala.math.BigDecimal) =
        tryCatch.value(JString(in.toString)) | parameter(Nil)

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(scala.math.BigDecimal(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: scala.math.BigDecimal) =
        Okay(in.toString)

    def avroSchema = JavaBigDecimalCoder.avroSchema

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        JavaBigDecimalCoder.decodeAvro(classLoader, in).map(new scala.math.BigDecimal(_))

    def encodeAvro(classLoader: ClassLoader, in: scala.math.BigDecimal, out: Encoder) =
        JavaBigDecimalCoder.encodeAvro(classLoader, in.bigDecimal, out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: scala.math.BigDecimal) =
        JavaBigDecimalCoder.encodeAvroDefaultJson(classLoader, in.bigDecimal)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (in == null) FailedG("required but missing", Nil) else decodeString(classLoader, in.toString)

    def encodeMongoDB(classLoader: ClassLoader, in: BigDecimal) =
        encodeString(classLoader, in)

    override def toString = "ScalaBigDecimalCoder"
}

/** Map a BigInt to a JInt */
object BigIntCoder extends StringSafeCoder[BigInt] {
    val mostSpecificClass = classOf[BigInt]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JInt(bi)       => Okay(bi)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: BigInt) =
        Okay(JInt(in))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(BigInt(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: BigInt) =
        tryCatch.value(in.toString) | parameter(Nil)

    lazy val avroSchema = (Schema.create(Schema.Type.BYTES), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        BigIntegerCoder.decodeAvro(classLoader, in).map(new BigInt(_))

    def encodeAvro(classLoader: ClassLoader, in: BigInt, out: Encoder) =
        BigIntegerCoder.encodeAvro(classLoader, in.bigInteger, out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: BigInt) =
        BigIntegerCoder.encodeAvroDefaultJson(classLoader, in.bigInteger)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (in == null) FailedG("required but missing", Nil) else decodeString(classLoader, in.toString)

    def encodeMongoDB(classLoader: ClassLoader, in: BigInt) =
        encodeString(classLoader, in)

    override def toString = "BigIntCoder"
}

/** Map a BigInteger to a JInt */
object BigIntegerCoder extends StringSafeCoder[JavaBigInteger] {
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[JavaBigInteger]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JInt(bi)       => Okay(bi.bigInteger)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: JavaBigInteger) =
        Okay(JInt(new BigInt(in)))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(new JavaBigInteger(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: JavaBigInteger) =
        tryCatch.value(in.toString) | parameter(Nil)

    lazy val avroSchema = (Schema.create(Schema.Type.BYTES), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            val byteBuffer = in.readBytes(null)
            if (byteBuffer.remaining < 1)
                FailedG("insufficient number of bytes to represent a BigInteger - got " +
                        byteBuffer.remaining + " but need at least 1", Nil)
            else {
                val bytes = Array.ofDim[Byte](byteBuffer.remaining)
                byteBuffer.get(bytes)
                Okay(new JavaBigInteger(bytes))
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: JavaBigInteger, out: Encoder) =
        catchingCoderException {
            out.writeBytes(ByteBuffer.wrap(in.toByteArray))
            Okay(())
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: JavaBigInteger) =
        catchingCoderException {
            Okay(jsonNodeFactory.textNode(new String(in.toByteArray, "ISO-8859-1")))
        }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (in == null) FailedG("required but missing", Nil) else decodeString(classLoader, in.toString)

    def encodeMongoDB(classLoader: ClassLoader, in: JavaBigInteger) =
        encodeString(classLoader, in)

    override def toString = "BigIntegerCoder"
}

/** Map a Boolean to a JBool */
object BooleanCoder extends StringSafeCoder[Boolean] {
    val mostSpecificClass = classOf[java.lang.Boolean].asInstanceOf[Class[Boolean]]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JBool(b)       => Okay(b)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a boolean", Nil)
        }

    def encode(classLoader: ClassLoader, in: Boolean) =
        Okay(JBool(in))

    def decodeString(classLoader: ClassLoader, in: String) =
        in.toLowerCase match {
            case "true"  => Okay(true)
            case "false" => Okay(false)
            case _       => FailedG("not \"true\" or \"false\"", Nil)
        }

    def encodeString(classLoader: ClassLoader, in: Boolean) =
        tryCatch.value(in.toString) | parameter(Nil)

    lazy val avroSchema = (Schema.create(Schema.Type.BOOLEAN), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readBoolean()) | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Boolean, out: Encoder) =
        tryCatch.value(out.writeBoolean(in)) | parameter(Nil)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Boolean) =
        Okay(jsonNodeFactory.booleanNode(in))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String            => decodeString(classLoader, s)
            case b: java.lang.Boolean => Okay(b.booleanValue)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not a boolean", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Boolean) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "BooleanCoder"
}

/** Map a Byte to a JInt */
object ByteCoder extends StringSafeCoder[Byte] {
    val mostSpecificClass = classOf[java.lang.Byte].asInstanceOf[Class[Byte]]

    private val upperBound = BigInt(java.lang.Byte.MAX_VALUE.toString)
    private val lowerBound = BigInt(java.lang.Byte.MIN_VALUE.toString)

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)                  => decodeString(classLoader, s)
            case JInt(bi) if bi > upperBound => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case JInt(bi) if bi < lowerBound => FailedG("number out of bounds (minimum is " + upperBound + ")", Nil)
            case JInt(bi)                    => Okay(bi.byteValue)
            case JNothing|JNull              => FailedG("required but missing", Nil)
            case _                           => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: Byte) =
        Okay(JInt(BigInt(in)))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Byte.parseByte(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Byte) =
        tryCatch.value(in.toString) | parameter(Nil)

    lazy val avroSchema = (Schema.createFixed("byte", "", "", 1), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](1)
            in.readFixed(bytes, 0, 1)
            bytes(0)
        } | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Byte, out: Encoder) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](1)
            bytes(0) = in
            out.writeFixed(bytes, 0, 1)
        } | parameter(Nil)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Byte) =
        Okay(jsonNodeFactory.textNode(new String(Array(in), "ISO-8859-1")))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case i: java.lang.Integer if i.intValue  > java.lang.Byte.MAX_VALUE => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case i: java.lang.Integer if i.intValue  < java.lang.Byte.MIN_VALUE => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)
            case l: java.lang.Long    if l.longValue > java.lang.Byte.MAX_VALUE => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case l: java.lang.Long    if l.longValue < java.lang.Byte.MIN_VALUE => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)

            case s: String            => decodeString(classLoader, s)
            case i: java.lang.Integer => Okay(i.byteValue)
            case l: java.lang.Long    => Okay(l.byteValue)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not an integer", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Byte) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "ByteCoder"
}

/** Map a Char to a JString */
object CharCoder extends StringSafeCoder[Char] {
    val mostSpecificClass = classOf[java.lang.Character].asInstanceOf[Class[Char]]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: Char) =
        tryCatch.value(JString(in.toString)) | parameter(Nil)

    def decodeString(classLoader: ClassLoader, in: String) =
        if (in.length == 1) {
            Okay(in.charAt(0))
        } else {
            FailedG("expected a string with exactly one character in it", Nil)
        }

    def encodeString(classLoader: ClassLoader, in: Char) =
        tryCatch.value(in.toString) | parameter(Nil)

    lazy val avroSchema = (Schema.createFixed("char", "", "", 2), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](2)
            in.readFixed(bytes, 0, 2)
            ((bytes(0) << 8) | bytes(1)).asInstanceOf[Char]
        } | parameter(Nil)

    private def encodeBytes(in: Char) = {
        val bytes = Array.ofDim[Byte](2)
        bytes(0) = ((in >>> 8) & 0xff).asInstanceOf[Byte]
        bytes(1) = ((in >>> 0) & 0xff).asInstanceOf[Byte]
        bytes
    }

    def encodeAvro(classLoader: ClassLoader, in: Char, out: Encoder) =
        tryCatch.value {
            out.writeFixed(encodeBytes(in), 0, 2)
        } | parameter(Nil)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Char) =
        Okay(jsonNodeFactory.textNode(new String(encodeBytes(in), "ISO-8859-1")))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String => decodeString(classLoader, s)
            case null      => FailedG("required but missing", Nil)
            case _         => FailedG("not a string", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Char) =
        encodeString(classLoader, in)

    override def toString = "CharCoder"
}

/* Map a Double to a JDouble */
object DoubleCoder extends StringSafeCoder[Double] {
    val mostSpecificClass = classOf[java.lang.Double].asInstanceOf[Class[Double]]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JDouble(d)     => Okay(d)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a double", Nil)
        }

    def encode(classLoader: ClassLoader, in: Double) =
        Okay(JDouble(in))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Double.parseDouble(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Double) =
        tryCatch.value(in.toString) | parameter(Nil)

    lazy val avroSchema = (Schema.create(Schema.Type.DOUBLE), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readDouble()) | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Double, out: Encoder) =
        tryCatch.value(out.writeDouble(in)) | parameter(Nil)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Double) =
        Okay(jsonNodeFactory.numberNode(in))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String           => decodeString(classLoader, s)
            case d: java.lang.Double => Okay(d)
            case null                => FailedG("required but missing", Nil)
            case _                   => FailedG("not a double", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Double) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "DoubleCoder"
}

/* Map a Float to a JDouble */
object FloatCoder extends StringSafeCoder[Float] {
    val mostSpecificClass = classOf[java.lang.Float].asInstanceOf[Class[Float]]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JDouble(d)     => Okay(d.floatValue)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a double", Nil)
        }

    def encode(classLoader: ClassLoader, in: Float) =
        Okay(JDouble(in))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Float.parseFloat(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Float) =
        tryCatch.value(in.toString) | parameter(Nil)

    lazy val avroSchema = (Schema.create(Schema.Type.FLOAT), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readFloat()) | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Float, out: Encoder) =
        tryCatch.value(out.writeFloat(in)) | parameter(Nil)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Float) =
        Okay(jsonNodeFactory.numberNode(in))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String           => decodeString(classLoader, s)
            case d: java.lang.Double => Okay(d.floatValue)
            case null                => FailedG("required but missing", Nil)
            case _                   => FailedG("not a double", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Float) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "FloatCoder"
}

/** Map an Integer to a JInt */
object IntCoder extends StringSafeCoder[Int] {
    val mostSpecificClass = classOf[java.lang.Integer].asInstanceOf[Class[Int]]

    private val upperBound = BigInt(java.lang.Integer.MAX_VALUE.toString)
    private val lowerBound = BigInt(java.lang.Integer.MIN_VALUE.toString)

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)                  => decodeString(classLoader, s)
            case JInt(bi) if bi > upperBound => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case JInt(bi) if bi < lowerBound => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)
            case JInt(bi)                    => Okay(bi.intValue)
            case JNothing|JNull              => FailedG("required but missing", Nil)
            case _                           => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: Int) =
        Okay(JInt(BigInt(in)))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Integer.parseInt(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Int) =
        tryCatch.value(in.toString) | parameter(Nil)

    lazy val avroSchema = (Schema.create(Schema.Type.INT), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readInt()) | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Int, out: Encoder) =
        tryCatch.value(out.writeInt(in)) | parameter(Nil)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Int) =
        Okay(jsonNodeFactory.numberNode(in))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case l: java.lang.Long if l.longValue > java.lang.Integer.MAX_VALUE => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case l: java.lang.Long if l.longValue < java.lang.Integer.MIN_VALUE => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)

            case s: String            => decodeString(classLoader, s)
            case i: java.lang.Integer => Okay(i)
            case l: java.lang.Long    => Okay(l.intValue)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not an integer", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: Int) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "IntCoder"
}

/** Map a Long to a JInt */
object LongCoder extends StringSafeCoder[Long] {
    val mostSpecificClass = classOf[java.lang.Long].asInstanceOf[Class[Long]]

    private val upperBound = BigInt(java.lang.Long.MAX_VALUE.toString)
    private val lowerBound = BigInt(java.lang.Long.MIN_VALUE.toString)

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)                  => decodeString(classLoader, s)
            case JInt(bi) if bi > upperBound => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case JInt(bi) if bi < lowerBound => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)
            case JInt(bi)                    => Okay(bi.longValue)
            case JNothing|JNull              => FailedG("required but missing", Nil)
            case _                           => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: Long) =
        Okay(JInt(BigInt(in)))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Long.parseLong(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Long) =
        tryCatch.value(in.toString) | parameter(Nil)

    lazy val avroSchema = (Schema.create(Schema.Type.LONG), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readLong()) | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Long, out: Encoder) =
        tryCatch.value(out.writeLong(in)) | parameter(Nil)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Long) =
        Okay(jsonNodeFactory.numberNode(in))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String            => decodeString(classLoader, s)
            case i: java.lang.Integer => Okay(i.longValue)
            case l: java.lang.Long    => Okay(l)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not an integer", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: Long) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "LongCoder"
}

/** Map a Short to a JInt */
object ShortCoder extends StringSafeCoder[Short] {
    val mostSpecificClass = classOf[java.lang.Short].asInstanceOf[Class[Short]]

    private val upperBound = BigInt(java.lang.Short.MAX_VALUE.toString)
    private val lowerBound = BigInt(java.lang.Short.MIN_VALUE.toString)

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)                  => decodeString(classLoader, s)
            case JInt(bi) if bi > upperBound => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case JInt(bi) if bi < lowerBound => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)
            case JInt(bi)                    => Okay(bi.shortValue)
            case JNothing|JNull              => FailedG("required but missing", Nil)
            case _                           => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: Short) =
        Okay(JInt(BigInt(in)))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Short.parseShort(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Short) =
        tryCatch.value(in.toString) | parameter(Nil)

    lazy val avroSchema = (Schema.createFixed("short", "", "", 2), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](2)
            in.readFixed(bytes, 0, 2)
            (
                ((bytes(0) << 8) & 0xff00) |
                ( bytes(1)       & 0x00ff)
            ).asInstanceOf[Short]
        } | parameter(Nil)

    private def encodeBytes(in: Short) = {
        val bytes = Array.ofDim[Byte](2)
        bytes(0) = ((in >>> 8) & 0xff).asInstanceOf[Byte]
        bytes(1) = ((in >>> 0) & 0xff).asInstanceOf[Byte]
        bytes
    }

    def encodeAvro(classLoader: ClassLoader, in: Short, out: Encoder) =
        tryCatch.value {
            out.writeFixed(encodeBytes(in), 0, 2)
        } | parameter(Nil)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Short) =
        Okay(jsonNodeFactory.textNode(new String(encodeBytes(in), "ISO-8859-1")))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case i: java.lang.Integer if i > java.lang.Short.MAX_VALUE => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case i: java.lang.Integer if i < java.lang.Short.MIN_VALUE => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)
            case l: java.lang.Long if l > java.lang.Short.MAX_VALUE    => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case l: java.lang.Long if l < java.lang.Short.MIN_VALUE    => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)

            case s: String            => decodeString(classLoader, s)
            case i: java.lang.Integer => Okay(i.shortValue)
            case l: java.lang.Long    => Okay(l.shortValue)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not an integer", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Short) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "ShortCoder"
}


/** Map something that encodes as a string to a JString */
abstract class StringLikeCoder[T] extends StringSafeCoder[T] {
    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T) =
        encodeString(classLoader, in).map(s => JString(s))

    lazy val avroSchema = (Schema.create(Schema.Type.STRING), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readString(null).toString()).orElse(parameter(Nil)).flatMap(decodeString(classLoader, _))

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        encodeString(classLoader, in).flatMap { s =>
            if (s == null) FailedG("cannot encode null string", Nil)
            else tryCatch.value(out.writeString(s)) | parameter(Nil)
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        encodeString(classLoader, in).map(jsonNodeFactory.textNode)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String => decodeString(classLoader, s)
            case null      => FailedG("required but missing", Nil)
            case _         => FailedG("not a string", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        encodeString(classLoader, in)

    override def toString = "StringCoder"
}

object StringCoder extends StringLikeCoder[String] {
    val mostSpecificClass = classOf[String]

    def decodeString(classLoader: ClassLoader, s: String) = Okay(s)
    def encodeString(classLoader: ClassLoader, s: String) = Okay(s)
}
