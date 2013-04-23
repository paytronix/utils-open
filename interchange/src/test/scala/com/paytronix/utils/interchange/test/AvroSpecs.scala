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
package test
package avro

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.util.Arrays
import scala.collection.JavaConverters.{asScalaBufferConverter, seqAsJavaListConverter}
import scala.collection.{immutable, mutable}

import net.liftweb.json.JsonAST.{JArray, JField, JObject, JString, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.EncoderFactory
import org.specs2.{SpecificationFeatures, SpecificationWithJUnit}
import org.specs2.matcher.MatchResult

import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, ResultG}
import com.paytronix.utils.interchange.test.fixtures.{
    AutoUnionFirst, AutoUnionSecond, BasicClass, CaseClass, Coders, ExplicitUnionFirst, ExplicitUnionSecond, JavaEnum, ScalaEnum
}

import AvroUtils.{schemaFromJValue, schemaToJValue}

final case class StartMarker(d: Double)
object StartMarker {
    def instance = StartMarker(math.random)
}
final case class EndMarker(d: Double)
object EndMarker {
    def instance = EndMarker(math.random)
}

final case class Wrapper[T] (
    start: StartMarker,
    content: T,
    end: EndMarker
)

object Helper extends SpecificationFeatures {
    implicit val builder = new com.paytronix.utils.extendedreflection.Builder(getClass.getClassLoader)

    val clazz = classOf[Wrapper[_]]
    def wrapperCoderFor[T](contentCoder: ComposableCoder[T]): ObjectCoder[Wrapper[T]] =
        ObjectCoder (
            clazz.asInstanceOf[Class[Wrapper[T]]],
            clazz.getConstructor(classOf[StartMarker], classOf[AnyRef], classOf[EndMarker]).asInstanceOf[java.lang.reflect.Constructor[Wrapper[T]]],
            List("start", "content", "end"),
            List (
                FieldCoding("start", Coding.forClassComposable[StartMarker].orThrow, clazz.getMethod("start"), Failed("")),
                FieldCoding("content", contentCoder, clazz.getMethod("content"), Failed("")),
                FieldCoding("end", Coding.forClassComposable[EndMarker].orThrow, clazz.getMethod("end"), Failed(""))
            ),
            flatten=false
        )

    def testDefaulting[T](contentCoder: ComposableCoder[T], default: T, validate: Option[ResultG[Any, T] => MatchResult[Any]] = None): MatchResult[Any] = {
        val coder = wrapperCoderFor(DefaultingCoder(contentCoder, default))
        val readerSchema = coder.avroSchema._1
        val readerSchemaJV = schemaToJValue(readerSchema).orThrow
        // haaaaxxxxx
        val writerSchemaJV =
            readerSchemaJV.replace(List("fields"), JArray((readerSchemaJV \ "fields").asInstanceOf[JArray].arr.filterNot(_ \ "name" == JString("content"))))
        val writerSchema = schemaFromJValue(writerSchemaJV).orThrow

        val startMark = StartMarker.instance
        val endMark = EndMarker.instance

        val baos = new ByteArrayOutputStream()
        val encoder = EncoderFactory.get.directBinaryEncoder(baos, null)
        coder.fieldCodings(0).coder.forceEncodeAvro(getClass.getClassLoader, startMark, encoder).orThrow
        coder.fieldCodings(2).coder.forceEncodeAvro(getClass.getClassLoader, endMark, encoder).orThrow
        encoder.flush()

        val bytes = baos.toByteArray

        val decoded = Coder(getClass.getClassLoader, coder).decodeAvro(writerSchema, bytes)
        (decoded.map(wrapper => (wrapper.start must_== startMark) and (wrapper.end must_== endMark)) getOrElse ok) and (
            validate.map(_(decoded.map(_.content))) getOrElse (decoded.orThrow.content must_== default)
        )
    }

    def testDefaultingNotUsable[T](contentCoder: ComposableCoder[T], default: T): MatchResult[Any] = {
        val coder = wrapperCoderFor(DefaultingCoder(contentCoder, default))
        coder.avroSchema must throwAn[Exception]
    }
}

import Helper._

class AvroScalarDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting for scalars" ^
    "JValue"           ! { testDefaulting(JValueCoder, JObject(List(JField("foo", JString("bar"))))) } ^
    "Unit"             ! { testDefaulting(UnitCoder, ()) } ^
    "Java BigDecimal"  ! { testDefaulting(JavaBigDecimalCoder, new java.math.BigDecimal("1.23")) } ^
    "Scala BigDecimal" ! { testDefaulting(ScalaBigDecimalCoder, BigDecimal("1.23")) } ^
    "Java BigInteger"  ! { testDefaulting(BigIntegerCoder, new java.math.BigInteger("123456")) } ^
    "Scala BigInt"     ! { testDefaulting(BigIntCoder, BigInt("123456")) } ^
    "Boolean"          ! { testDefaulting(BooleanCoder, true) } ^
    "Byte"             ! { testDefaulting(ByteCoder, 123.asInstanceOf[Byte]) } ^
    "Char"             ! { testDefaulting(CharCoder, 'a') } ^
    "Double"           ! { testDefaulting(DoubleCoder, 12.34) } ^
    "Float"            ! { testDefaulting(FloatCoder, 12.34f) } ^
    "Int"              ! { testDefaulting(IntCoder, 1234) } ^
    "Long"             ! { testDefaulting(LongCoder, 123456789012345L) } ^
    "Short"            ! { testDefaulting(ShortCoder, 12345.asInstanceOf[Short]) } ^
    "String"           ! { testDefaulting(StringCoder, "defaulted") }
}

class AvroArgumentDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting for argument lists" ^
    "Flat argument list"   ! { testDefaulting(FlatArgumentArrayCoder(StringCoder, StringCoder), Array[AnyRef]("foo", "bar")) } ^
    "Normal argument list" ! { testDefaulting(ArgumentArrayCoder(false, List(ArgumentCoding("a", StringCoder), ArgumentCoding("b", StringCoder))), Array[AnyRef]("foo", "bar")) }
}

class AvroBytesDefaultingSpecTest extends SpecificationWithJUnit {
    val bytes = Array[Byte](1, 2, 3, 4, 5, -120, -121, -122, -123, -124, -125)

    def is =
    "Avro defaulting for byte sequences" ^
    "Byte array" ! { testDefaulting(ByteArrayCoder, bytes, Some { result: ResultG[Any, Array[Byte]] =>
        result must beLike { case Okay(otherBytes) => if (Arrays.equals(bytes, otherBytes)) ok else ko }
    } ) } ^
    "Byte buffer" ! { testDefaulting(ByteBufferCoder, ByteBuffer.wrap(bytes), Some { result: ResultG[Any, ByteBuffer] =>
        val bb = result.orThrow
        val otherBytes = Array.ofDim[Byte](bb.remaining)
        bb.get(otherBytes)
        if (Arrays.equals(bytes, otherBytes)) ok else ko
    }) }
}

class AvroCollectionDefaultingSpecTest extends SpecificationWithJUnit {
    val javaMap = {
        val m = new java.util.HashMap[String, Int]
        m.put("foo", 1)
        m.put("bar", 2)
        m
    }

    def is =
    "Avro defaulting for collections" ^
    "Java Lists"           ! { testDefaulting(JavaListCoder(IntCoder), Arrays.asList(1,2,3,4)) } ^
    "Scala Lists"          ! { testDefaulting(ScalaListCoder(IntCoder), List(1,2,3,4)) } ^
    "Scala Seqs"           ! { testDefaulting(ScalaSeqCoder(IntCoder), mutable.ArrayBuffer(1,2,3,4)) } ^
    "Scala immutable Sets" ! { testDefaulting(ScalaImmutableSetCoder(IntCoder), immutable.Set(1,2,3,4)) } ^
    "Scala mutable Sets"   ! { testDefaulting(ScalaMutableSetCoder(IntCoder), mutable.Set(1,2,3,4)) } ^
    "Java Maps"            ! { testDefaulting(JavaMapCoder(StringCoder, IntCoder), javaMap) } ^
    "Scala immutable Maps" ! { testDefaulting(ScalaImmutableMapCoder(StringCoder, IntCoder), immutable.Map("foo" -> 1, "bar" -> 2)) } ^
    "Scala mutable Maps"   ! { testDefaulting(ScalaMutableMapCoder(StringCoder, IntCoder), mutable.Map("foo" -> 1, "bar" -> 2)) }
}

class AvroDateTimeDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting for dates and times" ^
    "java.util.Date"              ! { testDefaulting(JavaDateCoder, new java.util.Date()) } ^
    "java.sql.Date"               ! { testDefaulting(JavaSqlDateCoder, new java.sql.Date(System.currentTimeMillis)) } ^
    "java.sql.Timestamp"          ! { testDefaulting(JavaSqlTimestampCoder, new java.sql.Timestamp(System.currentTimeMillis)) } ^
    "org.joda.time.DateTime"      ! { testDefaulting(DateTimeCoder, org.joda.time.DateTime.now) } ^
    "org.joda.time.LocalDate"     ! { testDefaulting(LocalDateCoder, org.joda.time.LocalDate.now) } ^
    "org.joda.time.LocalDateTime" ! { testDefaulting(LocalDateTimeCoder, org.joda.time.LocalDateTime.now) } ^
    "org.joda.time.LocalTime"     ! { testDefaulting(LocalTimeCoder, org.joda.time.LocalTime.now) } ^
    "org.joda.time.Duration"      ! { testDefaulting(DurationCoder, new org.joda.time.Duration(1234)) }
}

class AvroEnumDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting for enumerations" ^
    "Java enum"  ! { testDefaulting(JavaEnumCoder(classOf[JavaEnum]), JavaEnum.TWO) } ^
    "Scala enum" ! { testDefaulting(ScalaEnumCoder(ScalaEnum), ScalaEnum.TWO) }
}

object Singleton
class AvroSingletonDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting for singletons"  ^ "Singletons" ! { testDefaulting(SingletonCoder(Singleton), Singleton) }
}

class AvroMongoDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting of Mongo types" ^
    "org.bson.types.ObjectId" ! { testDefaulting(ObjectIdCoder, new org.bson.types.ObjectId()) }
}

class AvroObjectDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting of objects" ^
    "BasicClass" ! { testDefaulting(Coders.basicClassCoder, BasicClass.make(1234, null, None)) and
                     testDefaultingNotUsable(Coders.basicClassCoder, BasicClass.make(1234, null, Some("barbaz"))) and
                     testDefaultingNotUsable(Coders.basicClassCoder, BasicClass.make(1234, "foobar", None)) }
}

class AvroResultDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting of Result" ^
    "Okay (not usable)" ! { testDefaultingNotUsable(ResultCoder(UnitCoder, IntCoder), Okay(123)) } ^
    "Failed" ! { testDefaulting(ResultCoder(UnitCoder, IntCoder), Failed("unknown failure")) }
}

class AvroTupleDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting of Tuples" ^
    "(Int)" ! { testDefaulting(Tuple1Coder(IntCoder), new Tuple1(123)) } ^
    "(Int, String)" ! { testDefaulting(Tuple2Coder(IntCoder, StringCoder), (123, "foobar")) } ^
    "(Int, String, Double)" ! { testDefaulting(Tuple3Coder(IntCoder, StringCoder, DoubleCoder), (123, "foobar", 12.34)) }
}

class AvroUnionDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting of unions" ^
    "Left(123): Either[Int, String]"                   ! { testDefaulting(EitherCoder(IntCoder, StringCoder), Left(123)) } ^
    "Right(\"foo\"): Either[Int, String] (not usable)" ! { testDefaultingNotUsable(EitherCoder(IntCoder, StringCoder), Right("foo")) } ^
    "AutoUnionFirst"                                   ! { testDefaulting(Coders.autoUnionCoder, AutoUnionFirst(1234)) } ^
    "AutoUnionSecond (not usable)"                     ! { testDefaultingNotUsable(Coders.autoUnionCoder, AutoUnionSecond("foo")) } ^
    "ExplicitUnionFirst"                               ! { testDefaulting(Coders.explicitUnionCoder, ExplicitUnionFirst(1234)) } ^
    "ExplicitUnionSecond"                              ! { testDefaultingNotUsable(Coders.explicitUnionCoder, ExplicitUnionSecond("foo")) }
}

class AvroWrapperDefaultingSpecTest extends SpecificationWithJUnit { def is =
    "Avro defaulting of wrappers" ^
    "Mapped"               ! { testDefaulting(IntCoder.transform(i => Okay(CaseClass(i, null, None)))(cc => Okay(cc.foo)), CaseClass(1234, null, None)) } ^
    "null"                 ! { testDefaulting(NullCoder(StringCoder), null) } ^
    "\"foo\" (not usable)" ! { testDefaultingNotUsable(NullCoder(StringCoder), "foo") } ^
    "None"                 ! { testDefaulting(OptionCoder(StringCoder), None) } ^
    "Some(\"foo\")"        ! { testDefaultingNotUsable(OptionCoder(StringCoder), Some("foo")) }
}



