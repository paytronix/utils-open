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

package com.paytronix.utils.interchange.format.json

import java.nio.ByteBuffer

import com.fasterxml.jackson.core.JsonFactory
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}
import org.specs2.execute.{Result => SpecsResult}
import org.specs2.matcher.{Matcher, MatchResult}

import com.paytronix.utils.interchange.base.{CoderResult, Receiver, formatFailedPath}
import com.paytronix.utils.interchange.test.fixtures.{JavaEnum, ScalaEnum}
import com.paytronix.utils.scala.result.{FailedG, Okay, Result}

import Arbitrary.arbitrary

object arbitraries {
    val nonnumericStr = Gen.frequency (
        (1, ""),
        (5, Gen.alphaStr),
        (5, Arbitrary.arbString.arbitrary.filter(s => !s.forall(Character.isDigit)))
    )

    val safeJavaBigDecimals = arbitrary[BigDecimal].map(_.bigDecimal).filter { bd =>
        try { new java.math.BigDecimal(bd.toString); true }
        catch { case nfe: NumberFormatException => false }
    }

    val safeScalaBigDecimals = arbitrary[BigDecimal].filter { bd =>
        try { new java.math.BigDecimal(bd.bigDecimal.toString); true }
        catch { case nfe: NumberFormatException => false }
    }

    implicit val arbJavaMathBigDecimals = Arbitrary(arbitrary[BigDecimal].map(_.bigDecimal))

    implicit val arbJavaMathBigInteger = Arbitrary(arbitrary[BigInt].map(_.bigInteger))
}

import arbitraries._

trait JsonMatchers { self: SpecificationWithJUnit =>
    def decodeMissing[A](decoder: JsonDecoder[A]): CoderResult[A] = {
        val jp = new JsonFactory().createParser("")
        val ijp = new InterchangeJsonParser(jp)
        ijp.nextValueIsMissing()
        val rec = new Receiver[A]
        decoder.run(ijp, rec) map { _ => rec.value }
    }

    def checkMissing[A](decoder: JsonDecoder[A]): SpecsResult =
        (decoder.fromString("null") must beMissingValue[A]).updateMessage("explicit null: " + _) and
        (formatFailedPath(decodeMissing(decoder)) must beMissingValue[A]).updateMessage("missing value: " + _)

    def beMissingValue[A]: Matcher[Result[A]] =
        beLike { case f@FailedG(_, _) =>
            f.message must beMatching("At source location \\d+:\\d+: required but missing")
        }
}

class unitJsonCoderTest extends SpecificationWithJUnit {
    def is = s2"""
        `unitJsonCoder`
            should write nothing $encode
            should read nothing $decode
    """

    def encode = (scalar.unitJsonCoder.encode.toString(()) ==== Okay("")) // FIXME assert that no field is emitted in an object context
    def decode = (scalar.unitJsonCoder.decode.fromString("") ==== Okay(()))
}

class booleanJsonCoderTest extends SpecificationWithJUnit with JsonMatchers {
    def is = s2"""
        `booleanJsonCoder`
            should write true for true and false for false $encode
            should decode true to true and false to false $decode
            should decode "true" to true and "false" to false $decodeString
            should fail to decode other strings $decodeInvalidString
            should fail to decode a missing value $decodeMissing
    """

    def encode =
        (scalar.booleanJsonCoder.encode.toString(true)  ==== Okay("true")) and
        (scalar.booleanJsonCoder.encode.toString(false) ==== Okay("false"))
    def decode =
        (scalar.booleanJsonCoder.decode.fromString("true")  ==== Okay(true)) and
        (scalar.booleanJsonCoder.decode.fromString("false") ==== Okay(false))
    def decodeString =
        (scalar.booleanJsonCoder.decode.fromString("\"true\"")  ==== Okay(true)) and
        (scalar.booleanJsonCoder.decode.fromString("\"false\"") ==== Okay(false))
    def decodeInvalidString =
        (scalar.booleanJsonCoder.decode.fromString("\"\"")  must beLike { case FailedG(_, _) => ok }) and
        (scalar.booleanJsonCoder.decode.fromString("\"0\"") must beLike { case FailedG(_, _) => ok })
    def decodeMissing = checkMissing(scalar.booleanJsonCoder.decode)
}

class byteJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `byteJsonCoder`
            should encode to numbers $encode
            should decode from in-range numbers $decode
            should fail to decode from numbers with a decimal component $decodeInvalidDecimal
            should fail to decode from out-of-range numbers $decodeInvalidOutOfRange
            should decode from in-range strings $decodeString
            should fail to decode a missing value $decodeMissing
    """

    def encode = prop { (b: Byte) => scalar.byteJsonCoder.encode.toString(b) ==== Okay(b.toString) }
    def decode = prop { (b: Byte) => scalar.byteJsonCoder.decode.fromString(b.toString) ==== Okay(b) }
    def decodeInvalidDecimal = prop { (f: Float) => scalar.byteJsonCoder.decode.fromString(f.toString) must beLike { case FailedG(_, _) => ok } }
    def decodeInvalidOutOfRange = Prop.forAll(arbitrary[Int].filter(i => i < Byte.MinValue | i > Byte.MaxValue)) { i =>
        scalar.byteJsonCoder.decode.fromString(i.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeString = prop { (b: Byte) => scalar.byteJsonCoder.decode.fromString(s""" "${b.toString}" """) ==== Okay(b) }
    def decodeMissing = checkMissing(scalar.booleanJsonCoder.decode)
}

class shortJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `shortJsonCoder`
            should encode to numbers $encode
            should decode from in-range numbers $decode
            should fail to decode from numbers with a decimal component $decodeInvalidDecimal
            should fail to decode from out-of-range numbers $decodeInvalidOutOfRange
            should decode from in-range strings $decodeString
            should fail to decode a missing value $decodeMissing
    """

    def encode = prop { (s: Short) => scalar.shortJsonCoder.encode.toString(s) ==== Okay(s.toString) }
    def decode = prop { (s: Short) => scalar.shortJsonCoder.decode.fromString(s.toString) ==== Okay(s) }
    def decodeInvalidDecimal = prop { (f: Float) => scalar.shortJsonCoder.decode.fromString(f.toString) must beLike { case FailedG(_, _) => ok } }
    def decodeInvalidOutOfRange = Prop.forAll(arbitrary[Int].filter(i => i < Short.MinValue | i > Short.MaxValue)) { i =>
        scalar.shortJsonCoder.decode.fromString(i.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeString = prop { (s: Short) => scalar.shortJsonCoder.decode.fromString(s""" "${s.toString}" """) ==== Okay(s) }
    def decodeMissing = checkMissing(scalar.shortJsonCoder.decode)
}

class intJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `intJsonCoder`
            should encode to numbers $encode
            should decode from in-range numbers $decode
            should fail to decode from numbers with a decimal component $decodeInvalidDecimal
            should fail to decode from out-of-range numbers $decodeInvalidOutOfRange
            should decode from in-range strings $decodeString
            should fail to decode a missing value $decodeMissing
    """

    def encode = prop { (i: Int) => scalar.intJsonCoder.encode.toString(i) ==== Okay(i.toString) }
    def decode = prop { (i: Int) => scalar.intJsonCoder.decode.fromString(i.toString) ==== Okay(i) }
    def decodeInvalidDecimal = prop { (f: Float) => scalar.intJsonCoder.decode.fromString(f.toString) must beLike { case FailedG(_, _) => ok } }
    def decodeInvalidOutOfRange = Prop.forAll(arbitrary[Long].filter(l => l < Int.MinValue | l > Int.MaxValue)) { l =>
        scalar.intJsonCoder.decode.fromString(l.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeString = prop { (i: Int) => scalar.intJsonCoder.decode.fromString(s""" "${i.toString}" """) ==== Okay(i) }
    def decodeMissing = checkMissing(scalar.intJsonCoder.decode)
}

class longJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `longJsonCoder`
            should encode to numbers $encode
            should decode from in-range numbers $decode
            should fail to decode from numbers with a decimal component $decodeInvalidDecimal
            should fail to decode from out-of-range numbers $decodeInvalidOutOfRange
            should decode from in-range strings $decodeString
            should fail to decode a missing value $decodeMissing
    """

    def encode = prop { (l: Long) => scalar.longJsonCoder.encode.toString(l) ==== Okay(l.toString) }
    def decode = prop { (l: Long) => scalar.longJsonCoder.decode.fromString(l.toString) ==== Okay(l) }
    def decodeInvalidDecimal = prop { (f: Float) => scalar.longJsonCoder.decode.fromString(f.toString) must beLike { case FailedG(_, _) => ok } }
    def decodeInvalidOutOfRange = Prop.forAll(arbitrary[BigInt].filter(i => i < Long.MinValue | i > Long.MaxValue)) { bi =>
        scalar.longJsonCoder.decode.fromString(bi.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeString = prop { (l: Long) => scalar.longJsonCoder.decode.fromString(s""" "${l.toString}" """) ==== Okay(l) }
    def decodeMissing = checkMissing(scalar.longJsonCoder.decode)
}

class floatJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `floatJsonCoder`
            should encode to numbers $encode
            should decode from numbers without a decimal $decodeIntegral
            should decode from numbers with a decimal $decodeReal
            should decode from strings without a decimal $decodeStringIntegral
            should decode from strings with a decimal $decodeStringReal
            should fail to decode a missing value $decodeMissing
    """

    def encode = prop { (f: Float) => scalar.floatJsonCoder.encode.toString(f) ==== Okay(f.toString) }
    def decodeIntegral = prop { (i: Int) => scalar.floatJsonCoder.decode.fromString(i.toString) ==== Okay(i: Float) }
    def decodeReal = prop { (f: Float) => scalar.floatJsonCoder.decode.fromString(f.toString) ==== Okay(f) }
    def decodeStringIntegral = prop { (i: Int) => scalar.floatJsonCoder.decode.fromString(s""" "${i.toString}" """) ==== Okay(i: Float) }
    def decodeStringReal = prop { (f: Float) => scalar.floatJsonCoder.decode.fromString(s""" "${f.toString}" """) ==== Okay(f) }
    def decodeMissing = checkMissing(scalar.floatJsonCoder.decode)
}

class doubleJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `doubleJsonCoder`
            should encode to numbers $encode
            should decode from numbers without a decimal $decodeIntegral
            should decode from numbers with a decimal $decodeReal
            should decode from strings without a decimal $decodeStringIntegral
            should decode from strings with a decimal $decodeStringReal
            should fail to decode a missing value $decodeMissing
    """

    def encode = prop { (d: Double) => scalar.doubleJsonCoder.encode.toString(d) ==== Okay(d.toString) }
    def decodeIntegral = prop { (i: Int) => scalar.doubleJsonCoder.decode.fromString(i.toString) ==== Okay(i: Double) }
    def decodeReal = prop { (d: Double) => scalar.doubleJsonCoder.decode.fromString(d.toString) ==== Okay(d) }
    def decodeStringIntegral = prop { (i: Int) => scalar.doubleJsonCoder.decode.fromString(s""" "${i.toString}" """) ==== Okay(i: Double) }
    def decodeStringReal = prop { (d: Double) => scalar.doubleJsonCoder.decode.fromString(s""" "${d.toString}" """) ==== Okay(d) }
    def decodeMissing = checkMissing(scalar.doubleJsonCoder.decode)
}

class javaBigIntegerJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `javaBigIntegerJsonCoder`
            should encode to strings $encode
            should decode from strings $decode
            should decode from numbers without a decimal $decodeIntegral
            should not decode from numbers with a decimal $decodeReal
            should fail to decode a missing value $decodeMissing
    """

    def encode = prop { (bi: java.math.BigInteger) => scalar.javaBigIntegerJsonCoder.encode.toString(bi) ==== Okay("\"" + bi.toString + "\"") }
    def decode = prop { (bi: java.math.BigInteger) => scalar.javaBigIntegerJsonCoder.decode.fromString(s""" "${bi.toString}" """) ==== Okay(bi) }
    def decodeIntegral = prop { (bi: java.math.BigInteger) => scalar.javaBigIntegerJsonCoder.decode.fromString(bi.toString) ==== Okay(bi) }
    def decodeReal = prop { (d: Double) =>
        scalar.javaBigIntegerJsonCoder.decode.fromString(d.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeMissing = checkMissing(scalar.javaBigIntegerJsonCoder.decode)
}

class scalaBigIntJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `scalaBigIntJsonCoder`
            should encode to strings $encode
            should decode from strings $decode
            should decode from numbers without a decimal $decodeIntegral
            should not decode from numbers with a decimal $decodeReal
            should fail to decode a missing value $decodeMissing
    """

    def encode = prop { (bi: BigInt) => scalar.scalaBigIntJsonCoder.encode.toString(bi) ==== Okay("\"" + bi.toString + "\"") }
    def decode = prop { (bi: BigInt) => scalar.scalaBigIntJsonCoder.decode.fromString(s""" "${bi.toString}" """) ==== Okay(bi) }
    def decodeIntegral = prop { (bi: BigInt) => scalar.scalaBigIntJsonCoder.decode.fromString(bi.toString) ==== Okay(bi) }
    def decodeReal = prop { (d: Double) =>
        scalar.scalaBigIntJsonCoder.decode.fromString(d.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeMissing = checkMissing(scalar.scalaBigIntJsonCoder.decode)
}

class javaBigDecimalJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `javaBigDecimalJsonCoder`
            should encode to strings $encode
            should decode from strings $decode
            should decode from numbers without a decimal $decodeIntegral
            should decode from numbers with a decimal $decodeReal
            should fail to decode a missing value $decodeMissing
    """

    def encode = Prop.forAll(safeJavaBigDecimals) { bd => scalar.javaBigDecimalJsonCoder.encode.toString(bd) ==== Okay("\"" + bd.toString + "\"") }
    def decode = Prop.forAll(safeJavaBigDecimals) { bd => scalar.javaBigDecimalJsonCoder.decode.fromString(s""" "${bd.toString}" """) ==== Okay(bd) }
    def decodeIntegral = prop { (bi: java.math.BigInteger) =>
        val bd = new java.math.BigDecimal(bi)
        scalar.javaBigDecimalJsonCoder.decode.fromString(bi.toString) ==== Okay(bd)
    }
    def decodeReal = prop { (d: Double) =>
        scalar.javaBigDecimalJsonCoder.decode.fromString(d.toString) ==== Okay(new java.math.BigDecimal(d.toString))
    }
    def decodeMissing = checkMissing(scalar.javaBigDecimalJsonCoder.decode)
}

class scalaBigDecimalJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `scalaBigDecimalJsonCoder`
            should encode to strings $encode
            should decode from strings $decode
            should decode from numbers without a decimal $decodeIntegral
            should decode from numbers with a decimal $decodeReal
            should fail to decode a missing value $decodeMissing
    """

    def encode = Prop.forAll(safeScalaBigDecimals) { bd => scalar.scalaBigDecimalJsonCoder.encode.toString(bd) ==== Okay("\"" + bd.toString + "\"") }
    def decode = Prop.forAll(safeScalaBigDecimals) { bd => scalar.scalaBigDecimalJsonCoder.decode.fromString(s""" "${bd.toString}" """) ==== Okay(bd) }
    def decodeIntegral = prop { (bi: java.math.BigInteger) =>
        val bd = BigDecimal(BigInt(bi))
        scalar.scalaBigDecimalJsonCoder.decode.fromString(bi.toString) ==== Okay(bd)
    }
    def decodeReal = prop { (d: Double) =>
        scalar.scalaBigDecimalJsonCoder.decode.fromString(d.toString) ==== Okay(BigDecimal(d.toString))
    }
    def decodeMissing = checkMissing(scalar.scalaBigDecimalJsonCoder.decode)
}

class charJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `charJsonCoder`
            should encode to strings $encode
            should decode from strings with one character $decode
            should not decode from the empty string $decodeEmptyString
            should not decode from from strings > 1 character long $decodeInvalidStrings
            should fail to decode a missing value $decodeMissing
    """

    def encode = prop { (c: Char) => scalar.charJsonCoder.encode.toString(c) ==== Okay("\"" + c + "\"") }
    def decode = prop { (c: Char) => scalar.charJsonCoder.decode.fromString(s""" "$c" """) ==== Okay(c) }
    def decodeEmptyString = scalar.charJsonCoder.decode.fromString("\"\"") must beLike { case FailedG(_, _) => ok }
    def decodeInvalidStrings = Prop.forAll(arbitrary[String].filter(_.length > 1)) { s =>
        scalar.charJsonCoder.decode.fromString("\"\"") must beLike { case FailedG(_, _) => ok }
    }
    def decodeMissing = checkMissing(scalar.charJsonCoder.decode)
}

class stringJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `stringJsonCoder`
            should round trip $roundTrip
            should fail to decode a missing value $decodeMissing
    """

    def roundTrip = prop { (s: String) => (scalar.stringJsonCoder.encode.toString(s) >>= scalar.stringJsonCoder.decode.fromString) ==== Okay(s) }
    def decodeMissing = checkMissing(scalar.stringJsonCoder.decode)
}

class byteBufferJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `byteBufferJsonCoder`
            should round trip $roundTrip
            should fail to decode a missing value $decodeMissing
    """

    def roundTrip = prop { (a: Array[Byte]) =>
        val bb = ByteBuffer.wrap(a)
        (scalar.byteBufferJsonCoder.encode.toString(bb) >>= scalar.byteBufferJsonCoder.decode.fromString) ==== Okay(bb)
    }
    def decodeMissing = checkMissing(scalar.byteBufferJsonCoder.decode)
}

class byteArrayJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `byteArrayJsonCoder`
            should round trip $roundTrip
            should fail to decode a missing value $decodeMissing
    """

    def roundTrip = prop { (a: Array[Byte]) =>
        (scalar.byteArrayJsonCoder.encode.toString(a) >>= scalar.byteArrayJsonCoder.decode.fromString) must beLike { case Okay(a2) =>
            (a.length ==== a2.length) and (a zip a2).foldLeft(ok: MatchResult[Any]) { (prev, t) => prev and (t._1 ==== t._2) }
        }
    }
    def decodeMissing = checkMissing(scalar.byteArrayJsonCoder.decode)
}

class javaEnumJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        javaEnumStringCoder
            must encode to the correct string $encode
            must decode from the correct string $decode
            must fail to decode a missing value $decodeMissing
    """

    lazy val coder = scalar.javaEnumJsonCoder[JavaEnum]

    implicit val arbJavaEnum = Arbitrary(Gen.oneOf(JavaEnum.values().toSeq))

    def encode = prop { (e: JavaEnum) => coder.encode.toString(e) ==== Okay("\"" + e.toString + "\"") }
    def decode = prop { (e: JavaEnum) => coder.decode.fromString("\"" + e.toString + "\"") ==== Okay(e) }
    def decodeMissing = checkMissing(coder.decode)
}

object ScalaEnum extends Enumeration {
    val BANANA = Value("banana")
    val APPLE = Value("apple")
    val CARROT = Value("carrot")
}

class scalaEnumJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        scalaEnumStringCoder
            must encode to the correct string $encode
            must decode from the correct string $decode
            must fail to decode a missing value $decodeMissing
    """

    lazy val coder = scalar.scalaEnumJsonCoder[ScalaEnum.type]

    implicit val arbScalaEnum = Arbitrary(Gen.oneOf(ScalaEnum.values.toSeq))

    def encode = prop { (e: ScalaEnum.Value) => coder.encode.toString(e) ==== Okay("\"" + e.toString + "\"") }
    def decode = prop { (e: ScalaEnum.Value) => coder.decode.fromString("\"" + e.toString + "\"") ==== Okay(e) }
    def decodeMissing = checkMissing(coder.decode)
}

