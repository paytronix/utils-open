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

import java.nio.ByteBuffer

import com.fasterxml.jackson.core.JsonFactory
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}
import org.specs2.execute.{Result => SpecsResult}
import org.specs2.matcher.{Matcher, MatchResult}

import com.paytronix.utils.interchange.base.{Receiver, formatFailedPath}
import com.paytronix.utils.interchange.test.fixtures.{JavaEnum, ScalaEnum}
import com.paytronix.utils.scala.result.{FailedG, Okay, Result}

import Arbitrary.arbitrary

import arbitraries._

object BigDecimalRender {
    def renderBigDecimal(bd: java.math.BigDecimal): String = {
        if (bd.scale < 0 || bd.scale > 50) {
            bd.toString
        } else {
            bd.toPlainString
        }
    }
    def renderBigDecimal(bd: BigDecimal): String = renderBigDecimal(bd.underlying)
}

class unitJsonCoderTest extends SpecificationWithJUnit with JsonMatchers {
    def is = s2"""
        `unitJsonCoder`
            should write null outside a field context $encodeNullCase
            should write nothing in a field context $encodeNothingCase
            should read nothing $decodeNothingCase
            should read null $decodeNullCase
    """

    def encodeNullCase = scalar.unitJsonCoder.encode.toString(()) ==== Okay("null")
    def encodeNothingCase = encodeField(scalar.unitJsonCoder.encode, ()) ==== Okay("")
    def decodeNothingCase = decodeMissing(scalar.unitJsonCoder.decode) ==== Okay(())
    def decodeNullCase = decode(scalar.unitJsonCoder.decode)("null") ==== Okay(())
}

class booleanJsonCoderTest extends SpecificationWithJUnit with JsonMatchers {
    def is = s2"""
        `booleanJsonCoder`
            should write true for true and false for false $encodeCase
            should decode true to true and false to false $decodeCase
            should decode "true" to true and "false" to false $decodeStringCase
            should fail to decode other strings $decodeInvalidStringCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase =
        (scalar.booleanJsonCoder.encode.toString(true)  ==== Okay("true")) and
        (scalar.booleanJsonCoder.encode.toString(false) ==== Okay("false"))
    def decodeCase =
        (decode(scalar.booleanJsonCoder.decode)("true")  ==== Okay(true)) and
        (decode(scalar.booleanJsonCoder.decode)("false") ==== Okay(false))
    def decodeStringCase =
        (decode(scalar.booleanJsonCoder.decode)("\"true\"")  ==== Okay(true)) and
        (decode(scalar.booleanJsonCoder.decode)("\"false\"") ==== Okay(false))
    def decodeInvalidStringCase =
        (decode(scalar.booleanJsonCoder.decode)("\"\"")  must beLike { case FailedG(_, _) => ok }) and
        (decode(scalar.booleanJsonCoder.decode)("\"0\"") must beLike { case FailedG(_, _) => ok })
    def decodeMissingCase = checkMissing(scalar.booleanJsonCoder.decode)
}

class byteJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `byteJsonCoder`
            should encode to numbers $encodeCase
            should decode from in-range numbers $decodeCase
            should fail to decode from numbers with a decimal component $decodeInvalidDecimalCase
            should fail to decode from out-of-range numbers $decodeInvalidOutOfRangeCase
            should decode from in-range strings $decodeStringCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = prop { (b: Byte) => scalar.byteJsonCoder.encode.toString(b) ==== Okay(b.toString) }
    def decodeCase = prop { (b: Byte) => decode(scalar.byteJsonCoder.decode)(b.toString) ==== Okay(b) }
    def decodeInvalidDecimalCase = prop { (f: Float) => decode(scalar.byteJsonCoder.decode)(f.toString) must beLike { case FailedG(_, _) => ok } }
    def decodeInvalidOutOfRangeCase = Prop.forAll(arbitrary[Int].filter(i => i < Byte.MinValue | i > Byte.MaxValue)) { i =>
        decode(scalar.byteJsonCoder.decode)(i.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeStringCase = prop { (b: Byte) => decode(scalar.byteJsonCoder.decode)(s""" "${b.toString}" """) ==== Okay(b) }
    def decodeMissingCase = checkMissing(scalar.booleanJsonCoder.decode)
}

class shortJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `shortJsonCoder`
            should encode to numbers $encodeCase
            should decode from in-range numbers $decodeCase
            should fail to decode from numbers with a decimal component $decodeInvalidDecimalCase
            should fail to decode from out-of-range numbers $decodeInvalidOutOfRangeCase
            should decode from in-range strings $decodeStringCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = prop { (s: Short) => scalar.shortJsonCoder.encode.toString(s) ==== Okay(s.toString) }
    def decodeCase = prop { (s: Short) => decode(scalar.shortJsonCoder.decode)(s.toString) ==== Okay(s) }
    def decodeInvalidDecimalCase = prop { (f: Float) => decode(scalar.shortJsonCoder.decode)(f.toString) must beLike { case FailedG(_, _) => ok } }
    def decodeInvalidOutOfRangeCase = Prop.forAll(arbitrary[Int].filter(i => i < Short.MinValue | i > Short.MaxValue)) { i =>
        decode(scalar.shortJsonCoder.decode)(i.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeStringCase = prop { (s: Short) => decode(scalar.shortJsonCoder.decode)(s""" "${s.toString}" """) ==== Okay(s) }
    def decodeMissingCase = checkMissing(scalar.shortJsonCoder.decode)
}

class intJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `intJsonCoder`
            should encode to numbers $encodeCase
            should decode from in-range numbers $decodeCase
            should fail to decode from numbers with a decimal component $decodeInvalidDecimalCase
            should fail to decode from out-of-range numbers $decodeInvalidOutOfRangeCase
            should decode from in-range strings $decodeStringCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = prop { (i: Int) => scalar.intJsonCoder.encode.toString(i) ==== Okay(i.toString) }
    def decodeCase = prop { (i: Int) => decode(scalar.intJsonCoder.decode)(i.toString) ==== Okay(i) }
    def decodeInvalidDecimalCase = prop { (f: Float) => decode(scalar.intJsonCoder.decode)(f.toString) must beLike { case FailedG(_, _) => ok } }
    def decodeInvalidOutOfRangeCase = Prop.forAll(arbitrary[Long].filter(l => l < Int.MinValue | l > Int.MaxValue)) { l =>
        decode(scalar.intJsonCoder.decode)(l.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeStringCase = prop { (i: Int) => decode(scalar.intJsonCoder.decode)(s""" "${i.toString}" """) ==== Okay(i) }
    def decodeMissingCase = checkMissing(scalar.intJsonCoder.decode)
}

class longJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `longJsonCoder`
            should encode to numbers $encodeCase
            should decode from in-range numbers $decodeCase
            should fail to decode from numbers with a decimal component $decodeInvalidDecimalCase
            should fail to decode from out-of-range numbers $decodeInvalidOutOfRangeCase
            should decode from in-range strings $decodeStringCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = prop { (l: Long) => scalar.longJsonCoder.encode.toString(l) ==== Okay(l.toString) }
    def decodeCase = prop { (l: Long) => decode(scalar.longJsonCoder.decode)(l.toString) ==== Okay(l) }
    def decodeInvalidDecimalCase = prop { (f: Float) => decode(scalar.longJsonCoder.decode)(f.toString) must beLike { case FailedG(_, _) => ok } }
    def decodeInvalidOutOfRangeCase = Prop.forAll(arbitrary[BigInt].filter(i => i < Long.MinValue | i > Long.MaxValue)) { bi =>
        decode(scalar.longJsonCoder.decode)(bi.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeStringCase = prop { (l: Long) => decode(scalar.longJsonCoder.decode)(s""" "${l.toString}" """) ==== Okay(l) }
    def decodeMissingCase = checkMissing(scalar.longJsonCoder.decode)
}

class floatJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `floatJsonCoder`
            should encode to numbers $encodeCase
            should decode from numbers without a decimal $decodeIntegralCase
            should decode from numbers with a decimal $decodeRealCase
            should decode from strings without a decimal $decodeStringIntegralCase
            should decode from strings with a decimal $decodeStringRealCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = prop { (f: Float) => scalar.floatJsonCoder.encode.toString(f) ==== Okay(f.toString) }
    def decodeIntegralCase = prop { (i: Int) => decode(scalar.floatJsonCoder.decode)(i.toString) ==== Okay(i: Float) }
    def decodeRealCase = prop { (f: Float) => decode(scalar.floatJsonCoder.decode)(f.toString) ==== Okay(f) }
    def decodeStringIntegralCase = prop { (i: Int) => decode(scalar.floatJsonCoder.decode)(s""" "${i.toString}" """) ==== Okay(i: Float) }
    def decodeStringRealCase = prop { (f: Float) => decode(scalar.floatJsonCoder.decode)(s""" "${f.toString}" """) ==== Okay(f) }
    def decodeMissingCase = checkMissing(scalar.floatJsonCoder.decode)
}

class doubleJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `doubleJsonCoder`
            should encode to numbers $encodeCase
            should decode from numbers without a decimal $decodeIntegralCase
            should decode from numbers with a decimal $decodeRealCase
            should decode from strings without a decimal $decodeStringIntegralCase
            should decode from strings with a decimal $decodeStringRealCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = prop { (d: Double) => scalar.doubleJsonCoder.encode.toString(d) ==== Okay(d.toString) }
    def decodeIntegralCase = prop { (i: Int) => decode(scalar.doubleJsonCoder.decode)(i.toString) ==== Okay(i: Double) }
    def decodeRealCase = prop { (d: Double) => decode(scalar.doubleJsonCoder.decode)(d.toString) ==== Okay(d) }
    def decodeStringIntegralCase = prop { (i: Int) => decode(scalar.doubleJsonCoder.decode)(s""" "${i.toString}" """) ==== Okay(i: Double) }
    def decodeStringRealCase = prop { (d: Double) => decode(scalar.doubleJsonCoder.decode)(s""" "${d.toString}" """) ==== Okay(d) }
    def decodeMissingCase = checkMissing(scalar.doubleJsonCoder.decode)
}

class javaBigIntegerJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `javaBigIntegerJsonCoder`
            should encode to int $encodeCase
            should decode from strings $decodeCase
            should decode from numbers without a decimal $decodeIntegralCase
            should not decode from numbers with a decimal $decodeRealCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = prop { (bi: java.math.BigInteger) => scalar.javaBigIntegerJsonCoder.encode.toString(bi) ==== Okay(bi.toString) }
    def decodeCase = prop { (bi: java.math.BigInteger) => decode(scalar.javaBigIntegerJsonCoder.decode)(s""" "${bi.toString}" """) ==== Okay(bi) }
    def decodeIntegralCase = prop { (bi: java.math.BigInteger) => decode(scalar.javaBigIntegerJsonCoder.decode)(bi.toString) ==== Okay(bi) }
    def decodeRealCase = prop { (d: Double) =>
        decode(scalar.javaBigIntegerJsonCoder.decode)(d.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeMissingCase = checkMissing(scalar.javaBigIntegerJsonCoder.decode)
}

class scalaBigIntJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `scalaBigIntJsonCoder`
            should encode to strings $encodeCase
            should decode from strings $decodeCase
            should decode from numbers without a decimal $decodeIntegralCase
            should not decode from numbers with a decimal $decodeRealCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = prop { (bi: BigInt) => scalar.scalaBigIntJsonCoder.encode.toString(bi) ==== Okay(bi.toString) }
    def decodeCase = prop { (bi: BigInt) => decode(scalar.scalaBigIntJsonCoder.decode)(s""" "${bi.toString}" """) ==== Okay(bi) }
    def decodeIntegralCase = prop { (bi: BigInt) => decode(scalar.scalaBigIntJsonCoder.decode)(bi.toString) ==== Okay(bi) }
    def decodeRealCase = prop { (d: Double) =>
        decode(scalar.scalaBigIntJsonCoder.decode)(d.toString) must beLike { case FailedG(_, _) => ok }
    }
    def decodeMissingCase = checkMissing(scalar.scalaBigIntJsonCoder.decode)
}

class javaBigDecimalJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `javaBigDecimalJsonCoder`
            should encode to strings $encodeCase
            should decode from strings $decodeCase
            should decode from numbers without a decimal $decodeIntegralCase
            should decode from numbers with a decimal $decodeRealCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = Prop.forAll(safeJavaBigDecimals) { bd => scalar.javaBigDecimalJsonCoder.encode.toString(bd) ==== Okay("\"" + BigDecimalRender.renderBigDecimal(bd) + "\"") }
    def decodeCase = Prop.forAll(safeJavaBigDecimals) { bd => decode(scalar.javaBigDecimalJsonCoder.decode)(s""" "${bd.toString}" """) ==== Okay(bd) }
    def decodeIntegralCase = prop { (bi: java.math.BigInteger) =>
        val bd = new java.math.BigDecimal(bi)
        decode(scalar.javaBigDecimalJsonCoder.decode)(bi.toString) ==== Okay(bd)
    }
    def decodeRealCase = prop { (d: Double) =>
        decode(scalar.javaBigDecimalJsonCoder.decode)(d.toString) ==== Okay(new java.math.BigDecimal(d.toString))
    }
    def decodeMissingCase = checkMissing(scalar.javaBigDecimalJsonCoder.decode)
}

class scalaBigDecimalJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `scalaBigDecimalJsonCoder`
            should encode to strings $encodeCase
            should decode from strings $decodeCase
            should decode from numbers without a decimal $decodeIntegralCase
            should decode from numbers with a decimal $decodeRealCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = Prop.forAll(safeScalaBigDecimals) { bd => scalar.scalaBigDecimalJsonCoder.encode.toString(bd) ==== Okay("\"" + BigDecimalRender.renderBigDecimal(bd) + "\"") }
    def decodeCase = Prop.forAll(safeScalaBigDecimals) { bd => decode(scalar.scalaBigDecimalJsonCoder.decode)(s""" "${bd.toString}" """) ==== Okay(bd) }
    def decodeIntegralCase = prop { (bi: java.math.BigInteger) =>
        val bd = BigDecimal(BigInt(bi))
        decode(scalar.scalaBigDecimalJsonCoder.decode)(bi.toString) ==== Okay(bd)
    }
    def decodeRealCase = prop { (d: Double) =>
        decode(scalar.scalaBigDecimalJsonCoder.decode)(d.toString) ==== Okay(BigDecimal(d.toString))
    }
    def decodeMissingCase = checkMissing(scalar.scalaBigDecimalJsonCoder.decode)
}

class charJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `charJsonCoder`
            should encode to strings $encodeCase
            should decode from strings with one character $decodeCase
            should round trip $roundTripCase
            should not decode from the empty string $decodeEmptyStringCase
            should not decode from from strings > 1 character long $decodeInvalidStringsCase
            should fail to decode a missing value $decodeMissingCase
    """

    def encodeCase = scalar.charJsonCoder.encode.toString('a') ==== Okay("\"a\"")
    def decodeCase = decode(scalar.charJsonCoder.decode)("\"a\"") ==== Okay('a')
    def roundTripCase = prop { (c: Char) =>
        (scalar.charJsonCoder.encode.toString(c) >>= scalar.charJsonCoder.decode.fromString) ==== Okay(c)
    }
    def decodeEmptyStringCase = decode(scalar.charJsonCoder.decode)("\"\"") must beLike { case FailedG(_, _) => ok }
    def decodeInvalidStringsCase = Prop.forAll(arbitrary[String].filter(_.length > 1)) { s =>
        decode(scalar.charJsonCoder.decode)("\"\"") must beLike { case FailedG(_, _) => ok }
    }
    def decodeMissingCase = checkMissing(scalar.charJsonCoder.decode)
}

class stringJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `stringJsonCoder`
            should round trip $roundTripCase
            should fail to decode a missing value $decodeMissingCase
    """

    def roundTripCase = prop { (s: String) => (scalar.stringJsonCoder.encode.toString(s) >>= scalar.stringJsonCoder.decode.fromString) ==== Okay(s) }
    def decodeMissingCase = checkMissing(scalar.stringJsonCoder.decode)
}

class byteBufferJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `byteBufferJsonCoder`
            should round trip $roundTripCase
            should fail to decode a missing value $decodeMissingCase
    """

    def roundTripCase = prop { (a: Array[Byte]) =>
        val bb = ByteBuffer.wrap(a)
        (scalar.byteBufferJsonCoder.encode.toString(bb) >>= scalar.byteBufferJsonCoder.decode.fromString) ==== Okay(bb)
    }
    def decodeMissingCase = checkMissing(scalar.byteBufferJsonCoder.decode)
}

class byteArrayJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `byteArrayJsonCoder`
            should round trip $roundTripCase
            should fail to decode a missing value $decodeMissingCase
    """

    def roundTripCase = prop { (a: Array[Byte]) =>
        (scalar.byteArrayJsonCoder.encode.toString(a) >>= scalar.byteArrayJsonCoder.decode.fromString) must beLike { case Okay(a2) =>
            (a.length ==== a2.length) and (a zip a2).foldLeft(ok: MatchResult[Any]) { (prev, t) => prev and (t._1 ==== t._2) }
        }
    }
    def decodeMissingCase = checkMissing(scalar.byteArrayJsonCoder.decode)
}

class javaEnumJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        javaEnumStringCoder
            must encode to the correct string $encodeCase
            must decode from the correct string $decodeCase
            must fail to decode a missing value $decodeMissingCase
    """

    lazy val coder = scalar.javaEnumJsonCoder[JavaEnum]

    implicit val arbJavaEnum = Arbitrary(Gen.oneOf(JavaEnum.values().toSeq))

    def encodeCase = prop { (e: JavaEnum) => coder.encode.toString(e) ==== Okay("\"" + e.toString + "\"") }
    def decodeCase = prop { (e: JavaEnum) => decode(coder.decode)("\"" + e.toString + "\"") ==== Okay(e) }
    def decodeMissingCase = checkMissing(coder.decode)
}

class scalaEnumJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        scalaEnumStringCoder
            must encode to the correct string $encodeCase
            must decode from the correct string $decodeCase
            must fail to decode a missing value $decodeMissingCase
    """

    lazy val coder = scalar.scalaEnumJsonCoder[ScalaEnum.type]

    implicit val arbScalaEnum = Arbitrary(Gen.oneOf(ScalaEnum.values.toSeq))

    def encodeCase = prop { (e: ScalaEnum.Value) => coder.encode.toString(e) ==== Okay("\"" + e.toString + "\"") }
    def decodeCase = prop { (e: ScalaEnum.Value) => decode(coder.decode)("\"" + e.toString + "\"") ==== Okay(e) }
    def decodeMissingCase = checkMissing(coder.decode)
}

