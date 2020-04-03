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

package com.paytronix.utils.interchange.format.avro

import java.nio.ByteBuffer
import java.util.Arrays
import scala.collection.JavaConverters.asScalaSetConverter

import org.apache.avro.Schema
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.interchange.test.fixtures.{JavaEnum, ScalaEnum, ScalaEnum2}
import com.paytronix.utils.scala.result.Okay

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

class unitAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `unitAvroCoder`
            must have correct schema (RECORD with no fields) $e1
            must encode to zero bytes $e2
            must decode from zero bytes $e3
            must be the implicit coder for Unit $e4
    """

    def e1 = (scalar.unitAvroCoder.schema.getType ==== Schema.Type.RECORD) and (scalar.unitAvroCoder.schema.getFields.size ==== 0)
    def e2 = scalar.unitAvroCoder.encode.toBytes(()) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte]()) }
    def e3 = scalar.unitAvroCoder.decode.fromBytes(scalar.unitAvroCoder.schema)(Array[Byte]()) ==== Okay(())
    def e4 = { import coders._; AvroCoder[Unit].encode.getClass must_== scalar.unitAvroCoder.encode.getClass }
}

class booleanAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `booleanAvroCoder`
            must have correct schema (BOOLEAN) $e1
            must encode false to a zero byte $e2
            must encode true to a one byte $e3
            must decode a zero byte to false $e4
            must encode a one byte to true $e5
            must decode defaults correctly $e6
            must be the implicit coder for Boolean $e7
    """

    def e1 = scalar.booleanAvroCoder.schema.getType ==== Schema.Type.BOOLEAN
    def e2 = scalar.booleanAvroCoder.encode.toBytes(false) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](0)) }
    def e3 = scalar.booleanAvroCoder.encode.toBytes(true) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](1)) }
    def e4 = scalar.booleanAvroCoder.decode.fromBytes(scalar.booleanAvroCoder.schema)(Array[Byte](0)) ==== Okay(false)
    def e5 = scalar.booleanAvroCoder.decode.fromBytes(scalar.booleanAvroCoder.schema)(Array[Byte](1)) ==== Okay(true)
    def e6 = prop { (b: Boolean) => decodeDefault(scalar.booleanAvroCoder.default(b)) ==== Okay(b) }
    def e7 = { import coders._; AvroCoder[Boolean].encode.getClass must_== scalar.booleanAvroCoder.encode.getClass }
}

class byteAvroCoderFixedTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `byteAvroCoderFixed`
            must have correct schema (FIXED, 1) $e1
            must encode to the correct bytes $e2
            must decode from the correct bytes $e3
            must decode defaults correctly $e4
            must be the implicit coder for Byte $e5
    """

    def e1 = scalar.byteAvroCoderFixed.schema.getType ==== Schema.Type.FIXED and scalar.byteAvroCoderFixed.schema.getFixedSize == 1
    def e2 = prop { (b: Byte) => scalar.byteAvroCoderFixed.encode.toBytes(b) must beLike { case Okay(a) => a must beEqualToArray(Array(b)) } }
    def e3 = prop { (b: Byte) => scalar.byteAvroCoderFixed.decode.fromBytes(scalar.byteAvroCoderFixed.schema)(Array[Byte](b)) ==== Okay(b) }
    def e4 = prop { (b: Byte) => decodeDefault(scalar.byteAvroCoderFixed.default(b)) ==== Okay(b) }
    def e5 = { import coders._; AvroCoder[Byte].encode.getClass must_== scalar.byteAvroCoderFixed.encode.getClass }
}

class byteAvroCoderIntTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `byteAvroCoderInt`
            must have correct schema (INT) $eschema
            must round trip $eprop
            must encode 0 to the corresponding zig-zag $e0
            must encode -1 to the corresponding zig-zag $en1
            must encode 1 to the corresponding zig-zag $e1
            must decode defaults correctly $edefault
    """

    def eschema = scalar.byteAvroCoderInt.schema.getType ==== Schema.Type.INT
    def eprop = prop { (b: Byte) =>
        (scalar.byteAvroCoderInt.encode.toBytes(b) >>= scalar.byteAvroCoderInt.decode.fromBytes(scalar.byteAvroCoderInt.schema)) ==== Okay(b)
    }
    def e0 = scalar.byteAvroCoderInt.encode.toBytes(0) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](0)) }
    def en1 = scalar.byteAvroCoderInt.encode.toBytes(-1) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](1)) }
    def e1 = scalar.byteAvroCoderInt.encode.toBytes(1) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](2)) }
    def edefault = prop { (b: Byte) => decodeDefault(scalar.byteAvroCoderInt.default(b)) ==== Okay(b) }
}


class shortAvroCoderFixedTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `shortAvroCoderFixed`
            must have correct schema (FIXED, 2) $e1
            must encode to the correct bytes $e2
            must decode from the correct bytes $e3
            must decode defaults correctly $e4
            must be the implicit coder for Short $e5
    """

    def e1 = scalar.shortAvroCoderFixed.schema.getType ==== Schema.Type.FIXED and scalar.shortAvroCoderFixed.schema.getFixedSize == 2
    def e2 = prop { (s: Short) =>
        scalar.shortAvroCoderFixed.encode.toBytes(s) must beLike { case Okay(a) =>
            a must beEqualToArray(Array[Byte]((s >>> 8 & 0xff).asInstanceOf[Byte], (s & 0xff).asInstanceOf[Byte]))
        }
    }
    def e3 = prop { (s: Short) =>
        val a = Array[Byte]((s >>> 8 & 0xff).asInstanceOf[Byte], (s & 0xff).asInstanceOf[Byte])
        scalar.shortAvroCoderFixed.decode.fromBytes(scalar.shortAvroCoderFixed.schema)(a) ==== Okay(s)
    }
    def e4 = prop { (s: Short) => decodeDefault(scalar.shortAvroCoderFixed.default(s)) ==== Okay(s) }
    def e5 = { import coders._; AvroCoder[Short].encode.getClass must_== scalar.shortAvroCoderFixed.encode.getClass }
}

class shortAvroCoderIntTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `shortAvroCoderInt`
            must have correct schema (INT) $eschema
            must round trip $eprop
            must encode 0 to the corresponding zig-zag $e0
            must encode -1 to the corresponding zig-zag $en1
            must encode 1 to the corresponding zig-zag $e1
            must decode defaults correctly $edefault
    """

    def eschema = scalar.shortAvroCoderInt.schema.getType ==== Schema.Type.INT
    def eprop = prop { (s: Short) =>
        (scalar.shortAvroCoderInt.encode.toBytes(s) >>= scalar.shortAvroCoderInt.decode.fromBytes(scalar.shortAvroCoderInt.schema)) ==== Okay(s)
    }
    def e0 = scalar.shortAvroCoderInt.encode.toBytes(0) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](0)) }
    def en1 = scalar.shortAvroCoderInt.encode.toBytes(-1) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](1)) }
    def e1 = scalar.shortAvroCoderInt.encode.toBytes(1) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](2)) }
    def edefault = prop { (s: Short) => decodeDefault(scalar.shortAvroCoderInt.default(s)) ==== Okay(s) }
}

class intAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `intAvroCoder`
            must have correct schema (INT) $eschema
            must round trip $eprop
            must encode 0 to the corresponding zig-zag $e0
            must encode -1 to the corresponding zig-zag $en1
            must encode 1 to the corresponding zig-zag $e1
            must decode defaults correctly $edefault
            must be the implicit coder for Int $eimplicit
    """

    def eschema = scalar.intAvroCoder.schema.getType ==== Schema.Type.INT
    def eprop = prop { (i: Int) =>
        (scalar.intAvroCoder.encode.toBytes(i) >>= scalar.intAvroCoder.decode.fromBytes(scalar.intAvroCoder.schema)) ==== Okay(i)
    }
    def e0 = scalar.intAvroCoder.encode.toBytes(0) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](0)) }
    def en1 = scalar.intAvroCoder.encode.toBytes(-1) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](1)) }
    def e1 = scalar.intAvroCoder.encode.toBytes(1) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](2)) }
    def edefault = prop { (i: Int) => decodeDefault(scalar.intAvroCoder.default(i)) ==== Okay(i) }
    def eimplicit = { import coders._; AvroCoder[Int].encode.getClass must_== scalar.intAvroCoder.encode.getClass }
}

class longAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `longAvroCoder`
            must have correct schema (LONG) $eschema
            must round trip $eprop
            must encode 0 to the corresponding zig-zag $e0
            must encode -1 to the corresponding zig-zag $en1
            must encode 1 to the corresponding zig-zag $e1
            must decode defaults correctly $edefault
            must be the implicit coder for Long $eimplicit
    """

    def eschema = scalar.longAvroCoder.schema.getType ==== Schema.Type.LONG
    def eprop = prop { (l: Long) =>
        (scalar.longAvroCoder.encode.toBytes(l) >>= scalar.longAvroCoder.decode.fromBytes(scalar.longAvroCoder.schema)) ==== Okay(l)
    }
    def e0 = scalar.longAvroCoder.encode.toBytes(0L) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](0)) }
    def en1 = scalar.longAvroCoder.encode.toBytes(-1L) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](1)) }
    def e1 = scalar.longAvroCoder.encode.toBytes(1L) must beLike { case Okay(a) => a must beEqualToArray(Array[Byte](2)) }
    def edefault = prop { (l: Long) => decodeDefault(scalar.longAvroCoder.default(l)) ==== Okay(l) }
    def eimplicit = { import coders._; AvroCoder[Long].encode.getClass must_== scalar.longAvroCoder.encode.getClass }
}

class floatAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `floatAvroCoder`
            must have correct schema (FLOAT) $eschema
            must round trip $eprop
            must decode defaults correctly $edefault
            must be the implicit coder for Float $eimplicit
    """

    def eschema = scalar.floatAvroCoder.schema.getType ==== Schema.Type.FLOAT
    def eprop = prop { (f: Float) =>
        (scalar.floatAvroCoder.encode.toBytes(f) >>= scalar.floatAvroCoder.decode.fromBytes(scalar.floatAvroCoder.schema)) ==== Okay(f)
    }
    def edefault = prop { (f: Float) => decodeDefault(scalar.floatAvroCoder.default(f)) ==== Okay(f) }
    def eimplicit = { import coders._; AvroCoder[Float].encode.getClass must_== scalar.floatAvroCoder.encode.getClass }
}

class doubleAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `doubleAvroCoder`
            must have correct schema (DOUBLE) $eschema
            must round trip $eprop
            must decode defaults correctly $edefault
            must be the implicit coder for Double $eimplicit
    """

    def eschema = scalar.doubleAvroCoder.schema.getType ==== Schema.Type.DOUBLE
    def eprop = prop { (d: Double) =>
        (scalar.doubleAvroCoder.encode.toBytes(d) >>= scalar.doubleAvroCoder.decode.fromBytes(scalar.doubleAvroCoder.schema)) ==== Okay(d)
    }
    def edefault = prop { (d: Double) => decodeDefault(scalar.doubleAvroCoder.default(d)) ==== Okay(d) }
    def eimplicit = { import coders._; AvroCoder[Double].encode.getClass must_== scalar.doubleAvroCoder.encode.getClass }
}

class javaBigIntegerAvroCoderBytesTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `javaBigIntegerAvroCoderBytes`
            must have correct schema (BYTES) $eschema
            must round trip $eprop
            must decode defaults correctly $edefault
            must be the implicit coder for java.math.BigInteger $eimplicit
    """

    def eschema = scalar.javaBigIntegerAvroCoderBytes.schema.getType ==== Schema.Type.BYTES
    def eprop = prop { (bi: java.math.BigInteger) =>
        (scalar.javaBigIntegerAvroCoderBytes.encode.toBytes(bi) >>=
         scalar.javaBigIntegerAvroCoderBytes.decode.fromBytes(scalar.javaBigIntegerAvroCoderBytes.schema)) ==== Okay(bi)
    }
    def edefault = prop { (bi: java.math.BigInteger) => decodeDefault(scalar.javaBigIntegerAvroCoderBytes.default(bi)) ==== Okay(bi) }
    def eimplicit = { import coders._; AvroCoder[java.math.BigInteger].encode.getClass must_== scalar.javaBigIntegerAvroCoderBytes.encode.getClass }
}

class javaBigIntegerAvroCoderStringTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `javaBigIntegerAvroCoderString`
            must have correct schema (STRING) $eschema
            must round trip $eprop
            must decode defaults correctly $edefault
    """

    def eschema = scalar.javaBigIntegerAvroCoderString.schema.getType ==== Schema.Type.STRING
    def eprop = prop { (bi: java.math.BigInteger) =>
        (scalar.javaBigIntegerAvroCoderString.encode.toBytes(bi) >>=
         scalar.javaBigIntegerAvroCoderString.decode.fromBytes(scalar.javaBigIntegerAvroCoderString.schema)) ==== Okay(bi)
    }
    def edefault = prop { (bi: java.math.BigInteger) => decodeDefault(scalar.javaBigIntegerAvroCoderString.default(bi)) ==== Okay(bi) }
}

class scalaBigIntAvroCoderBytesTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `scalaBigIntAvroCoderBytes`
            must have correct schema (BYTES) $eschema
            must round trip $eprop
            must decode defaults correctly $edefault
            must be the implicit coder for BigInt $eimplicit
    """

    def eschema = scalar.scalaBigIntAvroCoderBytes.schema.getType ==== Schema.Type.BYTES
    def eprop = prop { (bi: BigInt) =>
        (scalar.scalaBigIntAvroCoderBytes.encode.toBytes(bi) >>=
         scalar.scalaBigIntAvroCoderBytes.decode.fromBytes(scalar.scalaBigIntAvroCoderBytes.schema)) ==== Okay(bi)
    }
    def edefault = prop { (bi: BigInt) => decodeDefault(scalar.scalaBigIntAvroCoderBytes.default(bi)) ==== Okay(bi) }
    def eimplicit = { import coders._; AvroCoder[BigInt].encode.getClass must_== scalar.scalaBigIntAvroCoderBytes.encode.getClass }
}

class scalaBigIntAvroCoderStringTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `scalaBigIntAvroCoderString`
            must have correct schema (STRING) $eschema
            must round trip $eprop
            must decode defaults correctly $edefault
    """

    def eschema = scalar.scalaBigIntAvroCoderString.schema.getType ==== Schema.Type.STRING
    def eprop = prop { (bi: BigInt) =>
        (scalar.scalaBigIntAvroCoderString.encode.toBytes(bi) >>=
         scalar.scalaBigIntAvroCoderString.decode.fromBytes(scalar.scalaBigIntAvroCoderString.schema)) ==== Okay(bi)
    }
    def edefault = prop { (bi: BigInt) => decodeDefault(scalar.scalaBigIntAvroCoderString.default(bi)) ==== Okay(bi) }
}

class javaBigDecimalAvroCoderBytesTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `javaBigDecimalAvroCoderBytes`
            must have correct schema (BYTES) $eschema
            must round trip $eprop
            must decode defaults correctly $edefault
            must be the implicit coder for java.math.BigDecimal $eimplicit
    """

    def eschema = scalar.javaBigDecimalAvroCoderBytes.schema.getType ==== Schema.Type.BYTES
    def eprop = prop { (bd: java.math.BigDecimal) =>
        (scalar.javaBigDecimalAvroCoderBytes.encode.toBytes(bd) >>=
         scalar.javaBigDecimalAvroCoderBytes.decode.fromBytes(scalar.javaBigDecimalAvroCoderBytes.schema)) ==== Okay(bd)
    }
    def edefault = prop { (bd: java.math.BigDecimal) => decodeDefault(scalar.javaBigDecimalAvroCoderBytes.default(bd)) ==== Okay(bd) }
    def eimplicit = { import coders._; AvroCoder[java.math.BigDecimal].encode.getClass must_== scalar.javaBigDecimalAvroCoderBytes.encode.getClass }
}

class javaBigDecimalAvroCoderStringTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `javaBigDecimalAvroCoderString`
            must have correct schema (STRING) $eschema
            must round trip $eprop
            must make the correct strings $estrings
            must decode defaults correctly $edefault
    """

    def eschema = scalar.javaBigDecimalAvroCoderString.schema.getType ==== Schema.Type.STRING
    def eprop = Prop.forAll(safeJavaBigDecimals) { bd =>
        (scalar.javaBigDecimalAvroCoderString.encode.toBytes(bd) >>=
         scalar.javaBigDecimalAvroCoderString.decode.fromBytes(scalar.javaBigDecimalAvroCoderString.schema)) ==== Okay(bd)
    }
    def estrings = Prop.forAll(safeScalaBigDecimals) { bd =>
        scalar.scalaBigDecimalAvroCoderString.encode.toBytes(bd) must beLike { case Okay(a) => a must beAvroString(bd.toString) }
    }
    def edefault = Prop.forAll(safeJavaBigDecimals) { (bd: java.math.BigDecimal) =>
        decodeDefault(scalar.javaBigDecimalAvroCoderString.default(bd)) ==== Okay(bd)
    }
}

class scalaBigDecimalAvroCoderBytesTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `scalaBigDecimalAvroCoderBytes`
            must have correct schema (BYTES) $eschema
            must round trip $eprop
            must decode defaults correctly $edefault
            must be the implicit coder for BigDecimal $eimplicit
    """

    def eschema = scalar.scalaBigDecimalAvroCoderBytes.schema.getType ==== Schema.Type.BYTES
    def eprop = Prop.forAll(safeScalaBigDecimals) { bd =>
        (scalar.scalaBigDecimalAvroCoderBytes.encode.toBytes(bd) >>=
         scalar.scalaBigDecimalAvroCoderBytes.decode.fromBytes(scalar.scalaBigDecimalAvroCoderBytes.schema)) ==== Okay(bd)
    }
    def edefault = Prop.forAll(safeScalaBigDecimals) { bd => decodeDefault(scalar.scalaBigDecimalAvroCoderBytes.default(bd)) ==== Okay(bd) }
    def eimplicit = { import coders._; AvroCoder[BigDecimal].encode.getClass must_== scalar.scalaBigDecimalAvroCoderBytes.encode.getClass }
}

class scalaBigDecimalAvroCoderStringTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `scalaBigDecimalAvroCoderString`
            must have correct schema (STRING) $eschema
            must round trip $eprop
            must make the correct strings $estrings
            must decode defaults correctly $edefault
    """

    def eschema = scalar.scalaBigDecimalAvroCoderString.schema.getType ==== Schema.Type.STRING
    def eprop = Prop.forAll(safeScalaBigDecimals) { bd =>
        (scalar.scalaBigDecimalAvroCoderString.encode.toBytes(bd) >>=
         scalar.scalaBigDecimalAvroCoderString.decode.fromBytes(scalar.scalaBigDecimalAvroCoderString.schema)) ==== Okay(bd)
    }
    def estrings = Prop.forAll(safeScalaBigDecimals) { bd =>
        scalar.scalaBigDecimalAvroCoderString.encode.toBytes(bd) must beLike { case Okay(a) => a must beAvroString(bd.toString) }
    }
    def edefault = Prop.forAll(safeScalaBigDecimals) { bd => decodeDefault(scalar.scalaBigDecimalAvroCoderString.default(bd)) ==== Okay(bd) }
}

class charAvroCoderFixedTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `charAvroCoderFixed`
            must have correct schema (FIXED, 2) $e1
            must encode to the correct bytes $e2
            must decode from the correct bytes $e3
            must decode defaults correctly $e4
            must be the implicit coder for Char $e5
    """

    def e1 = scalar.charAvroCoderFixed.schema.getType ==== Schema.Type.FIXED and scalar.charAvroCoderFixed.schema.getFixedSize == 2
    def e2 = prop { (c: Char) =>
        scalar.charAvroCoderFixed.encode.toBytes(c) must beLike { case Okay(a) =>
            a must beEqualToArray(Array[Byte]((c >>> 8 & 0xff).asInstanceOf[Byte], (c & 0xff).asInstanceOf[Byte]))
        }
    }
    def e3 = prop { (c: Char) =>
        val a = Array[Byte]((c >>> 8 & 0xff).asInstanceOf[Byte], (c >>> 0 & 0xff).asInstanceOf[Byte])
        scalar.charAvroCoderFixed.decode.fromBytes(scalar.charAvroCoderFixed.schema)(a) ==== Okay(c)
    }
    def e4 = prop { (c: Char) => decodeDefault(scalar.charAvroCoderFixed.default(c)) ==== Okay(c) }
    def e5 = { import coders._; AvroCoder[Char].encode.getClass must_== scalar.charAvroCoderFixed.encode.getClass }
}

class charAvroCoderStringTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `charAvroCoderString`
            must have correct schema (STRING) $e1
            must encode to the correct bytes $e2
            must decode from the correct bytes $e3
            must decode defaults correctly $e4
    """

    def e1 = scalar.charAvroCoderString.schema.getType ==== Schema.Type.STRING
    def e2 = prop { (c: Char) =>
        scalar.charAvroCoderString.encode.toBytes(c) must beLike { case Okay(a) => a must beAvroString(new String(Array(c))) }
    }
    def e3 = prop { (c: Char) =>
        scalar.charAvroCoderString.decode.fromBytes(scalar.charAvroCoderString.schema)(makeAvroString(new String(Array(c)))) ==== Okay(c)
    }
    def e4 = prop { (c: Char) => decodeDefault(scalar.charAvroCoderString.default(c)) ==== Okay(c) }
}

class stringAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `stringAvroCoder`
            must have correct schema (STRING) $e1
            must encode to the correct bytes $e2
            must decode from the correct bytes $e3
            must decode defaults correctly $e4
            must be the implicit coder for String $e5
    """

    def e1 = scalar.stringAvroCoder.schema.getType ==== Schema.Type.STRING
    def e2 = prop { (s: String) =>
        scalar.stringAvroCoder.encode.toBytes(s) must beLike { case Okay(a) => a must beAvroString(s) }
    }
    def e3 = prop { (s: String) =>
        scalar.stringAvroCoder.decode.fromBytes(scalar.stringAvroCoder.schema)(makeAvroString(s)) ==== Okay(s)
    }
    def e4 = prop { (s: String) => decodeDefault(scalar.stringAvroCoder.default(s)) ==== Okay(s) }
    def e5 = { import coders._; AvroCoder[String].encode.getClass must_== scalar.stringAvroCoder.encode.getClass }

}

class byteBufferAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `byteBufferAvroCoder`
            must have correct schema (BYTES) $e1
            must encode to the correct bytes $e2
            must decode from the correct bytes $e3
            must decode defaults correctly $e4
            must be the implicit coder for ByteBuffer $e5
    """

    def e1 = scalar.byteBufferAvroCoder.schema.getType ==== Schema.Type.BYTES
    def e2 = prop { (a: Array[Byte]) =>
        val bb = ByteBuffer.wrap(a)
        scalar.byteBufferAvroCoder.encode.toBytes(bb) must beLike { case Okay(a) => a must beAvroBytes(bb) }
    }
    def e3 = prop { (a: Array[Byte]) =>
        val bb = ByteBuffer.wrap(a)
        scalar.byteBufferAvroCoder.decode.fromBytes(scalar.byteBufferAvroCoder.schema)(makeAvroBytes(bb)) must beLike { case Okay(bb) =>
            bb.array.mkString("[", ",", "]") ==== a.mkString("[", ",", "]")
        }
    }
    def e4 = prop { (a: Array[Byte]) =>
        val bb = ByteBuffer.wrap(a)
        decodeDefault(scalar.byteBufferAvroCoder.default(bb)) must beLike { case Okay(bb) =>
            bb.array.mkString("[", ",", "]") ==== a.mkString("[", ",", "]")
        }
    }
    def e5 = { import coders._; AvroCoder[ByteBuffer].encode.getClass must_== scalar.byteBufferAvroCoder.encode.getClass }
}

class byteArrayAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `byteArrayAvroCoder`
            must have correct schema (BYTES) $e1
            must encode to the correct bytes $e2
            must decode from the correct bytes $e3
            must decode defaults correctly $e4
            must be the implicit coder for Array[Byte] $e5
    """

    def e1 = scalar.byteArrayAvroCoder.schema.getType ==== Schema.Type.BYTES
    def e2 = prop { (a: Array[Byte]) =>
        val bb = ByteBuffer.wrap(a)
        scalar.byteArrayAvroCoder.encode.toBytes(a) must beLike { case Okay(a) => a must beAvroBytes(bb) }
    }
    def e3 = prop { (expected: Array[Byte]) =>
        val bb = ByteBuffer.wrap(expected)
        scalar.byteArrayAvroCoder.decode.fromBytes(scalar.byteArrayAvroCoder.schema)(makeAvroBytes(bb)) must beLike { case Okay(a) =>
            a.mkString("[", ",", "]") ==== expected.mkString("[", ",", "]")
        }
    }
    def e4 = prop { (expected: Array[Byte]) =>
        decodeDefault(scalar.byteArrayAvroCoder.default(expected)) must beLike { case Okay(a) =>
            a.mkString("[", ",", "]") ==== expected.mkString("[", ",", "]")
        }
    }
    def e5 = { import coders._; AvroCoder[Array[Byte]].encode.getClass must_== scalar.byteArrayAvroCoder.encode.getClass }
}

class javaEnumAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `javaEnumAvroCoder`
            must have correct schema (ENUM) $e1
            must have correct enum elements in schema, in correct order $e2
            must encode to the correct bytes $e3
            must decode from the correct bytes $e4
            must decode defaults correctly $e5
            must be the implicit coder for JavaEnum $e6
    """

    lazy val coder = scalar.javaEnumAvroCoder[JavaEnum]

    implicit val arbJavaEnum = Arbitrary(Gen.oneOf(JavaEnum.values().toSeq))
    val sortedEnumValues = Array(JavaEnum.APPLE, JavaEnum.BANANA, JavaEnum.CARROT)

    def e1 = coder.schema.getType ==== Schema.Type.ENUM
    def e2 = coder.schema.getEnumSymbols ==== Arrays.asList("APPLE", "BANANA", "CARROT")
    def e3 = prop { (e: JavaEnum) =>
        coder.encode.toBytes(e) must beLike { case Okay(a) => a must beAvroInt(sortedEnumValues.indexOf(e)) }
    }
    def e4 = prop { (e: JavaEnum) =>
        coder.decode.fromBytes(coder.schema)(zigZagEncode(sortedEnumValues.indexOf(e))) ==== Okay(e)
    }
    def e5 = prop { (e: JavaEnum) => decodeDefault(coder.default(e)) ==== Okay(e) }
    def e6 = { import coders._; AvroCoder[JavaEnum].schema.getName ==== "JavaEnum" }
}

class scalaEnumAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `scalaEnumAvroCoder`
            must have correct schema (ENUM) $e1
            must have correct enum elements in schema, in correct order $e2
            must be insulated from enum labels with non-identifier characters $e3
            must encode to the correct bytes $e4
            must decode from the correct bytes $e5
            must decode defaults correctly $e6
            must be the implicit coder for ScalaEnum.Value $e7
    """

    lazy val coder = scalar.scalaEnumAvroCoder[ScalaEnum.type]
    lazy val coder2 = scalar.scalaEnumAvroCoder[ScalaEnum2.type]

    val enumValues = ScalaEnum.values.toSeq
    val sortedEnumValues = Array(ScalaEnum.APPLE, ScalaEnum.BANANA, ScalaEnum.CARROT)

    implicit val arbScalaEnum = Arbitrary(Gen.oneOf(ScalaEnum.values.toSeq))
    implicit val arbScalaEnum2 = Arbitrary(Gen.oneOf(ScalaEnum2.values.toSeq))

    def e1 = coder.schema.getType ==== Schema.Type.ENUM
    def e2 = coder.schema.getEnumSymbols ==== Arrays.asList("apple", "banana", "carrot")
    def e3 = coder2.schema.getEnumSymbols ==== Arrays.asList("thingone", "three", "twoish")
    def e4 = prop { e: ScalaEnum.Value =>
        coder.encode.toBytes(e) must beLike { case Okay(a) => a must beAvroInt(sortedEnumValues.indexOf(e)) }
    }
    def e5 = prop { e: ScalaEnum.Value =>
        coder.decode.fromBytes(coder.schema)(zigZagEncode(sortedEnumValues.indexOf(e))) ==== Okay(e)
    }
    def e6 = prop { e: ScalaEnum.Value =>
        decodeDefault(coder.default(e)) ==== Okay(e)
    }
    def e7 = { import coders._; AvroCoder[ScalaEnum.Value].schema.getName ==== "ScalaEnum" }
}

object scalaEnumNamingFixtures {
    object DefaultNaming extends Enumeration { val one = Value("one") }
    @name("foo.bar.Overridden") object OverriddenName extends Enumeration { val two = Value("two") }
    @aliases("a", "b", "foo.bar.C") object AliasedName extends Enumeration { val three = Value("three") }
}

class scalaEnumNamingAvroCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        `scalaEnumAvroCoder` naming and aliasing
            must have correct name by default $edefault
            must honor @name $ename
            must honor @aliases $ealiases
    """

    import scalaEnumNamingFixtures._

    val defaultCoder = scalar.scalaEnumAvroCoder[DefaultNaming.type]
    val namedCoder   = scalar.scalaEnumAvroCoder[OverriddenName.type]
    val aliasedCoder = scalar.scalaEnumAvroCoder[AliasedName.type]

    def edefault = defaultCoder.schema.getFullName ==== "com.paytronix.utils.interchange.format.avro.scalaEnumNamingFixtures.DefaultNaming"
    def ename = namedCoder.schema.getFullName ==== "foo.bar.Overridden"
    def ealiases = aliasedCoder.schema.getAliases.asScala must containTheSameElementsAs(Seq (
        "com.paytronix.utils.interchange.format.avro.scalaEnumNamingFixtures.a",
        "com.paytronix.utils.interchange.format.avro.scalaEnumNamingFixtures.b", "foo.bar.C"
    ))
}
