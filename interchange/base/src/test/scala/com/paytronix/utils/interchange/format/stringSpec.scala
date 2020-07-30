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

package com.paytronix.utils.interchange.format.string

import java.nio.ByteBuffer
import javax.xml.bind.DatatypeConverter
import java.math.{ BigDecimal => JavaBigDecimal }

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.interchange.test.fixtures.{JavaEnum, ScalaEnum}
import com.paytronix.utils.scala.result.{FailedG, Okay}

import Arbitrary.arbitrary
import coders._

object testStuff {
    val nonnumericStr = Gen.frequency (
        (1, ""),
        (5, Gen.alphaStr),
        (5, Arbitrary.arbString.arbitrary.filter(s => !s.forall(Character.isDigit)))
    )

    val safeBigDecimals = arbitrary[BigDecimal].map(_.bigDecimal).filter { bd =>
        try { new JavaBigDecimal(bd.toString); true }
        catch { case nfe: NumberFormatException => false }
    }

    val inScaleBigDecimals = {
        val bd = new JavaBigDecimal(2)
        Gen.choose(0, 50) map { (i: Int) => BigDecimal(bd.setScale(i)) }
    }

    val posOutScaleBigDecimals = {
        val bd = new JavaBigDecimal(2)
        Gen.choose(51, 150) map { (i: Int) => BigDecimal(bd.setScale(i)) }
    }

    implicit val arbJavaMathBigInteger = Arbitrary(arbitrary[BigInt].map(_.bigInteger))

    def convertBigDecimalToString(bd: JavaBigDecimal): String = if (bd.scale < 0 || bd.scale > 50) bd.toString else bd.toPlainString
}

import testStuff._

class unitStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The unit coder for strings should
            be the implicit string coder for Unit       $e1
            be the implicit string decoder for Unit     $e2
            be the implicit string encoder for Unit     $e3
            encode as the empty string                  $e4
            decode successfully from any string         $e5
    """

    def e1 = StringCoder[Unit] ==== unitStringCoder
    def e2 = StringDecoder[Unit] ==== unitStringCoder.decode
    def e3 = StringEncoder[Unit] ==== unitStringCoder.encode
    def e4 = unitStringCoder.encode(()) ==== Okay("")
    def e5 = prop { (s: String) => unitStringCoder.decode(s) ==== Okay(()) }
}

class byteStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The byte coder for strings should
            be the implicit string decoder for Byte     $e1
            be the implicit string encoder for Byte     $e2
            encode bytes straightforwardly              $e3
            decode bytes straightforwardly              $e4
            fail to decode over range bytes             $e5
            fail to decode under range bytes            $e6
            round trip bytes                            $e7
            fail to decode arbitrary nonnumeric strings $e8
    """

    def e1 = StringDecoder[Byte] ==== byteStringCoder.decode
    def e2 = StringEncoder[Byte] ==== byteStringCoder.encode
    def e3 = prop { (b: Byte) => byteStringCoder.encode(b) ==== Okay(b.toString) }
    def e4 = prop { (b: Byte) => byteStringCoder.decode(b.toString) ==== Okay(b) }
    def e5 = Prop.forAll(Gen.choose(100, 200)) { i =>
        if (i > 127) byteStringCoder.decode(i.toString) must beLike { case FailedG(_, _) => ok }
        else         byteStringCoder.decode(i.toString) ==== Okay(i.byteValue)
    }
    def e6 = Prop.forAll(Gen.choose(-200, -100)) { i =>
        if (i < -128) byteStringCoder.decode(i.toString) must beLike { case FailedG(_, _) => ok }
        else          byteStringCoder.decode(i.toString) ==== Okay(i.byteValue)
    }
    def e7 = prop { (b: Byte) => (byteStringCoder.encode(b) >>= byteStringCoder.decode.apply) ==== Okay(b) }
    def e8 = Prop.forAll(nonnumericStr) { s => byteStringCoder.decode(s) must beLike { case FailedG(_, _) => ok } }
}

class shortStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The short coder for strings should
            be the implicit string decoder for Short    $e1
            be the implicit string encoder for Short    $e2
            encode shorts straightforwardly             $e3
            decode shorts straightforwardly             $e4
            fail to decode over range shorts            $e5
            fail to decode under range shorts           $e6
            round trip shorts                           $e7
            fail to decode arbitrary nonnumeric strings $e8
    """

    def e1 = StringDecoder[Short] ==== shortStringCoder.decode
    def e2 = StringEncoder[Short] ==== shortStringCoder.encode
    def e3 = prop { (s: Short) => shortStringCoder.encode(s) ==== Okay(s.toString) }
    def e4 = prop { (s: Short) => shortStringCoder.decode(s.toString) ==== Okay(s) }
    def e5 = Prop.forAll(Gen.choose(32700, 32800)) { i =>
        if (i > 32767) shortStringCoder.decode(i.toString) must beLike { case FailedG(_, _) => ok }
        else           shortStringCoder.decode(i.toString) ==== Okay(i.shortValue)
    }
    def e6 = Prop.forAll(Gen.choose(-32800, -32700)) { i =>
        if (i < -32768) shortStringCoder.decode(i.toString) must beLike { case FailedG(_, _) => ok }
        else            shortStringCoder.decode(i.toString) ==== Okay(i.shortValue)
    }
    def e7 = prop { (s: Short) => (shortStringCoder.encode(s) >>= shortStringCoder.decode.apply) ==== Okay(s) }
    def e8 = Prop.forAll(nonnumericStr) { s => shortStringCoder.decode(s) must beLike { case FailedG(_, _) => ok } }
}

class intStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The int coder for strings should
            be the implicit string decoder for Int      $e1
            be the implicit string encoder for Int      $e2
            encode ints straightforwardly               $e3
            decode ints straightforwardly               $e4
            fail to decode over range ints              $e5
            fail to decode under range ints             $e6
            round trip ints                             $e7
            fail to decode arbitrary nonnumeric strings $e8
    """

    def e1 = StringDecoder[Int] ==== intStringCoder.decode
    def e2 = StringEncoder[Int] ==== intStringCoder.encode
    def e3 = prop { (i: Int) => intStringCoder.encode(i) ==== Okay(i.toString) }
    def e4 = prop { (i: Int) => intStringCoder.decode(i.toString) ==== Okay(i) }
    def e5 = Prop.forAll(Gen.choose(2147483600L, 2147483700L)) { l =>
        if (l > 2147483647L) intStringCoder.decode(l.toString) must beLike { case FailedG(_, _) => ok }
        else                 intStringCoder.decode(l.toString) ==== Okay(l.intValue)
    }
    def e6 = Prop.forAll(Gen.choose(-2147483700L, -2147483600L)) { l =>
        if (l < -2147483648L) intStringCoder.decode(l.toString) must beLike { case FailedG(_, _) => ok }
        else                  intStringCoder.decode(l.toString) ==== Okay(l.intValue)
    }
    def e7 = prop { (i: Int) => (intStringCoder.encode(i) >>= intStringCoder.decode.apply) ==== Okay(i) }
    def e8 = Prop.forAll(nonnumericStr) { s => intStringCoder.decode(s) must beLike { case FailedG(_, _) => ok } }
}

class longStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The long coder for strings should
            be the implicit string decoder for Long      $e1
            be the implicit string encoder for Long      $e2
            encode longs straightforwardly               $e3
            decode longs straightforwardly               $e4
            fail to decode over range longs              $e5
            fail to decode under range longs             $e6
            round trip longs                             $e7
            fail to decode arbitrary nonnumeric strings  $e8
    """

    def upperBigInts = Gen.choose(0, 100) map { i => BigInt( "9223372036854775800") + i }
    def lowerBigInts = Gen.choose(0, 100) map { i => BigInt("-9223372036854775800") - i }

    def e1 = StringDecoder[Long] ==== longStringCoder.decode
    def e2 = StringEncoder[Long] ==== longStringCoder.encode
    def e3 = prop { (l: Long) => longStringCoder.encode(l) ==== Okay(l.toString) }
    def e4 = prop { (l: Long) => longStringCoder.decode(l.toString) ==== Okay(l) }
    def e5 = Prop.forAll(upperBigInts) { bi =>
        if (bi > BigInt("9223372036854775807")) longStringCoder.decode(bi.toString) must beLike { case FailedG(_, _) => ok }
        else                                    longStringCoder.decode(bi.toString) ==== Okay(bi.longValue)
    }
    def e6 = Prop.forAll(lowerBigInts) { bi =>
        if (bi < BigInt("-9223372036854775808")) longStringCoder.decode(bi.toString) must beLike { case FailedG(_, _) => ok }
        else                                     longStringCoder.decode(bi.toString) ==== Okay(bi.longValue)
    }
    def e7 = prop { (l: Long) => (longStringCoder.encode(l) >>= longStringCoder.decode.apply) ==== Okay(l) }
    def e8 = Prop.forAll(nonnumericStr) { s => longStringCoder.decode(s) must beLike { case FailedG(_, _) => ok } }
}

class floatStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The float coder for strings should
            be the implicit string decoder for Float     $e1
            be the implicit string encoder for Float     $e2
            encode floats straightforwardly              $e3
            decode floats straightforwardly              $e4
            round trip floats                            $e5
            fail to decode arbitrary nonnumeric strings  $e6
    """

    def e1 = StringDecoder[Float] ==== floatStringCoder.decode
    def e2 = StringEncoder[Float] ==== floatStringCoder.encode
    def e3 = prop { (f: Float) => floatStringCoder.encode(f) ==== Okay(f.toString) }
    def e4 = prop { (f: Float) => floatStringCoder.decode(f.toString) ==== Okay(f) }
    def e5 = prop { (f: Float) => (floatStringCoder.encode(f) >>= floatStringCoder.decode.apply) ==== Okay(f) }
    def e6 = Prop.forAll(nonnumericStr) { s => floatStringCoder.decode(s) must beLike { case FailedG(_, _) => ok } }
}

class doubleStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The double coder for strings should
            be the implicit string decoder for Double     $e1
            be the implicit string encoder for Double     $e2
            encode doubles straightforwardly              $e3
            decode doubles straightforwardly              $e4
            round trip doubles                            $e5
            fail to decode arbitrary nonnumeric strings   $e6
    """

    def e1 = StringDecoder[Double] ==== doubleStringCoder.decode
    def e2 = StringEncoder[Double] ==== doubleStringCoder.encode
    def e3 = prop { (d: Double) => doubleStringCoder.encode(d) ==== Okay(d.toString) }
    def e4 = prop { (d: Double) => doubleStringCoder.decode(d.toString) ==== Okay(d) }
    def e5 = prop { (d: Double) => (doubleStringCoder.encode(d) >>= doubleStringCoder.decode.apply) ==== Okay(d) }
    def e6 = Prop.forAll(nonnumericStr) { s => doubleStringCoder.decode(s) must beLike { case FailedG(_, _) => ok } }
}

class javaBigIntegerCodeStringrTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The java.math.BigInteger coder for strings should
            be the implicit string decoder for java.math.BigInteger     $e1
            be the implicit string encoder for java.math.BigInteger     $e2
            encode BigIntegers straightforwardly                        $e3
            decode BigIntegers straightforwardly                        $e4
            round trip BigIntegers                                      $e5
            fail to decode arbitrary nonnumeric strings                 $e6
    """

    def e1 = StringDecoder[java.math.BigInteger] ==== javaBigIntegerStringCoder.decode
    def e2 = StringEncoder[java.math.BigInteger] ==== javaBigIntegerStringCoder.encode
    def e3 = prop { (d: java.math.BigInteger) => javaBigIntegerStringCoder.encode(d) ==== Okay(d.toString) }
    def e4 = prop { (d: java.math.BigInteger) => javaBigIntegerStringCoder.decode(d.toString) ==== Okay(d) }
    def e5 = prop { (d: java.math.BigInteger) => (javaBigIntegerStringCoder.encode(d) >>= javaBigIntegerStringCoder.decode.apply) ==== Okay(d) }
    def e6 = Prop.forAll(nonnumericStr) { s => javaBigIntegerStringCoder.decode(s) must beLike { case FailedG(_, _) => ok } }
}

class javaBigDecimalStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The JavaBigDecimal coder for strings should
            be the implicit string decoder for JavaBigDecimal           $e1
            be the implicit string encoder for JavaBigDecimal           $e2
            encode BigDecimals straightforwardly                        $e3
            decode BigDecimals straightforwardly                        $e4
            round trip BigDecimals                                      $e5
            encode BigDecimal with scale 0 < s < 50                     $e6
            encode BigDecimal with scale 51 < s < 150                   $e7
            fail to decode arbitrary nonnumeric strings                 $e8
    """

    def e1 = StringDecoder[JavaBigDecimal] ==== javaBigDecimalStringCoder.decode
    def e2 = StringEncoder[JavaBigDecimal] ==== javaBigDecimalStringCoder.encode
    def e3 = Prop.forAll(safeBigDecimals) { d => javaBigDecimalStringCoder.encode(d) ==== Okay(convertBigDecimalToString(d)) }
    def e4 = Prop.forAll(safeBigDecimals) { d => javaBigDecimalStringCoder.decode(d.toString) ==== Okay(d) }
    def e5 = Prop.forAll(safeBigDecimals) { d => (javaBigDecimalStringCoder.encode(d) >>= javaBigDecimalStringCoder.decode.apply) ==== Okay(new JavaBigDecimal(d.toString)) }
    def e6 = Prop.forAll(inScaleBigDecimals) { d => javaBigDecimalStringCoder.encode(d.bigDecimal) ==== Okay(d.bigDecimal.toPlainString) }
    def e7 = Prop.forAll(posOutScaleBigDecimals) { d => javaBigDecimalStringCoder.encode(d.bigDecimal) ==== Okay(d.bigDecimal.toString) }
    def e8 = Prop.forAll(nonnumericStr) { s => javaBigDecimalStringCoder.decode(s) must beLike { case FailedG(_, _) => ok } }
}

class scalaBigIntStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The scala.math.BigInt coder for strings should
            be the implicit string decoder for BigInt     $e1
            be the implicit string encoder for BigInt     $e2
            encode BigInts straightforwardly              $e3
            decode BigInts straightforwardly              $e4
            round trip BigInts                            $e5
            fail to decode arbitrary nonnumeric strings   $e6
    """

    def e1 = StringDecoder[BigInt] ==== scalaBigIntStringCoder.decode
    def e2 = StringEncoder[BigInt] ==== scalaBigIntStringCoder.encode
    def e3 = prop { (d: BigInt) => scalaBigIntStringCoder.encode(d) ==== Okay(d.toString) }
    def e4 = prop { (d: BigInt) => scalaBigIntStringCoder.decode(d.toString) ==== Okay(d) }
    def e5 = prop { (d: BigInt) => (scalaBigIntStringCoder.encode(d) >>= scalaBigIntStringCoder.decode.apply) ==== Okay(d) }
    def e6 = Prop.forAll(nonnumericStr) { s => scalaBigIntStringCoder.decode(s) must beLike { case FailedG(_, _) => ok } }
}

class scalaBigDecimalStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        The scala.math.BigDecimal coder for strings should
            be the implicit string decoder for BigDecimal     $e1
            be the implicit string encoder for BigDecimal     $e2
            encode BigDecimals straightforwardly              $e3
            decode BigDecimals straightforwardly              $e4
            round trip BigDecimals                            $e5
            encode BigDecimal with scale 0 < s < 50           $e6
            encode BigDecimal with scale 51 < s < 150         $e7
            fail to decode arbitrary nonnumeric strings       $e8
    """

    def e1 = StringDecoder[BigDecimal] ==== scalaBigDecimalStringCoder.decode
    def e2 = StringEncoder[BigDecimal] ==== scalaBigDecimalStringCoder.encode
    def e3 = Prop.forAll(safeBigDecimals.map(BigDecimal.apply)) { d => scalaBigDecimalStringCoder.encode(d) ==== Okay(convertBigDecimalToString(d.bigDecimal)) }
    def e4 = Prop.forAll(safeBigDecimals.map(BigDecimal.apply)) { d => scalaBigDecimalStringCoder.decode(d.toString) ==== Okay(BigDecimal(d.bigDecimal.toString)) }
    def e5 = Prop.forAll(safeBigDecimals.map(BigDecimal.apply)) { d => (scalaBigDecimalStringCoder.encode(d) >>= scalaBigDecimalStringCoder.decode.apply) ==== Okay(d) }
    def e6 = Prop.forAll(inScaleBigDecimals) { d => scalaBigDecimalStringCoder.encode(d) ==== Okay(d.bigDecimal.toPlainString) }
    def e7 = Prop.forAll(posOutScaleBigDecimals) { d => scalaBigDecimalStringCoder.encode(d) ==== Okay(d.bigDecimal.toString) }
    def e8 = Prop.forAll(nonnumericStr) { s => scalaBigDecimalStringCoder.decode(s) must beLike { case FailedG(_, _) => ok } }
}

class byteBufferStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        byteBufferStringCoder
            must encode to the correct string $e1
            must decode from the correct string $e2
    """

    def e1 = prop { (a: Array[Byte]) => byteBufferStringCoder.encode(ByteBuffer.wrap(a)) ==== Okay(DatatypeConverter.printBase64Binary(a)) }
    def e2 = prop { (a: Array[Byte]) =>
        byteBufferStringCoder.decode(DatatypeConverter.printBase64Binary(a)) must beLike { case Okay(bb) =>
            bb.array.mkString("[", ",", "]") ==== a.mkString("[", ",", "]")
        }
    }
}

class byteArrayStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        byteArrayStringCoder
            must encode to the correct string $e1
            must decode from the correct string $e2
    """

    def e1 = prop { (a: Array[Byte]) => byteArrayStringCoder.encode(a) ==== Okay(DatatypeConverter.printBase64Binary(a)) }
    def e2 = prop { (expected: Array[Byte]) =>
        byteArrayStringCoder.decode(DatatypeConverter.printBase64Binary(expected)) must beLike { case Okay(a) =>
            a.mkString("[", ",", "]") ==== expected.mkString("[", ",", "]")
        }
    }
}

class javaEnumStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        javaEnumStringCoder
            must encode to the correct string $e1
            must decode from the correct string $e2
    """

    lazy val coder = javaEnumStringCoder[JavaEnum]

    implicit val arbJavaEnum = Arbitrary(Gen.oneOf(JavaEnum.values().toSeq))

    def e1 = prop { (e: JavaEnum) => coder.encode(e) ==== Okay(e.toString) }
    def e2 = prop { (e: JavaEnum) => coder.decode(e.toString) ==== Okay(e) }
}

class scalaEnumStringCoderTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""
        scalaEnumStringCoder
            must encode to the correct string $e1
            must decode from the correct string $e2
    """

    lazy val coder = scalaEnumStringCoder[ScalaEnum.type]

    implicit val arbScalaEnum = Arbitrary(Gen.oneOf(ScalaEnum.values.toSeq))

    def e1 = prop { (e: ScalaEnum.Value) => coder.encode(e) ==== Okay(e.toString) }
    def e2 = prop { (e: ScalaEnum.Value) => coder.decode(e.toString) ==== Okay(e) }
}
