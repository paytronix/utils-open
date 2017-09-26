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

package com.paytronix.utils.interchange.format.liftjson

import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.Printer.compact
import org.scalacheck.Arbitrary
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.interchange.format.json.{JsonCoder, derive, flatten}
import com.paytronix.utils.interchange.format.json.coders._
import com.paytronix.utils.scala.result.{FailedG, FailedParameterDefault, Okay, ResultG}

import Arbitrary.arbitrary

object liftJsonTestFixtures {
    final case class Foo(a: Int, b: String, c: Option[Int], d: List[String])
    implicit val arbFoo: Arbitrary[Foo] = Arbitrary {
        for {
            a <- arbitrary[Int]
            b <- arbitrary[String]
            c <- arbitrary[Option[Int]]
            d <- arbitrary[List[String]]
        } yield Foo(a, b, c, d)
    }
    def encodeFoo(foo: Foo): JValue =
        JObject(List (
            Some(JField("a", JInt(BigInt(foo.a)))),
            Some(JField("b", JString(foo.b))),
            foo.c.map { i => JField("c", JInt(BigInt(i))) },
            Some(JField("d", JArray(foo.d.map(JString.apply))))
        ).flatten)

    final case class InnerStructure(a: Int, b: String)
    object InnerStructure {
        implicit val jsonCoder: JsonCoder[InnerStructure] = derive.structure.coder[InnerStructure]
        implicit val arb = Arbitrary(for { a <- arbitrary[Int]; b <- arbitrary[String] } yield InnerStructure(a, b))
    }

    final case class StructureWithFlattening(o: Int, @flatten f: InnerStructure, p: String)
    object StructureWithFlattening {
        implicit val jsonCoder: JsonCoder[StructureWithFlattening] = derive.structure.coder[StructureWithFlattening]
        implicit val arb = Arbitrary(for { o <- arbitrary[Int]; f <- arbitrary[InnerStructure]; p <- arbitrary[String] }
                                     yield StructureWithFlattening(o, f, p))
    }

}

class liftJsonTest extends SpecificationWithJUnit with ScalaCheck {
    import liftJsonTestFixtures._

    def is = s2"""
        Lift-JSON compatibility must
            encode booleans                    $encodeBooleanCase
            decode booleans                    $decodeBooleanCase
            encode byte                        $encodeByteCase
            decode byte                        $decodeByteCase
            encode short                       $encodeShortCase
            decode short                       $decodeShortCase
            encode int                         $encodeIntCase
            decode int                         $decodeIntCase
            encode long                        $encodeLongCase
            decode long                        $decodeLongCase
            encode string                      $encodeStringCase
            decode string                      $decodeStringCase
            encode float                       $encodeFloatCase
            decode float                       $decodeFloatCase
            encode double                      $encodeDoubleCase
            decode double                      $decodeDoubleCase
            encode bigint                      $encodeBigIntCase
            decode bigint                      $decodeBigIntCase
            encode arrays                      $encodeArrayCase
            decode arrays                      $decodeArrayCase
            encode objects                     $encodeObjectCase
            decode objects                     $decodeObjectCase
            round trip stacks of options       $roundTripOptionStackCase
            round trip result with objects     $roundTripResultObjectCase
            decode objects with missing fields $decodeMissingFieldCase
            round trip objects with flattening $roundTripFlattenCase
    """

    def encodeBooleanCase = prop { (b: Boolean) => booleanJsonCoder.encode.toJValue(b) ==== Okay(JBool(b)) }
    def decodeBooleanCase = prop { (b: Boolean) => booleanJsonCoder.decode.fromJValue(JBool(b)) ==== Okay(b) }

    def encodeByteCase = prop { (b: Byte) => byteJsonCoder.encode.toJValue(b) ==== Okay(JInt(BigInt(b))) }
    def decodeByteCase = prop { (b: Byte) => byteJsonCoder.decode.fromJValue(JInt(BigInt(b))) ==== Okay(b) }

    def encodeShortCase = prop { (s: Short) => shortJsonCoder.encode.toJValue(s) ==== Okay(JInt(BigInt(s))) }
    def decodeShortCase = prop { (s: Short) => shortJsonCoder.decode.fromJValue(JInt(BigInt(s))) ==== Okay(s) }

    def encodeIntCase = prop { (i: Int) => intJsonCoder.encode.toJValue(i) ==== Okay(JInt(BigInt(i))) }
    def decodeIntCase = prop { (i: Int) => intJsonCoder.decode.fromJValue(JInt(BigInt(i))) ==== Okay(i) }

    def encodeLongCase = prop { (l: Long) => longJsonCoder.encode.toJValue(l) ==== Okay(JInt(BigInt(l))) }
    def decodeLongCase = prop { (l: Long) => longJsonCoder.decode.fromJValue(JInt(BigInt(l))) ==== Okay(l) }

    def encodeStringCase = prop { (s: String) => stringJsonCoder.encode.toJValue(s) ==== Okay(JString(s)) }
    def decodeStringCase = prop { (s: String) => stringJsonCoder.decode.fromJValue(JString(s)) ==== Okay(s) }

    def encodeFloatCase = prop { (f: Float) => floatJsonCoder.encode.toJValue(f) ==== Okay(JDouble(f)) }
    def decodeFloatCase = prop { (f: Float) => floatJsonCoder.decode.fromJValue(JDouble(f)) ==== Okay(f) }

    def encodeDoubleCase = prop { (d: Double) => doubleJsonCoder.encode.toJValue(d) ==== Okay(JDouble(d)) }
    def decodeDoubleCase = prop { (d: Double) => doubleJsonCoder.decode.fromJValue(JDouble(d)) ==== Okay(d) }

    def encodeBigIntCase = prop { (bi: BigInt) => scalaBigIntJsonCoder.encode.toJValue(bi) ==== Okay(JInt(bi)) }
    def decodeBigIntCase = prop { (bi: BigInt) => scalaBigIntJsonCoder.decode.fromJValue(JInt(bi)) ==== Okay(bi) }

    val listIntCoder = JsonCoder[List[Int]]
    def encodeArrayCase = prop { (l: List[Int]) => listIntCoder.encode.toJValue(l) ==== Okay(JArray(l.map(i => JInt(BigInt(i))))) }
    def decodeArrayCase = prop { (l: List[Int]) => listIntCoder.decode.fromJValue(JArray(l.map(i => JInt(BigInt(i))))) ==== Okay(l) }

    val objCoder = derive.structure.coder[Foo]
    def encodeObjectCase = prop { (f: Foo) =>
        val res = objCoder.encode.toJValue(f)
        res.orThrow
        res ==== Okay(encodeFoo(f))
    }
    def decodeObjectCase = prop { (f: Foo) => objCoder.decode.fromJValue(encodeFoo(f)) ==== Okay(f) }

    val optionStackCoder = optionJsonCoder(optionJsonCoder(optionJsonCoder(intJsonCoder)))
    def roundTripOptionStackCase = prop { (os: Option[Option[Option[Int]]]) =>
        val encRes = optionStackCoder.encode.toJValue(os)
        val intermediate = encRes.orThrow
        val decRes = optionStackCoder.decode.fromJValue(intermediate)
        decRes match {
            case Okay(os2) =>
                (os2 ==== os).updateMessage(s"${Thread.currentThread}: intermediate: ${compact(render(intermediate))}: " + _)
            case f@FailedG(t, _) =>
                println(s"${Thread.currentThread}: Failed with intermediate result: ${compact(render(intermediate))}:")
                t.printStackTrace(System.out)
                ko(f.message)
        }
    }

    implicit val fpdInt = FailedParameterDefault.value(9999)
    val resultObjectCoder = resultGJsonCoder(intJsonCoder, objCoder)
    implicit val arbResultObj: Arbitrary[ResultG[Int, Foo]] = Arbitrary {
        arbitrary[Boolean].flatMap {
            case true => arbitrary[Foo].map(Okay.apply)
            case false => arbitrary[Int].map(FailedG("failed!", _))
        }
    }
    def roundTripResultObjectCase = prop { (r: ResultG[Int, Foo]) =>
        val encRes = resultObjectCoder.encode.toJValue(r)
        val intermediate = encRes.orThrow
        resultObjectCoder.decode.fromJValue(intermediate) match {
            case Okay(r2) =>
                r match {
                    case Okay(foo) =>
                        (r2 ==== Okay(foo)).updateMessage(s"${Thread.currentThread}: intermediate: ${compact(render(intermediate))}: " + _)
                    case f@FailedG(_, _) =>
                        (r2 must beLike { case f2@FailedG(_, _) => (f2.message ==== f.message) and (f2.parameter ==== f.parameter) })
                            .updateMessage(s"${Thread.currentThread}: intermediate: ${compact(render(intermediate))}: " + _)
                }

            case f@FailedG(t, _) =>
                println(s"${Thread.currentThread}: Failed with intermediate result: ${compact(render(intermediate))}:")
                t.printStackTrace(System.out)
                ko(f.message)
        }
    }

    def decodeMissingFieldCase = {
        val jv = JObject(List (
            JField("a", JInt(BigInt(123))),
            JField("b", JString("hi")),
            JField("d", JArray(List(JString("a"), JString("b"), JString("c"))))
        ))

        objCoder.decode.fromJValue(jv) ==== Okay(Foo(123, "hi", None, List("a", "b", "c")))
    }


    def roundTripFlattenCase = prop { (swf: StructureWithFlattening) =>
        val encRes = StructureWithFlattening.jsonCoder.encode.toJValue(swf)
        val intermediate = encRes.orThrow
        StructureWithFlattening.jsonCoder.decode.fromJValue(intermediate) match {
            case Okay(swf2) => (swf2 ==== swf).updateMessage(s"${Thread.currentThread}: intermediate: ${compact(render(intermediate))}: " + _)
            case f@FailedG(t, _) =>
                println(s"${Thread.currentThread}: Failed with intermediate result: ${compact(render(intermediate))}:")
                t.printStackTrace(System.out)
                ko(f.message)
        }
    }
}
