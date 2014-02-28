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

import java.nio.ByteBuffer
import scala.collection.JavaConverters.asScalaBufferConverter

import org.apache.avro.Schema
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.interchange.base.union
import com.paytronix.utils.interchange.test.fixtures.{UntaggedUnionBase, UntaggedUnionFirst, UntaggedUnionSecond, UntaggedUnionInvalid}
import com.paytronix.utils.scala.result.{FailedG, Okay}

import Arbitrary.arbitrary

class deriveUnionCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        Explicitly derived union coders
            must have correct schema $eschema
            must encode correctly $eencode
            must decode correctly $edecode
            must decode defaults correctly $edefault
            must refuse to default to the second union alternate $edefaultinvalid
            fail to encode invalid union alternates $eencodeinvalid
    """

    import scalar.{intAvroCoder, stringAvroCoder}
    implicit val firstCoder = derive.structure.coder[UntaggedUnionFirst]
    implicit val secondCoder = derive.structure.coder[UntaggedUnionSecond]
    lazy val coder = derive.union.coder[UntaggedUnionBase](union.alt[UntaggedUnionFirst], union.alt[UntaggedUnionSecond])

    def eschema = (
        (coder.schema.getType ==== Schema.Type.UNION).updateMessage("schema type: " + _) and
        (coder.schema.getTypes.size ==== 2).updateMessage("schema union size: " + _) and
        (coder.schema.getTypes.get(0) must beLike { case s => (s.getType ==== Schema.Type.RECORD) and (s.getName ==== "UntaggedUnionFirst") })
            .updateMessage("schema union first alt: " + _) and
        (coder.schema.getTypes.get(1) must beLike { case s => (s.getType ==== Schema.Type.RECORD) and (s.getName ==== "UntaggedUnionSecond") })
            .updateMessage("schema union second alt: " + _)
    )

    def eencode = prop { (uub: UntaggedUnionBase) =>
        coder.encode.toBytes(uub) must beLike { case Okay(a) =>
            uub match {
                case UntaggedUnionFirst(i) => (a.take(1) must beEqualToArray(zigZagEncode(0))) and (a.drop(1) must beEqualToArray(zigZagEncode(i)))
                case UntaggedUnionSecond(s) => (a.take(1) must beEqualToArray(zigZagEncode(1))) and (a.drop(1) must beEqualToArray(makeAvroString(s)))
                case _ => ko
            }
        }
    }

    def edecode = prop { (uub: UntaggedUnionBase) =>
        val a = uub match {
            case UntaggedUnionFirst(i) =>
                val intBytes = zigZagEncode(i)
                val bb = ByteBuffer.allocate(1+intBytes.length)
                bb.put(0.asInstanceOf[Byte])
                bb.put(intBytes)
                bb.array

            case UntaggedUnionSecond(s) =>
                val stringBytes = makeAvroString(s)
                val bb = ByteBuffer.allocate(1+stringBytes.length)
                bb.put(2.asInstanceOf[Byte])
                bb.put(stringBytes)
                bb.array

            case _ => sys.error("generator shouldn't have made a UntaggedUnionInvalid")
        }

        coder.decode.fromBytes(coder.schema)(a) ==== Okay(uub)
    }

    def edefault = prop { (uuf: UntaggedUnionFirst) =>
        decodeDefault(coder.default(uuf)) ==== Okay(uuf)
    }

    def edefaultinvalid =
        (coder.encode.encodeDefaultJson(UntaggedUnionSecond("")) must beLike { case FailedG(_, _) => ok }) and
        (coder.encode.encodeDefaultJson(UntaggedUnionInvalid(0.0)) must beLike { case FailedG(_, _) => ok })

    def eencodeinvalid =
        coder.encode.toBytes(UntaggedUnionInvalid(0.0)) must beLike { case FailedG(_, _) => ok }
}

object deriveUnionImplicitCoderFixture {
    import scalar.{intAvroCoder, stringAvroCoder}

    @derive.union.implicitCoder
    @union(union.alt[A], union.alt[B].tag("B"))
    sealed abstract class ImplicitlyCodedUnion
    object ImplicitlyCodedUnion {
        implicit val arb: Arbitrary[ImplicitlyCodedUnion] = Arbitrary(Gen.oneOf(arbitrary[A], arbitrary[B]))
    }

    @derive.structure.implicitCoder final case class A(i: Int) extends ImplicitlyCodedUnion
    object A { implicit val arb: Arbitrary[A] = Arbitrary(arbitrary[Int].map(A.apply)) }
    @derive.structure.implicitCoder final case class B(s: String) extends ImplicitlyCodedUnion
    object B { implicit val arb: Arbitrary[B] = Arbitrary(arbitrary[String].map(B.apply)) }
}

import deriveUnionImplicitCoderFixture.ImplicitlyCodedUnion

class deriveUnionImplicitCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        Derived implicit coders for unions
            must have the correct schema $eschema
            trivially roundup values $etrivial
    """

    lazy val coder = ImplicitlyCodedUnion.avroCoder

    def eschema =
        (coder.schema.getType ==== Schema.Type.UNION).updateMessage("schema type: " + _) and
        (coder.schema.getTypes.size ==== 2).updateMessage("schema union types count: " + _) and
        (coder.schema.getTypes.get(0).getType ==== Schema.Type.RECORD).updateMessage("schema union first alt type: " + _) and
        (coder.schema.getTypes.get(1).getType ==== Schema.Type.RECORD).updateMessage("schema union second alt type: " + _)

    def etrivial = prop { (icu: ImplicitlyCodedUnion) =>
        (coder.encode.toBytes(icu) >>= coder.decode.fromBytes(coder.schema)) ==== Okay(icu)
    }
}

// FIXME derive encoder
// FIXME derive decoder
