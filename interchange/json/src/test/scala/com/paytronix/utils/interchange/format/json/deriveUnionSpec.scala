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

import scala.collection.JavaConverters.asScalaBufferConverter

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.interchange.base.union
import com.paytronix.utils.interchange.test.fixtures.{
    UntaggedUnionBase, UntaggedUnionFirst, UntaggedUnionSecond, UntaggedUnionInvalid,
    TaggedUnionBase, TaggedUnionFirst, TaggedUnionSecond, TaggedUnionSingleton, TaggedUnionSingletonWithProperties, TaggedUnionInvalid
}
import com.paytronix.utils.scala.result.{FailedG, Okay}

import Arbitrary.arbitrary

class deriveTaggedUnionCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        Explicitly derived tagged union coders
            must encode first correctly $encodeFirstCase
            must encode second correctly $encodeSecondCase
            must encode singleton correctly $encodeSingletonCase
            must encode singleton with properties correctly $encodeSingletonWithPropertiesCase
            must decode first correctly $decodeFirstCase
            must decode second correctly $decodeSecondCase
            must decode singleton correctly $decodeSingletonCase
            must decode singleton with properties correctly $decodeSingletonWithPropertiesCase
            fail to encode invalid union alternates $encodeInvalidCase
            fail to decode without the tag present $decodeInvalidCase
    """

    import scalar.{intJsonCoder, stringJsonCoder}
    implicit val firstCoder = derive.structure.coder[TaggedUnionFirst]
    implicit val secondCoder = derive.structure.coder[TaggedUnionSecond]
    implicit val singletonCoder = derive.structure.coder[TaggedUnionSingleton.type]
    implicit val singletonWithPropertiesCoder = derive.structure.coder[TaggedUnionSingletonWithProperties.type]

    lazy val coder = derive.taggedUnion.coder[TaggedUnionBase] (
        "tag",
        union.alt[TaggedUnionFirst].tag("first"),
        union.alt[TaggedUnionSecond].tag("second"),
        union.alt[TaggedUnionSingleton.type].tag("singleton"),
        union.alt[TaggedUnionSingletonWithProperties.type].tag("singletonWithProperties")
    )

    def encodeFirstCase =
        coder.encode.toString(TaggedUnionFirst(1)) ==== Okay(""" {"tag":"first","a":1} """.trim)
    def encodeSecondCase =
        coder.encode.toString(TaggedUnionSecond("foo")) ==== Okay(""" {"tag":"second","b":"foo"} """.trim)
    def encodeSingletonCase =
        coder.encode.toString(TaggedUnionSingleton) ==== Okay(""" {"tag":"singleton"} """.trim)
    def encodeSingletonWithPropertiesCase =
        coder.encode.toString(TaggedUnionSingletonWithProperties) ==== Okay(""" {"tag":"singletonWithProperties","codedProperty":"bar"} """.trim)

    def decodeFirstCase =
        decode(coder.decode)(""" {"a":1,"tag":"first"} """.trim) ==== Okay(TaggedUnionFirst(1))
    def decodeSecondCase =
        decode(coder.decode)(""" {"b":"foo","tag":"second"} """.trim) ==== Okay(TaggedUnionSecond("foo"))
    def decodeSingletonCase =
        decode(coder.decode)(""" {"tag":"singleton"} """.trim) ==== Okay(TaggedUnionSingleton)
    def decodeSingletonWithPropertiesCase =
        decode(coder.decode)(""" {"codedProperty":"bar","tag":"singletonWithProperties"} """.trim) ==== Okay(TaggedUnionSingletonWithProperties)

    def encodeInvalidCase =
        coder.encode.toString(TaggedUnionInvalid(1.1)) must beLike { case FailedG(_, _) => ok }
    def decodeInvalidCase =
        decode(coder.decode)(""" {"a":1} """.trim) must beLike { case FailedG(_, _) => ok }
}

object deriveTaggedUnionImplicitCoderFixture {
    import scalar.{intJsonCoder, stringJsonCoder}

    @derive.taggedUnion.implicitCoder("tag")
    @union(union.alt[A].tag("tag a"), union.alt[B].tag("tag b"))
    sealed abstract class ImplicitlyCodedUnion
    object ImplicitlyCodedUnion {
        implicit val arb: Arbitrary[ImplicitlyCodedUnion] = Arbitrary(Gen.oneOf(arbitrary[A], arbitrary[B]))
    }

    @derive.structure.implicitCoder final case class A(i: Int) extends ImplicitlyCodedUnion
    object A { implicit val arb: Arbitrary[A] = Arbitrary(arbitrary[Int].map(A.apply)) }
    @derive.structure.implicitCoder final case class B(s: String) extends ImplicitlyCodedUnion
    object B { implicit val arb: Arbitrary[B] = Arbitrary(arbitrary[String].map(B.apply)) }
}


class deriveTaggedUnionImplicitCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import deriveTaggedUnionImplicitCoderFixture.ImplicitlyCodedUnion

    def is = s2"""
        Derived implicit coders for unions
            trivially roundup values $trivialCase
    """

    lazy val coder = ImplicitlyCodedUnion.jsonCoder

    def trivialCase = prop { (icu: ImplicitlyCodedUnion) =>
        (coder.encode.toString(icu) >>= decode(coder.decode)) ==== Okay(icu)
    }
}

// FIXME derive encoder
// FIXME derive decoder

class deriveAdHocUnionCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        Explicitly derived ad-hoc union coders
            must encode first correctly $encodeFirstCase
            must encode second correctly $encodeSecondCase
            must decode first correctly $decodeFirstCase
            must decode second correctly $decodeSecondCase
            fail to encode invalid union alternates $encodeInvalidCase
            fail to decode invalid union alternates $decodeInvalidCase
    """

    import scalar.{intJsonCoder, stringJsonCoder}
    implicit val firstCoder = derive.structure.coder[UntaggedUnionFirst]
    implicit val secondCoder = derive.structure.coder[UntaggedUnionSecond]
    lazy val coder = derive.adHocUnion.coder[UntaggedUnionBase]("NAA", union.alt[UntaggedUnionFirst], union.alt[UntaggedUnionSecond])

    def encodeFirstCase =
        coder.encode.toString(UntaggedUnionFirst(1)) ==== Okay(""" {"a":1} """.trim)
    def encodeSecondCase =
        coder.encode.toString(UntaggedUnionSecond("foo")) ==== Okay(""" {"b":"foo"} """.trim)

    def decodeFirstCase =
        decode(coder.decode)(""" {"a":1} """.trim) ==== Okay(UntaggedUnionFirst(1))
    def decodeSecondCase =
        decode(coder.decode)(""" {"b":"foo"} """.trim) ==== Okay(UntaggedUnionSecond("foo"))

    def encodeInvalidCase =
        coder.encode.toString(UntaggedUnionInvalid(1.1)) must beLike { case FailedG(_, _) => ok }
    def decodeInvalidCase =
        decode(coder.decode)(""" {"c":1.1} """.trim) must beLike { case f@FailedG(_, _) => f.message must endWith("NAA") }
}

object deriveAdHocUnionImplicitCoderFixture {
    import scalar.{intJsonCoder, stringJsonCoder}

    @derive.adHocUnion.implicitCoder("NAA")
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


class deriveAdHocUnionImplicitCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import deriveAdHocUnionImplicitCoderFixture.ImplicitlyCodedUnion

    def is = s2"""
        Derived implicit coders for ad-hoc unions
            trivially roundup values $trivialCase
    """

    lazy val coder = ImplicitlyCodedUnion.jsonCoder

    def trivialCase = prop { (icu: ImplicitlyCodedUnion) =>
        (coder.encode.toString(icu) >>= decode(coder.decode)) ==== Okay(icu)
    }
}

// FIXME derive encoder
// FIXME derive decoder
