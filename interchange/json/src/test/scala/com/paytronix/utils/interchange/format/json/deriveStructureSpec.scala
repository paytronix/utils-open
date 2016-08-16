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

import java.util.Arrays
import scala.collection.JavaConverters.asScalaBufferConverter

import org.scalacheck.Arbitrary
import org.specs2.{ScalaCheck, SpecificationWithJUnit}
import scalaz.BijectionT.bijection

import com.paytronix.utils.interchange.base.default
import com.paytronix.utils.interchange.test.fixtures.{BasicClass, CaseClass, POJOClass}
import com.paytronix.utils.scala.result.{Failed, Okay, Result}

import Arbitrary.arbitrary

trait StructureJsonMatchers extends JsonMatchers { self: SpecificationWithJUnit =>
    def packCC(cc: CaseClass, extraFront: String = "", extraBack: String = ""): String =
        pack(cc.foo, cc.bar, cc.zip, extraFront, extraBack)
    def packBC(bc: BasicClass, extraFront: String = "", extraBack: String = ""): String =
        pack(bc.foo, bc.bar, bc.zip, extraFront, extraBack)
    def packPOJO(pc: POJOClass, extraFront: String = "", extraBack: String = ""): String =
        pack(pc.getFoo, pc.getBar, pc.getZip, extraFront, extraBack)

    def pack(foo: Int, bar: String, zip: Option[String], extraFront: String = "", extraBack: String = ""): String = {
        val barField = if (bar == null) "" else s""" "bar":${encodeString(bar)}, """.trim
        val fooField = s""" "foo":$foo """.trim
        val zipField = zip match { case Some(s) => s""" ,"zip":${encodeString(s)} """.trim; case None => "" }
        "{" + extraFront + barField + fooField + zipField + extraBack + "}"
    }
}

class jsonStructureCoderCaseClassTest extends SpecificationWithJUnit with ScalaCheck with StructureJsonMatchers {
    def is = s2"""
        Explicitly derived structure coder for CaseClass
            encode correctly $encodeCase
            decode correctly $decodeCase
            ignore extra fields when decoding $decodeIgnoreExtraCase
    """

    import container.optionJsonCoder
    import scalar.{intJsonCoder, stringJsonCoder}

    lazy val coder = derive.structure.coder[CaseClass]

    def encodeCase = prop { (cc: CaseClass) =>
        coder.encode.toString(cc) ==== Okay(packCC(cc))
    }

    def decodeCase = prop { (cc: CaseClass) =>
        decode(coder.decode)(packCC(cc)) ==== Okay(cc)
    }

    def decodeIgnoreExtraCase = prop { (cc: CaseClass, front: Boolean, back: Boolean) =>
        val extraFront = if (front) """ "extra": "front", """.trim else ""
        val extraBack = if (back) """ ,"extra": "back" """.trim else ""
        decode(coder.decode)(packCC(cc, extraFront, extraBack)) ==== Okay(cc)
    }
}

class jsonStructureCoderBasicClassTest extends SpecificationWithJUnit with ScalaCheck with StructureJsonMatchers {
    def is = s2"""
        Explicitly derived structure coder for BasicClass
            encode correctly $encodeCase
            decode correctly $decodeCase
    """

    import container.optionJsonCoder
    import scalar.{intJsonCoder, stringJsonCoder}

    lazy val coder = derive.structure.coder[BasicClass]

    def encodeCase = prop { (bc: BasicClass) =>
        coder.encode.toString(bc) ==== Okay(packBC(bc))
    }

    def decodeCase = prop { (bc: BasicClass) =>
        decode(coder.decode)(packBC(bc)) ==== Okay(bc)
    }
}

class jsonStructureCoderPOJOClassTest extends SpecificationWithJUnit with ScalaCheck with StructureJsonMatchers {
    def is = s2"""
        Explicitly derived structure coder for POJOClass
            encode correctly $encodeCase
            decode correctly $decodeCase
    """

    import container.optionJsonCoder
    import scalar.{intJsonCoder, stringJsonCoder}

    lazy val coder = derive.structure.coder[POJOClass]

    def encodeCase = prop { (pojo: POJOClass) =>
        coder.encode.toString(pojo) ==== Okay(packPOJO(pojo))
    }

    def decodeCase = prop { (pojo: POJOClass) =>
        decode(coder.decode)(packPOJO(pojo)) ==== Okay(pojo)
    }
}

/* 2014-08-27 RMM: having multiple annotation macros which addToCompanion causes the compiler to not emit the object class (Blah$) even though
                   it doesn't error at runtime.
object jsonStructureImplicitTestFixture {
    import scalar.intJsonCoder

    @derive.structure.implicitCoder
    case class ImplicitlyCodedStructure(a: Int)
    @derive.structure.implicitCoder
    case class ImplicitlyCodedStructure2(a: Int)
    object ImplicitlyCodedStructure2 {
        val existing = "companion"
    }
}

import jsonStructureImplicitTestFixture.{ImplicitlyCodedStructure, ImplicitlyCodedStructure2}

class jsonStructureImplicitTest extends SpecificationWithJUnit with JsonMatchers {
    def is = s2"""
        Implicit structure coders
            should be explicitly accessible via companion.JsonCoder $explicitCreateCase
            should be explicitly accessible via companion.JsonCoder even when companion already exists $explicitMergeCase
            should be implicitly accessible via materializer $materializeCreateCase
            should be implicitly accessible via materializer even when companion already exists $materializeMergeCase
            should not lose existing companion members $mergeCase
            round trip a value $trivialCase
    """

    def explicitCreateCase = ImplicitlyCodedStructure.jsonCoder.encode.codesAsObject ==== true
    def explicitMergeCase = ImplicitlyCodedStructure2.jsonCoder.encode.codesAsObject ==== true
    def materializeCreateCase = JsonCoder[ImplicitlyCodedStructure] ==== ImplicitlyCodedStructure.jsonCoder
    def materializeMergeCase = JsonCoder[ImplicitlyCodedStructure2] ==== ImplicitlyCodedStructure2.jsonCoder
    def mergeCase = ImplicitlyCodedStructure2.existing ==== "companion"
    def trivialCase = {
        val coder = JsonCoder[ImplicitlyCodedStructure]
        val ics = ImplicitlyCodedStructure(1)
        (coder.encode.toString(ics) >>= decode(coder.decode)) ==== Okay(ics)
    }
}
*/

object jsonStructureCustomizedCoderTestFixture {
    import scalar.charJsonCoder // implicitly the default for Char
    import scalar.intJsonCoder

    case class CustomizedCoderStructure(a: Int, b: Char, c: Char)
    object CustomizedCoderStructure {
        @derive.structure.customizedCoder[CustomizedCoderStructure]
        implicit object jsonCoder {
            val bCoder = scalar.stringJsonCoder.mapBijection(bijection (
                (c: Char) => Okay("foo" + c): Result[String],
                (s: String) => (if (s.startsWith("foo")) Okay(s.charAt(3)) else Failed("omg!")): Result[Char]
            ))
        }
    }
}

import jsonStructureCustomizedCoderTestFixture.CustomizedCoderStructure

class jsonStructureCustomizedCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        Customized structure coders
            should encode correctly $encodeCase
            should decode correctly $decodeCase
            round trip a value $trivialCase
    """

    lazy val coder = JsonCoder[CustomizedCoderStructure]

    def encodeCase = coder.encode.toString(CustomizedCoderStructure(1, 'b', 'C')) ==== Okay(""" {"a":1,"b":"foob","c":"C"} """.trim)
    def decodeCase = decode(coder.decode)(""" {"a":1,"b":"foob","c":"C"} """.trim) ==== Okay(CustomizedCoderStructure(1, 'b', 'C'))
    def trivialCase = prop { (a: Int, b: Char, c: Char) =>
        val ccs = CustomizedCoderStructure(a, b, c)
        (coder.encode.toString(ccs) >>= decode(coder.decode)) ==== Okay(ccs)
    }
}

object jsonStructureCustomizedEncoderTestFixture {
    import scalar.charJsonCoder // implicitly the default for Char
    import scalar.intJsonCoder

    case class CustomizedEncoderStructure(a: Int, b: Char, c: Char)
    object CustomizedEncoderStructure {
        @derive.structure.customizedEncoder[CustomizedEncoderStructure]
        implicit object jsonEncoder {
            val bEncoder = scalar.stringJsonCoder.encode.mapKleisli((c: Char) => Okay("foo" + c))
        }
    }
}

import jsonStructureCustomizedEncoderTestFixture.CustomizedEncoderStructure

class jsonStructureCustomizedEncoderTest extends SpecificationWithJUnit with JsonMatchers {
    def is = s2"""
        Customized structure encoders
            should encode correctly $encodeCase
    """

    lazy val encode = JsonEncoder[CustomizedEncoderStructure]

    def encodeCase = encode.toString(CustomizedEncoderStructure(1, 'b', 'C')) ==== Okay(""" {"a":1,"b":"foob","c":"C"} """.trim)
}

object jsonStructureCustomizedDecoderTestFixture {
    import scalar.charJsonCoder // implicitly the default for Char
    import scalar.intJsonCoder

    case class CustomizedDecoderStructure(a: Int, b: Char, c: Char)
    object CustomizedDecoderStructure {
        @derive.structure.customizedDecoder[CustomizedDecoderStructure]
        implicit object jsonDecoder {
            val bDecoder = scalar.stringJsonCoder.decode.mapKleisli(s => (if (s.startsWith("foo")) Okay(s.charAt(3)) else Failed("omg!")))

        }
    }
}

import jsonStructureCustomizedDecoderTestFixture.CustomizedDecoderStructure

class jsonStructureCustomizedDecoderTest extends SpecificationWithJUnit with JsonMatchers {
    def is = s2"""
        Customized structure coders
            should decode correctly $decodeCase
    """

    lazy val decoder = JsonDecoder[CustomizedDecoderStructure]

    def decodeCase = decode(decoder)("""{"a":1,"b":"foob","c":"C"}""") ==== Okay(CustomizedDecoderStructure(1, 'b', 'C'))
}

object jsonStructureDefaultingFixture {
    import scalar.intJsonCoder

    case class DefaultingStructure1(a: Int, @default(5 + 2) b: Int)
    object DefaultingStructure1 {
        implicit val jsonCoder: JsonCoder[DefaultingStructure1] = derive.structure.coder[DefaultingStructure1]
        implicit val arb = Arbitrary(for { i <- arbitrary[Int]; j <- arbitrary[Int] } yield DefaultingStructure1(i, j))
    }

    case class DefaultingStructure2(a: Int, b: Int = 5 + 2)
    object DefaultingStructure2 {
        implicit val jsonCoder: JsonCoder[DefaultingStructure2] = derive.structure.coder[DefaultingStructure2]
        implicit val arb = Arbitrary(for { i <- arbitrary[Int]; j <- arbitrary[Int] } yield DefaultingStructure2(i, j))
    }

    case class DefaultingStructure3(a: Int, @default(5 + 2) b: Int = 2)
    object DefaultingStructure3 {
        implicit val jsonCoder: JsonCoder[DefaultingStructure3] = derive.structure.coder[DefaultingStructure3]
        implicit val arb = Arbitrary(for { i <- arbitrary[Int]; j <- arbitrary[Int] } yield DefaultingStructure3(i, j))
    }
}

import jsonStructureDefaultingFixture.{DefaultingStructure1, DefaultingStructure2, DefaultingStructure3}

class jsonStructureDefaultingTest extends SpecificationWithJUnit with ScalaCheck with StructureJsonMatchers {
    def is = s2"""
        Structure annotated with @default or default arguments to the constructor
            must substitute the default value correctly when using @default $missingCaseWithAnnotation
            must round trip correctly when using @default $trivialCaseWithAnnotation
            must round trip correctly when using default arguments $trivialCaseWithDefaultArgument
            must substitute the default value correctly when using default arguments and @default $missingCaseWithAnnotationAndArgument
            must round trip correctly when using default arguments and @default $trivialCaseWithAnnotationAndArgument
    """

    lazy val coder1 = DefaultingStructure1.jsonCoder
    lazy val coder2 = DefaultingStructure2.jsonCoder
    lazy val coder3 = DefaultingStructure3.jsonCoder

    def missingCaseWithAnnotation = prop { (i: Int) =>
        decode(coder1.decode)(s""" {"a":$i} """) ==== Okay(DefaultingStructure1(i, 7))
    }

    def trivialCaseWithAnnotation = prop { (ds1: DefaultingStructure1) =>
        (coder1.encode.toString(ds1) >>= decode(coder1.decode)) ==== Okay(ds1)
    }

    /*
     * 2014-08-25 RMM: I can't seem to support this because the macro / reflection API doesn't appear to have a way to recover the default
     *                 argument, only that a default argument exists.
     */
    def missingCaseWithDefaultArgument = prop { (i: Int) =>
        decode(coder2.decode)(s""" {"a":$i} """) ==== Okay(DefaultingStructure2(i, 7))
    }

    def trivialCaseWithDefaultArgument = prop { (ds2: DefaultingStructure2) =>
        (coder2.encode.toString(ds2) >>= decode(coder2.decode)) ==== Okay(ds2)
    }

    def missingCaseWithAnnotationAndArgument = prop { (i: Int) =>
        decode(coder3.decode)(s""" {"a":$i} """) ==== Okay(DefaultingStructure3(i, 7))
    }

    def trivialCaseWithAnnotationAndArgument = prop { (ds3: DefaultingStructure3) =>
        (coder3.encode.toString(ds3) >>= decode(coder3.decode)) ==== Okay(ds3)
    }
}

object jsonStructureFlatteningTest {
    import scalar.{intJsonCoder, stringJsonCoder}

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

class jsonStructureFlatteningTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import jsonStructureFlatteningTest._

    def is = s2"""
        JSON @flatten fields
            should encode correctly $encodeCase
            should decode correctly $decodeCase
    """

    lazy val coder = JsonCoder[StructureWithFlattening]

    def encodeCase = prop { (swf: StructureWithFlattening) =>
        coder.encode.toString(swf) ==== Okay(s""" {"a":${swf.f.a},"b":${encodeString(swf.f.b)},"o":${swf.o},"p":${encodeString(swf.p)}} """.trim)
    }
    def decodeCase = prop { (swf: StructureWithFlattening) =>
        decode(coder.decode)(s""" {"a":${swf.f.a},"b":${encodeString(swf.f.b)},"o":${swf.o},"p":${encodeString(swf.p)}} """.trim) ==== Okay(swf)
    }
}

