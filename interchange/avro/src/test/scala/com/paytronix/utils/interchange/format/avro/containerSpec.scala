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
import java.util.Arrays
import scala.collection.JavaConverters.{asJavaCollectionConverter, asScalaBufferConverter, mapAsJavaMapConverter, mapAsScalaMapConverter}

import org.apache.avro.{Schema, io}
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}
import org.specs2.matcher.Matcher

import com.paytronix.utils.interchange.base.{InsecureContext, Receiver}
import com.paytronix.utils.interchange.format.string
import com.paytronix.utils.scala.result.{FailedException, FailedG, FailedParameterDefault, Okay, Result, ResultG}

import Arbitrary.arbitrary
import utils.makeField

class nullableAvroCoderTest extends SpecificationWithJUnit with AvroMatchers {
    def is = s2"""
        `nullableAvroCoder`
            must have correct schema (UNION of null and whatever) $eschema
            encode null correctly $enull
            encode non-null correctly $enonnull
            default to null $edefault
            allow defaulting to null $evaliddefault
            refuse to default to something other than null $einvaliddefault
    """

    val coder = container.nullableAvroCoder(scalar.stringAvroCoder)
    def eschema =
        (coder.schema.getType ==== Schema.Type.UNION) and
        (coder.schema.getTypes.asScala.toList must beLike { case List(nullSchema, stringSchema) =>
            (nullSchema.getType ==== Schema.Type.NULL) and (stringSchema.getType ==== Schema.Type.STRING)
        })
    def enull = coder.encode.toBytes(null: String) must beLike { case Okay(a) => a must beAvroInt(0) }
    def enonnull = coder.encode.toBytes("test") must beLike { case Okay(a) =>
        (a.slice(0, 1) must beAvroInt(1)) and (a.drop(1) must beAvroString("test"))
    }
    def edefault = decodeDefault(coder) ==== Okay(null: String)
    def evaliddefault = coder.encode.encodeDefaultJson(null) must beLike { case Okay(_) => ok }
    def einvaliddefault = coder.encode.encodeDefaultJson("test") must beLike { case FailedG(_, _) => ok }
}

class optionAvroCoderTest extends SpecificationWithJUnit with AvroMatchers {
    def is = s2"""
        `optionAvroCoder`
            must have correct schema (UNION of null and whatever) $eschema
            encode None correctly $enone
            encode Some correctly $esome
            default to null $edefault
            allow defaulting to null $evaliddefault
            refuse to default to something other than null $einvaliddefault
            must be the implicit coder for Option[…] $eimplicit
            work when containing a unit $eunit
            work when containing another option $enested
    """

    val coder = container.optionAvroCoder(scalar.stringAvroCoder)
    def eschema =
        (coder.schema.getType ==== Schema.Type.UNION) and
        (coder.schema.getTypes.asScala.toList must beLike { case List(nullSchema, stringSchema) =>
            (nullSchema.getType ==== Schema.Type.NULL) and (stringSchema.getType ==== Schema.Type.STRING)
        })
    def enone = coder.encode.toBytes(None) must beLike { case Okay(a) => a must beAvroInt(0) }
    def esome = coder.encode.toBytes(Some("test")) must beLike { case Okay(a) =>
        (a.slice(0, 1) must beAvroInt(1)) and (a.drop(1) must beAvroString("test"))
    }
    def edefault = decodeDefault(coder) ==== Okay(None: Option[String])
    def evaliddefault = coder.encode.encodeDefaultJson(None) must beLike { case Okay(_) => ok }
    def einvaliddefault = coder.encode.encodeDefaultJson(Some("test")) must beLike { case FailedG(_, _) => ok }
    def eimplicit = { import coders._; AvroCoder[Option[Unit]].encode.getClass must_== coder.encode.getClass }
    def eunit = {
        // this special case exists because if you encode () as NULL (which seems like a natural choice) then a union [null, null] will be
        // attempted and rejected
        import scalar.unitAvroCoder
        val c = container.optionAvroCoder[Unit]
        ((c.encode.toBytes(None) >>= c.decode.fromBytes(c.schema)) ==== Okay(None)).updateMessage("round-trip None: " + _) and
        ((c.encode.toBytes(Some(())) >>= c.decode.fromBytes(c.schema)) ==== Okay(Some(()))).updateMessage("round-trip Some(()): " + _)
    }
    def enested = {
        // this special case exists because Avro doesn't like unions of unions, so the option coder has to automatically switch to a record
        // on the Some case instead of just using the underlying value schema
        val inner = container.optionAvroCoder(scalar.intAvroCoder.encode, scalar.intAvroCoder.decode)
        val outer = container.optionAvroCoder(inner.encode, inner.decode)
        ((outer.encode.toBytes(None) >>= outer.decode.fromBytes(outer.schema)) ==== Okay(None)).updateMessage("round-trip None: " + _) and
        ((outer.encode.toBytes(Some(None)) >>= outer.decode.fromBytes(outer.schema)) ==== Okay(Some(None))).updateMessage("round-trip Some(None): " + _) and
        ((outer.encode.toBytes(Some(Some(1))) >>= outer.decode.fromBytes(outer.schema)) ==== Okay(Some(Some(1)))).updateMessage("round-trip Some(Some(1)): " + _)
    }
}

class eitherAvroCoderTest extends SpecificationWithJUnit with AvroMatchers with ScalaCheck {
    def is = s2"""
        `eitherAvroCoder`
            must have correct schema (UNION of Left and Right records) $eschema
            encode Left correctly $eleft
            encode Right correctly $eright
            default to null $edefault
            allow defaulting to null $evaliddefault
            refuse to default to something other than null $einvaliddefault
            must be the implicit coder for Either[…, …] $eimplicit
    """

    val coder = container.eitherAvroCoder(scalar.intAvroCoder, scalar.stringAvroCoder)
    def eschema =
        (coder.schema.getType ==== Schema.Type.UNION) and
        (coder.schema.getTypes.asScala.toList must beLike { case List(leftSchema, rightSchema) =>
            (leftSchema.getType ==== Schema.Type.RECORD).updateMessage("left schema type: " + _) and
            (leftSchema.getFields.size ==== 1).updateMessage("left schema field count: " + _) and
            (leftSchema.getFields.get(0).schema.getType ==== Schema.Type.INT).updateMessage("left schema field type: " + _) and
            (rightSchema.getType ==== Schema.Type.RECORD).updateMessage("right schema type: " + _) and
            (rightSchema.getFields.size ==== 1).updateMessage("right schema field count: " + _) and
            (rightSchema.getFields.get(0).schema.getType ==== Schema.Type.STRING).updateMessage("right schema field type: " + _)
        })
    def eleft = prop { (i: Int) =>
        coder.encode.toBytes(Left(i)) must beLike { case Okay(a) =>
            (a.slice(0, 1) must beAvroInt(0)) and (a.drop(1) must beAvroInt(i))
        }
    }
    def eright = prop { (s: String) =>
        coder.encode.toBytes(Right(s)) must beLike { case Okay(a) =>
            (a.slice(0, 1) must beAvroInt(1)) and (a.drop(1) must beAvroString(s))
        }
    }
    def edefault = prop { (i: Int) => decodeDefault(coder.default(Left(i))) ==== Okay(Left(i)) }
    def evaliddefault = coder.encode.encodeDefaultJson(Left(0)) must beLike { case Okay(_) => ok }
    def einvaliddefault = coder.encode.encodeDefaultJson(Right("test")) must beLike { case FailedG(_, _) => ok }
    def eimplicit = { import coders._; AvroCoder[Either[Unit, Unit]].encode.getClass must_== coder.encode.getClass }
}

class resultCoderTest extends SpecificationWithJUnit with AvroMatchers {
    def is = s2"""
        `resultGAvroCoder`
            must have correct schema (UNION of missing, okay, failednull and whatever) $eschema
            encode FailedG correctly $eencodefailed
            encode FailedG with a cause correctly $eencodefailedwithcause
            decode FailedG correctly $edecodefailed
            decode FailedG with a cause correctly $edecodefailedwithcause
            not carry the stack trace over $enostacktrace
            carry a null throwable message over $enullthrowablemessage
            encode Okay correctly $eencodeokay
            decode Okay correctly $edecodeokay
            default to missing correctly $edefault
            allow defaulting to Failed $evaliddefault
            refuse to default to something other than Failed $einvaliddefault
            must be the implicit coder for ResultG[…, …] $eimplicit
    """

    // FIXME should be a test that ensures the throwable reconstruction uses the InterchangeClassLoader

    val failedWithoutCause: ResultG[Int, String] = FailedG(new RuntimeException("test"), 123)
    val failedWithCause: ResultG[Int, String] = FailedG(new RuntimeException("test", new RuntimeException("test2")), 123)

    implicit val defaultInt = FailedParameterDefault.value(-123)
    val coder = container.resultGAvroCoder(scalar.intAvroCoder, scalar.stringAvroCoder)

    def eschema =
        (coder.schema.getType ==== Schema.Type.UNION) and
        (coder.schema.getTypes.asScala.toList must beLike { case List(missingSchema, okaySchema, failedSchema) =>
            (missingSchema.getType ==== Schema.Type.RECORD) and
            (okaySchema.getType ==== Schema.Type.RECORD) and
            (failedSchema.getType ==== Schema.Type.RECORD)
        })
    def eencodefailed =
        coder.encode.toBytes(failedWithoutCause) must beLike { case Okay(a) =>
            val bb = ByteBuffer.wrap(a)
            def read(n: Int): Array[Byte] = { val b = Array.ofDim[Byte](n); bb.get(b); b }
            (read(1) must beAvroInt(2)).updateMessage("union discriminator: " + _) and
            (read(27) must beAvroString("java.lang.RuntimeException")).updateMessage("throwable class: " + _) and
            (read(1) must beAvroInt(1)).updateMessage("throwable message null bit: " + _) and
            (read(5) must beAvroString("test")).updateMessage("throwable message: " + _) and
            (read(1) must beAvroInt(0)).updateMessage("throwable cause bit: " + _) and
            (read(bb.remaining) must beAvroInt(123)).updateMessage("failed param: " + _)
        }
    def eencodefailedwithcause =
        coder.encode.toBytes(failedWithCause) must beLike { case Okay(a) =>
            val bb = ByteBuffer.wrap(a)
            def read(n: Int): Array[Byte] = { val b = Array.ofDim[Byte](n); bb.get(b); b }
            (read(1) must beAvroInt(2)).updateMessage("union discriminator: " + _) and
            (read(27) must beAvroString("java.lang.RuntimeException")).updateMessage("outer throwable class: " + _) and
            (read(1) must beAvroInt(1)).updateMessage("outer throwable message null bit: " + _) and
            (read(5) must beAvroString("test")).updateMessage("outer throwable message: " + _) and
            (read(1) must beAvroInt(1)).updateMessage("outer throwable cause bit: " + _) and
            (read(27) must beAvroString("java.lang.RuntimeException")).updateMessage("inner throwable class: " + _) and
            (read(1) must beAvroInt(1)).updateMessage("inner throwable message null bit: " + _) and
            (read(6) must beAvroString("test2")).updateMessage("inner throwable message: " + _) and
            (read(1) must beAvroInt(0)).updateMessage("inner throwable cause bit: " + _) and
            (read(bb.remaining) must beAvroInt(123))
        }
    def edecodefailed =
        (coder.encode.toBytes(failedWithoutCause) >>= coder.decode.fromBytes(coder.schema)) must beLike {
            case Okay(FailedG(throwable, 123)) => throwable.getMessage ==== "test"
        }
    def edecodefailedwithcause =
        (coder.encode.toBytes(failedWithCause) >>= coder.decode.fromBytes(coder.schema)) must beLike {
            case Okay(FailedG(throwable, 123)) =>
                (throwable.getMessage ==== "test") and
                (throwable.getCause must not beNull) and
                (throwable.getCause.getMessage ==== "test2") and
                (throwable.getCause.getCause must beNull)
        }
    def enostacktrace =
        (coder.encode.toBytes(failedWithoutCause) >>= coder.decode.fromBytes(coder.schema)) must beLike {
            case Okay(FailedG(throwable, _)) =>
                (throwable.getStackTrace ==== throwable.getStackTrace) and // to ensure equality comparison is sane
                (throwable.getStackTrace !== failedWithoutCause.asFailed.throwable.getStackTrace)
        }
    def enullthrowablemessage =
        (coder.encode.toBytes(FailedG(new RuntimeException(null: String), 0)) >>= coder.decode.fromBytes(coder.schema)) must beLike {
            case Okay(FailedG(throwable, _)) => throwable.getMessage must beNull
        }
    def eencodeokay = coder.encode.toBytes(Okay("test")) must beLike { case Okay(a) =>
        val bb = ByteBuffer.wrap(a)
        def read(n: Int): Array[Byte] = { val b = Array.ofDim[Byte](n); bb.get(b); b }
        (read(1) must beAvroInt(1)).updateMessage("union discriminator: " + _) and
        (read(bb.remaining) must beAvroString("test")).updateMessage("okay value: " + _)
    }
    def edecodeokay = (coder.encode.toBytes(Okay("test")) >>= coder.decode.fromBytes(coder.schema)) ==== Okay(Okay("test"))
    def edefault = decodeDefault(coder) must beLike {
        case Okay(FailedG(throwable, param)) =>
            (throwable.getMessage ==== "unknown failure") and
            (throwable.getClass must_== classOf[FailedException]) and
            (param ==== -123)
    }
    def evaliddefault = coder.encode.encodeDefaultJson(failedWithoutCause) must beLike { case Okay(_) => ok }
    def einvaliddefault = coder.encode.encodeDefaultJson(Okay("test")) must beLike { case FailedG(_, _) => ok }
    def eimplicit = { import coders._; AvroCoder[ResultG[Unit, Unit]].encode.getClass must_== coder.encode.getClass }
}

class insecureAvroCoderTest extends SpecificationWithJUnit with AvroMatchers {
    def is = s2"""
        `insecureAvroCoder`
            must have the same schema as the underlying coder $eschema
            must encode the same as the underlying coder when in a secure context $esecureencode
            must encode to the default value when in an insecure context $einsecureencode
            must decode the input value when in a secure context $esecuredecode
            must decode the default value when in an insecure context $einsecuredecode
    """

    val coder = container.insecureAvroCoder(scalar.stringAvroCoder, "insecure")

    def eschema = coder.schema.toString ==== scalar.stringAvroCoder.schema.toString
    def esecureencode = InsecureContext.doWith(false) {
        coder.encode.toBytes("test") must beLike { case Okay(a) => a must beAvroString("test") }
    }
    def einsecureencode = InsecureContext.doWith(true) {
        coder.encode.toBytes("test") must beLike { case Okay(a) => a must beAvroString("insecure") }
    }
    def esecuredecode = InsecureContext.doWith(false) {
        coder.decode.fromBytes(coder.schema)(makeAvroString("test")) ==== Okay("test")
    }
    def einsecuredecode = InsecureContext.doWith(true) {
        coder.decode.fromBytes(coder.schema)(makeAvroString("test")) ==== Okay("insecure")
    }
}

abstract class avroArrayCoderSpecBase[S] extends SpecificationWithJUnit with AvroMatchers {
    def toIterable(s: S): Iterable[Int]
    def beCorrectSchema: Matcher[Schema] = beLike { case schema: Schema =>
        (schema.getType ==== Schema.Type.ARRAY) and (schema.getElementType.toString ==== scalar.intAvroCoder.schema.toString)
    }
    def encode(s: S): Array[Byte] = makeAvroArray(toIterable(s).toSeq.map { i => zigZagEncode(i) })
    def encode(s: S, bs: Int): Array[Byte] = makeAvroArray(toIterable(s).toSeq.map { i => zigZagEncode(i) }, blockSize=Some(bs))
    def beValidEncodingOf(s: S): Matcher[Result[Array[Byte]]] = beLike { case Okay(a) => a must beEqualToArray(encode(s)) }
}

class scalaListCodingTest extends avroArrayCoderSpecBase[List[Int]] with ScalaCheck {
    def is = s2"""
        Coding of List
            must have the correct schema $eschema
            encode lists correctly $eencode
            decode lists correctly $edecode
            decode blocked lists correctly $edecodeblocked
            decode defaults correctly $edefault
            must be the implicit coder for List[…] $eimplicit
    """

    def toIterable(l: List[Int]) = l
    val coder: AvroCoder[List[Int]] = container.avroArrayCoder(scalar.intAvroCoder)
    val gen100Ints = Gen.listOfN(100, arbitrary[Int])

    def eschema = coder.schema must beCorrectSchema
    def eencode = prop { (l: List[Int]) => coder.encode.toBytes(l) must beValidEncodingOf(l) }
    def edecode = prop { (l: List[Int]) => coder.decode.fromBytes(coder.schema)(encode(l)) ==== Okay(l) }
    def edecodeblocked = Prop.forAll(Gen.choose(1, 100)) { blockSize =>
        val l = gen100Ints.sample.get
        coder.decode.fromBytes(coder.schema)(encode(l, blockSize)) ==== Okay(l)
    }
    def edefault = prop { (l: List[Int]) => decodeDefault(coder.default(l)) ==== Okay(l) }
    def eimplicit = { import coders._; AvroCoder[List[Unit]].encode.getClass must_== coder.encode.getClass }
}

class scalaSetCodingTest extends avroArrayCoderSpecBase[Set[Int]] with ScalaCheck {
    def is = s2"""
        Coding of Set
            must have the correct schema $eschema
            encode lists correctly $eencode
            decode lists correctly $edecode
            decode blocked lists correctly $edecodeblocked
            decode defaults correctly $edefault
            must be the implicit coder for Set[…] $eimplicit
    """

    def toIterable(s: Set[Int]) = s
    val coder: AvroCoder[Set[Int]] = container.avroArrayCoder(scalar.intAvroCoder)
    val gen100Ints = Gen.listOfN(100, arbitrary[Int]).map(Set.empty ++ _)

    def eschema = coder.schema must beCorrectSchema
    def eencode = prop { (s: Set[Int]) => coder.encode.toBytes(s) must beValidEncodingOf(s) }
    def edecode = prop { (s: Set[Int]) => coder.decode.fromBytes(coder.schema)(encode(s)) ==== Okay(s) }
    def edecodeblocked = Prop.forAll(Gen.choose(1, 100)) { blockSize =>
        val s = gen100Ints.sample.get
        coder.decode.fromBytes(coder.schema)(encode(s, blockSize)) ==== Okay(s)
    }
    def edefault = prop { (s: Set[Int]) => decodeDefault(coder.default(s)) ==== Okay(s) }
    def eimplicit = { import coders._; AvroCoder[Set[Unit]].encode.getClass must_== coder.encode.getClass }
}

class javaListCodingTest extends avroArrayCoderSpecBase[java.util.List[Int]] with ScalaCheck {
    def is = s2"""
        Coding of java.util.List
            must have the correct schema $eschema
            encode lists correctly $eencode
            decode lists correctly $edecode
            decode blocked lists correctly $edecodeblocked
            decode defaults correctly $edefault
            must be the implicit coder for java.util.List[…] $eimplicit
    """

    def toIterable(l: java.util.List[Int]) = l.asScala
    val coder = container.javaListAvroCoder(scalar.intAvroCoder)
    implicit val arbJavaList: Arbitrary[java.util.List[Int]] =
        Arbitrary(arbitrary[List[Int]].map(l => new java.util.ArrayList(l.asJavaCollection)))

    val gen100Ints = Gen.listOfN(100, arbitrary[Int]).map(l => new java.util.ArrayList(l.asJavaCollection))

    def eschema = coder.schema must beCorrectSchema
    def eencode = prop { (l: java.util.List[Int]) => coder.encode.toBytes(l) must beValidEncodingOf(l) }
    def edecode = prop { (l: java.util.List[Int]) => coder.decode.fromBytes(coder.schema)(encode(l)) ==== Okay(l) }
    def edecodeblocked = Prop.forAll(Gen.choose(1, 100)) { blockSize =>
        val l = gen100Ints.sample.get
        coder.decode.fromBytes(coder.schema)(encode(l, blockSize)) ==== Okay(l)
    }
    def edefault = prop { (l: java.util.List[Int]) => decodeDefault(coder.default(l)) ==== Okay(l) }
    def eimplicit = { import coders._; AvroCoder[java.util.List[Unit]].encode.getClass must_== coder.encode.getClass }
}

final case class TestKey(a: Int, b: Int)
object TestKey {
    implicit lazy val avroCoder: AvroCoder[TestKey] = AvroCoder.make(encoder, decoder)
    val testKeySchema = {
        val s = Schema.createRecord("TestKey", "", "", false)
        s.setFields(Arrays.asList(makeField("a", scalar.intAvroCoder.schema, None),
            makeField("b", scalar.intAvroCoder.schema, None)))
        s
    }

    lazy val encoder: AvroEncoder[TestKey] = new AvroEncoder[TestKey] {
        val schema = testKeySchema
        val defaultJson = None
        def encodeDefaultJson(in: TestKey) = {
            val obj = jsonNodeFactory.objectNode
            obj.put("a", jsonNodeFactory.numberNode(in.a))
            obj.put("b", jsonNodeFactory.numberNode(in.b))

            Okay(obj)
        }
        def run(in: TestKey, out: io.Encoder) =
            scalar.intAvroCoder.encode.run(in.a, out) >> scalar.intAvroCoder.encode.run(in.b, out)
    }

    lazy val decoder: AvroDecoder[TestKey] = new AvroDecoder[TestKey] {
        val schema = testKeySchema
        val defaultJson = None
        def run(in: io.ResolvingDecoder, out: Receiver[TestKey]) = {
            val ar = new Receiver[Int]
            val br = new Receiver[Int]
            in.readFieldOrder
            scalar.intAvroCoder.decode.run(in, ar) >> scalar.intAvroCoder.decode.run(in, br) >> out(TestKey(ar.value, br.value))
        }
    }

    implicit lazy val arb = Arbitrary(Gen.resultOf(TestKey.apply _))
}

abstract class avroAssocArrayCoderSpecBase[S] extends SpecificationWithJUnit with AvroMatchers {
    def toIterable(s: S): Iterable[(TestKey, String)]
    def beCorrectSchema: Matcher[Schema] = beLike { case schema: Schema =>
        (schema.getType ==== Schema.Type.ARRAY) and
        (schema.getElementType.getType ==== Schema.Type.RECORD) and
        (schema.getElementType.getFields.size ==== 2) and
        (schema.getElementType.getFields.get(0).schema.toString ==== TestKey.testKeySchema.toString) and
        (schema.getElementType.getFields.get(1).schema.toString ==== scalar.stringAvroCoder.schema.toString)
    }
    def encodePair(k: TestKey, v: String): Array[Byte] = {
        val keyABytes = zigZagEncode(k.a)
        val keyBBytes = zigZagEncode(k.b)
        val valueBytes = makeAvroString(v)
        val bb = ByteBuffer.allocate(keyABytes.length + keyBBytes.length + valueBytes.length)
        bb.put(keyABytes)
        bb.put(keyBBytes)
        bb.put(valueBytes)
        bb.array
    }
    def encode(s: S): Array[Byte] = makeAvroArray(toIterable(s).toSeq.map(Function.tupled(encodePair)))
    def encode(s: S, bs: Int): Array[Byte] = makeAvroArray(toIterable(s).toSeq.map(Function.tupled(encodePair)), blockSize=Some(bs))
    def beValidEncodingOf(s: S): Matcher[Result[Array[Byte]]] = beLike { case Okay(a) => a must beEqualToArray(encode(s)) }
}

class scalaMapAssocCodingTest extends avroAssocArrayCoderSpecBase[Map[TestKey, String]] with ScalaCheck {
    def is = s2"""
        Coding of Map with a non-string key
            must have correct schema $eschema
            encode maps correctly $eencode
            decode maps correctly $edecode
            decode blocked lists correctly $edecodeblocked
            decode defaults correctly $edefault
            must be the implicit coder for Map[… non-string …,…] $eimplicit
    """

    def toIterable(m: Map[TestKey, String]) = m
    val coder: AvroCoder[Map[TestKey, String]] = container.avroAssocArrayCoder(TestKey.avroCoder, scalar.stringAvroCoder)

    val gen100Pairs = Gen.listOfN(100, arbitrary[(TestKey, String)]).map(Map.empty ++ _)

    def eschema = coder.schema must beCorrectSchema
    def eencode = prop { (m: Map[TestKey, String]) => coder.encode.toBytes(m) must beValidEncodingOf(m) }
    def edecode = prop { (m: Map[TestKey, String]) => coder.decode.fromBytes(coder.schema)(encode(m)) ==== Okay(m) }
    def edecodeblocked = Prop.forAll(Gen.choose(1, 100)) { blockSize =>
        val m = Map.empty ++ gen100Pairs.sample.get
        coder.decode.fromBytes(coder.schema)(encode(m, blockSize)) ==== Okay(m)
    }
    def edefault = prop { (m: Map[TestKey, String]) => decodeDefault(coder.default(m)) ==== Okay(m) }
    def eimplicit = { import coders._; AvroCoder[Map[TestKey, Unit]].encode.getClass must_== coder.encode.getClass }
}

class javaMapAssocCodingTest extends avroAssocArrayCoderSpecBase[java.util.Map[TestKey, String]] with ScalaCheck {
    def is = s2"""
        Coding of a java.util.Map with a non-string key
            must have correct schema $eschema
            encode maps correctly $eencode
            decode maps correctly $edecode
            decode blocked lists correctly $edecodeblocked
            decode defaults correctly $edefault
            must be the implicit coder for Map[… non-string …,…] $eimplicit
    """


    def toIterable(m: java.util.Map[TestKey, String]) = m.asScala
    val coder = container.javaMapAvroCoder(TestKey.avroCoder, scalar.stringAvroCoder)

    val gen100Pairs = Gen.listOfN(100, arbitrary[(TestKey, String)]).map(Map.empty ++ _)
    implicit val arbJavaMap = Arbitrary(arbitrary[Map[TestKey, String]].map(_.asJava))

    def eschema = coder.schema must beCorrectSchema
    def eencode = prop { (m: java.util.Map[TestKey, String]) => coder.encode.toBytes(m) must beValidEncodingOf(m) }
    def edecode = prop { (m: java.util.Map[TestKey, String]) => coder.decode.fromBytes(coder.schema)(encode(m)) ==== Okay(m) }
    def edecodeblocked = Prop.forAll(Gen.choose(1, 100)) { blockSize =>
        val m = (Map.empty ++ gen100Pairs.sample.get).asJava
        coder.decode.fromBytes(coder.schema)(encode(m, blockSize)) ==== Okay(m)
    }
    def edefault = prop { (m: java.util.Map[TestKey, String]) => decodeDefault(coder.default(m)) ==== Okay(m) }
    def eimplicit = { import coders._; AvroCoder[java.util.Map[TestKey, Unit]].encode.getClass must_== coder.encode.getClass }
}

abstract class avroMapCoderSpecBase[S] extends SpecificationWithJUnit with AvroMatchers {
    def toIterable(s: S): Iterable[(Int, String)]
    def beCorrectSchema: Matcher[Schema] = beLike { case schema: Schema =>
        (schema.getType ==== Schema.Type.MAP) and
        (schema.getValueType.getType ==== Schema.Type.STRING)
    }
    def encodePair(k: Int, v: String): Array[Byte] = {
        val keyBytes = makeAvroString(k.toString)
        val valueBytes = makeAvroString(v)
        val bb = ByteBuffer.allocate(keyBytes.length + valueBytes.length)
        bb.put(keyBytes)
        bb.put(valueBytes)
        bb.array
    }
    def encode(s: S): Array[Byte] = makeAvroArray(toIterable(s).toSeq.map(Function.tupled(encodePair)))
    def encode(s: S, bs: Int): Array[Byte] = makeAvroArray(toIterable(s).toSeq.map(Function.tupled(encodePair)), blockSize=Some(bs))
    def beValidEncodingOf(s: S): Matcher[Result[Array[Byte]]] = beLike { case Okay(a) => a must beEqualToArray(encode(s)) }
}

class scalaMapStringCodingTest extends avroMapCoderSpecBase[Map[Int, String]] with ScalaCheck {
    def is = s2"""
        Coding of Map with a stringable key
            must have correct schema $eschema
            encode maps correctly $eencode
            decode maps correctly $edecode
            decode blocked lists correctly $edecodeblocked
            decode defaults correctly $edefault
            must be the implicit coder for Map[… stringy …,…] $eimplicit
    """

    def toIterable(m: Map[Int, String]) = m
    val coder: AvroCoder[Map[Int, String]] = container.avroMapCoder(string.scalar.intStringCoder, scalar.stringAvroCoder)

    val gen100Pairs = Gen.listOfN(100, arbitrary[(Int, String)]).map(Map.empty ++ _)

    def eschema = coder.schema must beCorrectSchema
    def eencode = prop { (m: Map[Int, String]) => coder.encode.toBytes(m) must beValidEncodingOf(m) }
    def edecode = prop { (m: Map[Int, String]) => coder.decode.fromBytes(coder.schema)(encode(m)) ==== Okay(m) }
    def edecodeblocked = Prop.forAll(Gen.choose(1, 100)) { blockSize =>
        val m = Map.empty ++ gen100Pairs.sample.get
        coder.decode.fromBytes(coder.schema)(encode(m, blockSize)) ==== Okay(m)
    }
    def edefault = prop { (m: Map[Int, String]) => decodeDefault(coder.default(m)) ==== Okay(m) }
    def eimplicit = { import coders._; import string.coders._; AvroCoder[Map[Int, Unit]].encode.getClass must_== coder.encode.getClass }
}

class javaMapStringCodingTest extends avroMapCoderSpecBase[java.util.Map[Int, String]] with ScalaCheck {
    def is = s2"""
        Coding of a java.util.Map with a stringable key
            must have correct schema $eschema
            encode maps correctly $eencode
            decode maps correctly $edecode
            decode blocked lists correctly $edecodeblocked
            decode defaults correctly $edefault
            must be the implicit coder for Map[… stringy …,…] $eimplicit
    """


    def toIterable(m: java.util.Map[Int, String]) = m.asScala
    val coder = container.javaStringKeyedMapAvroCoder(string.scalar.intStringCoder, scalar.stringAvroCoder)

    val gen100Pairs = Gen.listOfN(100, arbitrary[(Int, String)]).map(Map.empty ++ _)
    implicit val arbJavaMap = Arbitrary(arbitrary[Map[Int, String]].map(_.asJava))

    def eschema = coder.schema must beCorrectSchema
    def eencode = prop { (m: java.util.Map[Int, String]) => coder.encode.toBytes(m) must beValidEncodingOf(m) }
    def edecode = prop { (m: java.util.Map[Int, String]) => coder.decode.fromBytes(coder.schema)(encode(m)) ==== Okay(m) }
    def edecodeblocked = Prop.forAll(Gen.choose(1, 100)) { blockSize =>
        val m = (Map.empty ++ gen100Pairs.sample.get).asJava
        coder.decode.fromBytes(coder.schema)(encode(m, blockSize)) ==== Okay(m)
    }
    def edefault = prop { (m: java.util.Map[Int, String]) => decodeDefault(coder.default(m)) ==== Okay(m) }
    def eimplicit = { import coders._; import string.coders._; AvroCoder[java.util.Map[Int, Unit]].encode.getClass must_== coder.encode.getClass }
}
