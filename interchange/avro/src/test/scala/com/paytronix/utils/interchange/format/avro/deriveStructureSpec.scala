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
import scala.collection.JavaConverters.asScalaBufferConverter

import org.apache.avro.Schema
import org.scalacheck.Arbitrary
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.interchange.base.default
import com.paytronix.utils.interchange.test.fixtures.{BasicClass, CaseClass, POJOClass}
import com.paytronix.utils.scala.result.Okay

import Arbitrary.arbitrary

trait StructureAvroMatchers extends AvroMatchers { self: SpecificationWithJUnit =>
    def pack(cc: CaseClass): Array[Byte] =
        pack(cc.foo, cc.bar, cc.zip)
    def pack(bc: BasicClass): Array[Byte] =
        pack(bc.foo, bc.bar, bc.zip)
    def pack(pc: POJOClass): Array[Byte] =
        pack(pc.getFoo, pc.getBar, pc.getZip)

    def pack(foo: Int, bar: String, zip: Option[String]): Array[Byte] = {
        val fooBytes = zigZagEncode(foo)
        val barDiscrimBytes = zigZagEncode(if (bar != null) 1 else 0)
        val barValueBytes = if (bar != null) makeAvroString(bar) else Array[Byte]()
        val zipDiscrimBytes = zigZagEncode(if (zip.isDefined) 1 else 0)
        val zipValueBytes = zip match { case Some(s) => makeAvroString(s); case None => Array[Byte]() }

        val bb = ByteBuffer.allocate (
            fooBytes.length +
            barDiscrimBytes.length + barValueBytes.length +
            zipDiscrimBytes.length + zipValueBytes.length
        )
        bb.put(barDiscrimBytes)
        bb.put(barValueBytes)
        bb.put(fooBytes)
        bb.put(zipDiscrimBytes)
        bb.put(zipValueBytes)
        bb.array
    }
}

class avroStructureCoderCaseClassTest extends SpecificationWithJUnit with ScalaCheck with StructureAvroMatchers {
    def is = s2"""
        Explicitly derived structure coder for CaseClass
            must have the correct schema $eschema
            encode correctly $eencode
            decode correctly $edecode
            decode defaults correctly $edefault
    """

    import container.optionAvroCoder
    import scalar.{intAvroCoder, stringAvroCoder}

    lazy val coder = derive.structure.coder[CaseClass]

    def eschema = {
        lazy val fields = coder.schema.getFields.asScala

        (coder.schema.getType ==== Schema.Type.RECORD).updateMessage("schema type: " + _) and
        (fields.size ==== 3).updateMessage("schema field count: " + _) and
        (fields must contain(fieldLike("foo", Schema.Type.INT))) and
        (fields must contain(fieldLike("bar", Schema.Type.UNION, Schema.Type.NULL, Schema.Type.STRING))) and
        (fields must contain(fieldLike("zip", Schema.Type.UNION, Schema.Type.NULL, Schema.Type.STRING)))
    }

    def eencode = prop { (cc: CaseClass) =>
        coder.encode.toBytes(cc) must beLike { case Okay(a) =>
            a must beEqualToArray(pack(cc))
        }
    }

    def edecode = prop { (cc: CaseClass) =>
        coder.decode.fromBytes(coder.schema)(pack(cc)) ==== Okay(cc)
    }

    def edefault = prop { (foo: Int) =>
        val cc = CaseClass(foo, null, None)
        decodeDefault(coder.default(cc)) ==== Okay(cc)
    }
}

class avroStructureCoderBasicClassTest extends SpecificationWithJUnit with ScalaCheck with StructureAvroMatchers {
    def is = s2"""
        Explicitly derived structure coder for BasicClass
            must have the correct schema $eschema
            encode correctly $eencode
            decode correctly $edecode
            decode defaults correctly $edefault
    """

    import container.optionAvroCoder
    import scalar.{intAvroCoder, stringAvroCoder}

    lazy val coder = derive.structure.coder[BasicClass]

    def eschema = {
        lazy val fields = coder.schema.getFields.asScala

        (coder.schema.getType ==== Schema.Type.RECORD).updateMessage("schema type: " + _) and
        (fields.size ==== 3).updateMessage("schema field count: " + _) and
        (fields must contain(fieldLike("foo", Schema.Type.INT))) and
        (fields must contain(fieldLike("bar", Schema.Type.UNION, Schema.Type.NULL, Schema.Type.STRING))) and
        (fields must contain(fieldLike("zip", Schema.Type.UNION, Schema.Type.NULL, Schema.Type.STRING)))
    }

    def eencode = prop { (bc: BasicClass) =>
        coder.encode.toBytes(bc) must beLike { case Okay(a) =>
            a must beEqualToArray(pack(bc))
        }
    }

    def edecode = prop { (bc: BasicClass) =>
        coder.decode.fromBytes(coder.schema)(pack(bc)) ==== Okay(bc)
    }

    def edefault = prop { (foo: Int) =>
        val bc = BasicClass.make(foo, null, None)
        decodeDefault(coder.default(bc)) ==== Okay(bc)
    }
}

class avroStructureCoderPOJOClassTest extends SpecificationWithJUnit with ScalaCheck with StructureAvroMatchers {
    def is = s2"""
        Explicitly derived structure coder for POJOClass
            must have the correct schema $eschema
            encode correctly $eencode
            decode correctly $edecode
            decode defaults correctly $edefault
    """

    import container.optionAvroCoder
    import scalar.{intAvroCoder, stringAvroCoder}

    lazy val coder = derive.structure.coder[POJOClass]

    def eschema = {
        lazy val fields = coder.schema.getFields.asScala

        (coder.schema.getType ==== Schema.Type.RECORD).updateMessage("schema type: " + _) and
        (fields.size ==== 3).updateMessage("schema field count: " + _) and
        (fields must contain(fieldLike("foo", Schema.Type.INT))) and
        (fields must contain(fieldLike("bar", Schema.Type.UNION, Schema.Type.NULL, Schema.Type.STRING))) and
        (fields must contain(fieldLike("zip", Schema.Type.UNION, Schema.Type.NULL, Schema.Type.STRING)))
    }

    def eencode = prop { (pc: POJOClass) =>
        coder.encode.toBytes(pc) must beLike { case Okay(a) =>
            a must beEqualToArray(pack(pc))
        }
    }

    def edecode = prop { (pc: POJOClass) =>
        coder.decode.fromBytes(coder.schema)(pack(pc)) ==== Okay(pc)
    }

    def edefault = prop { (foo: Int) =>
        val pc = POJOClass.make(foo, null, None)
        decodeDefault(coder.default(pc)) ==== Okay(pc)
    }
}

/* 2014-08-27 RMM: having multiple annotation macros which addToCompanion causes the compiler to not emit the object class (Blah$) even though
                   it doesn't error at runtime.
object avroStructureImplicitTestFixture {
    import scalar.intAvroCoder

    @derive.structure.implicitCoder
    case class ImplicitlyCodedStructure(a: Int)
    @derive.structure.implicitCoder
    case class ImplicitlyCodedStructure2(a: Int)
    object ImplicitlyCodedStructure2 {
        val existing = "companion"
    }
}

import avroStructureImplicitTestFixture.{ImplicitlyCodedStructure, ImplicitlyCodedStructure2}

class avroStructureImplicitTest extends SpecificationWithJUnit {
    def is = s2"""
        Implicit structure coders
            should be explicitly accessible via companion.avroCoder $eexplicit
            should be explicitly accessible via companion.avroCoder even when companion already exists $eexplicit2
            should be implicitly accessible via materializer $ematerialize
            should be implicitly accessible via materializer even when companion already exists $ematerialize
            should not lose existing companion members $ecompanionpreserved
            round trip a value $etrivial
    """

    def eexplicit = ImplicitlyCodedStructure.avroCoder.schema.getType ==== Schema.Type.RECORD
    def eexplicit2 = ImplicitlyCodedStructure2.avroCoder.schema.getType ==== Schema.Type.RECORD
    def ematerialize = AvroCoder[ImplicitlyCodedStructure] ==== ImplicitlyCodedStructure.avroCoder
    def ematerialize2 = AvroCoder[ImplicitlyCodedStructure2] ==== ImplicitlyCodedStructure2.avroCoder
    def ecompanionpreserved = ImplicitlyCodedStructure2.existing ==== "companion"
    def etrivial = {
        val coder = AvroCoder[ImplicitlyCodedStructure]
        val ics = ImplicitlyCodedStructure(1)
        (coder.encode.toBytes(ics) >>= coder.decode.fromBytes(coder.schema)) ==== Okay(ics)
    }
}
*/

object avroStructureCustomizedCoderTestFixture {
    import scalar.charAvroCoderFixed // implicitly the default for Char
    import scalar.intAvroCoder

    case class CustomizedCoderStructure(a: Int, b: Char, c: Char)
    object CustomizedCoderStructure {
        @derive.structure.customizedCoder[CustomizedCoderStructure]
        implicit object avroCoder {
            val bCoder = scalar.charAvroCoderString
        }
    }
}

import avroStructureCustomizedCoderTestFixture.CustomizedCoderStructure

class avroStructureCustomizedCoderTest extends SpecificationWithJUnit with AvroMatchers {
    def is = s2"""
        Customized structure coders
            should encode correctly $eencode
            should decode correctly $edecode
            round trip a value $etrivial
    """

    lazy val coder = AvroCoder[CustomizedCoderStructure]

    def eencode = coder.encode.toBytes(CustomizedCoderStructure(1, 'b', 'C')) must beLike { case Okay(a) =>
        (a.take(1) must beAvroInt(1)).updateMessage("field a: " + _) and
        (a.drop(1).take(2) must beAvroString("b")).updateMessage("field b: " + _) and
        (a.drop(3) must beEqualToArray(Array[Byte](0, 67))).updateMessage("field c: " + _)
    }
    def edecode = {
        val bb = ByteBuffer.allocate(5)
        bb.put(zigZagEncode(1))
        bb.put(makeAvroString("b"))
        bb.put(0.asInstanceOf[Byte])
        bb.put(67.asInstanceOf[Byte])
        coder.decode.fromBytes(coder.schema)(bb.array) ==== Okay(CustomizedCoderStructure(1, 'b', 'C'))
    }
    def etrivial = {
        val coder = AvroCoder[CustomizedCoderStructure]
        val ccs = CustomizedCoderStructure(1, 'a', 'b')
        (coder.encode.toBytes(ccs) >>= coder.decode.fromBytes(coder.schema)) ==== Okay(ccs)
    }
}

object avroStructureCustomizedEncoderTestFixture {
    import scalar.charAvroCoderFixed // implicitly the default for Char
    import scalar.intAvroCoder

    case class CustomizedEncoderStructure(a: Int, b: Char, c: Char)
    object CustomizedEncoderStructure {
        @derive.structure.customizedEncoder[CustomizedEncoderStructure]
        implicit object avroEncoder {
            val bEncoder = scalar.charAvroCoderString.encode
        }
    }
}

import avroStructureCustomizedEncoderTestFixture.CustomizedEncoderStructure

class avroStructureCustomizedEncoderTest extends SpecificationWithJUnit with AvroMatchers {
    def is = s2"""
        Customized structure encoders
            should encode correctly $eencode
    """

    lazy val encode = AvroEncoder[CustomizedEncoderStructure]

    def eencode = encode.toBytes(CustomizedEncoderStructure(1, 'b', 'C')) must beLike { case Okay(a) =>
        (a.take(1) must beAvroInt(1)).updateMessage("field a: " + _) and
        (a.drop(1).take(2) must beAvroString("b")).updateMessage("field b: " + _) and
        (a.drop(3) must beEqualToArray(Array[Byte](0, 67))).updateMessage("field c: " + _)
    }
}

object avroStructureCustomizedDecoderTestFixture {
    import scalar.charAvroCoderFixed // implicitly the default for Char
    import scalar.intAvroCoder

    case class CustomizedDecoderStructure(a: Int, b: Char, c: Char)
    object CustomizedDecoderStructure {
        @derive.structure.customizedDecoder[CustomizedDecoderStructure]
        implicit object avroDecoder {
            val bDecoder = scalar.charAvroCoderString.decode
        }
    }
}

import avroStructureCustomizedDecoderTestFixture.CustomizedDecoderStructure

class avroStructureCustomizedDecoderTest extends SpecificationWithJUnit with AvroMatchers {
    def is = s2"""
        Customized structure coders
            should decode correctly $edecode
    """

    lazy val decode = AvroDecoder[CustomizedDecoderStructure]

    def edecode = {
        val bb = ByteBuffer.allocate(5)
        bb.put(zigZagEncode(1))
        bb.put(makeAvroString("b"))
        bb.put(0.asInstanceOf[Byte])
        bb.put(67.asInstanceOf[Byte])
        decode.fromBytes(decode.schema)(bb.array) ==== Okay(CustomizedDecoderStructure(1, 'b', 'C'))
    }
}

class avroStructureComposedClassTest extends SpecificationWithJUnit with StructureAvroMatchers {
    def is = s2"""
        Derived structure coders for class composed from traits
            must have the correct schema $eschema
    """

    import scalar.{doubleAvroCoder, intAvroCoder, stringAvroCoder}
    lazy val coder = derive.structure.coder[com.paytronix.utils.interchange.test.fixtures.ComposedClass]

    def eschema = {
        lazy val fields = coder.schema.getFields.asScala
        (coder.schema.getType ==== Schema.Type.RECORD).updateMessage("schema type: " + _) and
        (fields.size ==== 3).updateMessage("schema field count: " + _) and
        (fields must contain(fieldLike("a", Schema.Type.STRING))) and
        (fields must contain(fieldLike("b", Schema.Type.INT))) and
        (fields must contain(fieldLike("c", Schema.Type.DOUBLE)))
    }
}

class avroStructurePrimitiveRefinementTest extends SpecificationWithJUnit with StructureAvroMatchers {
    def is = s2"""
        Derived structure coders for class using type functions refining to primitives
            must have the correct schema $eschema
    """

    import scalar._
    lazy val coder = derive.structure.coder[com.paytronix.utils.interchange.test.fixtures.PrimitiveRefinementTestClass]

    def eschema = {
        lazy val fields = coder.schema.getFields.asScala
        (coder.schema.getType ==== Schema.Type.RECORD).updateMessage("schema type: " + _) and
        (fields.size ==== 8).updateMessage("schema field count: " + _) and
        // unit fields are ignored by the modeller since they are most often side-effecting things
        (fields must contain(fieldLike("aBoolean", Schema.Type.BOOLEAN))) and
        (fields must contain(fieldLike("aByte", Schema.Type.FIXED))) and
        (fields must contain(fieldLike("aShort", Schema.Type.FIXED))) and
        (fields must contain(fieldLike("aChar", Schema.Type.FIXED))) and
        (fields must contain(fieldLike("aInt", Schema.Type.INT))) and
        (fields must contain(fieldLike("aLong", Schema.Type.LONG))) and
        (fields must contain(fieldLike("aFloat", Schema.Type.FLOAT))) and
        (fields must contain(fieldLike("aDouble", Schema.Type.DOUBLE)))
    }
}

class avroStructureLazyValTest extends SpecificationWithJUnit with StructureAvroMatchers {
    def is = s2"""
        Derived structure coders for classes with lazy vals
            must have the correct schema $eschema
    """

    import scalar.stringAvroCoder
    lazy val coder = derive.structure.coder[com.paytronix.utils.interchange.test.fixtures.ClassWithLazyVal]

    def eschema = {
        lazy val fields = coder.schema.getFields.asScala
        (coder.schema.getType ==== Schema.Type.RECORD).updateMessage("schema type: " + _) and
        (fields.size ==== 1).updateMessage("schema field count: " + _) and
        (fields must contain(fieldLike("a", Schema.Type.STRING)))
    }
}

class avroStructureNotCodedTest extends SpecificationWithJUnit with StructureAvroMatchers {
    def is = s2"""
        Derived structure coders for classes with @notCoded vars
            must have the correct schema $eschema
    """

    import scalar.stringAvroCoder
    lazy val coder = derive.structure.coder[com.paytronix.utils.interchange.test.fixtures.ClassWithNotCodedVar]

    def eschema = {
        lazy val fields = coder.schema.getFields.asScala
        (coder.schema.getType ==== Schema.Type.RECORD).updateMessage("schema type: " + _) and
        (fields.size ==== 1).updateMessage("schema field count: " + _) and
        (fields must contain(fieldLike("a", Schema.Type.STRING)))
    }
}

object avroStructureDefaultingFixture {
    import scalar.intAvroCoder

    case class DefaultingStructure1(a: Int, @default(5 + 2) b: Int)
    object DefaultingStructure1 {
        implicit val avroCoder: AvroCoder[DefaultingStructure1] = derive.structure.coder[DefaultingStructure1]
        implicit val arb = Arbitrary(for { i <- arbitrary[Int]; j <- arbitrary[Int] } yield DefaultingStructure1(i, j))
    }

    case class DefaultingStructure2(a: Int, b: Int = 5 + 2)
    object DefaultingStructure2 {
        implicit val avroCoder: AvroCoder[DefaultingStructure2] = derive.structure.coder[DefaultingStructure2]
        implicit val arb = Arbitrary(for { i <- arbitrary[Int]; j <- arbitrary[Int] } yield DefaultingStructure2(i, j))
    }

    case class DefaultingStructure3(a: Int, @default(5 + 2) b: Int = 2)
    object DefaultingStructure3 {
        implicit val avroCoder: AvroCoder[DefaultingStructure3] = derive.structure.coder[DefaultingStructure3]
        implicit val arb = Arbitrary(for { i <- arbitrary[Int]; j <- arbitrary[Int] } yield DefaultingStructure3(i, j))
    }
}

import avroStructureDefaultingFixture.{DefaultingStructure1, DefaultingStructure2, DefaultingStructure3}

class avroStructureDefaultingTest extends SpecificationWithJUnit with ScalaCheck with StructureAvroMatchers {
    def is = s2"""
        Structure annotated with @default or default arguments to the constructor
            must have a correct schema when using @default $eschema1
            must decode from an older version of the structure without the field correctly when using @default $edecode1
            must round trip correctly when using @default $etrivial1
            must have a correct schema when using default arguments $eschema2
            must round trip correctly when using default arguments $etrivial2
            must have a correct schema when using default arguments and @default $eschema3
            must decode from an older version of the structure without the field correctly when using default arguments and @default $edecode3
            must round trip correctly when using default arguments and @default $etrivial3
    """

    lazy val writerSchema1 = {
        val r = Schema.createRecord("DefaultingStructure1", "", "com.paytronix.utils.interchange.format.avro.avroStructureDefaultingFixture", false)
        r.setFields(Arrays.asList(utils.makeField("a", Schema.create(Schema.Type.INT), None)))
        r
    }

    lazy val writerSchema2 = {
        val r = Schema.createRecord("DefaultingStructure2", "", "com.paytronix.utils.interchange.format.avro.avroStructureDefaultingFixture", false)
        r.setFields(Arrays.asList(utils.makeField("a", Schema.create(Schema.Type.INT), None)))
        r
    }

    lazy val writerSchema3 = {
        val r = Schema.createRecord("DefaultingStructure3", "", "com.paytronix.utils.interchange.format.avro.avroStructureDefaultingFixture", false)
        r.setFields(Arrays.asList(utils.makeField("a", Schema.create(Schema.Type.INT), None)))
        r
    }

    lazy val coder1 = DefaultingStructure1.avroCoder
    lazy val coder2 = DefaultingStructure2.avroCoder
    lazy val coder3 = DefaultingStructure3.avroCoder

    def eschema1 =
        (coder1.schema.getName ==== writerSchema1.getName).updateMessage("schema name: " + _) and
        (coder1.schema.getNamespace ==== writerSchema1.getNamespace).updateMessage("schema namespace: " + _) and
        (coder1.schema.getFields.size ==== (writerSchema1.getFields.size + 1)).updateMessage("schema field count: " + _)

    def edecode1 = prop { (i: Int) =>
        coder1.decode.fromBytes(writerSchema1)(zigZagEncode(i)) ==== Okay(DefaultingStructure1(i, 7))
    }

    def etrivial1 = prop { (ds1: DefaultingStructure1) =>
        (coder1.encode.toBytes(ds1) >>= coder1.decode.fromBytes(coder1.schema)) ==== Okay(ds1)
    }

    def eschema2 =
        (coder2.schema.getName ==== writerSchema2.getName).updateMessage("schema name: " + _) and
        (coder2.schema.getNamespace ==== writerSchema2.getNamespace).updateMessage("schema namespace: " + _) and
        (coder2.schema.getFields.size ==== (writerSchema2.getFields.size + 1)).updateMessage("schema field count: " + _)

    /*
     * 2014-07-29 RMM: I can't seem to support this because the macro / reflection API doesn't appear to have a way to recover the default
     *                 argument, only that a default argument exists.
     */
    def edecode2 = prop { (i: Int) =>
        coder2.decode.fromBytes(writerSchema2)(zigZagEncode(i)) ==== Okay(DefaultingStructure2(i, 7))
    }

    def etrivial2 = prop { (ds2: DefaultingStructure2) =>
        (coder2.encode.toBytes(ds2) >>= coder2.decode.fromBytes(coder2.schema)) ==== Okay(ds2)
    }

    def eschema3 =
        (coder3.schema.getName ==== writerSchema3.getName).updateMessage("schema name: " + _) and
        (coder3.schema.getNamespace ==== writerSchema3.getNamespace).updateMessage("schema namespace: " + _) and
        (coder3.schema.getFields.size ==== (writerSchema3.getFields.size + 1)).updateMessage("schema field count: " + _)

    def edecode3 = prop { (i: Int) =>
        coder3.decode.fromBytes(writerSchema3)(zigZagEncode(i)) ==== Okay(DefaultingStructure3(i, 7))
    }

    def etrivial3 = prop { (ds3: DefaultingStructure3) =>
        (coder3.encode.toBytes(ds3) >>= coder3.decode.fromBytes(coder3.schema)) ==== Okay(ds3)
    }
}
