//
// Copyright 2010-2014 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange.test.fixtures

import scala.language.higherKinds

import org.scalacheck.{Arbitrary, Gen}

import com.paytronix.utils.interchange.base.{name, coded, notCoded, nullable}
import com.paytronix.utils.scala.result.{Failed, Okay}
import com.paytronix.utils.scala.types.Identity

import Arbitrary.arbitrary

case class CaseClass(foo: Int, @nullable bar: String, zip: Option[String])

object CaseClass {
    implicit val arb = Arbitrary {
        for {
            foo <- arbitrary[Int]
            bar <- arbitrary[Option[String]].map(_.orNull)
            zip <- arbitrary[Option[String]]
        } yield CaseClass(foo, bar, zip)
    }
}

class BasicClass {
    var foo: Int = 0
    @nullable var bar: String = null
    var zip: Option[String] = null

    override def equals(other: Any): Boolean = other match {
        case (other: BasicClass) if other.foo == foo && other.bar == bar && other.zip == zip => true
        case _ => false
    }

    override def toString = "BasicClass(" + foo + ", " + (if (bar != null) "\"" + bar + "\"" else "null") + ", " + zip + ")"
}

object BasicClass {
    def make(foo: Int, bar: String, zip: Option[String]): BasicClass = {
        val inst = new BasicClass
        inst.foo = foo
        inst.bar = bar
        inst.zip = zip
        inst
    }

    implicit val arb = Arbitrary {
        for {
            foo <- arbitrary[Int]
            bar <- arbitrary[Option[String]].map(_.orNull)
            zip <- arbitrary[Option[String]]
        } yield make(foo, bar, zip)
    }
}

class POJOClass {
    private var foo: Int = 0
    private var bar: String = null
    private var zip: Option[String] = null

    def getFoo(): Int = foo
    def setFoo(i: Int): Unit = foo = i
    @nullable def getBar(): String = bar
    def setBar(s: String): Unit = bar = s
    def getZip(): Option[String] = zip
    def setZip(os: Option[String]): Unit = zip = os

    override def equals(in: Any): Boolean = in match {
        case (other: POJOClass) if other.getFoo() == foo && other.getBar() == bar && other.getZip() == zip => true
        case _ => false
    }

    override def toString = "POJOClass(" + foo + ", " + (if (bar != null) "\"" + bar + "\"" else "null") + ", " + zip + ")"
}

object POJOClass {
    def make(foo: Int, bar: String, zip: Option[String]): POJOClass = {
        val inst = new POJOClass
        inst.foo = foo
        inst.bar = bar
        inst.zip = zip
        inst
    }

    implicit val arb = Arbitrary {
        for {
            foo <- arbitrary[Int]
            bar <- arbitrary[Option[String]].map(_.orNull)
            zip <- arbitrary[Option[String]]
        } yield make(foo, bar, zip)
    }
}

object WrappingObject {
    // Uses the same name in a sub scope just to ensure we find the right symbol

    object SubWrappingObject {
        case class WrappedClass(a: Int)
    }

    object WrappedEnum extends Enumeration {
        val One = Value("One")
        val Two = Value("Two")
    }

    case class WrappedClass(a: Int, b: Option[WrappedEnum.Value])
}

trait BaseTrait1 {
    var a: String = null
}

trait BaseTrait2 {
    var b: Int = 0
}

class ComposedClass extends BaseTrait1 with BaseTrait2 {
    var c: Double = 0.0

    override def equals(in: Any): Boolean = in match {
        case (other: ComposedClass) if other.a == a && other.b == b && other.c == c => true
        case _ => false
    }

    override def toString = "ComposedClass { a = \"" + a + "\", b = " + b + ", c = " + c + " }"
}

case class Wrapper(s: String)
final case class WrapperVal(s: String) extends AnyVal

class MethodAndCtor(a: CaseClass, b: Int, @nullable c: String, d: Option[Double]) {
    def method(a: CaseClass, b: Int, @nullable c: String, d: Option[Double]): String = null
}

trait MethodTrait {
    def method(a: CaseClass, b: Int, @nullable c: String, d: Option[Double]): String
}

trait NamedMethodTrait {
    def method(@name("a") _a: CaseClass, @name("b") _b: Int, @name("c") @nullable _c: String, @name("d") _d: Option[Double]): String
}

trait PrimitiveRefinementTestTrait[F[_]] {
    def aUnit: F[Unit]
    def aBoolean: F[Boolean]
    def aByte: F[Byte]
    def aShort: F[Short]
    def aChar: F[Char]
    def aInt: F[Int]
    def aLong: F[Long]
    def aFloat: F[Float]
    def aDouble: F[Double]
}

class PrimitiveRefinementTestClass extends PrimitiveRefinementTestTrait[Identity] {
    var aUnit: Unit = ()
    var aBoolean: Boolean = false
    var aByte: Byte = 0
    var aShort: Short = 0
    var aChar: Char = '0'
    var aInt: Int = 0
    var aLong: Long = 0L
    var aFloat: Float = 0.0f
    var aDouble: Double = 0.0
}

class ClassWithMultipleConstructors(val a: Int, val b: String, c: Double) /* this constructor should be ignored due to c */ {
    protected def this(a: Int, b: String, d: String) = this(a, b, 0.0) // this constructor would be preferred, but should be ignored because it's protected
    def this(a: Int, b: String) = this(a, b, 0.0)
    def this() = this(0, "", 0.0)

    var d: String = ""

    override def equals(in: Any): Boolean = in match {
        case (other: ClassWithMultipleConstructors) if other.a == a && other.b == b && other.d == d => true
        case _ => false
    }
}

sealed abstract class UntaggedUnionBase
object UntaggedUnionBase {
    implicit val arb = Arbitrary[UntaggedUnionBase](Gen.oneOf(arbitrary[UntaggedUnionFirst], arbitrary[UntaggedUnionSecond]))
}
final case class UntaggedUnionFirst(a: Int) extends UntaggedUnionBase
object UntaggedUnionFirst {
    implicit val arb: Arbitrary[UntaggedUnionFirst] = Arbitrary(arbitrary[Int].map(UntaggedUnionFirst.apply))
}
final case class UntaggedUnionSecond(b: String) extends UntaggedUnionBase
object UntaggedUnionSecond {
    implicit val arb: Arbitrary[UntaggedUnionSecond] = Arbitrary(arbitrary[String].map(UntaggedUnionSecond.apply))
}
final case class UntaggedUnionInvalid(c: Double) extends UntaggedUnionBase
/*
object AutoUnionBaseCoding extends AutomaticUnionCoding[AutoUnionBase] {
    val noApplicableAlternative = "no applicable alternative"
    alternative[AutoUnionFirst]
    alternative[AutoUnionSecond]
}
*/
sealed abstract class TaggedUnionBase
object TaggedUnionBase {
    implicit val arb = Arbitrary[TaggedUnionBase](Gen.oneOf(arbitrary[TaggedUnionFirst], arbitrary[TaggedUnionSecond]))
}
final case class TaggedUnionFirst(a: Int) extends TaggedUnionBase
object TaggedUnionFirst {
    implicit val arb: Arbitrary[TaggedUnionFirst] = Arbitrary(arbitrary[Int].map(TaggedUnionFirst.apply))
}
final case class TaggedUnionSecond(b: String) extends TaggedUnionBase
object TaggedUnionSecond {
    implicit val arb: Arbitrary[TaggedUnionSecond] = Arbitrary(arbitrary[String].map(TaggedUnionSecond.apply))
}
final case class TaggedUnionInvalid(c: Double) extends TaggedUnionBase
object TaggedUnionSingleton extends TaggedUnionBase
object TaggedUnionSingletonWithProperties extends TaggedUnionBase {
    val nonCodedProperty = "foo"
    @coded val codedProperty = "bar"
}

sealed abstract  class FlattenedTaggedUnion
case class FlattenedTaggedUnionChild(a: Int) extends FlattenedTaggedUnion
object FlattenedTaggedUnionChild {
    implicit val arb: Arbitrary[FlattenedTaggedUnionChild] = Arbitrary(arbitrary[Int].map(FlattenedTaggedUnionChild.apply))
} 

final case class WrapperClass(field: Option[CaseClass])
//object WrapperClassCoding extends WrapperCoding

final case class EnclosingFlattened(a: Flattened, b: String)
final case class Flattened(a: String, c: Int)

class ClassWithLazyVal {
    var a: String = ""
    lazy val b: Uncodable = null

    override def equals(in: Any): Boolean = in match {
        case (other: ClassWithLazyVal) if other.a == a && other.b == b => true
        case _ => false
    }

    override def toString = "ClassWithLazyVal { a = \"" + a + "\", b = " + b + " }"
}

class ClassWithNotCodedVar {
    var a: String = ""
    @notCoded var b: Uncodable = null

    override def equals(in: Any): Boolean = in match {
        case (other: ClassWithNotCodedVar) if other.a == a && other.b == b => true
        case _ => false
    }

    override def toString = "ClassWithNotCoderVar { a = \"" + a + "\", b = " + b + " }"
}

case class Uncodable(trash: Int)
//object UncodableCoding extends NoCoding // heh

object ScalaEnum extends Enumeration {
    val BANANA = Value("banana")
    val APPLE = Value("apple")
    val CARROT = Value("carrot")
}

object ScalaEnum2 extends Enumeration {
    val THING_ONE = Value("thing one")
    val TWO = Value("two ish")
    val THREE = Value("three")
}

case class ScalaEnumWrapper(a: ScalaEnum.Value)
case class JavaEnumWrapper(a: JavaEnum)

class EnclosingClass {
    case class EnclosedCaseClass(a: Int)
}

case class DiagnosticTestOuterClass(a: Map[String, DiagnosticTestMiddleClass])
case class DiagnosticTestMiddleClass(b: List[DiagnosticTestTerminalClass])
case class DiagnosticTestTerminalClass(c: Int)
