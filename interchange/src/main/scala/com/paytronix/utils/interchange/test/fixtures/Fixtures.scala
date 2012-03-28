//
// Copyright 2010-2012 Paytronix Systems, Inc.
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

import javax.inject.Named
import com.paytronix.utils.extendedreflection.{Builder, ClassTypeR}
import com.paytronix.utils.interchange._
import com.paytronix.utils.scala.result.{Failed, Okay}
import com.paytronix.utils.scala.types.Identity

case class CaseClass(foo: Int, @Nullable bar: String, zip: Option[String])

class BasicClass {
    var foo: Int = 0
    @Nullable var bar: String = null
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
}

class POJOClass {
    private var foo: Int = 0
    private var bar: String = null
    private var zip: Option[String] = null

    def getFoo(): Int = foo
    def setFoo(i: Int): Unit = foo = i
    @Nullable def getBar(): String = bar
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

case class Wrapper(cc: CaseClass)

class MethodAndCtor(a: CaseClass, b: Int, @Nullable c: String, d: Option[Double]) {
    def method(a: CaseClass, b: Int, @Nullable c: String, d: Option[Double]): String = null
}

trait MethodTrait {
    def method(a: CaseClass, b: Int, @Nullable c: String, d: Option[Double]): String
}

trait NamedMethodTrait {
    def method(@Named("a") _a: CaseClass, @Named("b") _b: Int, @Named("c") @Nullable _c: String, @Named("d") _d: Option[Double]): String
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
    def aUnit: Unit = ()
    def aBoolean: Boolean = false
    def aByte: Byte = 0
    def aShort: Short = 0
    def aChar: Char = '0'
    def aInt: Int = 0
    def aLong: Long = 0L
    def aFloat: Float = 0.0f
    def aDouble: Double = 0.0
}

case class CaseClassWithSideCar(foo: Int, bar: String, zip: Option[String])

object CaseClassWithSideCarCoding extends ModifyObjectCoding {
    modify {
        case fc@FieldCoding("bar", coder, _, _) => Some(fc.copy(coder=NullCoder(coder)))
    }
}

class ClassWithMultipleConstructors(val a: Int, val b: String, c: Double) /* this constructor should be ignored due to c */ {
    private def this(a: Int, b: String, d: String) = this(a, b, 0.0) // this constructor would be preferred, but should be ignored because it's private
    def this(a: Int, b: String) = this(a, b, 0.0)
    def this() = this(0, "", 0.0)

    var d: String = ""

    override def equals(in: Any): Boolean = in match {
        case (other: ClassWithMultipleConstructors) if other.a == a && other.b == b && other.d == d => true
        case _ => false
    }
}

class ClassWithIllegalConstructor(val a: Int, val b: String) {
    @ConstructorForCoding def this(a: Int, b: Int, c: Int) = this(a, "")
}

sealed abstract class AutoUnionBase
final case class AutoUnionFirst(a: Int) extends AutoUnionBase
final case class AutoUnionSecond(b: String) extends AutoUnionBase
final case class AutoUnionInvalid(c: Double) extends AutoUnionBase

object AutoUnionBaseCoding extends AutomaticUnionCoding[AutoUnionBase] {
    alternative[AutoUnionFirst]
    alternative[AutoUnionSecond]
}

sealed abstract class ExplicitUnionBase
final case class ExplicitUnionFirst(a: Int) extends ExplicitUnionBase
final case class ExplicitUnionSecond(b: String) extends ExplicitUnionBase
final case class ExplicitUnionInvalid(c: Double) extends ExplicitUnionBase
object ExplicitUnionSingleton extends ExplicitUnionBase
object ExplicitUnionSingletonWithProperties extends ExplicitUnionBase {
    val nonCodedProperty = "foo"
    @Coded val codedProperty = "bar"
}

object ExplicitUnionBaseCoding extends ExplicitUnionCoding[ExplicitUnionBase] {
    alternative[ExplicitUnionFirst]("first")
    alternative[ExplicitUnionSecond]("second")
    alternative[ExplicitUnionSingleton.type]("singleton")
    alternative[ExplicitUnionSingletonWithProperties.type]("singletonWithProperties")
}

final case class WrapperClass(field: Option[CaseClass])
object WrapperClassCoding extends WrapperCoding

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
    @NotCoded var b: Uncodable = null

    override def equals(in: Any): Boolean = in match {
        case (other: ClassWithNotCodedVar) if other.a == a && other.b == b => true
        case _ => false
    }

    override def toString = "ClassWithNotCoderVar { a = \"" + a + "\", b = " + b + " }"
}

case class Uncodable(trash: Int)
object UncodableCoding extends NoCoding // heh

object ScalaEnum extends Enumeration {
    val ONE = Value("ONE")
    val TWO = Value("TWO")
    val THREE = Value("THREE")
}

case class ScalaEnumWrapper(a: ScalaEnum.Value)
case class JavaEnumWrapper(a: JavaEnum)

class EnclosingClass {
    case class EnclosedCaseClass(a: Int)
}

case class DiagnosticTestOuterClass(a: Map[String, DiagnosticTestMiddleClass])
case class DiagnosticTestMiddleClass(b: List[DiagnosticTestTerminalClass])
case class DiagnosticTestTerminalClass(c: Int)

object Coders {
    import com.paytronix.utils.interchange.test.Helpers.builder

    lazy val caseClassCoder = {
        val caseClass = classOf[CaseClass]
        ObjectCoder(
            clazz = caseClass,
            constructor = caseClass.getConstructor(classOf[Int], classOf[String], classOf[Option[_]]),
            constructorFieldNames = List("foo", "bar", "zip"),
            fieldCodings = List(
                FieldCoding("bar", NullCoder(StringCoder),   caseClass.getMethod("bar"), Failed("no setter")),
                FieldCoding("foo", IntCoder,                 caseClass.getMethod("foo"), Failed("no setter")),
                FieldCoding("zip", OptionCoder(StringCoder), caseClass.getMethod("zip"), Failed("no setter"))
            ),
            flatten = false
        )
    }

    lazy val wrappedClassCoder = {
        val wrappedClass = classOf[WrappingObject.WrappedClass]
        ObjectCoder(
            clazz = wrappedClass,
            constructor = wrappedClass.getConstructor(classOf[Int], classOf[Option[_]]),
            constructorFieldNames = List("a", "b"),
            fieldCodings = List(
                FieldCoding("a", IntCoder, wrappedClass.getMethod("a"), Failed("no setter")),
                FieldCoding("b", OptionCoder(ScalaEnumCoder(WrappingObject.WrappedEnum)), wrappedClass.getMethod("b"), Failed("no setter"))
            ),
            flatten = false
        )
    }

    lazy val subWrappedClassCoder = {
        val subWrappedClass = classOf[WrappingObject.SubWrappingObject.WrappedClass]
        ObjectCoder(
            clazz = subWrappedClass,
            constructor = subWrappedClass.getConstructor(classOf[Int]),
            constructorFieldNames = List("a"),
            fieldCodings = List(
                FieldCoding("a", IntCoder, subWrappedClass.getMethod("a"), Failed("no setter"))
            ),
            flatten = false
        )
    }

    lazy val basicClassCoder = {
        val basicClass = classOf[BasicClass]
        ObjectCoder(
            clazz = basicClass,
            constructor = basicClass.getConstructor(),
            constructorFieldNames = Nil,
            fieldCodings = List(
                FieldCoding("bar", NullCoder(StringCoder),   basicClass.getMethod("bar"), Okay(basicClass.getMethod("bar_$eq", classOf[String]))),
                FieldCoding("foo", IntCoder,                 basicClass.getMethod("foo"), Okay(basicClass.getMethod("foo_$eq", classOf[Int]))),
                FieldCoding("zip", OptionCoder(StringCoder), basicClass.getMethod("zip"), Okay(basicClass.getMethod("zip_$eq", classOf[Option[_]])))
            ),
            flatten = false
        )
    }

    lazy val pojoClassCoder = {
        val pojoClass = classOf[POJOClass]
        ObjectCoder(
            clazz = pojoClass,
            constructor = pojoClass.getConstructor(),
            constructorFieldNames = Nil,
            fieldCodings = List(
                FieldCoding("bar", NullCoder(StringCoder),   pojoClass.getMethod("getBar"), Okay(pojoClass.getMethod("setBar", classOf[String]))),
                FieldCoding("foo", IntCoder,                 pojoClass.getMethod("getFoo"), Okay(pojoClass.getMethod("setFoo", classOf[Int]))),
                FieldCoding("zip", OptionCoder(StringCoder), pojoClass.getMethod("getZip"), Okay(pojoClass.getMethod("setZip", classOf[Option[_]])))
            ),
            flatten = false
        )
    }

    lazy val composedClassCoder = {
        val composedClass = classOf[ComposedClass]
        val trait1Class = classOf[BaseTrait1]
        val trait2Class = classOf[BaseTrait2]
        ObjectCoder(
            clazz = composedClass,
            constructor = composedClass.getConstructor(),
            constructorFieldNames = Nil,
            fieldCodings = List(
                FieldCoding("a", StringCoder, trait1Class.getMethod("a"), Okay(trait1Class.getMethod("a_$eq", classOf[String]))),
                FieldCoding("b", IntCoder,    trait2Class.getMethod("b"), Okay(trait2Class.getMethod("b_$eq", classOf[Int]))),
                FieldCoding("c", DoubleCoder, composedClass.getMethod("c"), Okay(composedClass.getMethod("c_$eq", classOf[Double])))
            ),
            flatten = false
        )
    }

    lazy val wrapperCoder = {
        val wrapperClass = classOf[Wrapper]
        ObjectCoder(
            clazz = wrapperClass,
            constructor = wrapperClass.getConstructor(classOf[CaseClass]),
            constructorFieldNames = List("cc"),
            fieldCodings = List(
                FieldCoding("cc", caseClassCoder, wrapperClass.getMethod("cc"), Failed("no setter"))
            ),
            flatten = false
        )
    }

    lazy val methodAndCtorArgumentCoder = ArgumentArrayCoder(false, List(
        ArgumentCoding("a", caseClassCoder), ArgumentCoding("b", IntCoder),
        ArgumentCoding("c", NullCoder(StringCoder)), ArgumentCoding("d", OptionCoder(DoubleCoder))
    ))

    lazy val caseClassWithSideCarReflectedCoder = {
        val caseClassWithSideCar = classOf[CaseClassWithSideCar]
        ObjectCoder(
            clazz = caseClassWithSideCar,
            constructor = caseClassWithSideCar.getConstructor(classOf[Int], classOf[String], classOf[Option[_]]),
            constructorFieldNames = List("foo", "bar", "zip"),
            fieldCodings = List(
                FieldCoding("bar", StringCoder,              caseClassWithSideCar.getMethod("bar"), Failed("no setter")),
                FieldCoding("foo", IntCoder,                 caseClassWithSideCar.getMethod("foo"), Failed("no setter")),
                FieldCoding("zip", OptionCoder(StringCoder), caseClassWithSideCar.getMethod("zip"), Failed("no setter"))
            ),
            flatten = false
        )
    }

    lazy val caseClassWithSideCarExplicitCoder = {
        val caseClassWithSideCar = classOf[CaseClassWithSideCar]
        ObjectCoder(
            clazz = caseClassWithSideCar,
            constructor = caseClassWithSideCar.getConstructor(classOf[Int], classOf[String], classOf[Option[_]]),
            constructorFieldNames = List("foo", "bar", "zip"),
            fieldCodings = List(
                FieldCoding("bar", NullCoder(StringCoder),   caseClassWithSideCar.getMethod("bar"), Failed("no setter")),
                FieldCoding("foo", IntCoder,                 caseClassWithSideCar.getMethod("foo"), Failed("no setter")),
                FieldCoding("zip", OptionCoder(StringCoder), caseClassWithSideCar.getMethod("zip"), Failed("no setter"))
            ),
            flatten = false
        )
    }

    lazy val autoUnionCoder = {
        val firstClass = classOf[AutoUnionFirst]
        val secondClass = classOf[AutoUnionSecond]

        val firstCoder =
            (builder.typeRFor(firstClass).flatMap(Reflection.reflectObjectCoder(firstClass.getClassLoader, _))
             .orThrow.asInstanceOf[ComposableCoder[AutoUnionFirst]])
        val secondCoder =
            (builder.typeRFor(secondClass).flatMap(Reflection.reflectObjectCoder(secondClass.getClassLoader, _))
             .orThrow.asInstanceOf[ComposableCoder[AutoUnionSecond]])

        AutomaticUnionCoder[AutoUnionBase](false, List(firstCoder, secondCoder))
    }

    lazy val explicitUnionCoder = {
        val firstClass = classOf[ExplicitUnionFirst]
        val secondClass = classOf[ExplicitUnionSecond]
        val singletonClass = ExplicitUnionSingleton.getClass.asInstanceOf[Class[ExplicitUnionBase]]
        val singletonWithPropertiesClass = ExplicitUnionSingletonWithProperties.getClass.asInstanceOf[Class[ExplicitUnionBase]]

        implicit val builder = new Builder(getClass.getClassLoader)

        val firstCoder =
            builder.typeRFor(firstClass).flatMap(Coding.forTypeComposable(firstClass.getClassLoader, _)).orThrow.asCoderFor[ExplicitUnionFirst]
        val secondCoder =
            builder.typeRFor(secondClass).flatMap(Coding.forTypeComposable(secondClass.getClassLoader, _)).orThrow.asCoderFor[ExplicitUnionSecond]
        val singletonCoder =
            builder.typeRFor(singletonClass).flatMap(Coding.forTypeComposable(secondClass.getClassLoader, _)).orThrow.asCoderFor[ExplicitUnionBase]
        val singletonWithPropertiesCoder =
            builder.typeRFor(singletonWithPropertiesClass).flatMap(Coding.forTypeComposable(secondClass.getClassLoader, _)).orThrow.asCoderFor[ExplicitUnionBase]

        ExplicitUnionCoder[ExplicitUnionBase](false, "_type", List(
            ExplicitUnionAlternative("first", firstClass, firstCoder),
            ExplicitUnionAlternative("second", secondClass, secondCoder),
            ExplicitUnionAlternative("singleton", singletonClass, singletonCoder),
            ExplicitUnionAlternative("singletonWithProperties", singletonWithPropertiesClass, singletonWithPropertiesCoder)
        ))
    }

    lazy val classWithLazyValCoder = {
        val classWithLazyVal = classOf[ClassWithLazyVal]
        ObjectCoder(
            clazz = classWithLazyVal,
            constructor = classWithLazyVal.getConstructor(),
            constructorFieldNames = Nil,
            fieldCodings = List(
                FieldCoding("a", StringCoder, classWithLazyVal.getMethod("a"), Okay(classWithLazyVal.getMethod("a_$eq", classOf[String])))
            ),
            flatten = false
        )
    }

    lazy val classWithNotCodedVarCoder = {
        val classWithNotCodedVar = classOf[ClassWithNotCodedVar]
        ObjectCoder(
            clazz = classWithNotCodedVar,
            constructor = classWithNotCodedVar.getConstructor(),
            constructorFieldNames = Nil,
            fieldCodings = List(
                FieldCoding("a", StringCoder, classWithNotCodedVar.getMethod("a"), Okay(classWithNotCodedVar.getMethod("a_$eq", classOf[String])))
            ),
            flatten = false
        )
    }

    lazy val scalaEnumCoder = ScalaEnumCoder(ScalaEnum)
    lazy val scalaEnumWrapperCoder = {
        val scalaEnumWrapperClass = classOf[ScalaEnumWrapper]
        ObjectCoder(
            clazz = scalaEnumWrapperClass,
            constructor = scalaEnumWrapperClass.getConstructor(classOf[Enumeration#Value]),
            constructorFieldNames = List("a"),
            fieldCodings = List(
                FieldCoding("a", scalaEnumCoder, scalaEnumWrapperClass.getMethod("a"), Failed("no setter"))
            ),
            flatten = false
        )
    }

    lazy val javaEnumCoder = JavaEnumCoder(classOf[JavaEnum])
    lazy val javaEnumWrapperCoder = {
        val javaEnumWrapperClass = classOf[JavaEnumWrapper]
        ObjectCoder(
            clazz = javaEnumWrapperClass,
            constructor = javaEnumWrapperClass.getConstructor(classOf[JavaEnum]),
            constructorFieldNames = List("a"),
            fieldCodings = List(
                FieldCoding("a", javaEnumCoder, javaEnumWrapperClass.getMethod("a"), Failed("no setter"))
            ),
            flatten = false
        )
    }

    lazy val classWithMultipleConstructorsCoder = {
        val classWithMultipleConstructors = classOf[ClassWithMultipleConstructors]
        ObjectCoder(
            clazz = classWithMultipleConstructors,
            constructor = classWithMultipleConstructors.getConstructor(classOf[Int], classOf[String]),
            constructorFieldNames = List("a", "b"),
            fieldCodings = List(
                FieldCoding("a", IntCoder, classWithMultipleConstructors.getMethod("a"), Failed("no setter")),
                FieldCoding("b", StringCoder, classWithMultipleConstructors.getMethod("b"), Failed("no setter")),
                FieldCoding("d", StringCoder, classWithMultipleConstructors.getMethod("d"), Okay(classWithMultipleConstructors.getMethod("d_$eq", classOf[String])))
            ),
            flatten = false
        )
    }

    lazy val flattenedCoder = {
        val clazz = classOf[Flattened]
        ObjectCoder (
            clazz = clazz,
            constructor = clazz.getConstructor(classOf[String], classOf[Int]),
            constructorFieldNames = List("a", "c"),
            fieldCodings = List (
                FieldCoding("a", StringCoder, clazz.getMethod("a"), Failed("no setter")),
                FieldCoding("c", IntCoder, clazz.getMethod("c"), Failed("no setter"))
            ),
            flatten = true
        )
    }

    lazy val enclosingFlattenedObjectCoder = {
        val clazz = classOf[EnclosingFlattened]
        ObjectCoder (
            clazz = clazz,
            constructor = clazz.getConstructor(classOf[Flattened], classOf[String]),
            constructorFieldNames = List("a", "b"),
            fieldCodings = List (
                FieldCoding("a", flattenedCoder, clazz.getMethod("a"), Failed("no setter")),
                FieldCoding("b", StringCoder, clazz.getMethod("b"), Failed("no setter"))
            ),
            flatten = false
        )
    }

    lazy val enclosingFlattenedArgumentArrayCoder = {
        ArgumentArrayCoder(false, List(ArgumentCoding("a", flattenedCoder), ArgumentCoding("b", StringCoder)))
    }
}
