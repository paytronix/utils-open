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

package com.paytronix.utils.interchange.test

import java.math.{BigDecimal => JavaBigDecimal, BigInteger}
import java.util.{List => JavaList, Map => JavaMap, Date => JavaDate}
import scala.collection.immutable.{Map => ImmutableMap, Set => ImmutableSet}
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}
import scala.math.{BigDecimal => ScalaBigDecimal}
import net.liftweb.json.Implicits.{int2jvalue, string2jvalue}
import net.liftweb.json.JsonAST.{JArray, JNothing, JObject, JValue}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import org.specs._
import org.specs.runner.JUnit4
import com.paytronix.utils.extendedreflection.Builder
import com.paytronix.utils.interchange._
import com.paytronix.utils.scala.result.{Failed, Okay, Result}

import fixtures._
import Helpers._

class CodingTestSpecsAsTest extends JUnit4(CodingTestSpecs)

object CodingTestSpecs extends Specification {
    noDetailedDiffs()

    class BigIntTV          extends TestValue[BigInt]              { val test: BigInt              = null     }
    class BigIntegerTV      extends TestValue[BigInteger]          { val test: BigInteger          = null     }
    class BooleanTV         extends TestValue[Boolean]             { val test: Boolean             = false    }
    class BoxedBooleanTV    extends TestValue[java.lang.Boolean]   { val test: java.lang.Boolean   = null     }
    class BoxedByteTV       extends TestValue[java.lang.Byte]      { val test: java.lang.Byte      = null     }
    class BoxedCharacterTV  extends TestValue[java.lang.Character] { val test: java.lang.Character = null     }
    class BoxedIntegerTV    extends TestValue[java.lang.Integer]   { val test: java.lang.Integer   = null     }
    class BoxedLongTV       extends TestValue[java.lang.Long]      { val test: java.lang.Long      = null     }
    class BoxedShortTV      extends TestValue[java.lang.Short]     { val test: java.lang.Short     = null     }
    class ByteTV            extends TestValue[Byte]                { val test: Byte                = 0        }
    class CharTV            extends TestValue[Char]                { val test: Char                = '\u0000' }
    class DoubleTV          extends TestValue[Double]              { val test: Double              = 0.0      }
    class FloatTV           extends TestValue[Float]               { val test: Float               = 0.0f     }
    class IntTV             extends TestValue[Int]                 { val test: Int                 = 0        }
    class JValueTV          extends TestValue[JValue]              { val test: JValue              = JNothing }
    class JavaBigDecimalTV  extends TestValue[JavaBigDecimal]      { val test: JavaBigDecimal      = null     }
    class JavaDateTV        extends TestValue[JavaDate]            { val test: JavaDate            = null     }
    class LongTV            extends TestValue[Long]                { val test: Long                = 0L       }
    class ScalaBigDecimalTV extends TestValue[ScalaBigDecimal]     { val test: ScalaBigDecimal     = null     }
    class ShortTV           extends TestValue[Short]               { val test: Short               = 0        }
    class StringTV          extends TestValue[String]              { val test: String              = null     }

    class ImmutableMapTV   extends TestValue[ImmutableMap[String, Int]] { val test: ImmutableMap[String, Int] = null }
    class JavaListStringTV extends TestValue[JavaList[String]]          { val test: JavaList[String]          = null }
    class JavaMapTV        extends TestValue[JavaMap[String, Int]]      { val test: JavaMap[String, Int]      = null }
    class ListStringTV     extends TestValue[List[String]]              { val test: List[String]              = null }
    class MutableMapTV     extends TestValue[MutableMap[String, Int]]   { val test: MutableMap[String, Int]   = null }
    class OptionIntTV      extends TestValue[Option[Int]]               { val test: Option[Int]               = null }
    class ResultStringTV   extends TestValue[Result[String]]            { val test: Result[String]            = null }
    class ResultUnitTV     extends TestValue[Result[Unit]]              { val test: Result[Unit]              = null }
    class EitherTV         extends TestValue[Either[String, Int]]       { val test: Either[String, Int]       = null }

    class ImmutableSetTV extends TestValue[ImmutableSet[String]] { val test: ImmutableSet[String] = null }
    class MutableSetTV extends TestValue[MutableSet[String]]     { val test: MutableSet[String]   = null }

    class Tuple1TV extends TestValue[Tuple1[Byte]]                    { val test: Tuple1[Byte] = null }
    class Tuple2TV extends TestValue[(Byte, Char)]                    { val test: (Byte, Char) = null }
    class Tuple3TV extends TestValue[(Byte, Char, Short)]             { val test: (Byte, Char, Short) = null }
    class Tuple4TV extends TestValue[(Byte, Char, Short, Int)]        { val test: (Byte, Char, Short, Int) = null }
    class Tuple5TV extends TestValue[(Byte, Char, Short, Int, Long)]  { val test: (Byte, Char, Short, Int, Long) = null }
    class Tuple6TV extends TestValue[(Byte, Char, Short, Int, Long, String)] {
        val test: (Byte, Char, Short, Int, Long, String) = null
    }

    class BasicClassTV    extends TestValue[BasicClass]    { val test: BasicClass    = null }
    class CaseClassTV     extends TestValue[CaseClass]     { val test: CaseClass     = null }
    class ComposedClassTV extends TestValue[ComposedClass] { val test: ComposedClass = null }
    class POJOClassTV     extends TestValue[POJOClass]     { val test: POJOClass     = null }
    class WrapperTV       extends TestValue[Wrapper]       { val test: Wrapper       = null }

    class NestedOptionTV extends TestValue[Option[Option[Option[String]]]] {
        val test: Option[Option[Option[String]]] = null
    }

    class WrappedClassTV extends TestValue[WrappingObject.WrappedClass] {
        val test: WrappingObject.WrappedClass = null
    }

    class SubWrappedClassTV extends TestValue[WrappingObject.SubWrappingObject.WrappedClass] {
        val test: WrappingObject.SubWrappingObject.WrappedClass = null
    }

    class UncodableTV            extends TestValue[Uncodable]            { val test: Uncodable            = null }
    class ClassWithLazyValTV     extends TestValue[ClassWithLazyVal]     { val test: ClassWithLazyVal     = null }
    class ClassWithNotCodedVarTV extends TestValue[ClassWithNotCodedVar] { val test: ClassWithNotCodedVar = null }
    class ScalaEnumWrapperTV     extends TestValue[ScalaEnumWrapper]     { val test: ScalaEnumWrapper     = null }
    class JavaEnumWrapperTV      extends TestValue[JavaEnumWrapper]      { val test: JavaEnumWrapper      = null }

    class ClassWithMultipleConstructorsTV extends TestValue[ClassWithMultipleConstructors] {
        val test: ClassWithMultipleConstructors = null
    }
    class ClassWithIllegalConstructorTV extends TestValue[ClassWithIllegalConstructor] {
        val test: ClassWithIllegalConstructor = null
    }
    class EnclosedCaseClassTV extends TestValue[EnclosingClass#EnclosedCaseClass] {
        val test: EnclosingClass#EnclosedCaseClass = null
    }

    "Coding frontend" should {
        Coding.reset.before

        "support quick decode" in {
            Coding.decode[CaseClass](("foo" -> 1) ~ ("bar" -> "baz") ~ ("zip" -> "qux")) must_== Okay(CaseClass(1, "baz", Some("qux")))
        }

        "support quick encode" in {
            Coding.encode(CaseClass(1, "baz", Some("qux"))) withFailureParameter Nil must matchEncodedJson(("foo" -> 1) ~ ("bar" -> "baz") ~ ("zip" -> "qux"))
        }
    }

    "Coder detection" should {
        import fixtures._
        import Coders._

        Coding.reset.before

        "work for scalar types" in {
            new BigIntTV().coder          must_== Okay(BigIntCoder)
            new BigIntegerTV().coder      must_== Okay(BigIntegerCoder)
            new BooleanTV().coder         must_== Okay(BooleanCoder)
            new ByteTV().coder            must_== Okay(ByteCoder)
            new CharTV().coder            must_== Okay(CharCoder)
            new DoubleTV().coder          must_== Okay(DoubleCoder)
            new FloatTV().coder           must_== Okay(FloatCoder)
            new IntTV().coder             must_== Okay(IntCoder)
            new JValueTV().coder          must_== Okay(JValueCoder)
            new JavaBigDecimalTV().coder  must_== Okay(JavaBigDecimalCoder)
            new JavaDateTV().coder        must_== Okay(JavaDateCoder)
            new LongTV().coder            must_== Okay(LongCoder)
            new ScalaBigDecimalTV().coder must_== Okay(ScalaBigDecimalCoder)
            new ShortTV().coder           must_== Okay(ShortCoder)
            new StringTV().coder          must_== Okay(StringCoder)
        }

        "work for java boxed types" in {
            new BoxedBooleanTV().coder   must_== Okay(BooleanCoder)
            new BoxedByteTV().coder      must_== Okay(ByteCoder)
            new BoxedCharacterTV().coder must_== Okay(CharCoder)
            new BoxedIntegerTV().coder   must_== Okay(IntCoder)
            new BoxedLongTV().coder      must_== Okay(LongCoder)
            new BoxedShortTV().coder     must_== Okay(ShortCoder)
        }

        "work for optional types" in {
            new OptionIntTV().coder    must_== Okay(OptionCoder(IntCoder))
            new ResultStringTV().coder must_== Okay(ResultCoder(UnitCoder, StringCoder))
            new ResultUnitTV().coder   must_== Okay(ResultCoder(UnitCoder, UnitCoder))
        }

        "work for Either" in {
            new EitherTV().coder must_== Okay(EitherCoder(StringCoder, IntCoder))
        }

        "work for simple collection types" in {
            new JavaListStringTV().coder must_== Okay(JavaListCoder(StringCoder))
            new ListStringTV().coder     must_== Okay(ScalaListCoder(StringCoder))
        }

        "work for map types" in {
            new JavaMapTV().coder      must_== Okay(JavaMapCoder(StringCoder, IntCoder))
            new ImmutableMapTV().coder must_== Okay(ScalaImmutableMapCoder(StringCoder, IntCoder))
            new MutableMapTV().coder   must_== Okay(ScalaMutableMapCoder(StringCoder, IntCoder))
        }

        "work for set types" in {
            new ImmutableSetTV().coder must_== Okay(ScalaImmutableSetCoder(StringCoder))
            new MutableSetTV().coder must_== Okay(ScalaMutableSetCoder(StringCoder))
        }

        "work for tuple types" in {
            new Tuple1TV().coder must_== Okay(Tuple1Coder(ByteCoder))
            new Tuple2TV().coder must_== Okay(Tuple2Coder(ByteCoder, CharCoder))
            new Tuple3TV().coder must_== Okay(Tuple3Coder(ByteCoder, CharCoder, ShortCoder))
            new Tuple4TV().coder must_== Okay(Tuple4Coder(ByteCoder, CharCoder, ShortCoder, IntCoder))
            new Tuple5TV().coder must_== Okay(Tuple5Coder(ByteCoder, CharCoder, ShortCoder, IntCoder, LongCoder))
            new Tuple6TV().coder must_== Okay(Tuple6Coder(ByteCoder, CharCoder, ShortCoder, IntCoder, LongCoder, StringCoder))
        }

        "work for object types" in {
            new BasicClassTV().coder    must_== Okay(basicClassCoder)
            new CaseClassTV().coder     must_== Okay(caseClassCoder)
            new ComposedClassTV().coder must_== Okay(composedClassCoder)
            new POJOClassTV().coder     must_== Okay(pojoClassCoder)
        }

        "work for nested object types" in {
            new WrapperTV().coder must_== Okay(wrapperCoder)
        }

        "work for methods" in {
            val meth = classOf[MethodAndCtor].getMethods.find(_.getName == "method").head

            Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._1) must_== Okay(methodAndCtorArgumentCoder)
            Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._2) must_== Okay(StringCoder)
        }

        "work for methods of traits without @Named" in {
            val meth = classOf[MethodTrait].getMethods.find(_.getName == "method").head

            Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._1) must_== Okay(methodAndCtorArgumentCoder)
            Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._2) must_== Okay(StringCoder)
        }

        "work for methods of traits with @Named" in {
            val meth = classOf[NamedMethodTrait].getMethods.find(_.getName == "method").head

            Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._1) must_== Okay(methodAndCtorArgumentCoder)
            Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._2) must_== Okay(StringCoder)
        }

        "work for classes implementing traits with types refined to primitives (yes, this is crazy!)" in {
            val clazz = classOf[PrimitiveRefinementTestClass]

            def check(name: String, expected: ComposableCoder[_]): Unit = {
                val meth = clazz.getMethod(name)
                val result = Reflection.reflect(clazz.getClassLoader, meth)

                result.map(_._2) must_== Okay(expected)
            }

            check("aUnit", UnitCoder)
            check("aBoolean", BooleanCoder)
            check("aByte", ByteCoder)
            check("aShort", ShortCoder)
            check("aChar", CharCoder)
            check("aInt", IntCoder)
            check("aLong", LongCoder)
            check("aFloat", FloatCoder)
            check("aDouble", DoubleCoder)
        }

        "work for constructors" in {
            val ctor = classOf[MethodAndCtor].getConstructors.head
            Reflection.reflect(ctor.getDeclaringClass.getClassLoader, ctor) must_== Okay(methodAndCtorArgumentCoder)
        }

        "work for parameterized types in parameterized types" in {
            new NestedOptionTV().coder must_== Okay(OptionCoder(OptionCoder(OptionCoder(StringCoder))))
        }

        "work for classes enclosed in objects" in {
            new WrappedClassTV().coder must_== Okay(wrappedClassCoder)
            new SubWrappedClassTV().coder must_== Okay(subWrappedClassCoder)
        }

        "fail to code The Uncodable" in {
            new UncodableTV().coder must not(verify(_.isDefined))
        }

        "not attempt to code lazy vals" in {
            new ClassWithLazyValTV().coder must_== Okay(classWithLazyValCoder)
        }

        "not attempt to code @NotCoded vars" in {
            new ClassWithNotCodedVarTV().coder must_== Okay(classWithNotCodedVarCoder)
        }

        "work for enums" in {
            new ScalaEnumWrapperTV().coder must_== Okay(scalaEnumWrapperCoder)
            new JavaEnumWrapperTV().coder must_== Okay(javaEnumWrapperCoder)
        }

        "properly exclude constructors with non-property arguments" in {
            new ClassWithMultipleConstructorsTV().coder must_== Okay(classWithMultipleConstructorsCoder)
        }

        "properly fail when an incorrect constructor is forced" in {
            new ClassWithIllegalConstructorTV().coder must not(verify(_.isDefined))
        }

        "property fail for inner classes" in {
            new EnclosedCaseClassTV().coder must not(verify(_.isDefined))
        }
    }

    "Coder registration" should {
        import fixtures._
        import Coders._

        Coding.reset.before

        "allow specific registration" in {
            val clazz = classOf[CaseClass]
            Coding.register(clazz, Coding.registration("CaseClass", () => pojoClassCoder))
            builder.typeRFor(clazz).flatMap(Coding.forTypeComposable(clazz.getClassLoader, _)) must_== Okay(pojoClassCoder)
        }

        "override automatic reflection" in {
            val clazz = classOf[CaseClass]
            builder.typeRFor(clazz).flatMap(Coding.forTypeComposable(clazz.getClassLoader, _)) must_== Okay(caseClassCoder)
            Cache.reset
            Coding.register(clazz, Coding.registration("CaseClass", () => pojoClassCoder))
            builder.typeRFor(clazz).flatMap(Coding.forTypeComposable(clazz.getClassLoader, _)) must_== Okay(pojoClassCoder)
        }

        "use sidecars when present" in {
            // assert this to make sure that we're really testing something
            val clazz = classOf[CaseClassWithSideCar]
            builder.typeRFor(clazz).flatMap(Reflection.reflectObjectCoder(clazz.getClassLoader, _)) must_== Okay(caseClassWithSideCarReflectedCoder)
            builder.typeRFor(clazz).flatMap(Coding.forTypeComposable(clazz.getClassLoader, _)) must_== Okay(caseClassWithSideCarExplicitCoder)
        }
    }


    "Coder diagnostics" should {
        import Coders._

        val cl = this.getClass.getClassLoader

        "work for manual coding" in {
            Coder(cl, wrapperCoder).decode(JObject(Nil)) must beLike { case Failed("At cc: required but missing") => true }
            Coder(cl, wrapperCoder).decode(("cc" -> ("bar" -> "baz"))) must beLike { case Failed("At cc.foo: required but missing") => true }
        }

        "work for automatic coding" in {
            val v: JValue = ("a" -> ("k" -> ("b" -> JArray(("d" -> "foo") :: Nil))))
            Coding.decode[DiagnosticTestOuterClass](v) must beLike { case Failed("At a[\"k\"].b[0].c: required but missing") => true }
        }

    }
}
