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
import org.specs2.SpecificationWithJUnit
import org.specs2.execute.{Result => SpecsResult}
import com.paytronix.utils.extendedreflection.Builder
import com.paytronix.utils.interchange._
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, parameter}

import fixtures._
import Helpers._

object CodingFixtures {
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
}

import CodingFixtures._

class CodingFrontendSpecTest extends SpecificationWithJUnit {
    def is =
        "Coding frontend should" ^
        "support quick decode" ! resetCoding {
            Coding.decode[CaseClass](("foo" -> 1) ~ ("bar" -> "baz") ~ ("zip" -> "qux")) must_== Okay(CaseClass(1, "baz", Some("qux")))
        } ^
        "support quick encode" ! resetCoding {
            Coding.encode(CaseClass(1, "baz", Some("qux"))) | parameter(Nil) must matchEncodedJson(("foo" -> 1) ~ ("bar" -> "baz") ~ ("zip" -> "qux"))
        }
}

class CoderDetectionSpecTest extends SpecificationWithJUnit {
    import fixtures._
    import Coders._

    def is =
        "Coder detection should" ^
        "work for scalar types" ! resetCoding {
            { new BigIntTV().coder          must_== Okay(BigIntCoder) } and
            { new BigIntegerTV().coder      must_== Okay(BigIntegerCoder) } and
            { new BooleanTV().coder         must_== Okay(BooleanCoder) } and
            { new ByteTV().coder            must_== Okay(ByteCoder) } and
            { new CharTV().coder            must_== Okay(CharCoder) } and
            { new DoubleTV().coder          must_== Okay(DoubleCoder) } and
            { new FloatTV().coder           must_== Okay(FloatCoder) } and
            { new IntTV().coder             must_== Okay(IntCoder) } and
            { new JValueTV().coder          must_== Okay(JValueCoder) } and
            { new JavaBigDecimalTV().coder  must_== Okay(JavaBigDecimalCoder) } and
            { new JavaDateTV().coder        must_== Okay(JavaDateCoder) } and
            { new LongTV().coder            must_== Okay(LongCoder) } and
            { new ScalaBigDecimalTV().coder must_== Okay(ScalaBigDecimalCoder) } and
            { new ShortTV().coder           must_== Okay(ShortCoder) } and
            { new StringTV().coder          must_== Okay(StringCoder) }
        } ^
        "work for java boxed types" ! resetCoding {
            { new BoxedBooleanTV().coder   must_== Okay(BooleanCoder) } and
            { new BoxedByteTV().coder      must_== Okay(ByteCoder) } and
            { new BoxedCharacterTV().coder must_== Okay(CharCoder) } and
            { new BoxedIntegerTV().coder   must_== Okay(IntCoder) } and
            { new BoxedLongTV().coder      must_== Okay(LongCoder) } and
            { new BoxedShortTV().coder     must_== Okay(ShortCoder) }
        } ^
        "work for optional types" ! resetCoding {
            { new OptionIntTV().coder    must_== Okay(OptionCoder(IntCoder)) } and
            { new ResultStringTV().coder must_== Okay(ResultCoder(UnitCoder, StringCoder)) } and
            { new ResultUnitTV().coder   must_== Okay(ResultCoder(UnitCoder, UnitCoder)) }
        } ^
        "work for Either" ! resetCoding {
            new EitherTV().coder must_== Okay(EitherCoder(StringCoder, IntCoder))
        } ^
        "work for simple collection types" ! {
            { new JavaListStringTV().coder must_== Okay(JavaListCoder(StringCoder)) } and
            { new ListStringTV().coder     must_== Okay(ScalaListCoder(StringCoder)) }
        } ^
        "work for map types" ! resetCoding {
            { new JavaMapTV().coder      must_== Okay(JavaMapCoder(StringCoder, IntCoder)) } and
            { new ImmutableMapTV().coder must_== Okay(ScalaImmutableMapCoder(StringCoder, IntCoder)) } and
            { new MutableMapTV().coder   must_== Okay(ScalaMutableMapCoder(StringCoder, IntCoder)) }
        } ^
        "work for set types" ! resetCoding {
            { new ImmutableSetTV().coder must_== Okay(ScalaImmutableSetCoder(StringCoder)) } and
            { new MutableSetTV().coder must_== Okay(ScalaMutableSetCoder(StringCoder)) }
        } ^
        "work for tuple types" ! resetCoding {
            { new Tuple1TV().coder must_== Okay(Tuple1Coder(ByteCoder)) } and
            { new Tuple2TV().coder must_== Okay(Tuple2Coder(ByteCoder, CharCoder)) } and
            { new Tuple3TV().coder must_== Okay(Tuple3Coder(ByteCoder, CharCoder, ShortCoder)) } and
            { new Tuple4TV().coder must_== Okay(Tuple4Coder(ByteCoder, CharCoder, ShortCoder, IntCoder)) } and
            { new Tuple5TV().coder must_== Okay(Tuple5Coder(ByteCoder, CharCoder, ShortCoder, IntCoder, LongCoder)) } and
            { new Tuple6TV().coder must_== Okay(Tuple6Coder(ByteCoder, CharCoder, ShortCoder, IntCoder, LongCoder, StringCoder)) }
        } ^
        "work for object types" ! resetCoding {
            { new BasicClassTV().coder    must_== Okay(basicClassCoder) } and
            { new CaseClassTV().coder     must_== Okay(caseClassCoder) } and
            { new ComposedClassTV().coder must_== Okay(composedClassCoder) } and
            { new POJOClassTV().coder     must_== Okay(pojoClassCoder) }
        } ^
        "work for nested object types" ! resetCoding {
            new WrapperTV().coder must_== Okay(wrapperCoder)
        } ^
        "work for methods" ! resetCoding {
            val meth = classOf[MethodAndCtor].getMethods.find(_.getName == "method").head

            { Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._1) must_== Okay(methodAndCtorArgumentCoder) } and
            { Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._2) must_== Okay(StringCoder) }
        } ^
        "work for methods of traits without @Named" ! resetCoding {
            val meth = classOf[MethodTrait].getMethods.find(_.getName == "method").head

            { Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._1) must_== Okay(methodAndCtorArgumentCoder) } and
            { Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._2) must_== Okay(StringCoder) }
        } ^
        "work for methods of traits with @Named" ! resetCoding {
            val meth = classOf[NamedMethodTrait].getMethods.find(_.getName == "method").head

            { Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._1) must_== Okay(methodAndCtorArgumentCoder) } and
            { Reflection.reflect(meth.getDeclaringClass.getClassLoader, meth).map(_._2) must_== Okay(StringCoder) }
        } ^
        "work for classes implementing traits with types refined to primitives (yes, this is crazy!)" ! resetCoding {
            val clazz = classOf[PrimitiveRefinementTestClass]

            def check(name: String, expected: ComposableCoder[_]): SpecsResult = {
                val meth = clazz.getMethod(name)
                val result = Reflection.reflect(clazz.getClassLoader, meth)

                result.map(_._2) must_== Okay(expected)
            }

            check("aUnit", UnitCoder) and
            check("aBoolean", BooleanCoder) and
            check("aByte", ByteCoder) and
            check("aShort", ShortCoder) and
            check("aChar", CharCoder) and
            check("aInt", IntCoder) and
            check("aLong", LongCoder) and
            check("aFloat", FloatCoder) and
            check("aDouble", DoubleCoder)
        } ^
        "work for constructors" ! resetCoding {
            val ctor = classOf[MethodAndCtor].getConstructors.head
            Reflection.reflect(ctor.getDeclaringClass.getClassLoader, ctor) must_== Okay(methodAndCtorArgumentCoder)
        } ^
        "work for parameterized types in parameterized types" ! resetCoding {
            new NestedOptionTV().coder must_== Okay(OptionCoder(OptionCoder(OptionCoder(StringCoder))))
        } ^
        "work for classes enclosed in objects" ! resetCoding {
            { new WrappedClassTV().coder must_== Okay(wrappedClassCoder) } and
            { new SubWrappedClassTV().coder must_== Okay(subWrappedClassCoder) }
        } ^
        "fail to code The Uncodable" ! resetCoding {
            new UncodableTV().coder must beLike { case FailedG(_, _) => ok }
        } ^
        "not attempt to code lazy vals" ! resetCoding {
            new ClassWithLazyValTV().coder must_== Okay(classWithLazyValCoder)
        } ^
        "not attempt to code @NotCoded vars" ! resetCoding {
            new ClassWithNotCodedVarTV().coder must_== Okay(classWithNotCodedVarCoder)
        } ^
        "work for enums" ! resetCoding {
            { new ScalaEnumWrapperTV().coder must_== Okay(scalaEnumWrapperCoder) } and
            { new JavaEnumWrapperTV().coder must_== Okay(javaEnumWrapperCoder) }
        } ^
        "properly exclude constructors with non-property arguments" ! resetCoding {
            new ClassWithMultipleConstructorsTV().coder must_== Okay(classWithMultipleConstructorsCoder)
        } ^
        "properly fail when an incorrect constructor is forced" ! resetCoding {
            new ClassWithIllegalConstructorTV().coder must beLike { case FailedG(_, _) => ok }
        } ^
        "property fail for inner classes" ! resetCoding {
            new EnclosedCaseClassTV().coder must beLike { case FailedG(_, _) => ok }
        }
}

class CoderRegistrationSpecTest extends SpecificationWithJUnit {
    import fixtures._
    import Coders._

    def is =
        "Coder registration should" ^
        "allow specific registration" ! resetCoding {
            val clazz = classOf[CaseClass]
            Coding.register(clazz, Coding.registration("CaseClass", () => pojoClassCoder))
            builder.typeRFor(clazz).flatMap(Coding.forTypeComposable(clazz.getClassLoader, _)) must_== Okay(pojoClassCoder)
        } ^
        "override automatic reflection" ! resetCoding {
            val clazz = classOf[CaseClass]

            { builder.typeRFor(clazz).flatMap(Coding.forTypeComposable(clazz.getClassLoader, _)) must_== Okay(caseClassCoder) } and
            {
                Cache.reset
                Coding.register(clazz, Coding.registration("CaseClass", () => pojoClassCoder))
                builder.typeRFor(clazz).flatMap(Coding.forTypeComposable(clazz.getClassLoader, _)) must_== Okay(pojoClassCoder)
            }
        } ^
        "use sidecars when present" ! resetCoding {
            // assert this to make sure that we're really testing something
            val clazz = classOf[CaseClassWithSideCar]

            { builder.typeRFor(clazz).flatMap(Reflection.reflectObjectCoder(clazz.getClassLoader, _)) must_== Okay(caseClassWithSideCarReflectedCoder) } and
            { builder.typeRFor(clazz).flatMap(Coding.forTypeComposable(clazz.getClassLoader, _)) must_== Okay(caseClassWithSideCarExplicitCoder) }
        }
}

class CoderDiagnosticsSpecTest extends SpecificationWithJUnit {
    import Coders._
    val cl = this.getClass.getClassLoader

    def is =
        "Coder diagnostics should" ^
        "work for manual coding" ! {
            { Coder(cl, wrapperCoder).decode(JObject(Nil)) must beLike { case Failed.Message("At cc: required but missing") => ok } } and
            { Coder(cl, wrapperCoder).decode(("cc" -> ("bar" -> "baz"))) must beLike { case Failed.Message("At cc/foo: required but missing") => ok } }
        } ^
        "work for automatic coding" ! {
            val v: JValue = ("a" -> ("k" -> ("b" -> JArray(("d" -> "foo") :: Nil))))
            Coding.decode[DiagnosticTestOuterClass](v) must beLike { case Failed.Message("At a[\"k\"]/b[0]/c: required but missing") => ok }
        }
}
