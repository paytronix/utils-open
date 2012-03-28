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

package com.paytronix.utils.extendedreflection

import scala.tools.scalap.scalax.rules.{scalasig => sig}
import ch.qos.logback.classic.BasicConfigurator
import org.specs._
import org.specs.runner.JUnit4
import com.paytronix.utils.scala.result.{ResultG, Okay}

class ExtendedReflectionTestSpecsAsTest extends JUnit4(ExtendedReflectionTestSpecs)

object Shared {
    implicit val builder = new Builder(getClass.getClassLoader)
}

object ExtendedReflectionTestSpecs extends Specification {
    import Shared.builder
    import Helpers._

    val UnitClass = classOf[Unit]
    val BooleanClass = classOf[Boolean]
    val ByteClass = classOf[Byte]
    val ShortClass = classOf[Short]
    val CharClass = classOf[Char]
    val IntClass = classOf[Int]
    val LongClass = classOf[Long]
    val FloatClass = classOf[Float]
    val DoubleClass = classOf[Double]
    val JavaListClass = classOf[java.util.List[_]]
    val OptionClass = classOf[Option[_]]
    val StringClass = classOf[String]
    val Tuple2Class = classOf[Tuple2[_,_]]

    def ownedByLibraryInterface(cmr: ClassMemberR): Boolean = cmr.ownerName.qualified match {
        case "java.lang.Object"|"scala.Product"|"scala.Equals" => true
        case _ => false
    }

    def withConstructor(numParams: Int)(f: ConstructorR => Unit)(implicit classR: ClassR): Unit = {
        val ctorROption = classR.constructors.find(_.parameters.length == numParams)
        ctorROption must verify(_.isDefined)
        ctorROption.get.ownerName.qualified must_== classR.name.qualified
        f(ctorROption.get)
    }

    def withProperty(f: PropertyR => Unit)(implicit propInfo: (String, TypeRMatcher), classR: ClassR): Unit =
        withPropertyNamed(propInfo._1)(f)

    def withPropertyNamed(n: String)(f: PropertyR => Unit)(implicit classR: ClassR): Unit =
        withAnyPropertyNamed(n)(propR => {
            propR.ownerName.qualified must_== classR.name.qualified
            f(propR)
        })

    def withAnyPropertyNamed(n: String)(f: PropertyR => Unit)(implicit classR: ClassR): Unit = {
        val propROption = classR.properties.find(_.name == n)
        propROption must verify(_.isDefined)
        f(propROption.get)
    }

    def withMethod(n: String, numParams: Int)(f: MethodR => Unit)(implicit classR: ClassR): Unit =
        withAnyMethod(n, numParams)(methR => {
            methR.ownerName.qualified must_== classR.name.qualified
            f(methR)
        })

    def withAnyMethod(n: String, numParams: Int)(f: MethodR => Unit)(implicit classR: ClassR): Unit = {
        val methROption = classR.methods.find(methR => methR.name == n && methR.parameters.length == numParams)
        methROption must verify(_.isDefined)
        f(methROption.get)
    }


    "Extended reflection on a plain Scala class" should {
        lazy val targetClassRResult = builder.classRFor(classOf[PlainScalaClass])
        implicit def targetClassR = targetClassRResult.orThrow

        "not fail" in {
            targetClassRResult must verify(_.isDefined)
        }

        "model only the expected members" in {
            targetClassR.members.filterNot(ownedByLibraryInterface).map(_.name) must_== List(
                "<init>",
                "<init>",
                "<init>",
                "<init>",

                "aBeanPropertyLazyVal",
                "aBeanPropertyVal",
                "aBeanPropertyVar",
                "aBooleanBeanPropertyLazyVal",
                "aBooleanBeanPropertyVal",
                "aBooleanBeanPropertyVar",
                "aLazyValString",
                "aPrivateVal",
                "aPrivateVar",
                "aValInt",
                "aValOptionBoolean",
                "aValOptionByte",
                "aValOptionChar",
                "aValOptionDouble",
                "aValOptionFloat",
                "aValOptionInt",
                "aValOptionLong",
                "aValOptionShort",
                "aValOptionUnit",
                "aVarOptionString",
                "anAnnotatedVal",
                "anAnnotatedVar",
                "ctorValParam",
                "ctorVarParam",

                "aDefDouble",
                "aFunctionOfInt",
                "aFunctionOfIntAndString",
                "aFunctionOfOptionString",
                "aNullaryFunction",
                "aPrivateDef",
                "anAnnotatedMethod",
                "anOverloadedFunction",
                "anOverloadedFunction",
                "methodWithMisleadingName_$eq",
                "setMethodWithMisleadingName"
            )
        }



        "model the zero arg constructor" in {
            withConstructor(0)(ctor => {
                ctor.annotations must beEmpty
            })
        }

        "model the one arg constructor" in {
            withConstructor(1)(ctor => {
                ctor.annotations must beEmpty
                ctor must haveParametersLike("i" -> IntClass)
            })
        }

        "model the three arg constructor" in {
            withConstructor(3)(ctor => {
                ctor.annotations must beEmpty
                ctor must haveParametersLike("ctorParam" -> IntClass, "ctorValParam" -> StringClass, "ctorVarParam" -> (OptionClass, StringClass))
            })
        }

        "model a val" in {
            implicit val property = ("aValInt", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a var with an Option type" in {
            implicit val property = ("aVarOptionString", (OptionClass, StringClass): TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must beValidSetter
                prop.others must beEmpty
            })
        }

        "model a def with no argument list" in {
            withMethod("aDefDouble", 0)(meth => {
                meth.annotations must beEmpty
                meth must haveResultLike(DoubleClass)
            })
        }

        "model a lazy val" in {
            implicit val property = ("aLazyValString", StringClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a nullary function" in {
            withMethod("aNullaryFunction", 0)(meth => {
                meth.annotations must beEmpty
                meth must haveResultLike(UnitClass)
            })
        }

        "model a function of Int" in {
            withMethod("aFunctionOfInt", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("in" -> IntClass)
                meth must haveResultLike(IntClass)
            })
        }

        "model a function of Option[String]" in {
            withMethod("aFunctionOfOptionString", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("in" -> (OptionClass, StringClass))
                meth must haveResultLike((OptionClass, StringClass))
            })
        }

        "model a function of Int and String" in {
            withMethod("aFunctionOfIntAndString", 2)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("i" -> IntClass, "s" -> StringClass)
                meth must haveResultLike((Tuple2Class, IntClass, StringClass))
            })
        }

        "model an overloaded function with one argument" in {
            withMethod("anOverloadedFunction", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("i" -> IntClass)
                meth must haveResultLike(IntClass)
            })
        }

        "model an overloaded function with two arguments" in {
            withMethod("anOverloadedFunction", 2)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("i" -> IntClass, "s" -> StringClass)
                meth must haveResultLike(StringClass)
            })
        }

        "model a @BeanProperty val" in {
            implicit val property = ("aBeanPropertyVal", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beLike {
                    case bpgetter :: Nil =>
                        bpgetter must beValidBeanGetter
                }
            })
        }

        "model a @BeanProperty var" in {
            implicit val property = ("aBeanPropertyVar", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                prop.getter must beValidGetter
                prop.setter must beValidSetter
                prop.others must beLike {
                    case bpgetter :: bpsetter :: Nil =>
                        bpgetter must beValidBeanGetter
                        Okay(bpsetter) must beValidBeanSetter
                }
            })
        }

        "model a @BeanProperty lazy val" in {
            implicit val property = ("aBeanPropertyLazyVal", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beLike {
                    case bpgetter :: Nil =>
                        bpgetter must beValidBeanGetter
                }
            })
        }

        "model a @BooleanBeanProperty val" in {
            implicit val property = ("aBooleanBeanPropertyVal", BooleanClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beLike {
                    case bpgetter :: Nil =>
                        bpgetter must beValidBooleanBeanGetter
                }
            })
        }

        "model a @BooleanBeanProperty var" in {
            implicit val property = ("aBooleanBeanPropertyVar", BooleanClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                prop.getter must beValidGetter
                prop.setter must beValidSetter
                prop.others must beLike {
                    case bpgetter :: bpsetter :: Nil =>
                        bpgetter must beValidBooleanBeanGetter
                        Okay(bpsetter) must beValidBeanSetter
                }
            })
        }

        "model a @BooleanBeanProperty lazy val" in {
            implicit val property = ("aBooleanBeanPropertyLazyVal", BooleanClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beLike {
                    case bpgetter :: Nil =>
                        bpgetter must beValidBooleanBeanGetter
                }
            })
        }

        "model an annotated constructor" in {
            withConstructor(2)(ctor => {
                ctor.annotations must beLike {
                    case annot :: Nil => annot must beLike { case (_: TestAnnotation) => true }
                }
                ctor must haveParametersLike("i" -> IntClass, "s" -> StringClass)
            })
        }

        "model an annotated method" in {
            withMethod("anAnnotatedMethod", 2)(meth => {
                meth.annotations must beLike {
                    case annot :: Nil => annot must beLike { case (_: TestAnnotation) => true }
                }
                meth must haveParametersLike("i" -> IntClass, "s" -> StringClass)
                meth.parameters.find(_.name == Some("s")) must beLike {
                    case Some(paramR) => paramR.annotations must beLike {
                        case annot :: Nil => annot must beLike { case (_: TestAnnotation) => true }
                    }
                }
                meth must haveResultLike((Tuple2Class, IntClass, StringClass))
            })
        }

        "model an annotated val" in {
            implicit val property = ("anAnnotatedVal", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beLike {
                    case annot :: Nil => annot must beLike { case (_: TestAnnotation) => true }
                }
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model an annotated var" in {
            implicit val property = ("anAnnotatedVar", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beLike {
                    case annot :: Nil => annot must beLike { case (_: TestAnnotation) => true }
                }
                prop.getter must beValidGetter
                prop.setter must beValidSetter
                prop.others must beEmpty
            })
        }

        "properly cache ClassRs" in {
            val ctx = new Builder(getClass.getClassLoader)
            if (ctx.classRFor(classOf[PlainScalaClass]) ne ctx.classRFor(classOf[PlainScalaClass])) {
                fail("First instance of ClassR was not returned again for a second call")
            } else {
                true must_== true // to avoid "skipped"
            }
        }

        "properly cache signatures" in {
            val ctx = new Builder(getClass.getClassLoader)
            if (ctx.sigFor(classOf[PlainScalaClass]) ne ctx.sigFor(classOf[PlainScalaClass])) {
                fail("First instance of ScalaSig was not returned again for a second call")
            } else {
                true must_== true // to avoid "skipped"
            }
        }

        "model non-public vals" in {
            implicit val property = ("aPrivateVal", IntClass: TypeRMatcher)
            val propROption = targetClassR.nonPublicProperties.find(_.name == property._1)
            propROption must verify(_.isDefined)
            val propR = propROption.get
            propR.ownerName.qualified must_== targetClassR.name.qualified
            propR.annotations must beEmpty
            propR.getter must beValidGetter
            propR.setter must notBePresent
            propR.others must beEmpty
            propR.isPublic must_== false
            propR.getter.isPublic must_== false
        }

        "model non-public vars" in {
            implicit val property = ("aPrivateVar", IntClass: TypeRMatcher)
            val propROption = targetClassR.nonPublicProperties.find(_.name == property._1)
            propROption must verify(_.isDefined)
            val propR = propROption.get
            propR.ownerName.qualified must_== targetClassR.name.qualified
            propR.annotations must beEmpty
            propR.getter must beValidGetter
            propR.setter must beValidSetter
            propR.others must beEmpty
            propR.isPublic must_== false
            propR.getter.isPublic must_== false
            propR.setter.orThrow.isPublic must_== false
        }

        "model non-public defs" in {
            val methROption = targetClassR.nonPublicMethods.find(_.name == "aPrivateDef")
            methROption must verify(_.isDefined)
            val methR = methROption.get
            methR.ownerName.qualified must_== targetClassR.name.qualified
            methR.annotations must beEmpty
            methR.isPublic must_== false
            methR.parameters must beEmpty
            methR must haveResultLike(IntClass)
        }

        "model methods with names that make them look like setters" in {
            withMethod("setMethodWithMisleadingName", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("i" -> IntClass)
                meth must haveResultLike(UnitClass)
            })
            withMethod("methodWithMisleadingName_$eq", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("i" -> IntClass)
                meth must haveResultLike(UnitClass)
            })
        }

        "model Option[Boolean]" in {
            implicit val property = ("aValOptionBoolean", (OptionClass, BooleanClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        }

        "model Option[Byte]" in {
            implicit val property = ("aValOptionByte", (OptionClass, ByteClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        }

        "model Option[Char]" in {
            implicit val property = ("aValOptionChar", (OptionClass, CharClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        }

        "model Option[Double]" in {
            implicit val property = ("aValOptionDouble", (OptionClass, DoubleClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        }

        "model Option[Float]" in {
            implicit val property = ("aValOptionFloat", (OptionClass, FloatClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        }

        "model Option[Int]" in {
            implicit val property = ("aValOptionInt", (OptionClass, IntClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        }

        "model Option[Long]" in {
            implicit val property = ("aValOptionLong", (OptionClass, LongClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        }

        "model Option[Short]" in {
            implicit val property = ("aValOptionShort", (OptionClass, ShortClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        }

        "model Option[Unit]" in {
            implicit val property = ("aValOptionUnit", (OptionClass, UnitClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        }
    }

    "Extended reflection on a Scala case class" should {
        lazy val targetClassRResult = builder.classRFor(classOf[CaseClass])
        implicit def targetClassR = targetClassRResult.orThrow

        "not fail" in {
            targetClassRResult must verify(_.isDefined)
        }

        "model only the expected members" in {
            targetClassR.members.filterNot(ownedByLibraryInterface).map(_.name) must_== List(
                "<init>",

                "aLazyValString",
                "aValInt",
                "aVarOptionString",
                "ctorParam",

                "aDefDouble",
                "aFunctionOfInt"
            )
        }

        "model the constructor" in {
            withConstructor(1)(ctor => {
                ctor.annotations must beEmpty
                ctor must haveParametersLike("ctorParam" -> IntClass)
            })
        }

        "model a val" in {
            implicit val property = ("aValInt", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a var with an Option type" in {
            implicit val property = ("aVarOptionString", (OptionClass, StringClass): TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must beValidSetter
                prop.others must beEmpty
            })
        }

        "model a def with no argument list" in {
            withMethod("aDefDouble", 0)(meth => {
                meth.annotations must beEmpty
                meth must haveResultLike(DoubleClass)
            })
        }

        "model a lazy val" in {
            implicit val property = ("aLazyValString", StringClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a function of Int" in {
            withMethod("aFunctionOfInt", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("in" -> IntClass)
                meth must haveResultLike(IntClass)
            })
        }
    }

    "Extended reflection on a Scala composed class" should {
        lazy val mixedClassRResult = builder.classRFor(classOf[MixedClass])
        implicit def mixedClassR = mixedClassRResult.orThrow

        "not fail" in {
            mixedClassRResult must verify(_.isDefined)
        }

        "model only the expected members" in {
            mixedClassR.members.filterNot(ownedByLibraryInterface).map(_.name) must_== List(
                "<init>",

                "firstMethod",
                "secondMethod",
                "thirdMethod"
            )
        }

        "model the interfaces" in {
            mixedClassR.interfaces must exist(_.name.qualified == NameR(classOf[FirstMixin].getName).qualified)
            mixedClassR.interfaces must exist(_.name.qualified == NameR(classOf[SecondMixin].getName).qualified)
        }

        "model the constructor" in {
            withConstructor(0)(ctor => {
                ctor.annotations must beEmpty
            })
        }

        "model the first method" in {
            withAnyMethod("firstMethod", 1)(meth => {
                meth.ownerName.qualified must_== NameR(classOf[FirstMixin].getName).qualified
                meth.annotations must beEmpty
                meth must haveParametersLike("in" -> StringClass)
                meth must haveResultLike(StringClass)
            })
        }

        "model the second method" in {
            withAnyMethod("secondMethod", 1)(meth => {
                meth.ownerName.qualified must_== NameR(classOf[SecondMixin].getName).qualified
                meth.annotations must beEmpty
                meth must haveParametersLike("in" -> IntClass)
                meth must haveResultLike(IntClass)
            })
        }

        "model the third method" in {
            withMethod("thirdMethod", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("in" -> DoubleClass)
                meth must haveResultLike(DoubleClass)
            })
        }
    }

    "Extended reflection on a class enclosed in an object" should {
        lazy val targetClassRResult = builder.classRFor(classOf[EnclosingObject.SubEnclosingObject.CaseClass])
        implicit def targetClassR = targetClassRResult.orThrow

        "not fail" in {
            targetClassRResult must verify(_.isDefined)
        }

        "model only the expected members" in {
            targetClassR.members.filterNot(ownedByLibraryInterface).map(_.name) must_== List(
                "<init>",

                "ctorParam"
            )
        }


        "model the one arg constructor" in {
            withConstructor(1)(ctor => {
                ctor.annotations must beEmpty
                ctor must haveParametersLike("ctorParam" -> IntClass)
            })
        }

        "model a val" in {
            implicit val property = ("ctorParam", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }
    }

    /* 2010-09-17 RMM: Don't need this to work at the moment since we can't support instantiation of inner classes for JSON decoding anyway.

    "Extended reflection on a class enclosed in a class" should {
        lazy val targetClassRResult = builder.classRFor(classOf[EnclosingObject.SubEnclosingClass#CaseClass])
        implicit def targetClassR = targetClassRResult.orThrow

        "not fail" in {
            targetClassRResult must verify(_.isDefined)
        }

        "model only the expected members" in {
            targetClassR.members.filterNot(ownedByLibraryInterface).map(_.name) must_== List(
                "<init>",

                "ctorParam"
            )
        }


        "model the one arg constructor" in {
            withConstructor(1)(ctor => {
                ctor.annotations must beEmpty
                ctor must haveParametersLike("ctorParam" -> IntClass)
            })
        }

        "model a val" in {
            implicit val property = ("ctorParam", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }
    }
    */

    "Extended reflection on a plain Java class" should {
        lazy val targetClassRResult = builder.classRFor(classOf[PlainJavaClass])
        implicit def targetClassR = targetClassRResult.orThrow

        "not fail" in {
            targetClassRResult must verify(_.isDefined)
        }

        "model only the expected members" in {
            targetClassR.members.filterNot(ownedByLibraryInterface).map(_.name) must_== List(
                "<init>",
                "<init>",
                "<init>",

                "aReadOnlyBoolean",
                "aReadOnlyInt",
                "aReadWriteInt",
                "aReadWriteListOfString",
                "anAnnotatedInt",
                "intWithAnnotatedReader",

                "aNormalMethod",
                "anAnnotatedIntMethod",
                "setMethodWithMisleadingName"
            )
        }

        "model the zero arg constructor" in {
            withConstructor(0)(ctor => {
                ctor.annotations must beEmpty
            })
        }

        "model the one arg constructor" in {
            withConstructor(1)(ctor => {
                ctor.annotations must beEmpty
                ctor must haveParametersLike("i" -> IntClass)
            })
        }

        "model the two arg constructor" in {
            withConstructor(2)(ctor => {
                ctor.annotations must beLike {
                    case annot :: Nil => annot must beLike { case (_: TestAnnotation) => true }
                }
                ctor must haveParametersLike("i" -> IntClass, "s" -> StringClass)
                ctor.parameters.find(_.name == Some("s")) must beLike {
                    case Some(paramR) => paramR.annotations must beLike {
                        case annot :: Nil => annot must beLike { case (_: TestAnnotation) => true }
                    }
                }
            })
        }

        "model a read only int bean property" in {
            implicit val property = ("aReadOnlyInt", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidBeanGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a read write int bean property" in {
            implicit val property = ("aReadWriteInt", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidBeanGetter
                prop.setter must beValidBeanSetter
                prop.others must beEmpty
            })
        }

        "model a read write List<String> bean property" in {
            implicit val property = ("aReadWriteListOfString", (JavaListClass, StringClass): TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidBeanGetter
                prop.setter must beValidBeanSetter
                prop.others must beEmpty
            })
        }

        "model a read only boolean bean property" in {
            implicit val property = ("aReadOnlyBoolean", BooleanClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidBooleanBeanGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model an annotated int bean property (annotated at field level)" in {
            implicit val property = ("anAnnotatedInt", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beLike {
                    case annot :: Nil => annot must beLike { case (_: TestAnnotation) => true }
                }
                prop.getter must beValidBeanGetter
                prop.setter must beValidBeanSetter
                prop.others must beEmpty
            })
        }

        "model an annotated int bean property (annotated at getter level)" in {
            implicit val property = ("intWithAnnotatedReader", IntClass: TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beLike {
                    case annot :: Nil => annot must beLike { case (_: TestAnnotation) => true }
                }
                prop.getter must beValidBeanGetter
                prop.setter must beValidBeanSetter
                prop.others must beEmpty
            })
        }

        "model a normal method" in {
            withMethod("aNormalMethod", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("i" -> IntClass)
                meth must haveResultLike(UnitClass)
            })
        }

        "model an annotated method" in {
            withMethod("anAnnotatedIntMethod", 0)(meth => {
                meth.annotations must beLike {
                    case annot :: Nil => annot must beLike { case (_: TestAnnotation) => true }
                }
                meth must haveResultLike(IntClass)
            })
        }

        "model method with a name that makes it look like a setter" in {
            withMethod("setMethodWithMisleadingName", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("i" -> IntClass)
                meth must haveResultLike(UnitClass)
            })
        }
    }

    "Extended reflection on a plain Java interface (without @Named)" should {
        lazy val targetClassRResult = builder.classRFor(classOf[JavaInterfaceWithoutNamed])
        implicit def targetClassR = targetClassRResult.orThrow

        "not fail" in {
            targetClassRResult must verify(_.isDefined)
        }

        "model only the expected members" in {
            targetClassR.members.map(_.name) must_== List(
                "doSomething"
            )
        }

        "model the method" in {
            withMethod("doSomething", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike(IntClass)
                meth must haveResultLike(UnitClass)
            })
        }
    }

    "Extended reflection on a plain Java interface (with @Named)" should {
        lazy val targetClassRResult = builder.classRFor(classOf[JavaInterfaceWithNamed])
        implicit def targetClassR = targetClassRResult.orThrow

        "not fail" in {
            targetClassRResult must verify(_.isDefined)
        }

        "model only the expected members" in {
            targetClassR.members.map(_.name) must_== List(
                "doSomething"
            )
        }

        "model the method" in {
            withMethod("doSomething", 1)(meth => {
                meth.annotations must beEmpty
                meth must haveParametersLike("i" -> IntClass)
                meth must haveResultLike(UnitClass)
            })
        }
    }

    "Extended reflection should resolve type aliases" should {
        lazy val targetClassRResult = builder.classRFor(classOf[TypeTester])
        implicit def targetClassR = targetClassRResult.orThrow

        "not fail" in {
            targetClassRResult must verify(_.isDefined)
        }

        "model a type alias with no arguments (f)" in {
            implicit val property = ("f", (classOf[P[_]], StringClass): TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a type alias with one argument (g)" in {
            implicit val property = ("g", (classOf[Q[_, _]], IntClass, StringClass): TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a type alias with two arguments with one ignored (h)" in {
            implicit val property = ("h", (classOf[Q[_, _]], ByteClass, StringClass): TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a type alias with one argument to an external type alias (i)" in {
            implicit val property = ("i", (classOf[ResultG[_, _]], UnitClass, FloatClass): TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a type alias with one argument to an external class (j)" in {
            implicit val property = ("j", (classOf[ResultG[_, _]], StringClass, DoubleClass): TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a type alias in a nested object (k)" in {
            implicit val property = ("k", (classOf[P[_]], StringClass): TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }

        "model a class-local type alias (l)" in {
            implicit val property = ("l", (classOf[ResultG[_, _]], UnitClass, CharClass): TypeRMatcher)
            withProperty(prop => {
                prop.annotations must beEmpty
                prop.getter must beValidGetter
                prop.setter must notBePresent
                prop.others must beEmpty
            })
        }
    }
}
