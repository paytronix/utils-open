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
import org.specs2.SpecificationWithJUnit
import com.paytronix.utils.scala.result.{ResultG, Okay}

object Shared {
    implicit val builder = new Builder(getClass.getClassLoader)
}

import Shared.builder
import Helpers._

class PlainScalaClassSpecTest extends SpecificationWithJUnit {
    lazy val targetClassRResult = builder.classRFor(classOf[PlainScalaClass])
    implicit def targetClassR = targetClassRResult.orThrow

    def is =
        "Extended reflection on a plain Scala class should" ^
        "not fail" ! {
            targetClassRResult must beLike { case Okay(_) => ok }
        } ^
        "model only the expected members" ! {
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
        } ^
        "model the zero arg constructor" ! {
            withConstructor(0) { ctor =>
                ctor.annotations must beEmpty
            }
        } ^
        "model the one arg constructor" ! {
            withConstructor(1) { ctor =>
                { ctor.annotations must beEmpty } and
                { ctor must haveParametersLike("i" -> IntClass) }
            }
        } ^
        "model the three arg constructor" ! {
            withConstructor(3) { ctor =>
                { ctor.annotations must beEmpty } and
                { ctor must haveParametersLike("ctorParam" -> IntClass, "ctorValParam" -> StringClass, "ctorVarParam" -> (OptionClass, StringClass)) }
            }
        } ^
        "model a val" ! {
            implicit val property = ("aValInt", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a var with an Option type" ! {
            implicit val property = ("aVarOptionString", (OptionClass, StringClass): TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must beValidSetter } and
                { prop.others must beEmpty }
            }
        } ^
        "model a def with no argument list" ! {
            withMethod("aDefDouble", 0) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveResultLike(DoubleClass) }
            }
        } ^
        "model a lazy val" ! {
            implicit val property = ("aLazyValString", StringClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a nullary function" ! {
            withMethod("aNullaryFunction", 0) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveResultLike(UnitClass) }
            }
        } ^
        "model a function of Int" ! {
            withMethod("aFunctionOfInt", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("in" -> IntClass) }
                { meth must haveResultLike(IntClass) }
            }
        } ^
        "model a function of Option[String]" ! {
            withMethod("aFunctionOfOptionString", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("in" -> (OptionClass, StringClass)) } and
                { meth must haveResultLike((OptionClass, StringClass)) }
            }
        } ^
        "model a function of Int and String" ! {
            withMethod("aFunctionOfIntAndString", 2) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("i" -> IntClass, "s" -> StringClass) } and
                { meth must haveResultLike((Tuple2Class, IntClass, StringClass)) }
            }
        } ^
        "model an overloaded function with one argument" ! {
            withMethod("anOverloadedFunction", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("i" -> IntClass) } and
                { meth must haveResultLike(IntClass) }
            }
        } ^
        "model an overloaded function with two arguments" ! {
            withMethod("anOverloadedFunction", 2) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("i" -> IntClass, "s" -> StringClass) } and
                { meth must haveResultLike(StringClass) }
            }
        } ^
        "model a @BeanProperty val" ! {
            implicit val property = ("aBeanPropertyVal", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beLike {
                    case bpgetter :: Nil =>
                        bpgetter must beValidBeanGetter
                } }
            }
        } ^
        "model a @BeanProperty var" ! {
            implicit val property = ("aBeanPropertyVar", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                { prop.getter must beValidGetter } and
                { prop.setter must beValidSetter } and
                { prop.others must beLike {
                    case bpgetter :: bpsetter :: Nil =>
                        bpgetter must beValidBeanGetter
                        Okay(bpsetter) must beValidBeanSetter
                } }
            }
        } ^
        "model a @BeanProperty lazy val" ! {
            implicit val property = ("aBeanPropertyLazyVal", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beLike {
                    case bpgetter :: Nil =>
                        bpgetter must beValidBeanGetter
                } }
            }
        } ^
        "model a @BooleanBeanProperty val" ! {
            implicit val property = ("aBooleanBeanPropertyVal", BooleanClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beLike {
                    case bpgetter :: Nil =>
                        bpgetter must beValidBooleanBeanGetter
                } }
            }
        } ^
        "model a @BooleanBeanProperty var" ! {
            implicit val property = ("aBooleanBeanPropertyVar", BooleanClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                { prop.getter must beValidGetter } and
                { prop.setter must beValidSetter } and
                { prop.others must beLike {
                    case bpgetter :: bpsetter :: Nil =>
                        bpgetter must beValidBooleanBeanGetter
                        Okay(bpsetter) must beValidBeanSetter
                } }
            }
        } ^
        "model a @BooleanBeanProperty lazy val" ! {
            implicit val property = ("aBooleanBeanPropertyLazyVal", BooleanClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and // currently extended reflection doesn't capture scala-only annotations (StaticAnnotation)
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beLike {
                    case bpgetter :: Nil =>
                        bpgetter must beValidBooleanBeanGetter
                } }
            }
        } ^
        "model an annotated constructor" ! {
            withConstructor(2) { ctor =>
                { ctor.annotations must beLike {
                    case annot :: Nil => annot must beLike { case _: TestAnnotation => ok }
                } } and
                { ctor must haveParametersLike("i" -> IntClass, "s" -> StringClass) }
            }
        } ^
        "model an annotated method" ! {
            withMethod("anAnnotatedMethod", 2) { meth =>
                { meth.annotations must beLike {
                    case annot :: Nil => annot must beLike { case _: TestAnnotation => ok }
                } } and
                { meth must haveParametersLike("i" -> IntClass, "s" -> StringClass) } and
                { meth.parameters.find(_.name == Some("s")) must beLike {
                    case Some(paramR) => paramR.annotations must beLike {
                        case annot :: Nil => annot must beLike { case _: TestAnnotation => ok }
                    }
                } } and
                { meth must haveResultLike((Tuple2Class, IntClass, StringClass)) }
            }
        } ^
        "model an annotated val" ! {
            implicit val property = ("anAnnotatedVal", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beLike {
                    case annot :: Nil => annot must beLike { case _: TestAnnotation => ok }
                } } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model an annotated var" ! {
            implicit val property = ("anAnnotatedVar", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beLike {
                    case annot :: Nil => annot must beLike { case _: TestAnnotation => ok }
                } } and
                { prop.getter must beValidGetter } and
                { prop.setter must beValidSetter } and
                { prop.others must beEmpty }
            }
        } ^
        "properly cache ClassRs" ! {
            val ctx = new Builder(getClass.getClassLoader)
            if (ctx.classRFor(classOf[PlainScalaClass]) != ctx.classRFor(classOf[PlainScalaClass]))
                ko("First instance of ClassR was not returned again for a second call")
            else
                true must_== true // to avoid "skipped"
        } ^
        "properly cache signatures" ! {
            val ctx = new Builder(getClass.getClassLoader)
            if (ctx.sigFor(classOf[PlainScalaClass]) != ctx.sigFor(classOf[PlainScalaClass]))
                ko("First instance of ScalaSig was not returned again for a second call")
            else
                true must_== true // to avoid "skipped"
        } ^
        "model non-public vals" ! {
            implicit val property = ("aPrivateVal", IntClass: TypeRMatcher)
            val propROption = targetClassR.nonPublicProperties.find(_.name == property._1)
            propROption must beLike { case Some(propR) =>
                { propR.ownerName.qualified must_== targetClassR.name.qualified } and
                { propR.annotations must beEmpty } and
                { propR.getter must beValidGetter } and
                { propR.setter must notBePresent } and
                { propR.others must beEmpty } and
                { propR.isPublic must_== false } and
                { propR.getter.isPublic must_== false }
            }
        } ^
        "model non-public vars" ! {
            implicit val property = ("aPrivateVar", IntClass: TypeRMatcher)
            val propROption = targetClassR.nonPublicProperties.find(_.name == property._1)
            propROption must beLike { case Some(propR) =>
                { propR.ownerName.qualified must_== targetClassR.name.qualified } and
                { propR.annotations must beEmpty } and
                { propR.getter must beValidGetter } and
                { propR.setter must beValidSetter } and
                { propR.others must beEmpty  } and
                { propR.isPublic must_== false } and
                { propR.getter.isPublic must_== false } and
                { propR.setter.orThrow.isPublic must_== false }
            }
        } ^
        "model non-public defs" ! {
            val methROption = targetClassR.nonPublicMethods.find(_.name == "aPrivateDef")
            methROption must beLike { case Some(methR) =>
                { methR.ownerName.qualified must_== targetClassR.name.qualified } and
                { methR.annotations must beEmpty } and
                { methR.isPublic must_== false } and
                { methR.parameters must beEmpty } and
                { methR must haveResultLike(IntClass) }
            }
        } ^
        "model methods with names that make them look like setters" ! {
            withMethod("setMethodWithMisleadingName", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("i" -> IntClass) } and
                { meth must haveResultLike(UnitClass) }
            }
            withMethod("methodWithMisleadingName_$eq", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("i" -> IntClass) } and
                { meth must haveResultLike(UnitClass) }
            }
        } ^
        "model Option[Boolean]" ! {
            implicit val property = ("aValOptionBoolean", (OptionClass, BooleanClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        } ^
        "model Option[Byte]" ! {
            implicit val property = ("aValOptionByte", (OptionClass, ByteClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        } ^
        "model Option[Char]" ! {
            implicit val property = ("aValOptionChar", (OptionClass, CharClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        } ^
        "model Option[Double]" ! {
            implicit val property = ("aValOptionDouble", (OptionClass, DoubleClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        } ^
        "model Option[Float]" ! {
            implicit val property = ("aValOptionFloat", (OptionClass, FloatClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        } ^
        "model Option[Int]" ! {
            implicit val property = ("aValOptionInt", (OptionClass, IntClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        } ^
        "model Option[Long]" ! {
            implicit val property = ("aValOptionLong", (OptionClass, LongClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        } ^
        "model Option[Short]" ! {
            implicit val property = ("aValOptionShort", (OptionClass, ShortClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        } ^
        "model Option[Unit]" ! {
            implicit val property = ("aValOptionUnit", (OptionClass, UnitClass): TypeRMatcher)
            withProperty(_.getter must beValidGetter)
        }
}

class ScalaCaseClassSpecTest extends SpecificationWithJUnit {
    lazy val targetClassRResult = builder.classRFor(classOf[CaseClass])
    implicit def targetClassR = targetClassRResult.orThrow

    def is =
        "Extended reflection on a Scala case class should" ^
        "not fail" ! {
            targetClassRResult must beLike { case Okay(_) => ok }
        } ^
        "model only the expected members" ! {
            targetClassR.members.filterNot(ownedByLibraryInterface).map(_.name) must_== List(
                "<init>",

                "aLazyValString",
                "aValInt",
                "aVarOptionString",
                "ctorParam",

                "aDefDouble",
                "aFunctionOfInt"
            )
        } ^
        "model the constructor" ! {
            withConstructor(1) { ctor =>
                { ctor.annotations must beEmpty } and
                { ctor must haveParametersLike("ctorParam" -> IntClass) }
            }
        } ^
        "model a val" ! {
            implicit val property = ("aValInt", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a var with an Option type" ! {
            implicit val property = ("aVarOptionString", (OptionClass, StringClass): TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must beValidSetter } and
                { prop.others must beEmpty }
            }
        } ^
        "model a def with no argument list" ! {
            withMethod("aDefDouble", 0) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveResultLike(DoubleClass) }
            }
        } ^
        "model a lazy val" ! {
            implicit val property = ("aLazyValString", StringClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a function of Int" ! {
            withMethod("aFunctionOfInt", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("in" -> IntClass) } and
                { meth must haveResultLike(IntClass) }
            }
        }
}

class ScalaComposedClassSpecTest extends SpecificationWithJUnit {
    lazy val mixedClassRResult = builder.classRFor(classOf[MixedClass])
    implicit def mixedClassR = mixedClassRResult.orThrow

    def is =
        "Extended reflection on a Scala composed class should" ^
        "not fail" ! {
            mixedClassRResult must beLike { case Okay(_) => ok }
        } ^
        "model only the expected members" ! {
            mixedClassR.members.filterNot(ownedByLibraryInterface).map(_.name) must_== List(
                "<init>",

                "firstMethod",
                "secondMethod",
                "thirdMethod"
            )
        } ^
        "model the interfaces" ! {
            { mixedClassR.interfaces must have (_.name.qualified must_== NameR(classOf[FirstMixin].getName).qualified) } and
            { mixedClassR.interfaces must have (_.name.qualified must_== NameR(classOf[SecondMixin].getName).qualified) }
        } ^
        "model the constructor" ! {
            withConstructor(0) { ctor =>
                ctor.annotations must beEmpty
            }
        } ^
        "model the first method" ! {
            withAnyMethod("firstMethod", 1) { meth =>
                { meth.ownerName.qualified must_== NameR(classOf[FirstMixin].getName).qualified } and
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("in" -> StringClass) } and
                { meth must haveResultLike(StringClass) }
            }
        } ^
        "model the second method" ! {
            withAnyMethod("secondMethod", 1) { meth =>
                { meth.ownerName.qualified must_== NameR(classOf[SecondMixin].getName).qualified } and
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("in" -> IntClass) } and
                { meth must haveResultLike(IntClass) }
            }
        } ^
        "model the third method" ! {
            withMethod("thirdMethod", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("in" -> DoubleClass) } and
                { meth must haveResultLike(DoubleClass) }
            }
        }
}

class ObjectEnclosingClassSpecTest extends SpecificationWithJUnit {
    lazy val targetClassRResult = builder.classRFor(classOf[EnclosingObject.SubEnclosingObject.CaseClass])
    implicit def targetClassR = targetClassRResult.orThrow

    def is =
        "Extended reflection on a class enclosed in an object should" ^
        "not fail" ! {
            targetClassRResult must beLike { case Okay(_) => ok }
        } ^
        "model only the expected members" ! {
            targetClassR.members.filterNot(ownedByLibraryInterface).map(_.name) must_== List(
                "<init>",

                "ctorParam"
            )
        } ^
        "model the one arg constructor" ! {
            withConstructor(1) { ctor =>
                { ctor.annotations must beEmpty } and
                { ctor must haveParametersLike("ctorParam" -> IntClass) }
            }
        } ^
        "model a val" ! {
            implicit val property = ("ctorParam", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        }
}

    /* 2010-09-17 RMM: Don't need this to work at the moment since we can't support instantiation of inner classes for JSON decoding anyway.

    "Extended reflection on a class enclosed in a class" should {
        lazy val targetClassRResult = builder.classRFor(classOf[EnclosingObject.SubEnclosingClass#CaseClass])
        implicit def targetClassR = targetClassRResult.orThrow

        "not fail" ! {
            targetClassRResult must verify(_.isDefined)
        }

        "model only the expected members" ! {
            targetClassR.members.filterNot(ownedByLibraryInterface).map(_.name) must_== List(
                "<init>",

                "ctorParam"
            )
        }


        "model the one arg constructor" ! {
            withConstructor(1)(ctor => {
                ctor.annotations must beEmpty
                ctor must haveParametersLike("ctorParam" -> IntClass)
            })
        }

        "model a val" ! {
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

class PlainJavaClassSpecTest extends SpecificationWithJUnit {
    lazy val targetClassRResult = builder.classRFor(classOf[PlainJavaClass])
    implicit def targetClassR = targetClassRResult.orThrow

    def is = "Extended reflection on a plain Java class should" ^
        "not fail" ! {
            targetClassRResult must beLike { case Okay(_) => ok }
        } ^
        "model only the expected members" ! {
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
        } ^
        "model the zero arg constructor" ! {
            withConstructor(0) { ctor =>
                ctor.annotations must beEmpty
            }
        } ^
        "model the one arg constructor" ! {
            withConstructor(1) { ctor =>
                ctor.annotations must beEmpty
                ctor must haveParametersLike("i" -> IntClass)
            }
        } ^
        "model the two arg constructor" ! {
            withConstructor(2) { ctor =>
                { ctor.annotations must beLike {
                    case annot :: Nil => annot must beLike { case _: TestAnnotation => ok }
                } } and
                { ctor must haveParametersLike("i" -> IntClass, "s" -> StringClass) } and
                { ctor.parameters.find(_.name == Some("s")) must beLike {
                    case Some(paramR) => paramR.annotations must beLike {
                        case annot :: Nil => annot must beLike { case _: TestAnnotation => ok }
                    }
                } }
            }
        } ^
        "model a read only int bean property" ! {
            implicit val property = ("aReadOnlyInt", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidBeanGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a read write int bean property" ! {
            implicit val property = ("aReadWriteInt", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidBeanGetter } and
                { prop.setter must beValidBeanSetter } and
                { prop.others must beEmpty }
            }
        } ^
        "model a read write List<String> bean property" ! {
            implicit val property = ("aReadWriteListOfString", (JavaListClass, StringClass): TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidBeanGetter } and
                { prop.setter must beValidBeanSetter } and
                { prop.others must beEmpty }
            }
        } ^
        "model a read only boolean bean property" ! {
            implicit val property = ("aReadOnlyBoolean", BooleanClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidBooleanBeanGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model an annotated int bean property (annotated at field level)" ! {
            implicit val property = ("anAnnotatedInt", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beLike {
                    case annot :: Nil => annot must beLike { case _: TestAnnotation => ok }
                } } and
                { prop.getter must beValidBeanGetter } and
                { prop.setter must beValidBeanSetter } and
                { prop.others must beEmpty }
            }
        } ^
        "model an annotated int bean property (annotated at getter level)" ! {
            implicit val property = ("intWithAnnotatedReader", IntClass: TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beLike {
                    case annot :: Nil => annot must beLike { case _: TestAnnotation => ok }
                } } and
                { prop.getter must beValidBeanGetter } and
                { prop.setter must beValidBeanSetter } and
                { prop.others must beEmpty }
            }
        } ^
        "model a normal method" ! {
            withMethod("aNormalMethod", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("i" -> IntClass) } and
                { meth must haveResultLike(UnitClass) }
            }
        } ^
        "model an annotated method" ! {
            withMethod("anAnnotatedIntMethod", 0) { meth =>
                { meth.annotations must beLike {
                    case annot :: Nil => annot must beLike { case _: TestAnnotation => ok }
                } } and
                { meth must haveResultLike(IntClass) }
            }
        } ^
        "model method with a name that makes it look like a setter" ! {
            withMethod("setMethodWithMisleadingName", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("i" -> IntClass) } and
                { meth must haveResultLike(UnitClass) }
            }
        }
}

class JavaInterfaceWithoutNamedSpecTest extends SpecificationWithJUnit {
    lazy val targetClassRResult = builder.classRFor(classOf[JavaInterfaceWithoutNamed])
    implicit def targetClassR = targetClassRResult.orThrow

    def is =
        "Extended reflection on a plain Java interface (without @Named) should" ^
        "not fail" ! {
            targetClassRResult must beLike { case Okay(_) => ok }
        } ^
        "model only the expected members" ! {
            targetClassR.members.map(_.name) must_== List(
                "doSomething"
            )
        } ^
        "model the method" ! {
            withMethod("doSomething", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike(IntClass) } and
                { meth must haveResultLike(UnitClass) }
            }
        }
}

class JavaInterfaceWithNamedSpecTest extends SpecificationWithJUnit {
    lazy val targetClassRResult = builder.classRFor(classOf[JavaInterfaceWithNamed])
    implicit def targetClassR = targetClassRResult.orThrow

    def is =
        "Extended reflection on a plain Java interface (with @Named) should" ^
        "not fail" ! {
            targetClassRResult must beLike { case Okay(_) => ok }
        } ^
        "model only the expected members" ! {
            targetClassR.members.map(_.name) must_== List(
                "doSomething"
            )
        } ^
        "model the method" ! {
            withMethod("doSomething", 1) { meth =>
                { meth.annotations must beEmpty } and
                { meth must haveParametersLike("i" -> IntClass) } and
                { meth must haveResultLike(UnitClass) }
            }
        }
}

class TypeAliasSpecTest extends SpecificationWithJUnit {
    lazy val targetClassRResult = builder.classRFor(classOf[TypeTester])
    implicit def targetClassR = targetClassRResult.orThrow

    def is =
        "Extended reflection should resolve type aliases should" ^
        "not fail" ! {
            targetClassRResult must beLike { case Okay(_) => ok }
        } ^
        "model a type alias with no arguments (f)" ! {
            implicit val property = ("f", (classOf[P[_]], StringClass): TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a type alias with one argument (g)" ! {
            implicit val property = ("g", (classOf[Q[_, _]], IntClass, StringClass): TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a type alias with two arguments with one ignored (h)" ! {
            implicit val property = ("h", (classOf[Q[_, _]], ByteClass, StringClass): TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a type alias with one argument to an external type alias (i)" ! {
            implicit val property = ("i", (classOf[ResultG[_, _]], UnitClass, FloatClass): TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a type alias with one argument to an external class (j)" ! {
            implicit val property = ("j", (classOf[ResultG[_, _]], StringClass, DoubleClass): TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a type alias in a nested object (k)" ! {
            implicit val property = ("k", (classOf[P[_]], StringClass): TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        } ^
        "model a class-local type alias (l)" ! {
            implicit val property = ("l", (classOf[ResultG[_, _]], UnitClass, CharClass): TypeRMatcher)
            withProperty { prop =>
                { prop.annotations must beEmpty } and
                { prop.getter must beValidGetter } and
                { prop.setter must notBePresent } and
                { prop.others must beEmpty }
            }
        }
}
