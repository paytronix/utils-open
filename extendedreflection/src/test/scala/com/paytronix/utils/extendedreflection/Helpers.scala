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

import org.specs2.matcher.{Matcher, MatchResult, MustMatchers, StandardMatchResults}
import com.paytronix.utils.scala.result.{Result, Okay}

object Helpers extends MustMatchers with StandardMatchResults {
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

    def withConstructor[A](numParams: Int)(f: ConstructorR => A)(implicit classR: ClassR): A = {
        val ctorROption = classR.constructors.find(_.parameters.length == numParams)
        ctorROption must beLike { case Some(_) => ok }
        ctorROption.get.ownerName.qualified must_== classR.name.qualified
        f(ctorROption.get)
    }

    def withProperty[A](f: PropertyR => A)(implicit propInfo: (String, TypeRMatcher), classR: ClassR): A =
        withPropertyNamed(propInfo._1)(f)

    def withPropertyNamed[A](n: String)(f: PropertyR => A)(implicit classR: ClassR): A =
        withAnyPropertyNamed(n)(propR => {
            propR.ownerName.qualified must_== classR.name.qualified
            f(propR)
        })

    def withAnyPropertyNamed[A](n: String)(f: PropertyR => A)(implicit classR: ClassR): A = {
        val propROption = classR.properties.find(_.name == n)
        propROption must beLike { case Some(_) => ok }
        f(propROption.get)
    }

    def withMethod[A](n: String, numParams: Int)(f: MethodR => A)(implicit classR: ClassR): A =
        withAnyMethod(n, numParams)(methR => {
            methR.ownerName.qualified must_== classR.name.qualified
            f(methR)
        })

    def withAnyMethod[A](n: String, numParams: Int)(f: MethodR => A)(implicit classR: ClassR): A = {
        val methROption = classR.methods.find(methR => methR.name == n && methR.parameters.length == numParams)
        methROption must beLike { case Some(_) => ok }
        f(methROption.get)
    }


    sealed abstract class TypeRMatcher {
        def apply(in: TypeR): Boolean
    }

    final case class ClassTypeRMatcher(clazz: Class[_]) extends TypeRMatcher {
        def apply(in: TypeR) = in match {
            case ClassTypeRFor(c) if c == clazz => true
            case _ => false
        }

        override def toString = clazz.getName
    }
    implicit def classToTypeRMatcher(in: Class[_]): ClassTypeRMatcher = ClassTypeRMatcher(in)

    final case class ParameterizedType1Matcher(headClass: Class[_], arg: Class[_]) extends TypeRMatcher {
        def apply(in: TypeR) = in match {
            case ParameterizedTypeRWith(ClassTypeRFor(headClass), ClassTypeRFor(arg) :: Nil) => true
            case _ => false
        }

        override def toString = headClass.getName + "[" + arg.getName + "]"
    }
    implicit def classTupleToParameterizedType1Matcher(in: (Class[_], Class[_])): ParameterizedType1Matcher =
        ParameterizedType1Matcher(in._1, in._2)

    final case class ParameterizedType2Matcher(headClass: Class[_], arg1: Class[_], arg2: Class[_]) extends TypeRMatcher {
        def apply(in: TypeR) = in match {
            case ParameterizedTypeRWith(ClassTypeRFor(headClass), ClassTypeRFor(arg1) :: ClassTypeRFor(arg2) :: Nil) => true
            case _ => false
        }

        override def toString = headClass.getName + "[" + arg1.getName + ", " + arg2.getName + "]"
    }
    implicit def classTupleToParameterizedType2Matcher(in: (Class[_], Class[_], Class[_])): ParameterizedType2Matcher =
        ParameterizedType2Matcher(in._1, in._2, in._3)

    implicit def parameterTupleToMatchingTuple(in: (String, Class[_])): (String, TypeRMatcher) = (in._1, in._2)
    implicit def parameterTupleOfTuple2ToMatchingTuple(in: (String, (Class[_], Class[_]))): (String, TypeRMatcher) = (in._1, in._2)
    implicit def parameterTupleOfTuple3ToMatchingTuple(in: (String, (Class[_], Class[_], Class[_]))): (String, TypeRMatcher) = (in._1, in._2)

    // These functions have "first" and "rest" to not run into overloading / erasure issues
    def haveParametersLike(first: (String, TypeRMatcher), rest: (String, TypeRMatcher)*): Matcher[MethodLikeR] =
        matchParameters((first :: rest.toList).map { case (n, m) => (Some(n), m) }: _*)
    def haveParametersLike(first: TypeRMatcher, rest: TypeRMatcher*): Matcher[MethodLikeR] =
        matchParameters((first :: rest.toList).map(m => (None, m)): _*)

    def matchParameters(m: (Option[String], TypeRMatcher)*): Matcher[MethodLikeR] =
        (mlikeR: MethodLikeR) =>
            (
                if (mlikeR.parameters.length != m.length) ko("wrong number of parameters for " + mlikeR + ", expected " + m.length)
                else ok
            ) and (
                mlikeR.parameters.zip(m).zipWithIndex.foldLeft[MatchResult[None.type]](ok) { (prev, tup) =>
                    prev and (tup match {
                        case ((paramR, (nameToMatch@Some(_), _)), index) if paramR.name != nameToMatch =>
                            ko("expected parameter " + index + " (" + paramR + ") of " + mlikeR + " to be named " + nameToMatch + " but is named " + paramR.name)

                        case ((paramR, (_, typeMatcher)), index) if !typeMatcher(paramR.typeR) =>
                            ko("expected type of parameter " + index + " (" + paramR + ") of " + mlikeR + " to match " + typeMatcher)

                        case _ =>
                            ok
                    })
                }
            )

    def haveResultLike(m: TypeRMatcher): Matcher[MethodR] =
        (methR: MethodR) =>
            if (m(methR.result)) ok
            else ko("result type of " + methR + " doesn't match " + m)

    def beValidGetter(implicit propInfo: (String, TypeRMatcher)): Matcher[MethodR] =
        validGetter(propInfo._1, propInfo._2)
    def beValidGetter(n: String, m: TypeRMatcher): Matcher[MethodR] =
        validGetter(n, m)

    def beValidBeanGetter(implicit propInfo: (String, TypeRMatcher)): Matcher[MethodR] =
        validGetter("get" + upcaseFirst(propInfo._1), propInfo._2)
    def beValidBeanGetter(n: String, m: TypeRMatcher): Matcher[MethodR] =
        validGetter("get" + upcaseFirst(n), m)

    def beValidBooleanBeanGetter(implicit propInfo: (String, TypeRMatcher)): Matcher[MethodR] =
        validGetter("is" + upcaseFirst(propInfo._1), propInfo._2)
    def beValidBooleanBeanGetter(n: String, m: TypeRMatcher): Matcher[MethodR] =
        validGetter("is" + upcaseFirst(n), m)

    def validGetter(n: String, m: TypeRMatcher): Matcher[MethodR] =
        (methR: MethodR) => (methR.name must_== n) and (methR must haveResultLike(m))

    def upcaseFirst(in: String) = in match {
        case "" => ""
        case _ => Character.toUpperCase(in.charAt(0)) + in.substring(1)
    }

    def beValidSetter(implicit propInfo: (String, TypeRMatcher)): Matcher[Result[MethodR]] =
        validSetter(propInfo._1 + "_$eq", propInfo._2)
    def beValidSetter(n: String, m: TypeRMatcher): Matcher[Result[MethodR]] =
        validSetter(n + "_$eq", m)

    def beValidBeanSetter(implicit propInfo: (String, TypeRMatcher)): Matcher[Result[MethodR]] =
        validSetter("set" + upcaseFirst(propInfo._1), propInfo._2)
    def beValidBeanSetter(n: String, m: TypeRMatcher): Matcher[Result[MethodR]] =
        validSetter("set" + upcaseFirst(n), m)

    def validSetter(n: String, m: TypeRMatcher): Matcher[Result[MethodR]] =
        beLike {
            case Okay(methR) =>
                (methR.name must_== n) and
                (methR must (haveResultLike(classOf[Unit]) and haveParametersLike(m)))

            case _ =>
                ko("setter " + n + " is not present, but should be")
        }


    def notBePresent: Matcher[Result[_]] =
        beLike {
            case Okay(v) => ko("should not be Okay, but was and contained " + v)
            case _ => ok
        }
}
