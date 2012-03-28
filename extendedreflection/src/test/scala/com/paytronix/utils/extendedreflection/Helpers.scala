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

import org.specs.matcher.Matcher
import com.paytronix.utils.scala.result.{Result, Okay}

object Helpers {
    implicit def matchResultToExtendedMatchResult(in: (Boolean, String, String)): ExtendedMatchResult = ExtendedMatchResult(in)
    implicit def extendedMatchResultToMatchResult(in: ExtendedMatchResult): (Boolean, String, String) = in.in

    final case class ExtendedMatchResult(in: (Boolean, String, String)) {
        /** Allows easy composition of matchers */
        def >> (other: => ExtendedMatchResult): ExtendedMatchResult =
            in match {
                case (true, _, _) => other
                case _ => this
            }
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
        ParametersMatcher((first :: rest.toList).map { case (n, m) => (Some(n), m) }: _*)
    def haveParametersLike(first: TypeRMatcher, rest: TypeRMatcher*): Matcher[MethodLikeR] =
        ParametersMatcher((first :: rest.toList).map(m => (None, m)): _*)

    case class ParametersMatcher(m: (Option[String], TypeRMatcher)*) extends Matcher[MethodLikeR] {
        def apply(mlikeR: => MethodLikeR) =
            if (mlikeR.parameters.length != m.length) (false, "ok", "wrong number of parameters for " + mlikeR + ", expected " + m.length)
            else mlikeR.parameters.zip(m).zipWithIndex.foldLeft((true, "ok", "ko"))((prev, tup) => prev >> (tup match {
                case ((paramR, (nameToMatch@Some(_), _)), index) if paramR.name != nameToMatch =>
                    (false, "ok", "expected parameter " + index + " (" + paramR + ") of " + mlikeR + " to be named " + nameToMatch +
                     " but is named " + paramR.name)

                case ((paramR, (_, typeMatcher)), index) if !typeMatcher(paramR.typeR) =>
                    (false, "ok", "expected type of parameter " + index + " (" + paramR + ") of " + mlikeR + " to match " + typeMatcher)

                case _ => (true, "ok", "ko")
            }))
    }

    case class haveResultLike(m: TypeRMatcher) extends Matcher[MethodR] {
        def apply(methR: => MethodR) =
            (m(methR.result), "ok", "result type of " + methR + " doesn't match " + m)
    }

    def beValidGetter(implicit propInfo: (String, TypeRMatcher)): Matcher[MethodR] =
        ValidGetterMatcher(propInfo._1, propInfo._2)
    def beValidGetter(n: String, m: TypeRMatcher): Matcher[MethodR] =
        ValidGetterMatcher(n, m)

    def beValidBeanGetter(implicit propInfo: (String, TypeRMatcher)): Matcher[MethodR] =
        ValidGetterMatcher("get" + upcaseFirst(propInfo._1), propInfo._2)
    def beValidBeanGetter(n: String, m: TypeRMatcher): Matcher[MethodR] =
        ValidGetterMatcher("get" + upcaseFirst(n), m)

    def beValidBooleanBeanGetter(implicit propInfo: (String, TypeRMatcher)): Matcher[MethodR] =
        ValidGetterMatcher("is" + upcaseFirst(propInfo._1), propInfo._2)
    def beValidBooleanBeanGetter(n: String, m: TypeRMatcher): Matcher[MethodR] =
        ValidGetterMatcher("is" + upcaseFirst(n), m)

    case class ValidGetterMatcher(n: String, m: TypeRMatcher) extends Matcher[MethodR] {
        def apply(methR: => MethodR) =
            if (methR.name != n)       (false, "ok", "expected getter named " + n + " but is " + methR.name)
            else haveResultLike(m)(methR)
    }

    def upcaseFirst(in: String) = in match {
        case "" => ""
        case _ => Character.toUpperCase(in.charAt(0)) + in.substring(1)
    }

    def beValidSetter(implicit propInfo: (String, TypeRMatcher)): Matcher[Result[MethodR]] =
        ValidSetterMatcher(propInfo._1 + "_$eq", propInfo._2)
    def beValidSetter(n: String, m: TypeRMatcher): Matcher[Result[MethodR]] =
        ValidSetterMatcher(n + "_$eq", m)

    def beValidBeanSetter(implicit propInfo: (String, TypeRMatcher)): Matcher[Result[MethodR]] =
        ValidSetterMatcher("set" + upcaseFirst(propInfo._1), propInfo._2)
    def beValidBeanSetter(n: String, m: TypeRMatcher): Matcher[Result[MethodR]] =
        ValidSetterMatcher("set" + upcaseFirst(n), m)

    case class ValidSetterMatcher(n: String, m: TypeRMatcher) extends Matcher[Result[MethodR]] {
        def apply(b: => Result[MethodR]) = b match {
            case Okay(methR) if methR.name != n =>
                (false, "ok", "expected setter named " + n + " but is " + methR.name)

            case Okay(methR) =>
                haveResultLike(classOf[Unit])(methR) >> haveParametersLike(m)(methR)

            case _ =>
                (false, "ok", "setter " + n + " is not present, but should be")
        }

    }

    object notBePresent extends Matcher[Result[_]] {
        def apply(b: => Result[_]) = b match {
            case Okay(v) => (false, "ok", "should not be Okay, but was and contained " + v)
            case _ => (true, "ok", "ko")
        }
    }
}
