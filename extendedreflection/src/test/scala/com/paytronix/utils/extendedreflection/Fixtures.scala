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

import scala.reflect.{BeanProperty, BooleanBeanProperty}
import com.paytronix.utils.scala.result.{Result, ResultG}

case class CaseClass(ctorParam: Int) {
    sys.error("instantiation is bad!")
    val aValInt: Int = 1
    var aVarOptionString: Option[String] = None
    def aDefDouble: Double = 0.1
    lazy val aLazyValString: String = aValInt + " " + aVarOptionString + " " + aDefDouble
    def aFunctionOfInt(in: Int) = aValInt + in
}

class PlainScalaClass(ctorParam: Int, val ctorValParam: String, var ctorVarParam: Option[String]) {
    sys.error("instantiation is bad!")
    def this() = this(1, "foo", None)
    def this(i: Int) = { this(); aVarOptionString = Some(i.toString) }
    @TestAnnotation def this(i: Int, s: String) = { this(i, s, None) }
    val aValInt: Int = 1
    var aVarOptionString: Option[String] = None
    def aDefDouble: Double = 0.1
    lazy val aLazyValString: String = aValInt + " " + aVarOptionString + " " + aDefDouble
    def aNullaryFunction() = ()
    def aFunctionOfInt(in: Int) = aValInt + in
    def aFunctionOfOptionString(in: Option[String]) = aVarOptionString.map(_ + " foo")
    def aFunctionOfIntAndString(i: Int, s: String) = (i, s)
    def anOverloadedFunction(i: Int) = i
    def anOverloadedFunction(i: Int, s: String) = i + s

    val aValOptionBoolean: Option[Boolean] = None
    val aValOptionByte: Option[Byte] = None
    val aValOptionChar: Option[Char] = None
    val aValOptionDouble: Option[Double] = None
    val aValOptionFloat: Option[Float] = None
    val aValOptionInt: Option[Int] = None
    val aValOptionLong: Option[Long] = None
    val aValOptionShort: Option[Short] = None
    val aValOptionUnit: Option[Unit] = None

    private val aPrivateVal: Int = 0
    private var aPrivateVar: Int = 0
    private def aPrivateDef: Int = 0

    @BeanProperty val aBeanPropertyVal = 0
    @BeanProperty var aBeanPropertyVar = 0
    @BeanProperty lazy val aBeanPropertyLazyVal = 0

    @BooleanBeanProperty val aBooleanBeanPropertyVal = false
    @BooleanBeanProperty var aBooleanBeanPropertyVar = false
    @BooleanBeanProperty lazy val aBooleanBeanPropertyLazyVal = false

    @TestAnnotation def anAnnotatedMethod(i: Int, @TestAnnotation s: String) = (i, s)
    @TestAnnotation val anAnnotatedVal = 0
    @TestAnnotation var anAnnotatedVar = 0

    def setMethodWithMisleadingName(i: Int) = {}
    def methodWithMisleadingName_= (i: Int) = {}
}

trait P[A]
trait Q[A, B]

object EnclosingObject {
    type F = P[String]
    type G[A] = Q[A, String]
    type H[A, B] = G[B]
    type I[A] = Result[A]
    type J[A] = ResultG[String, A]

    class SubEnclosingClass {
        sys.error("instantiation is bad!")

        case class CaseClass(ctorParam: Int) {
            sys.error("instantiation is bad!")
        }
    }
    object SubEnclosingObject {
        type K[A] = P[A]

        case class CaseClass(ctorParam: Int) {
            sys.error("instantiation is bad!")
        }
    }
}

abstract class TypeTester {
    type L[A] = Result[A]

    val f: EnclosingObject.F // P[String]
    val g: EnclosingObject.G[Int] // Q[Int, String]
    val h: EnclosingObject.H[Int, Byte] // G[Byte]: Q[Byte, String]
    val i: EnclosingObject.I[Float] // Result[Float]: ResultG[Unit, Float]
    val j: EnclosingObject.J[Double] // ResultG[String, Double]
    val k: EnclosingObject.SubEnclosingObject.K[Short] // P[Short]
    val l: L[Char] // Result[Char]: ResultG[Unit, Char]
}

trait FirstMixin {
    def firstMethod(in: String) = in
}

trait SecondMixin {
    def secondMethod(in: Int) = in
}

class MixedClass extends FirstMixin with SecondMixin {
    sys.error("instantiation is bad!")
    def thirdMethod(in: Double) = in
}
