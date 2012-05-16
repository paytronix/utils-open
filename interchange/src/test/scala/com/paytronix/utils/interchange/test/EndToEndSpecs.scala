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
import scala.collection.immutable.{Map => ImmutableMap}
import scala.collection.mutable.{Map => MutableMap}
import scala.math.{BigDecimal => ScalaBigDecimal}
import scala.reflect.Manifest
import net.liftweb.json.Implicits.{int2jvalue, string2jvalue}
import net.liftweb.json.JsonAST.{JNothing, JValue}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import org.specs2.SpecificationWithJUnit
import org.specs2.execute.{Result => SpecsResult}
import com.paytronix.utils.extendedreflection.Builder
import com.paytronix.utils.interchange._
import com.paytronix.utils.scala.result.{Okay, parameter}

import fixtures._
import Helpers._

class EndToEndSpecTest extends SpecificationWithJUnit {
    def encodeAndDecode[T](in: T)(implicit m: Manifest[T]): SpecsResult =
        Coding.encode[T](in).flatMap(Coding.decode[T](_)) must_== Okay(in)

    def is =
        "End to end, coding should" ^
        "decode an encoded CaseClass" ! resetCoding {
            { encodeAndDecode(CaseClass(1, "foo", Some("bub"))) } and
            { encodeAndDecode(CaseClass(1, "foo", None)) } and
            { encodeAndDecode(CaseClass(1, null, Some("bub"))) }
        } ^
        "decode an encoded BasicClass" ! resetCoding {
            { encodeAndDecode(BasicClass.make(1, "foo", Some("bub"))) } and
            { encodeAndDecode(BasicClass.make(1, "foo", None)) } and
            { encodeAndDecode(BasicClass.make(1, null, Some("bub"))) }
        } ^
        "decode an encoded POJOClass" ! resetCoding {
            { encodeAndDecode(POJOClass.make(1, "foo", Some("bub"))) } and
            { encodeAndDecode(POJOClass.make(1, "foo", None)) } and
            { encodeAndDecode(POJOClass.make(1, null, Some("bub"))) }
        } ^
        "decode an encoded WrappingObject.WrappedClass" ! resetCoding {
            encodeAndDecode(WrappingObject.WrappedClass(1, Some(WrappingObject.WrappedEnum.Two)))
        } ^
        "decode an encoded WrappingObject.SubWrappingObject.WrappedClass" ! resetCoding {
            encodeAndDecode(WrappingObject.SubWrappingObject.WrappedClass(1))
        } ^
        "decode an encoded ComposedClass" ! resetCoding {
            val v = new ComposedClass
            v.a = "foo"
            v.b = 2
            v.c = 1.5

            encodeAndDecode(v)
        } ^
        "decode an encoded Wrapper" ! resetCoding {
            encodeAndDecode(Wrapper(CaseClass(1, "foo", Some("bub"))))
        } ^
        "decode an encoded CaseClassWithSideCar" ! resetCoding {
            { encodeAndDecode(CaseClassWithSideCar(1, "foo", Some("bub"))) } and
            { encodeAndDecode(CaseClassWithSideCar(1, "foo", None)) } and
            { encodeAndDecode(CaseClassWithSideCar(1, null, Some("bub"))) }
        } ^
        "decode an encoded ClassWithMultipleConstructors" ! resetCoding {
            val v = new ClassWithMultipleConstructors(1, "foo", 0.0)
            v.d = "bar"
            encodeAndDecode(v)
        } ^
        "decode an encoded ClassWithLazyVal" ! resetCoding {
            val v = new ClassWithLazyVal
            v.a = "foo"
            encodeAndDecode(v)
        } ^
        "decode an encoded ClassWithNotCodedVar" ! resetCoding {
            val v = new ClassWithNotCodedVar
            v.a = "foo"
            encodeAndDecode(v)
        } ^
        "decode an encoded ScalaEnumWrapper" ! resetCoding {
            { encodeAndDecode(ScalaEnumWrapper(ScalaEnum.ONE)) } and
            { encodeAndDecode(ScalaEnumWrapper(ScalaEnum.TWO)) }
        } ^
        "decode an encoded JavaEnumWrapper" ! resetCoding {
            { encodeAndDecode(JavaEnumWrapper(JavaEnum.ONE)) } and
            { encodeAndDecode(JavaEnumWrapper(JavaEnum.TWO)) }
        } ^
        "decode encoded union values" ! resetCoding {
            { encodeAndDecode[AutoUnionBase](AutoUnionFirst(1)) } and
            { encodeAndDecode[AutoUnionBase](AutoUnionSecond("foo")) } and
            { encodeAndDecode[ExplicitUnionBase](ExplicitUnionFirst(1)) } and
            { encodeAndDecode[ExplicitUnionBase](ExplicitUnionSecond("foo")) } and
            { encodeAndDecode[ExplicitUnionBase](ExplicitUnionSingleton) }
        } ^
        "decode encode wrapper values" ! resetCoding {
            encodeAndDecode(WrapperClass(Some(CaseClass(1, "foo", Some("bub")))))
        } ^
        "encode wrapper values without the wrapping field" ! resetCoding {
            Coding.encode(WrapperClass(Some(CaseClass(1, "baz", None)))) | parameter(Nil) must matchEncodedJson(("foo" -> 1) ~ ("bar" -> "baz"))
        }
}
