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
import org.specs._
import org.specs.runner.JUnit4
import com.paytronix.utils.extendedreflection.Builder
import com.paytronix.utils.interchange._
import com.paytronix.utils.scala.result.Okay

import fixtures._
import Helpers._

class EndToEndTestSpecsAsTest extends JUnit4(EndToEndTestSpecs)

object EndToEndTestSpecs extends Specification
{
    noDetailedDiffs()

    "End to end, coding" should {
        Coding.reset.before

        def encodeAndDecode[T](in: T)(implicit m: Manifest[T]): Unit =
            Coding.encode[T](in).flatMap(Coding.decode[T](_)) must_== Okay(in)

        "decode an encoded CaseClass" in {
            encodeAndDecode(CaseClass(1, "foo", Some("bub")))
            encodeAndDecode(CaseClass(1, "foo", None))
            encodeAndDecode(CaseClass(1, null, Some("bub")))
        }

        "decode an encoded BasicClass" in {
            encodeAndDecode(BasicClass.make(1, "foo", Some("bub")))
            encodeAndDecode(BasicClass.make(1, "foo", None))
            encodeAndDecode(BasicClass.make(1, null, Some("bub")))
        }

        "decode an encoded POJOClass" in {
            encodeAndDecode(POJOClass.make(1, "foo", Some("bub")))
            encodeAndDecode(POJOClass.make(1, "foo", None))
            encodeAndDecode(POJOClass.make(1, null, Some("bub")))
        }

        "decode an encoded WrappingObject.WrappedClass" in {
            encodeAndDecode(WrappingObject.WrappedClass(1, Some(WrappingObject.WrappedEnum.Two)))
        }

        "decode an encoded WrappingObject.SubWrappingObject.WrappedClass" in {
            encodeAndDecode(WrappingObject.SubWrappingObject.WrappedClass(1))
        }

        "decode an encoded ComposedClass" in {
            val v = new ComposedClass
            v.a = "foo"
            v.b = 2
            v.c = 1.5

            encodeAndDecode(v)
        }

        "decode an encoded Wrapper" in {
            encodeAndDecode(Wrapper(CaseClass(1, "foo", Some("bub"))))
        }

        "decode an encoded CaseClassWithSideCar" in {
            encodeAndDecode(CaseClassWithSideCar(1, "foo", Some("bub")))
            encodeAndDecode(CaseClassWithSideCar(1, "foo", None))
            encodeAndDecode(CaseClassWithSideCar(1, null, Some("bub")))
        }

        "decode an encoded ClassWithMultipleConstructors" in {
            val v = new ClassWithMultipleConstructors(1, "foo", 0.0)
            v.d = "bar"
            encodeAndDecode(v)
        }

        "decode an encoded ClassWithLazyVal" in {
            val v = new ClassWithLazyVal
            v.a = "foo"
            encodeAndDecode(v)
        }

        "decode an encoded ClassWithNotCodedVar" in {
            val v = new ClassWithNotCodedVar
            v.a = "foo"
            encodeAndDecode(v)
        }

        "decode an encoded ScalaEnumWrapper" in {
            encodeAndDecode(ScalaEnumWrapper(ScalaEnum.ONE))
            encodeAndDecode(ScalaEnumWrapper(ScalaEnum.TWO))
        }

        "decode an encoded JavaEnumWrapper" in {
            encodeAndDecode(JavaEnumWrapper(JavaEnum.ONE))
            encodeAndDecode(JavaEnumWrapper(JavaEnum.TWO))
        }

        "decode encoded union values" in {
            encodeAndDecode[AutoUnionBase](AutoUnionFirst(1))
            encodeAndDecode[AutoUnionBase](AutoUnionSecond("foo"))

            encodeAndDecode[ExplicitUnionBase](ExplicitUnionFirst(1))
            encodeAndDecode[ExplicitUnionBase](ExplicitUnionSecond("foo"))
            encodeAndDecode[ExplicitUnionBase](ExplicitUnionSingleton)
        }

        "decode encode wrapper values" in {
            encodeAndDecode(WrapperClass(Some(CaseClass(1, "foo", Some("bub")))))
        }

        "encode wrapper values without the wrapping field" in {
            Coding.encode(WrapperClass(Some(CaseClass(1, "baz", None)))) withFailureParameter Nil must matchEncodedJson(("foo" -> 1) ~ ("bar" -> "baz"))
        }
    }
}
