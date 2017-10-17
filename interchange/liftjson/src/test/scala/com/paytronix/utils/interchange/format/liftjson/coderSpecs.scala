//
// Copyright 2016 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange.format.liftjson

import net.liftweb.json.JsonAST._
import org.specs2.{ScalaCheck, SpecificationWithJUnit}
import net.liftweb.common.{Box, Full, Empty,Failure, ParamFailure,EmptyBox}

import com.paytronix.utils.interchange.format.json.{JsonCoder, derive,scalar}
import com.paytronix.utils.interchange.format.json.coders._
import com.paytronix.utils.interchange.format.liftjson
import com.paytronix.utils.interchange.format.liftjson.coders._
import com.paytronix.utils.scala.result.{Okay, Result}

import scala.collection.JavaConverters.{asJavaCollectionConverter, asScalaBufferConverter, mapAsJavaMapConverter, mapAsScalaMapConverter}

import com.fasterxml.jackson.core.JsonToken
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}
import org.specs2.matcher.{Matcher, MatchResult}

import com.paytronix.utils.interchange.base.{CoderFailure, InsecureContext, Receiver}
import com.paytronix.utils.interchange.format.string
import com.paytronix.utils.scala.result.{FailedException, FailedG, FailedParameterDefault, Okay, ResultG, unless}

import Arbitrary.arbitrary
import com.paytronix.utils.interchange.format.json


case class InnerClass(test: List[String])
object InnerClass {
    implicit val jsonCoder = derive.structure.coder[InnerClass]
}

case class TestClass(
    s: String,
    i: Int,
    a: List[Int],
    d: Double,
    o: InnerClass,
    b: Boolean
)
object TestClass {
    implicit val jsonCoder = derive.structure.coder[TestClass]
}


class LiftJsonCoderTest extends SpecificationWithJUnit with ScalaCheck {

    def is = s2"""
        Lift-JSON compatibility must
            encode JObject                                                          $encodeJObjectCase
            decode JObject                                                          $decodeJObjectCase
            encode from case class to JValue to String and all the way back again   $encodeAndDecodeCaseClassCase
    """

    val jObjectWithAllTypes = {
        JObject(List(
                JField("s", JString("123")),
                JField("i", JInt(10)),
                JField("a", JArray(List(JArray(List(JInt(1), JInt(2), JInt(3)))))),
                JField("d", JDouble(5.5)),
                JField("o", JObject(List(JField("test", JArray(List(JString("test"))))))),
                JField("b", JBool(false))
            )
        )
    }

    def encodeJObjectCase = {
        jObjectCoder.encode.toString(jObjectWithAllTypes) === Okay("""{"s":"123","i":10,"a":[[1,2,3]],"d":5.5,"o":{"test":["test"]},"b":false}""")
    }

    def decodeJObjectCase = {
        jObjectCoder.decode.fromString("""{"s":"123","i":10,"a":[[1,2,3]],"d":5.5,"o":{"test":["test"]},"b":false}""") === Okay(jObjectWithAllTypes)
    }

    def encodeAndDecodeCaseClassCase = {
        val testInstance = TestClass("123", 10, List(1,2,3), 5.5, InnerClass(List("test")), false)
        val testClassCoder: JsonCoder[TestClass] = JsonCoder[TestClass]

        {
            testClassCoder.encode.toJValue(testInstance) >>=
            { x => jValueCoder.encode.toString(x) } >>=
            { x => jValueCoder.decode.fromString(x) } >>=
            { x => testClassCoder.decode.fromJValue(x) }
        } === Okay(testInstance)
    }
}