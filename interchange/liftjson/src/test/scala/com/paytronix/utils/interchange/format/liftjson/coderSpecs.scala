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

    def encodeJObjectCase = {
        jObjectCoder.encode.toString(jObjectWithAllTypes) === Okay("""{"s":"123","i":"10","a":[["1","2","3"]],"d":5.5,"o":{"test":["test"]},"b":false}""")
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

object BoxJsonCoderTestHelpers{
//custom equals method for Failure objects because the .equals() method doesn't work as intended for
//Throwable Objects (in the exception parameter of Failure objects)
def failuresAreEqual(a: Failure,b: Failure): Boolean = {
    if(a.msg!=b.msg){
        return false
    }
    if(a.exception.toString()!=b.exception.toString()){
        return false
    }
    if((a.chain==Empty && b.chain!=Empty)||(a.chain!=Empty && b.chain==Empty)){
        return false
    }
    if(a.chain!=Empty && b.chain!=Empty){
        return(failuresAreEqual(a.chain.openOr(Empty).asInstanceOf[Failure],b.chain.openOr(Empty).asInstanceOf[Failure]))
    }
    return true
}
}

class FailureAndParamFailuresAreEqualTest extends SpecificationWithJUnit with JsonMatchers {
    def is = {
        "Tests for failuresAreEqual and paramFailuresAreEqual functions" ^
        "two identical Failures are equal" ! {
            val resultBox: Failure = Failure("testMsg",Full(new Throwable("msg")),Empty)
            val resultBox1: Failure = Failure("testMsg",Full(new Throwable("msg")),Empty)
            BoxJsonCoderTestHelpers.failuresAreEqual(resultBox1,resultBox) must_== true
        }^
        "two identical Failures with chains are equal" ! {
            val resultBox: Failure = Failure("testMsg",Full(new Throwable("msg")),Full(Failure("testMsgNested",Full(new Throwable("msg")),Empty)))
            val resultBox1: Failure = Failure("testMsg",Full(new Throwable("msg")),Full(Failure("testMsgNested",Full(new Throwable("msg")),Empty)))
            BoxJsonCoderTestHelpers.failuresAreEqual(resultBox1,resultBox) must_== true
        }^
        "two Failures with different chains are inequal" ! {
            val resultBox: Failure = Failure("testMsg",Full(new Throwable("msg")),Full(Failure("testMsgNestedDiff",Full(new Throwable("msg")),Empty)))
            val resultBox1: Failure = Failure("testMsg",Full(new Throwable("msg")),Full(Failure("testMsgNested",Full(new Throwable("msg")),Empty)))
            BoxJsonCoderTestHelpers.failuresAreEqual(resultBox1,resultBox) must_== false
        }^
        "one Failure without a chain and another Failure with a chain are inequal" ! {
            val resultBox: Failure = Failure("testMsg",Full(new Throwable("msg")),Empty)
            val resultBox1: Failure = Failure("testMsg",Full(new Throwable("msg")),Full(Failure("testMsgNested",Full(new Throwable("msg")),Empty)))
            BoxJsonCoderTestHelpers.failuresAreEqual(resultBox1,resultBox) must_== false
        }
    }
}

class BoxJsonCoderTest extends SpecificationWithJUnit with JsonMatchers {
    //initialize a boxJsonCoder to test with
    val boxJsonCoder = coders.boxJsonCoder[Int]
    val boxJsonOptionCoder = coders.boxJsonCoder[Option[Int]]
    
    def is = {
        "Tests for BoxCoder with different types of Box subclasses" ^
        /*
                Encoding Tests
        */
        "encode a Full container of a primitive type" ! {
            val testBox: Box[Int] = Full(12)
            boxJsonCoder.encode.toString(testBox) must_== Okay("12")
        }^
        "encode a Full container of a non-null non-primitive type" ! {
            val testBox: Box[Option[Int]] = Full(Option(12))
            boxJsonOptionCoder.encode.toString(testBox) must_== Okay("[12]")
        }^
        "encode a Full container of a non-null non-primitive type" ! {
            val testBox: Box[Option[Int]] = Full(None)
            boxJsonOptionCoder.encode.toString(testBox) must_== Okay("[]")
        }^
        "encode an Empty container" ! {
            val testBox: Box[Int] = Empty
            boxJsonCoder.encode.toString(testBox) must_== Okay("null")
        }^
        "encode a Failure container with an Empty exception" ! {
            val testBox: Box[Int] = Failure("testMsg",Empty,Empty)
            println(boxJsonCoder.encode.toString(testBox))
            boxJsonCoder.encode.toString(testBox) must_== Okay("""{"result":"failed","errorCode":"system.error","errorMessage":"testMsg"}""")
        }^
        "encode a Failure container with a Full exception" ! {
            val testBox: Box[Int] = Failure("testMsg",Full(new Throwable("This is our error")),Empty)
            boxJsonCoder.encode.toString(testBox) must_== Okay("""{"result":"failed","errorCode":"system.error","errorMessage":"testMsg","exception":{"isA":"java.lang.Throwable","message":"This is our error"}}""")
        }^
        "encode a Failure container with a Full exception that has no message" ! {
            val testBox: Box[Int] = Failure("testMsg",Full(new Throwable()),Empty)
            boxJsonCoder.encode.toString(testBox) must_== Okay("""{"result":"failed","errorCode":"system.error","errorMessage":"testMsg","exception":{"isA":"java.lang.Throwable"}}""")
        }^
        "encode a Failure container with a Full chain" ! {
            val testBox: Box[Int] = Failure("testMsg",Full(new Throwable("This is our error")),Full(Failure("testMsgChain",Full(new Throwable("This is our errorChain")),Empty)))
            boxJsonCoder.encode.toString(testBox) must_== Okay("""{"result":"failed","errorCode":"system.error","errorMessage":"testMsg","exception":{"isA":"java.lang.Throwable","message":"This is our error"},"chain":{"result":"failed","errorCode":"system.error","errorMessage":"testMsgChain","exception":{"isA":"java.lang.Throwable","message":"This is our errorChain"}}}""")
        }^
        "encode a Failure container with a double-nested chain" ! {
            val testBox: Box[Int] = Failure("testMsg",Full(new Throwable("This is our error")),Full(Failure("testMsgChain",Full(new Throwable("This is our errorChain")),Full(Failure("testMsgNestedChain",Full(new Throwable("This is our errorNestedChain")),Empty)))))
            boxJsonCoder.encode.toString(testBox) must_== Okay("""{"result":"failed","errorCode":"system.error","errorMessage":"testMsg","exception":{"isA":"java.lang.Throwable","message":"This is our error"},"chain":{"result":"failed","errorCode":"system.error","errorMessage":"testMsgChain","exception":{"isA":"java.lang.Throwable","message":"This is our errorChain"},"chain":{"result":"failed","errorCode":"system.error","errorMessage":"testMsgNestedChain","exception":{"isA":"java.lang.Throwable","message":"This is our errorNestedChain"}}}}""")
        }^
        /*
                Decoding Tests
        */
        "decode a String to a Full container of a primitive type" ! {
            val resultBox: Box[Int] = Full(12)
            val decodeStr = "12"
            decode(boxJsonCoder.decode)(decodeStr) must_== Okay(resultBox)
        }^
        "decode a String to a Full container of a non-primitive type" ! {
            val resultBox: Box[Option[Int]] = Full(Option(12))
            val decodeStr = "[12]"
            decode(boxJsonOptionCoder.decode)(decodeStr) must_== Okay(resultBox)
        }^
        "decode a String to an Empty container" ! {
            val resultBox: Box[Int] = Empty
            val decodeStr = null
            decode(boxJsonCoder.decode)(decodeStr) must_== Okay(resultBox)
        }^
        "decode a String to a Failure container with an Empty exception" ! {
            val resultBox: Box[Int] = Failure("testMsg",Empty,Empty)
            val decodeStr = """{"result":"failed","errorMessage":"testMsg"}"""
            decode(boxJsonCoder.decode)(decodeStr) must_== Okay(resultBox)
        }^
        "decode a String to a Failure container with a Full error" ! {
            val resultBox = Failure("testMsg",Full(new Error("This is our error")),Empty)
            val decodeStr = """{"result":"failed","errorMessage":"testMsg","exception":{"isA":"java.lang.Error","message":"This is our error"}}"""
            val decodedObj = decode(boxJsonCoder.decode)(decodeStr).getOrElse(Empty).asInstanceOf[Failure]
            BoxJsonCoderTestHelpers.failuresAreEqual(resultBox,decodedObj) must_== true
        }^
        "decode a String to a Failure container with a Full exception" ! {
            val resultBox = Failure("testMsg",Full(new Throwable("This is our error")),Empty)
            val decodeStr = """{"result":"failed","errorMessage":"testMsg","exception":{"isA":"java.lang.Throwable","message":"This is our error"}}"""
            val decodedObj = decode(boxJsonCoder.decode)(decodeStr).getOrElse(Empty).asInstanceOf[Failure]
            BoxJsonCoderTestHelpers.failuresAreEqual(resultBox,decodedObj) must_== true
        }^
        "decode a String to a Failure container with a Full chain" ! {
            println("\n\n Entered the Failure with a chain test case\n")
            val resultBox = Failure("testMsg",Full(new Throwable("This is our error")),Full(Failure("testMsgChain",Full(new Throwable("This is our errorChain")),Empty)))
            val decodeStr = """{"result":"failed","errorMessage":"testMsg","exception":{"isA":"java.lang.Throwable","message":"This is our error"},"chain":{"result":"failed","errorMessage":"testMsgChain","exception":{"isA":"java.lang.Throwable","message":"This is our errorChain"}}}"""
            val decodedObj = decode(boxJsonCoder.decode)(decodeStr).getOrElse(Empty).asInstanceOf[Failure]
            BoxJsonCoderTestHelpers.failuresAreEqual(decodedObj,resultBox) must_== true
        }^
        "decode a String to a ParamFailure container with an Empty exception and a String param" ! {
            val resultBox: Box[Int] = ParamFailure("testMsg",Empty,Empty,"testParam")
            val decodeStr = """{"result":"failed","errorMessage":"testMsg","param":"testParam"}"""
            decode(boxJsonCoder.decode)(decodeStr) must_== Okay(resultBox)
        }^ /*
                Tests that try encoding/decoding in the same line
        */
        "decoding and then encoding a Failure container with every parameter filled in equals itself" ! {
            val decodeStr = """{"result":"failed","errorCode":"system.error","errorMessage":"testMsg","exception":{"isA":"java.lang.Throwable","message":"This is our error"},"chain":{"result":"failed","errorCode":"system.error","errorMessage":"testMsgChain","exception":{"isA":"java.lang.Throwable","message":"This is our errorChain"}}}"""
            boxJsonCoder.encode.toString(decode(boxJsonCoder.decode)(decodeStr).getOrElse(Empty).asInstanceOf[Failure]) must_== Okay(decodeStr)
        }^
        "encoding and then decoding a string representation of a Failure container equals itself" ! {
            val resultBox = Failure("testMsg",Full(new Throwable("This is our error")),Full(Failure("testMsgChain",Full(new Throwable("This is our errorChain")),Empty)))
            val decodedObj = decode(boxJsonCoder.decode)(boxJsonCoder.encode.toString(resultBox).getOrElse("")).getOrElse(Empty).asInstanceOf[Failure]
            BoxJsonCoderTestHelpers.failuresAreEqual(decodedObj,resultBox) must_== true
        }
    }
}