//
// Copyright 2012 Paytronix Systems, Inc.
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

package com.paytronix.utils.lift

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.json.JsonAST.{JArray, JObject, JNothing, JNull, JString, JValue}
import net.liftweb.json.Implicits.{double2jvalue, int2jvalue, string2jvalue}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import org.slf4j.{Logger, LoggerFactory}
import org.specs2.SpecificationWithJUnit
import org.specs2.matcher.Matcher
import com.paytronix.utils.interchange._
import com.paytronix.utils.interchange.test.fixtures._
import com.paytronix.utils.interchange.test.Helpers._
import com.paytronix.utils.scala.result.{FailedG, Okay, Result, ResultG}

class BoxCoderSpecTest extends SpecificationWithJUnit with test.Helpers {
    val log = LoggerFactory.getLogger(getClass)

    val cl = getClass.getClassLoader

    class BoxStringTV extends TestValue[Box[String]] { val test: Box[String] = Empty }
    class BoxUnitTV extends TestValue[Box[Unit]]     { val test: Box[Unit]   = Empty }

    val testCoder = BoxCoder(StringCoder)
    val testUnitCoder = BoxCoder(UnitCoder)
    val nestedCoder = BoxCoder(BoxCoder(BoxCoder(StringCoder)))
    val nestedListCoder = BoxCoder(BoxCoder(BoxCoder(ScalaListCoder(StringCoder))))

    def failureJSON(message: String, chain: JValue = JNothing, param: (String, JValue) = (null, JNothing)): JObject = {
        var jobj = ("result" -> "failed") ~ ("errorMessage" -> message) ~ ("errorCode" -> "system.error") ~ ("failure" -> message)
        if (chain != JNothing)
            jobj = jobj ~ ("chain" -> chain)
        if (param._1 != null)
            jobj = jobj ~ ("paramIsA" -> param._1) ~ ("param" -> param._2)
        jobj
    }

    val paramFailureWithString = Failure("failed", Empty, Full(Failure("chained failure"))) ~> "additional param"
    val paramFailureWithStringJSON: JObject = failureJSON("failed", failureJSON("chained failure"), "java.lang.String" -> "additional param")

    val paramFailureWithInt = Failure("failed", Empty, Full(Failure("chained failure"))) ~> 1234
    val paramFailureWithIntJSON: JObject = failureJSON("failed", failureJSON("chained failure"), "java.lang.Integer" -> 1234)

    val paramFailureWithCaseClass = Failure("failed", Empty, Full(Failure("chained failure"))) ~> CaseClass(1, "foo", Some("bar"))
    val paramFailureWithCaseClassJSON: JObject = failureJSON("failed", failureJSON("chained failure"),
        "com.paytronix.utils.interchange.test.fixtures.CaseClass" -> (
            ("bar" -> "foo") ~ ("foo" -> 1) ~ ("zip" -> "bar")
        )
    )

    /** Matcher for unusual box expressions, as the equality comparison on Box does not treat Empty/Failure properly */
    def matchBox(expectedBox: Box[_]): Matcher[ResultG[_, Box[_]]] =
        beLike {
            case Okay(actualBox) =>
                def doesMatch(a: Box[_], b: Box[_]): Boolean = (a,b) match {
                    case (Full(aBox: Box[_]), Full(bBox: Box[_])) => doesMatch(aBox, bBox)
                    case (Full(aVal), Full(bVal)) => aVal == bVal
                    case (Empty, Empty) => true
                    case (Failure(aMsg, aCause, aChain), Failure(bMsg, bCause, bChain)) =>
                        aMsg == bMsg && doesMatch(aCause, bCause) && doesMatch(aChain, bChain)
                    case _ => false
                }

                if (doesMatch(actualBox, expectedBox)) ok
                else ko(actualBox + " does not match Okay(" + expectedBox + ")")
        }

    def is =
        "BoxCoder should" ^
        "basically decode" ! resetCoderSettings {
            { testCoder.decode(cl, JString("foo")) must matchBox(Full("foo")) } and
            { testCoder.decode(cl, JNothing) must matchBox(Empty) } and
            { testCoder.decode(cl, JNull) must matchBox(Empty) }
        } ^
        "basically encode" ! resetCoderSettings {
            { testCoder.encode(cl, Full("foo")) must matchEncodedJson(JString("foo")) } and
            { testCoder.encode(cl, Empty) must matchEncodedJson(JNothing) }
        } ^
        "basically round trip via Avro" ! resetCoderSettings {
            { avroRoundTrip(testCoder, Full("foo")) } and
            { avroRoundTrip(testCoder, Empty) }
        } ^
        "encode failures" ! resetCoderSettings {
            { testCoder.encode(cl, Failure("failed")) must matchEncodedJson(failureJSON("failed")) } and
            { testCoder.encode(cl, Failure("failed", Empty, Full(Failure("chained failure")))) must
                 matchEncodedJson(failureJSON("failed", failureJSON("chained failure"))) }
        } ^
        "decode failures" ! resetCoderSettings {
            { testCoder.decode(cl, failureJSON("failed")) must matchBox(Failure("failed")) } and
            { testCoder.decode(cl, failureJSON("failed", failureJSON("chained failure"))) must
                matchBox(Failure("failed", Empty, Full(Failure("chained failure")))) }
        } ^
        "round trip failures via Avro" ! resetCoderSettings {
            { avroRoundTrip(testCoder, Failure("failed")) } and
            { avroRoundTrip(testCoder, Failure("failed", Empty, Full(Failure("chained failure")))) }
        } ^
        "encode nested boxes" ! resetCoderSettings {
            { nestedCoder.encode(cl, Full(Full(Full("foo")))) must matchEncodedJson(JArray(JArray(JString("foo") :: Nil) :: Nil)) } and
            { nestedCoder.encode(cl, Full(Full(Empty))) must matchEncodedJson(JArray(JArray(JNothing :: Nil) :: Nil)) } and
            { nestedCoder.encode(cl, Full(Empty)) must matchEncodedJson(JArray(JNothing :: Nil)) } and
            { nestedCoder.encode(cl, Empty) must matchEncodedJson(JNothing) } and
            { nestedCoder.encode(cl, Full(Full(Failure("failed")))) must matchEncodedJson(JArray(JArray(failureJSON("failed") :: Nil) :: Nil)) } and
            { nestedCoder.encode(cl, Full(Failure("failed"))) must matchEncodedJson(JArray(failureJSON("failed") :: Nil)) } and
            { nestedCoder.encode(cl, Failure("failed")) must matchEncodedJson(failureJSON("failed")) }
        } ^
        "encode nested boxes with list terminals" ! resetCoderSettings {
            { nestedListCoder.encode(cl, Full(Full(Full("foo" :: Nil)))).must(
                matchEncodedJson(JArray(JArray(JArray(JString("foo") :: Nil) :: Nil) :: Nil))
            ) } and
            { nestedListCoder.encode(cl, Full(Full(Empty))) must matchEncodedJson(JArray(JArray(JNothing :: Nil) :: Nil)) } and
            { nestedListCoder.encode(cl, Full(Empty)) must matchEncodedJson(JArray(JNothing :: Nil)) } and
            { nestedListCoder.encode(cl, Empty) must matchEncodedJson(JNothing) } and
            { nestedListCoder.encode(cl, Full(Full(Failure("failed")))) must matchEncodedJson(JArray(JArray((failureJSON("failed")) :: Nil) :: Nil)) } and
            { nestedListCoder.encode(cl, Full(Failure("failed"))) must matchEncodedJson(JArray(failureJSON("failed") :: Nil)) } and
            { nestedListCoder.encode(cl, Failure("failed")) must matchEncodedJson(failureJSON("failed")) }
        } ^
        "decode nested boxes" ! resetCoderSettings {
            { nestedCoder.decode(cl, JArray(JArray(JString("foo") :: Nil) :: Nil)) must_== Okay(Full(Full(Full("foo")))) } and
            { nestedCoder.decode(cl, JArray(JArray(JNothing :: Nil) :: Nil)) must_== Okay(Full(Full(Empty))) } and
            { nestedCoder.decode(cl, JArray(JArray(JNull :: Nil) :: Nil)) must_== Okay(Full(Full(Empty))) } and
            { nestedCoder.decode(cl, JArray(JArray(Nil) :: Nil)) must_== Okay(Full(Full(Empty))) } and
            { nestedCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Full(Empty)) } and
            { nestedCoder.decode(cl, JArray(JNull :: Nil)) must_== Okay(Full(Empty)) } and
            { nestedCoder.decode(cl, JArray(Nil)) must_== Okay(Full(Empty)) } and
            { nestedCoder.decode(cl, JNothing) must_== Okay(Empty) } and
            { nestedCoder.decode(cl, JNull) must_== Okay(Empty) } and
            { nestedCoder.decode(cl, JArray(JArray(failureJSON("failed") :: Nil) :: Nil)) must_== Okay(Full(Full(Failure("failed")))) } and
            { nestedCoder.decode(cl, JArray(failureJSON("failed") :: Nil)) must_== Okay(Full(Failure("failed"))) } and
            { nestedCoder.decode(cl, failureJSON("failed")) must_== Okay(Failure("failed")) }
        } ^
        "decode nested boxes with list terminals" ! resetCoderSettings {
            { nestedListCoder.decode(cl, JArray(JArray(JArray(JString("foo") :: Nil) :: Nil) :: Nil)) must_== Okay(Full(Full(Full("foo" :: Nil)))) } and
            { nestedListCoder.decode(cl, JArray(JArray(JArray(Nil) :: Nil) :: Nil)) must_== Okay(Full(Full(Full(Nil)))) } and
            { nestedListCoder.decode(cl, JArray(JArray(JNothing :: Nil) :: Nil)) must_== Okay(Full(Full(Empty))) } and
            { nestedListCoder.decode(cl, JArray(JArray(JNull :: Nil) :: Nil)) must_== Okay(Full(Full(Empty))) } and
            { nestedListCoder.decode(cl, JArray(JArray(Nil) :: Nil)) must_== Okay(Full(Full(Empty))) } and
            { nestedListCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Full(Empty)) } and
            { nestedListCoder.decode(cl, JArray(JNull :: Nil)) must_== Okay(Full(Empty)) } and
            { nestedListCoder.decode(cl, JArray(Nil)) must_== Okay(Full(Empty)) } and
            { nestedListCoder.decode(cl, JNothing) must_== Okay(Empty) } and
            { nestedListCoder.decode(cl, JNull) must_== Okay(Empty) } and
            { nestedListCoder.decode(cl, JArray(JArray(failureJSON("failed") :: Nil) :: Nil)) must_== Okay(Full(Full(Failure("failed")))) } and
            { nestedListCoder.decode(cl, JArray(failureJSON("failed") :: Nil)) must_== Okay(Full(Failure("failed"))) } and
            { nestedListCoder.decode(cl, failureJSON("failed")) must_== Okay(Failure("failed")) }
        } ^
        "round trip nested boxes via Avro" ! resetCoderSettings {
            { avroRoundTrip(nestedCoder, Full(Full(Full("foo")))) } and
            { avroRoundTrip(nestedCoder, Full(Full(Empty))) } and
            { avroRoundTrip(nestedCoder, Full(Empty)) } and
            { avroRoundTrip(nestedCoder, Empty) } and
            { avroRoundTrip(nestedCoder, Full(Full(Failure("failed")))) } and
            { avroRoundTrip(nestedCoder, Full(Failure("failed"))) } and
            { avroRoundTrip(nestedCoder, Failure("failed")) }
        } ^
        "honor hideFailures given at configuration time" ! resetCoderSettings {
            { BoxCoder(StringCoder, Some(true)).encode(cl, Failure("test")) must_== Okay(JNothing) } and
            { BoxCoder(StringCoder, Some(false)).encode(cl, Failure("test")) must matchEncodedJson(failureJSON("test")) }
        } ^
        "honor hideFailures given at configuration time even if set otherwise at encode time" ! resetCoderSettings {
            CoderSettings.hideFailures.set(true)

            { BoxCoder(StringCoder, Some(true)).encode(cl, Failure("test")) must_== Okay(JNothing) } and
            { BoxCoder(StringCoder, Some(false)).encode(cl, Failure("test")) must matchEncodedJson(failureJSON("test")) }
        } ^
        "honor hideFailures given at encode time" ! resetCoderSettings {
            CoderSettings.hideFailures.set(true)

            BoxCoder(StringCoder).encode(cl, Failure("test")) must_== Okay(JNothing)
        } ^
        "encode ParamFailures" ! resetCoderSettings {
            { testCoder.encode(cl, paramFailureWithString) must matchEncodedJson(paramFailureWithStringJSON) } and
            { testCoder.encode(cl, paramFailureWithInt) must matchEncodedJson(paramFailureWithIntJSON) } and
            { testCoder.encode(cl, paramFailureWithCaseClass) must matchEncodedJson(paramFailureWithCaseClassJSON) }
        }

        "decode ParamFailures" ! resetCoderSettings {
            { testCoder.decode(cl, paramFailureWithStringJSON) must matchBox(paramFailureWithString) } and
            { testCoder.decode(cl, paramFailureWithIntJSON) must matchBox(paramFailureWithInt) } and
            { testCoder.decode(cl, paramFailureWithCaseClassJSON) must matchBox(paramFailureWithCaseClass) }
        }

        "round trip ParamFailures" ! resetCoderSettings {
            { avroRoundTrip(testCoder, paramFailureWithString) } and
            { avroRoundTrip(testCoder, paramFailureWithInt) } and
            { avroRoundTrip(testCoder, paramFailureWithCaseClass) }
        } ^
        "gracefully degrade when encoding complicated ParamFailures" ! resetCoderSettings {
            log.warn("The following warning about decoding an incoming ParamFailure is expected:")

            testCoder.encode(cl, Failure("failed") ~> ("foo", 1)) must matchEncodedJson(failureJSON("failed"))
        } ^
        "gracefully degrade when encoding complicated ParamFailures via Avro" ! resetCoderSettings {
            log.warn("The following warning about decoding an incoming ParamFailure is expected:")
            avroRoundTripExpect(testCoder, Failure("failed") ~> ("foo", 1)) { (result: Result[Box[String]]) =>
                result must beLike {
                    case Okay(value) => value must_== Failure("failed")
                }
            }
        } ^
        "encode Units" ! resetCoderSettings {
            { testUnitCoder.encode(cl, Full(())) must matchEncodedJson(JArray(JNothing :: Nil)) } and
            { testUnitCoder.encode(cl, Empty) must matchEncodedJson(JNothing) }
        } ^
        "decode Units" ! resetCoderSettings {
            { testUnitCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Full(())) } and
            { testUnitCoder.decode(cl, JArray(Nil)) must_== Okay(Full(())) } and
            { testUnitCoder.decode(cl, JNothing) must_== Okay(Empty) } and
            { testUnitCoder.decode(cl, JNull) must_== Okay(Empty) }
        } ^
        "round trip Units" ! resetCoderSettings {
            { testUnitCoder.encode(cl, Full(())).flatMap(testUnitCoder.decode(cl, _)) must_== Okay(Full(())) } and
            { avroRoundTrip(testUnitCoder, Full(())) } and
            { avroRoundTrip(testUnitCoder, Empty) } and
            { avroRoundTrip(testUnitCoder, paramFailureWithString) }
        } ^
        "be detected" ! resetCoderSettings {
            { new BoxStringTV().coder must_== Okay(BoxCoder(StringCoder)) } and
            { new BoxUnitTV().coder   must_== Okay(BoxCoder(UnitCoder)) }
        }
}
