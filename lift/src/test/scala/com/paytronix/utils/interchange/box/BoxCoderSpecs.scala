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
import net.liftweb.json.JsonAST.{JArray, JObject, JNothing, JNull, JString}
import net.liftweb.json.Implicits.{double2jvalue, int2jvalue, string2jvalue}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import org.slf4j.{Logger, LoggerFactory}
import org.specs._
import org.specs.matcher.Matcher
import org.specs.runner.JUnit4
import com.paytronix.utils.interchange._
import com.paytronix.utils.interchange.test.fixtures._
import com.paytronix.utils.interchange.test.Helpers._
import com.paytronix.utils.scala.result.{FailedG, Okay, Result, ResultG}

class BoxCoderTestSpecsAsTest extends JUnit4(BoxCoderTestSpecs)

object BoxCoderTestSpecs extends Specification {
    noDetailedDiffs()

    val log = LoggerFactory.getLogger(getClass)

    val cl = getClass.getClassLoader

    def avroRoundTrip[T](inst: ComposableCoder[T], value: T): Unit =
        avroRoundTrip(Coder(cl, inst), value)

    def avroRoundTrip[T](inst: ComposableCoder[T], value: T, expectation: Result[T] => Unit): Unit =
        avroRoundTrip(Coder(cl, inst), value, expectation)

    def avroRoundTrip[T](inst: Coder[T], value: T): Unit =
        avroRoundTrip[T](inst, value, (decoded: Result[T]) => {
            if (!decoded.isDefined) decoded.asInstanceOf[FailedG[_]].throwable.printStackTrace()
            decoded must verify(_.isDefined)
            if (value.asInstanceOf[AnyRef] ne null) decoded.orThrow must_== value
            else decoded.orThrow must verify(_.asInstanceOf[AnyRef] eq null)

            ()
        })

    def avroRoundTrip[T](inst: Coder[T], value: T, expectation: Result[T] => Unit): Unit = {
        val encoded = inst.encodeAvro(value)
        encoded must verify(_.isDefined)

        val decoded = encoded.flatMap(inst.decodeAvro(inst.avroSchema, _))

        expectation(decoded)

        ()
    }

    class BoxStringTV extends TestValue[Box[String]] { val test: Box[String] = Empty }
    class BoxUnitTV extends TestValue[Box[Unit]]     { val test: Box[Unit]   = Empty }

    "BoxCoder" should {
        CoderSettings.reset.before

        val testCoder = BoxCoder(StringCoder)
        val testUnitCoder = BoxCoder(UnitCoder)
        val nestedCoder = BoxCoder(BoxCoder(BoxCoder(StringCoder)))
        val nestedListCoder = BoxCoder(BoxCoder(BoxCoder(ScalaListCoder(StringCoder))))

        /** Matcher for unusual box expressions, as the equality comparison on Box does not treat Empty/Failure properly */
        case class matchBox(expectedBox: Box[_]) extends Matcher[ResultG[_, Box[_]]] {
            def apply(actualResult: => ResultG[_, Box[_]]) = {
                def doesMatch(a: Box[_], b: Box[_]): Boolean = (a,b) match {
                    case (Full(aBox: Box[_]), Full(bBox: Box[_])) => doesMatch(aBox, bBox)
                    case (Full(aVal), Full(bVal)) => aVal == bVal
                    case (Empty, Empty) => true
                    case (Failure(aMsg, aCause, aChain), Failure(bMsg, bCause, bChain)) =>
                        aMsg == bMsg && doesMatch(aCause, bCause) && doesMatch(aChain, bChain)
                    case _ => false
                }

                val matched = doesMatch(actualResult.orThrow, expectedBox)
                (matched, "ok", actualResult + " does not match Okay(" + expectedBox + ")")
            }
        }


        "basically decode" in {
            testCoder.decode(cl, JString("foo")) must matchBox(Full("foo"))
            testCoder.decode(cl, JNothing) must matchBox(Empty)
            testCoder.decode(cl, JNull) must matchBox(Empty)
        }

        "basically encode" in {
            testCoder.encode(cl, Full("foo")) must matchEncodedJson(JString("foo"))
            testCoder.encode(cl, Empty) must matchEncodedJson(JNothing)
        }

        "basically round trip via Avro" in {
            avroRoundTrip(testCoder, Full("foo"))
            avroRoundTrip(testCoder, Empty)
        }

        "encode failures" in {
            testCoder.encode(cl, Failure("failed")) must matchEncodedJson("failure" -> "failed")
            testCoder.encode(cl, Failure("failed", Empty, Full(Failure("chained failure")))) must
                 matchEncodedJson(("failure" -> "failed") ~ ("chain" -> ("failure" -> "chained failure")))
        }

        "decode failures" in {
            testCoder.decode(cl, ("failure" -> "failed")) must matchBox(Failure("failed"))
            testCoder.decode(cl, ("failure" -> "failed") ~ ("chain" -> ("failure" -> "chained failure"))) must
                matchBox(Failure("failed", Empty, Full(Failure("chained failure"))))
        }

        "round trip failures via Avro" in {
            avroRoundTrip(testCoder, Failure("failed"))
            avroRoundTrip(testCoder, Failure("failed", Empty, Full(Failure("chained failure"))))
        }

        "encode nested boxes" in {
            nestedCoder.encode(cl, Full(Full(Full("foo")))) must matchEncodedJson(JArray(JArray(JString("foo") :: Nil) :: Nil))
            nestedCoder.encode(cl, Full(Full(Empty))) must matchEncodedJson(JArray(JArray(JNothing :: Nil) :: Nil))
            nestedCoder.encode(cl, Full(Empty)) must matchEncodedJson(JArray(JNothing :: Nil))
            nestedCoder.encode(cl, Empty) must matchEncodedJson(JNothing)

            nestedCoder.encode(cl, Full(Full(Failure("failed")))) must matchEncodedJson(JArray(JArray(("failure" -> "failed") :: Nil) :: Nil))
            nestedCoder.encode(cl, Full(Failure("failed"))) must matchEncodedJson(JArray(("failure" -> "failed") :: Nil))
            nestedCoder.encode(cl, Failure("failed")) must matchEncodedJson("failure" -> "failed")
        }

        "encode nested boxes with list terminals" in {
            nestedListCoder.encode(cl, Full(Full(Full("foo" :: Nil)))).must(
                matchEncodedJson(JArray(JArray(JArray(JString("foo") :: Nil) :: Nil) :: Nil))
            )
            nestedListCoder.encode(cl, Full(Full(Empty))) must matchEncodedJson(JArray(JArray(JNothing :: Nil) :: Nil))
            nestedListCoder.encode(cl, Full(Empty)) must matchEncodedJson(JArray(JNothing :: Nil))
            nestedListCoder.encode(cl, Empty) must matchEncodedJson(JNothing)

            nestedListCoder.encode(cl, Full(Full(Failure("failed")))) must matchEncodedJson(JArray(JArray(("failure" -> "failed") :: Nil) :: Nil))
            nestedListCoder.encode(cl, Full(Failure("failed"))) must matchEncodedJson(JArray(("failure" -> "failed") :: Nil))
            nestedListCoder.encode(cl, Failure("failed")) must matchEncodedJson("failure" -> "failed")
        }

        // unlike JSON, nothing special about lists in boxes for Avro, so don't cover it

        "decode nested boxes" in {
            nestedCoder.decode(cl, JArray(JArray(JString("foo") :: Nil) :: Nil)) must_== Okay(Full(Full(Full("foo"))))
            nestedCoder.decode(cl, JArray(JArray(JNothing :: Nil) :: Nil)) must_== Okay(Full(Full(Empty)))
            nestedCoder.decode(cl, JArray(JArray(JNull :: Nil) :: Nil)) must_== Okay(Full(Full(Empty)))
            nestedCoder.decode(cl, JArray(JArray(Nil) :: Nil)) must_== Okay(Full(Full(Empty)))
            nestedCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Full(Empty))
            nestedCoder.decode(cl, JArray(JNull :: Nil)) must_== Okay(Full(Empty))
            nestedCoder.decode(cl, JArray(Nil)) must_== Okay(Full(Empty))
            nestedCoder.decode(cl, JNothing) must_== Okay(Empty)
            nestedCoder.decode(cl, JNull) must_== Okay(Empty)

            nestedCoder.decode(cl, JArray(JArray(("failure" -> "failed") :: Nil) :: Nil)) must_== Okay(Full(Full(Failure("failed"))))
            nestedCoder.decode(cl, JArray(("failure" -> "failed") :: Nil)) must_== Okay(Full(Failure("failed")))
            nestedCoder.decode(cl, "failure" -> "failed") must_== Okay(Failure("failed"))
        }

        "decode nested boxes with list terminals" in {
            nestedListCoder.decode(cl, JArray(JArray(JArray(JString("foo") :: Nil) :: Nil) :: Nil)) must_== Okay(Full(Full(Full("foo" :: Nil))))
            nestedListCoder.decode(cl, JArray(JArray(JArray(Nil) :: Nil) :: Nil)) must_== Okay(Full(Full(Full(Nil))))
            nestedListCoder.decode(cl, JArray(JArray(JNothing :: Nil) :: Nil)) must_== Okay(Full(Full(Empty)))
            nestedListCoder.decode(cl, JArray(JArray(JNull :: Nil) :: Nil)) must_== Okay(Full(Full(Empty)))
            nestedListCoder.decode(cl, JArray(JArray(Nil) :: Nil)) must_== Okay(Full(Full(Empty)))
            nestedListCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Full(Empty))
            nestedListCoder.decode(cl, JArray(JNull :: Nil)) must_== Okay(Full(Empty))
            nestedListCoder.decode(cl, JArray(Nil)) must_== Okay(Full(Empty))
            nestedListCoder.decode(cl, JNothing) must_== Okay(Empty)
            nestedListCoder.decode(cl, JNull) must_== Okay(Empty)
            nestedListCoder.decode(cl, JArray(JArray(("failure" -> "failed") :: Nil) :: Nil)) must_== Okay(Full(Full(Failure("failed"))))
            nestedListCoder.decode(cl, JArray(("failure" -> "failed") :: Nil)) must_== Okay(Full(Failure("failed")))
            nestedListCoder.decode(cl, "failure" -> "failed") must_== Okay(Failure("failed"))
        }

        // unlike JSON, nothing special about lists in boxes for Avro, so don't cover it

        "round trip nested boxes via Avro" in {
            avroRoundTrip(nestedCoder, Full(Full(Full("foo"))))
            avroRoundTrip(nestedCoder, Full(Full(Empty)))
            avroRoundTrip(nestedCoder, Full(Empty))
            avroRoundTrip(nestedCoder, Empty)

            avroRoundTrip(nestedCoder, Full(Full(Failure("failed"))))
            avroRoundTrip(nestedCoder, Full(Failure("failed")))
            avroRoundTrip(nestedCoder, Failure("failed"))
        }

        "honor hideFailures given at configuration time" in {
            BoxCoder(StringCoder, Some(true)).encode(cl, Failure("test")) must_== Okay(JNothing)
            BoxCoder(StringCoder, Some(false)).encode(cl, Failure("test")) must matchEncodedJson("failure" -> "test")
        }

        "honor hideFailures given at configuration time even if set otherwise at encode time" in {
            CoderSettings.hideFailures.set(true)
            BoxCoder(StringCoder, Some(true)).encode(cl, Failure("test")) must_== Okay(JNothing)
            BoxCoder(StringCoder, Some(false)).encode(cl, Failure("test")) must matchEncodedJson("failure" -> "test")
        }

        "honor hideFailures given at encode time" in {
            CoderSettings.hideFailures.set(true)
            BoxCoder(StringCoder).encode(cl, Failure("test")) must_== Okay(JNothing)
        }

        val paramFailureWithString = Failure("failed", Empty, Full(Failure("chained failure"))) ~> "additional param"
        val paramFailureWithStringJSON: JObject = (
            ("failure" -> "failed") ~
            ("chain" -> ("failure" -> "chained failure")) ~
            ("paramIsA" -> "java.lang.String") ~
            ("param" -> "additional param")
        )

        val paramFailureWithInt = Failure("failed", Empty, Full(Failure("chained failure"))) ~> 1234
        val paramFailureWithIntJSON: JObject = (
            ("failure" -> "failed") ~
            ("chain" -> ("failure" -> "chained failure")) ~
            ("paramIsA" -> "java.lang.Integer") ~
            ("param" -> 1234)
        )

        val paramFailureWithCaseClass = Failure("failed", Empty, Full(Failure("chained failure"))) ~> CaseClass(1, "foo", Some("bar"))
        val paramFailureWithCaseClassJSON: JObject = (
            ("failure" -> "failed") ~
            ("chain" -> ("failure" -> "chained failure")) ~
            ("paramIsA" -> "com.paytronix.utils.interchange.test.fixtures.CaseClass") ~
            ("param" -> (
                ("bar" -> "foo") ~ ("foo" -> 1) ~ ("zip" -> "bar")
            ))
        )

        "encode ParamFailures" in {
            testCoder.encode(cl, paramFailureWithString) must matchEncodedJson(paramFailureWithStringJSON)
            testCoder.encode(cl, paramFailureWithInt) must matchEncodedJson(paramFailureWithIntJSON)
            testCoder.encode(cl, paramFailureWithCaseClass) must matchEncodedJson(paramFailureWithCaseClassJSON)
        }

        "decode ParamFailures" in {
            testCoder.decode(cl, paramFailureWithStringJSON) must matchBox(paramFailureWithString)
            testCoder.decode(cl, paramFailureWithIntJSON) must matchBox(paramFailureWithInt)
            testCoder.decode(cl, paramFailureWithCaseClassJSON) must matchBox(paramFailureWithCaseClass)
        }

        "round trip ParamFailures" in {
            avroRoundTrip(testCoder, paramFailureWithString)
            avroRoundTrip(testCoder, paramFailureWithInt)
            avroRoundTrip(testCoder, paramFailureWithCaseClass)
        }

        "gracefully degrade when encoding complicated ParamFailures" in {
            log.warn("The following warning about decoding an incoming ParamFailure is expected:")
            testCoder.encode(cl, Failure("failed") ~> ("foo", 1)) must matchEncodedJson("failure" -> "failed")
        }

        "gracefully degrade when encoding complicated ParamFailures via Avro" in {
            log.warn("The following warning about decoding an incoming ParamFailure is expected:")
            avroRoundTrip(testCoder, Failure("failed") ~> ("foo", 1), (result: Result[Box[String]]) => {
                result must verify (_.isDefined)
                result.orThrow must_== Failure("failed")
                ()
            })
        }

        "encode Units" in {
            testUnitCoder.encode(cl, Full(())) must matchEncodedJson(JArray(JNothing :: Nil))
            testUnitCoder.encode(cl, Empty) must matchEncodedJson(JNothing)
        }

        "decode Units" in {
            testUnitCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Full(()))
            testUnitCoder.decode(cl, JArray(Nil)) must_== Okay(Full(()))
            testUnitCoder.decode(cl, JNothing) must_== Okay(Empty)
            testUnitCoder.decode(cl, JNull) must_== Okay(Empty)
        }

        "round trip Units" in {
            testUnitCoder.encode(cl, Full(())).flatMap(testUnitCoder.decode(cl, _)) must_== Okay(Full(()))

            avroRoundTrip(testUnitCoder, Full(()))
            avroRoundTrip(testUnitCoder, Empty)
            avroRoundTrip(testUnitCoder, paramFailureWithString)
        }

    }

    "Coder detection" should {
        "work for Box" in {
            new BoxStringTV().coder must_== Okay(BoxCoder(StringCoder))
            new BoxUnitTV().coder   must_== Okay(BoxCoder(UnitCoder))
        }
    }
}
