//
// Copyright 2014 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange.format.json

import scala.collection.JavaConverters.{asJavaCollectionConverter, asScalaBufferConverter, mapAsJavaMapConverter, mapAsScalaMapConverter}

import com.fasterxml.jackson.core.JsonToken
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}
import org.specs2.matcher.{Matcher, MatchResult}

import com.paytronix.utils.interchange.base.{CoderFailure, InsecureContext, Receiver}
import com.paytronix.utils.interchange.format.string
import com.paytronix.utils.scala.result.{FailedException, Failed, FailedG, FailedParameterDefault, Okay, Result, ResultG, unless}

import Arbitrary.arbitrary
import container.ResultGHideFailures

object TestObjectFixture {
    object ResultFieldBehavior extends Enumeration {
        val NoResultField, ResultFieldSuccess, ResultFieldBaz = Value
    }

    final case class TestObj(a: Int, b: Float) {
        def json(result: ResultFieldBehavior.Value) = {
            import ResultFieldBehavior._
            val resultField = result match {
                case ResultFieldSuccess => """ ,"result":"success" """.trim
                case ResultFieldBaz     => """ ,"result":"baz" """.trim
                case _                  => ""
            }
            s"""{"a":$a,"b":$b$resultField}"""
        }
    }

    implicit val arbTestObj: Arbitrary[TestObj] =
        Arbitrary {
            for {
                a <- arbitrary[Int]
                b <- arbitrary[Float]
            } yield TestObj(a, b)
        }

    import ResultFieldBehavior._

    implicit val implicitTestObjCoder = objectCoder(NoResultField)

    def objectCoder(resultField: ResultFieldBehavior.Value): JsonCoder[TestObj] = JsonCoder.make (
        new JsonEncoder[TestObj] {
            val mightBeNull = false
            val codesAsObject = true
            def run(in: TestObj, out: InterchangeJsonGenerator) =
                out.writeStartObject() >>
                out.writeFieldName("a") >> out.writeNumber(in.a) >>
                out.writeFieldName("b") >> out.writeNumber(in.b) >>
                {
                    if (resultField == ResultFieldBaz)
                        out.writeFieldName("result") >> out.writeString("baz")
                    else
                        Okay.unit
                } >>
                out.writeEndObject()
        },
        new JsonDecoder[TestObj] {
            val mightBeNull = false
            val codesAsObject = true
            def run(in: InterchangeJsonParser, out: Receiver[TestObj]) = {
                var a: Int = 0
                var b: Float = 0.0f

                in.require(JsonToken.START_OBJECT) >>
                in.advanceTokenUnguarded() >>
                in.require(JsonToken.FIELD_NAME) >> unless(in.fieldName == "a")(in.unexpectedToken("field a")) >>
                in.advanceTokenUnguarded() >>
                in.require(JsonToken.VALUE_NUMBER_INT) >> { a = in.intValue; Okay.unit } >>
                in.advanceTokenUnguarded() >>
                in.require(JsonToken.FIELD_NAME) >> unless(in.fieldName == "b")(in.unexpectedToken("field b")) >>
                in.advanceTokenUnguarded() >>
                in.require(JsonToken.VALUE_NUMBER_FLOAT) >> { b = in.floatValue; Okay.unit } >>
                {
                    resultField match {
                        case NoResultField => Okay.unit
                        case ResultFieldSuccess =>
                            in.advanceTokenUnguarded() >>
                            in.require(JsonToken.FIELD_NAME) >> unless(in.fieldName == "result")(in.unexpectedToken("field result")) >>
                            in.advanceTokenUnguarded() >>
                            in.require(JsonToken.VALUE_STRING) >> unless(in.stringValue == "success")(in.unexpectedToken("field result to equal success"))
                        case ResultFieldBehavior.ResultFieldBaz =>
                            in.advanceTokenUnguarded() >>
                            in.require(JsonToken.FIELD_NAME) >> unless(in.fieldName == "result")(in.unexpectedToken("field result")) >>
                            in.advanceTokenUnguarded() >>
                            in.require(JsonToken.VALUE_STRING) >> unless(in.stringValue == "baz")(in.unexpectedToken("field result to equal baz"))
                    }
                } >>
                in.advanceTokenUnguarded() >>
                in.require(JsonToken.END_OBJECT) >>
                out(TestObj(a, b))
            }
        }
    )
}

class nullableJsonCoderTest extends SpecificationWithJUnit with JsonMatchers {
    def is = s2"""
        `nullableJsonCoder`
            should encode to null when null in a scalar $encodeNullCase
            should encode to nothing when null in a field $encodeNothingCase
            should encode to non-null when non-null $encodeNonNullCase
            should decode from null $decodeNullCase
            should decode from nothing $decodeNothingCase
            should decode from non-null $decodeNonNullCase
    """

    val coder = container.nullableJsonCoder(scalar.stringJsonCoder)

    def encodeNullCase = coder.encode.toString(null) ==== Okay("null")
    def encodeNothingCase = encodeField(coder.encode, null) ==== Okay("")
    def encodeNonNullCase = coder.encode.toString("foo") ==== Okay("\"foo\"")
    def decodeNullCase = decode(coder.decode)("null") ==== Okay(null)
    def decodeNothingCase = decodeMissing(coder.decode) ==== Okay(null)
    def decodeNonNullCase = decode(coder.decode)("\"foo\"") ==== Okay("foo")
}

class optionJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `optionJsonCoder`
            should encode to null when None in a scalar $encodeNullCase
            should encode to nothing when None in a field $encodeNothingCase
            should encode to non-null when Some $encodeNonNullCase
            should decode from null $decodeNullCase
            should decode from nothing $decodeNothingCase
            should decode from non-null $decodeNonNullCase
            should be the implicit coder for Option[…] $implicitCheckCase
        nested `optionJsonCoder`
            should encode to null when None in a scalar $encodeNestedNullCase
            should encode to nothing when None in a field $encodeNestedNothingCase
            should encode to [] when Some(None) $encodeNestedEmptyArrayCase
            should encode to [a] when Some $encodeNestedNonNullCase
            should decode from null $decodeNestedNullCase
            should decode from nothing $decodeNestedNothingCase
            should decode from [] $decodeNestedEmptyArrayCase
            should decode from [null] $decodeNestedArrayWithNullCase
            should decode from [a] $decodeNestedNonNullCase
            should not decode from [a,b] $decodeNestedTwoElementArrayCase
            should round trip Option[Option[String]] $roundTripOptOptStringCase
            should round trip Option[Option[Unit]] $roundTripOptOptUnitCase
            should round trip Option[Option[String >: Null]] $roundTripOptOptNullableStringCase
    """

    def opt[A](c: JsonCoder[A]) = container.optionJsonCoder[A](c)
    val str = scalar.stringJsonCoder

    val coder = opt(str)
    val optOptStringCoder = opt(opt(str))
    val optOptUnitCoder = opt(opt(scalar.unitJsonCoder))
    val optOptNullableStringCoder = opt(opt(container.nullableJsonCoder(str)))

    def encodeNullCase = coder.encode.toString(None) ==== Okay("null")
    def encodeNothingCase = encodeField(coder.encode, None) ==== Okay("")
    def encodeNonNullCase = coder.encode.toString(Some("foo")) ==== Okay("\"foo\"")
    def decodeNullCase = decode(coder.decode)("null") ==== Okay(None)
    def decodeNothingCase =  decodeMissing(coder.decode) ==== Okay(None)
    def decodeNonNullCase = decode(coder.decode)("\"foo\"") ==== Okay(Some("foo"))
    def implicitCheckCase = { import coders._; JsonCoder[Option[String]].encode.getClass must_== coder.encode.getClass }
    def encodeNestedNullCase = optOptStringCoder.encode.toString(None) ==== Okay("null")
    def encodeNestedNothingCase = encodeField(optOptStringCoder.encode, None) ==== Okay("")
    def encodeNestedEmptyArrayCase = optOptStringCoder.encode.toString(Some(None)) ==== Okay("[]")
    def encodeNestedNonNullCase = optOptStringCoder.encode.toString(Some(Some("foo"))) ==== Okay("[\"foo\"]")
    def decodeNestedNullCase = decode(optOptStringCoder.decode)("null") ==== Okay(None)
    def decodeNestedNothingCase = decodeMissing(optOptStringCoder.decode) ==== Okay(None)
    def decodeNestedEmptyArrayCase = decode(optOptStringCoder.decode)("[]") ==== Okay(Some(None))
    def decodeNestedArrayWithNullCase = decode(optOptStringCoder.decode)("[null]") ==== Okay(Some(None))
    def decodeNestedNonNullCase = decode(optOptStringCoder.decode)("[\"foo\"]") ==== Okay(Some(Some("foo")))
    def decodeNestedTwoElementArrayCase = decode(optOptStringCoder.decode)("[\"foo\", \"bar\"]") must beLike { case FailedG(_, _) => ok }
    def roundTripOptOptStringCase = prop { (oos: Option[Option[String]]) =>
        val res = optOptStringCoder.encode.toString(oos)
        ((res >>= decode(optOptStringCoder.decode)) ==== Okay(oos)).updateMessage(s"failed with encode result $res: " + _)
    }
    def roundTripOptOptUnitCase = prop { (oou: Option[Option[Unit]]) =>
        val res = optOptUnitCoder.encode.toString(oou)
        ((res >>= decode(optOptUnitCoder.decode)) ==== Okay(oou)).updateMessage(s"failed with encode result $res: " + _)
    }
    def roundTripOptOptNullableStringCase = Prop.forAll(arbitrary[Option[Option[Option[String]]]].map {
        _.map { _.map { _.orNull } }
    }) { oons =>
        val res = optOptNullableStringCoder.encode.toString(oons)
        ((res >>= decode(optOptNullableStringCoder.decode)) ==== Okay(oons)).updateMessage(s"failed with encode result $res: " + _)
    }
}

class eitherJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `eitherJsonCoder`
            should encode left correctly $encodeLeftCase
            should encode right correctly $encodeRightCase
            should decode left correctly $decodeLeftCase
            should decode right correctly $decodeRightCase
            should ignore other fields before and after $decodeIgnoreOtherCase
            should fail to decode where multiple left or right fields present $decodeFailConflictingCase
            should fail to decode where neither left nor right field present $decodeFailMissingCase
            should fail to decode a missing value $decodeMissingCase
    """

    val coder = container.eitherJsonCoder("l", scalar.intJsonCoder, "r", scalar.floatJsonCoder)

    def encodeLeftCase = prop { (i: Int) =>
        coder.encode.toString(Left(i)) ==== Okay("{\"l\":" + i + "}")
    }
    def encodeRightCase = prop { (f: Float) =>
        coder.encode.toString(Right(f)) ==== Okay("{\"r\":" + f + "}")
    }
    def decodeLeftCase = prop { (i: Int) =>
        decode(coder.decode)("{\"l\": " + i + "}") ==== Okay(Left(i))
    }
    def decodeRightCase = prop { (f: Float) =>
        decode(coder.decode)("{\"r\": " + f + "}") ==== Okay(Right(f))
    }
    def decodeIgnoreOtherCase =
        decode(coder.decode)("{\"o1\": [1,2,3], \"l\": 1, \"o2\": {\"a\": 2}}") ==== Okay(Left(1))
    def decodeFailConflictingCase =
        decode(coder.decode)("{\"l\": 1, \"l\": 2}") must beLike { case FailedG(_, _) => ok }
    def decodeFailMissingCase =
        decode(coder.decode)("{\"x\": 1, \"y\": 2}") must beLike { case FailedG(_, _) => ok }
    def decodeMissingCase =
        checkMissing(coder.decode)
}

object ResultGJsonCoderTestFixture {
    implicit def arbResult[E: Arbitrary, A: Arbitrary]: Arbitrary[ResultG[E, A]] =
        Arbitrary {
            arbitrary[Boolean] flatMap {
                case true => arbitrary[A].map(Okay.apply)
                case false => arbitrary[E].map(FailedG("test", _))
            }
        }

    class TestException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
}

class resultGJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import ResultGJsonCoderTestFixture._
    import TestObjectFixture._
    import ResultFieldBehavior._

    def is = s2"""
        `resultGJsonCoder` when `Okay` value can be null
            should encode Okay(Some) as [a]                                          $encodeNullableOkayCase
            should encode Okay(None) as []                                           $encodeNullableOkayNullCase
            should encode FailedG correctly                                          $encodeNullableFailedCase
            should encode FailedG with a cause correctly                             $encodeNullableFailedWithCauseCase
            should encode FailedG without throwable in insecure mode                 $encodeNullableFailedInsecureCase
            should encode FailedG to null when hiding failures                       $encodeNullableHideFailuresCase
            should decode [a] as Okay                                                $decodeNullableOkayCase
            should fail to decode [a, b]                                             $decodeNullableTooManyElementsCase
            should decode [] as Okay                                                 $decodeNullableEmptyArrayCase
            should decode {"result":"failed",…} using the throwable if secure        $decodeNullableFailedThrowableCase
            should decode {"result":"failed",…} with a cause                         $decodeNullableFailedThrowableWithCauseCase
            should decode {"result":"failed",…} using the message if insecure        $decodeNullableFailedMessageCase
            should decode {"result":"failed",…} to unknown cause if hiding failures  $decodeNullableHideFailuresCase
            should round trip various values                                         $encodeNullableRoundTripCase

        `resultGJsonCoder` when `Okay` value cannot be null and is an object
            should encode Okay as {…,"result":"success"}                             $encodeObjectOkayCase
            should encode Okay as {…} and not overwrite result                       $encodeObjectOkayWithResultCase
            should encode FailedG correctly                                          $encodeObjectFailedCase
            should encode FailedG with a cause correctly                             $encodeObjectFailedWithCauseCase
            should encode FailedG without throwable in insecure mode                 $encodeObjectFailedInsecureCase
            should encode FailedG to null when hiding failures                       $encodeObjectHideFailuresCase
            should decode {a} as Okay                                                $decodeObjectOkayCase
            should decode {"result":"failed",…} using the throwable if secure        $decodeObjectFailedThrowableCase
            should decode {"result":"failed",…} with a cause                         $decodeObjectFailedThrowableWithCauseCase
            should decode {"result":"failed",…} using the message if insecure        $decodeObjectFailedMessageCase
            should decode {"result":"failed",…} to unknown cause if hiding failures  $decodeObjectHideFailuresCase
            should round trip various values                                         $encodeObjectRoundTripCase

        `resultGJsonCoder` when `Okay` value can be null
            should encode Okay as a                                                  $encodeNonObjectOkayCase
            should encode FailedG correctly                                          $encodeNonObjectFailedCase
            should encode FailedG with a cause correctly                             $encodeNonObjectFailedWithCauseCase
            should encode FailedG without throwable in insecure mode                 $encodeNonObjectFailedInsecureCase
            should encode FailedG to null when hiding failures                       $encodeNonObjectHideFailuresCase
            should decode a as Okay                                                  $decodeNonObjectOkayCase
            should decode {"result":"failed",…} using the throwable if secure        $decodeNonObjectFailedThrowableCase
            should decode {"result":"failed",…} with a cause                         $decodeNonObjectFailedThrowableWithCauseCase
            should decode {"result":"failed",…} using the message if insecure        $decodeNonObjectFailedMessageCase
            should decode {"result":"failed",…} to unknown cause if hiding failures  $decodeNonObjectHideFailuresCase
            should round trip various values                                         $encodeNonObjectRoundTripCase

        `resultGJsonCoder` when `Failed` parameter type might be null
            should decode no parameter to Unit                                       $encodeFailedGWithUnitParamCase
            should encode Unit to no parameter                                       $decodeFailedGWithUnitParamCase
            should decode no parameter to None                                       $decodeFailedGWithNoneParamCase
            should encode None to no parameter                                       $encodeFailedGWithNoneParamCase
            should decode parameter to Some                                          $decodeFailedGWithSomeParamCase
            should encode Some to a parameter                                        $encodeFailedGWithSomeParamCase

        `resultGJsonCoder`
            must be the implicit coder for `ResultG` $implicitCase
    """

    val objectJSON = TestObj(123, 45.67f).json(NoResultField)
    val objectWithResultSuccessJSON = TestObj(123, 45.67f).json(ResultFieldSuccess)
    val objectWithResultBazJSON = TestObj(123, 45.67f).json(ResultFieldBaz)

    val failedUnit: Failed = FailedG(new TestException("test"), ())
    val failedNone: FailedG[Option[String]] = FailedG(new TestException("test"), None)
    val failedSome: FailedG[Option[String]] = FailedG(new TestException("test"), Some("testParam"))
    val failed: FailedG[Int] = FailedG(new TestException("test"), 123)
    val failedWithCause: FailedG[Int] = FailedG(new TestException("test", new TestException("test2")), 123)

    def beEqualRG[E, A](expected: ResultG[E, A]): Matcher[ResultG[E, A]] =
        expected match {
            case Okay(a) =>
                beLike { case Okay(a2) => a ==== a2 }
            case FailedG(t, p) =>
                beLike { case FailedG(t2, p2) =>
                    def eqishThrowable(t: Throwable, t2: Throwable): MatchResult[Any] =
                        (t.getClass must_== t2.getClass) and
                        (t.getMessage ==== t2.getMessage) and
                        {
                            t.getCause match {
                                case null  => t2.getCause must beLike { case null => ok }
                                case cause => t2.getCause must beLike { case null => ko; case cause2 => eqishThrowable(cause, cause2) }
                            }
                        }
                    eqishThrowable(t, t2) and (p ==== p2)
                }
        }

    def beSecureFailed[A]: Matcher[ResultG[Int, A]] = beEqualRG(failed)
    def beFailedWithCause[A]: Matcher[ResultG[Int, A]] = beEqualRG(failedWithCause)
    def beInsecureFailed[A]: Matcher[ResultG[Int, A]] = beEqualRG(FailedG("test", 123))
    def beMissingFailed[A]: Matcher[ResultG[Int, A]] = beEqualRG(FailedG("unknown cause", 321))

    val failedExcName = "com.paytronix.utils.interchange.format.json.ResultGJsonCoderTestFixture$TestException"
    val failedJSONCommon = """ "errorCode":"system.error","errorMessage":"test","result":"failed","param":123 """.trim
    val testThrowableJSON = s""" "isA":"$failedExcName","message":"test" """.trim
    val failedJSONSecure = s""" {$failedJSONCommon,"throwable":{$testThrowableJSON}} """.trim
    val failedJSONInsecure = s""" {$failedJSONCommon} """.trim
    val failedJSONWithCause = s""" {$failedJSONCommon,"throwable":{$testThrowableJSON,"cause":{"isA":"$failedExcName","message":"test2"}}} """.trim
    val failedNoParamJSON = s""" {"errorCode":"system.error","errorMessage":"test","result":"failed","throwable":{$testThrowableJSON}} """.trim
    val failedStringParamJSON = s""" {"errorCode":"system.error","errorMessage":"test","result":"failed","param":"testParam","throwable":{$testThrowableJSON}} """.trim

    implicit val fpdInt = FailedParameterDefault.value(321)

    val nullableRGC = container.resultGJsonCoder(scalar.intJsonCoder, container.optionJsonCoder(objectCoder(NoResultField)))

    def encodeNullableOkayCase =
        nullableRGC.encode.toString(Okay(Some(TestObj(123, 45.67f)))) ==== Okay(s"""[$objectJSON]""")
    def encodeNullableOkayNullCase =
        nullableRGC.encode.toString(Okay(None)) ==== Okay("[]")
    def encodeNullableFailedCase =
        nullableRGC.encode.toString(failed) ==== Okay(failedJSONSecure)
    def encodeNullableFailedWithCauseCase =
        nullableRGC.encode.toString(failedWithCause) ==== Okay(failedJSONWithCause)
    def encodeNullableFailedInsecureCase =
        InsecureContext.doWith(true) { nullableRGC.encode.toString(failed) ==== Okay(failedJSONInsecure) }
    def encodeNullableHideFailuresCase =
        ResultGHideFailures.doWith(true) { nullableRGC.encode.toString(failed) ==== Okay("null") }
    def decodeNullableOkayCase =
        decode(nullableRGC.decode)(s"[$objectJSON]") ==== Okay(Okay(Some(TestObj(123, 45.67f))))
    def decodeNullableTooManyElementsCase =
        decode(nullableRGC.decode)(s"[$objectWithResultSuccessJSON,$objectWithResultSuccessJSON]") must beLike { case FailedG(_, _) => ok }
    def decodeNullableEmptyArrayCase =
        decode(nullableRGC.decode)(s"[]") ==== Okay(Okay(None))
    def decodeNullableFailedThrowableCase =
        decode(nullableRGC.decode)(failedJSONSecure) must beLike { case Okay(rg) => rg must beSecureFailed }
    def decodeNullableFailedThrowableWithCauseCase =
        decode(nullableRGC.decode)(failedJSONWithCause) must beLike { case Okay(rg) => rg must beFailedWithCause }
    def decodeNullableFailedMessageCase =
        InsecureContext.doWith(true) {
            decode(nullableRGC.decode)(failedJSONSecure) must beLike { case Okay(rg) => rg must beInsecureFailed }
        }
    def decodeNullableHideFailuresCase =
        ResultGHideFailures.doWith(true) {
            decode(nullableRGC.decode)(failedJSONSecure) must beLike { case Okay(rg) => rg must beMissingFailed }
        }
    def encodeNullableRoundTripCase =
        prop { (rg: ResultG[Int, Option[TestObj]]) =>
            (nullableRGC.encode.toString(rg) >>= decode(nullableRGC.decode)) must beLike { case Okay(rg) => rg must beEqualRG(rg) }
        }


    val objectRGC = container.resultGJsonCoder(scalar.intJsonCoder, objectCoder(ResultFieldSuccess))
    val objectWithResultRGC = container.resultGJsonCoder(scalar.intJsonCoder, objectCoder(ResultFieldBaz))

    def encodeObjectOkayCase =
        objectRGC.encode.toString(Okay(TestObj(123, 45.67f))) ==== Okay(objectWithResultSuccessJSON)
    def encodeObjectOkayWithResultCase =
        objectWithResultRGC.encode.toString(Okay(TestObj(123, 45.67f))) ==== Okay(objectWithResultBazJSON)
    def encodeObjectFailedCase =
        objectRGC.encode.toString(failed) ==== Okay(failedJSONSecure)
    def encodeObjectFailedWithCauseCase =
        objectRGC.encode.toString(failedWithCause) ==== Okay(failedJSONWithCause)
    def encodeObjectFailedInsecureCase =
        InsecureContext.doWith(true) { objectRGC.encode.toString(failed) ==== Okay(failedJSONInsecure) }
    def encodeObjectHideFailuresCase =
        ResultGHideFailures.doWith(true) { objectRGC.encode.toString(failed) ==== Okay("null") }
    def decodeObjectOkayCase =
        decode(objectRGC.decode)(objectWithResultSuccessJSON) ==== Okay(Okay(TestObj(123, 45.67f)))
    def decodeObjectFailedThrowableCase =
        decode(objectRGC.decode)(failedJSONSecure) must beLike { case Okay(rg) => rg must beSecureFailed }
    def decodeObjectFailedThrowableWithCauseCase =
        decode(objectRGC.decode)(failedJSONWithCause) must beLike { case Okay(rg) => rg must beFailedWithCause }
    def decodeObjectFailedMessageCase =
        InsecureContext.doWith(true) {
            decode(objectRGC.decode)(failedJSONSecure) must beLike { case Okay(rg) => rg must beInsecureFailed }
        }
    def decodeObjectHideFailuresCase = ok
        ResultGHideFailures.doWith(true) {
            decode(objectRGC.decode)(failedJSONSecure) must beLike { case Okay(rg) => rg must beMissingFailed }
        }
    def encodeObjectRoundTripCase =
        prop { (rg: ResultG[Int, TestObj]) =>
            (objectRGC.encode.toString(rg) >>= decode(objectRGC.decode)) must beLike { case Okay(rg) => rg must beEqualRG(rg) }
        }

    val nonObjectRGC = container.resultGJsonCoder(scalar.intJsonCoder, scalar.doubleJsonCoder)

    def encodeNonObjectOkayCase =
        nonObjectRGC.encode.toString(Okay(1.5)) ==== Okay("1.5")
    def encodeNonObjectFailedCase =
        nonObjectRGC.encode.toString(failed) ==== Okay(failedJSONSecure)
    def encodeNonObjectFailedWithCauseCase =
        nonObjectRGC.encode.toString(failedWithCause) ==== Okay(failedJSONWithCause)
    def encodeNonObjectFailedInsecureCase =
        InsecureContext.doWith(true) { nonObjectRGC.encode.toString(failed) ==== Okay(failedJSONInsecure) }
    def encodeNonObjectHideFailuresCase =
        ResultGHideFailures.doWith(true) { nonObjectRGC.encode.toString(failed) ==== Okay("null") }
    def decodeNonObjectOkayCase =
        decode(nonObjectRGC.decode)("1.5") ==== Okay(Okay(1.5))
    def decodeNonObjectFailedThrowableCase =
        decode(nonObjectRGC.decode)(failedJSONSecure) must beLike { case Okay(rg) => rg must beSecureFailed }
    def decodeNonObjectFailedThrowableWithCauseCase =
        decode(nonObjectRGC.decode)(failedJSONWithCause) must beLike { case Okay(rg) => rg must beFailedWithCause }
    def decodeNonObjectFailedMessageCase =
        InsecureContext.doWith(true) {
            decode(nonObjectRGC.decode)(failedJSONSecure) must beLike { case Okay(rg) => rg must beInsecureFailed }
        }
    def decodeNonObjectHideFailuresCase = ok
        ResultGHideFailures.doWith(true) {
            decode(nonObjectRGC.decode)(failedJSONSecure) must beLike { case Okay(rg) => rg must beMissingFailed }
        }
    def encodeNonObjectRoundTripCase =
        prop { (rg: ResultG[Int, Double]) =>
            (nonObjectRGC.encode.toString(rg) >>= decode(nonObjectRGC.decode)) must beLike { case Okay(rg) => rg must beEqualRG(rg) }
        }

    val failedUnitRGC = container.resultGJsonCoder(scalar.unitJsonCoder, scalar.doubleJsonCoder)

    def decodeFailedGWithUnitParamCase =
        {
            decode(failedUnitRGC.decode)(failedNoParamJSON) must beLike { case Okay(rg) => rg must beEqualRG(failedUnit) }
        }
    def encodeFailedGWithUnitParamCase =
        failedUnitRGC.encode.toString(failedUnit) ==== Okay(failedNoParamJSON)

    val failedOptionRGC = container.resultGJsonCoder(container.optionJsonCoder(scalar.stringJsonCoder), scalar.doubleJsonCoder)

    def decodeFailedGWithNoneParamCase =
        {
            decode(failedOptionRGC.decode)(failedNoParamJSON) must beLike { case Okay(rg) => rg must beEqualRG(failedNone) }
        }
    def encodeFailedGWithNoneParamCase =
        failedOptionRGC.encode.toString(failedNone) ==== Okay(failedNoParamJSON)

    def decodeFailedGWithSomeParamCase =
        {
            decode(failedOptionRGC.decode)(failedStringParamJSON) must beLike { case Okay(rg) => rg must beEqualRG(failedSome) }
        }
    def encodeFailedGWithSomeParamCase =
        failedOptionRGC.encode.toString(failedSome) ==== Okay(failedStringParamJSON)

    def implicitCase = { import coders._; JsonCoder[ResultG[Int, Double]].encode.getClass must_== nonObjectRGC.encode.getClass }
}

class insecureJsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `insecureJsonCoder`
            should operate exactly like the underlying coder when in a secure context $secureCase
            should encode nothing in an insecure context $encodeInsecureCase
            should decode the substitute in an insecure context $decodeInsecureCase
    """

    val coder = container.insecureJsonCoder(scalar.intJsonCoder, 1)

    def secureCase = prop { (i: Int) => (coder.encode.toString(i) >>= decode(coder.decode)) ==== Okay(i) }
    def encodeInsecureCase = InsecureContext.doWith(true) { coder.encode.toString(123) ==== Okay("null") }
    def decodeInsecureCase =
        prop { (i: Int) => InsecureContext.doWith(true) { coder.decode.fromString(i.toString) } ==== Okay(1) } and
        { InsecureContext.doWith(true) { coder.decode.fromString(""" {"a": "b"} """) } ==== Okay(1) }
}

class scalaListCodingTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import TestObjectFixture._
    import ResultFieldBehavior._

    def is = s2"""
        Coding of List
            must encode lists correctly $encodeCase
            must decode lists correctly $decodeCase
            must round trip lists of objects $roundTripObjectsCase
            must round trip lists of lists $roundTripArraysCase
            must be the implicit coder for List[…] $implicitCase
    """

    val coder: JsonCoder[List[Int]] = container.jsonArrayCoder(scalar.intJsonCoder)
    val objCoder: JsonCoder[List[TestObj]] = container.jsonArrayCoder(objectCoder(NoResultField))
    val aryCoder: JsonCoder[List[List[Int]]] = container.jsonArrayCoder(coder)

    def encodeCase = prop { (l: List[Int]) => coder.encode.toString(l) ==== Okay(l.mkString("[", ",", "]")) }
    def decodeCase = prop { (l: List[Int]) => coder.decode.fromString(l.mkString("[", ",", "]")) ==== Okay(l) }
    def roundTripObjectsCase = prop { (l: List[TestObj]) => (objCoder.encode.toString(l) >>= objCoder.decode.fromString) ==== Okay(l) }
    def roundTripArraysCase = prop { (l: List[List[Int]]) => (aryCoder.encode.toString(l) >>= aryCoder.decode.fromString) ==== Okay(l) }

    def implicitCase = { import coders._; JsonCoder[List[Int]].encode.getClass must_== coder.encode.getClass }
}

class scalaSetCodingTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import TestObjectFixture._
    import ResultFieldBehavior._

    def is = s2"""
        Coding of Set
            must encode lists correctly $encodeCase
            must decode lists correctly $decodeCase
            must round trip lists of objects $roundTripObjectsCase
            must round trip lists of lists $roundTripArraysCase
            must be the implicit coder for Set[…] $implicitCase
    """

    val coder: JsonCoder[Set[Int]] = container.jsonArrayCoder(scalar.intJsonCoder)
    val objCoder: JsonCoder[Set[TestObj]] = container.jsonArrayCoder(objectCoder(NoResultField))
    val aryCoder: JsonCoder[Set[Set[Int]]] = container.jsonArrayCoder(coder)

    def encodeCase = prop { (s: Set[Int]) => coder.encode.toString(s) ==== Okay(s.mkString("[", ",", "]")) }
    def decodeCase = prop { (s: Set[Int]) => coder.decode.fromString(s.mkString("[", ",", "]")) ==== Okay(s) }
    def roundTripObjectsCase = prop { (s: Set[TestObj]) => (objCoder.encode.toString(s) >>= objCoder.decode.fromString) ==== Okay(s) }
    def roundTripArraysCase = prop { (s: Set[Set[Int]]) => (aryCoder.encode.toString(s) >>= aryCoder.decode.fromString) ==== Okay(s) }

    def implicitCase = { import coders._; JsonCoder[Set[Int]].encode.getClass must_== coder.encode.getClass }
}

class javaListCodingTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import TestObjectFixture._
    import ResultFieldBehavior._

    def is = s2"""
        Coding of java.util.List
            must encode lists correctly $encodeCase
            must decode lists correctly $decodeCase
            must round trip lists of objects $roundTripObjectsCase
            must round trip lists of lists $roundTripArraysCase
            must be the implicit coder for List[…] $implicitCase
    """

    def toIterable(l: java.util.List[Int]) = l.asScala
    implicit def arbJavaList[A: Arbitrary]: Arbitrary[java.util.List[A]] =
        Arbitrary(arbitrary[List[A]].map(l => new java.util.ArrayList(l.asJavaCollection)))

    val coder: JsonCoder[java.util.List[Int]] = container.javaListJsonCoder(scalar.intJsonCoder)
    val objCoder: JsonCoder[java.util.List[TestObj]] = container.javaListJsonCoder(objectCoder(NoResultField))
    val aryCoder: JsonCoder[java.util.List[java.util.List[Int]]] = container.javaListJsonCoder(coder)

    def encodeCase = prop { (l: java.util.List[Int]) => coder.encode.toString(l) ==== Okay(l.asScala.mkString("[", ",", "]")) }
    def decodeCase = prop { (l: java.util.List[Int]) => coder.decode.fromString(l.asScala.mkString("[", ",", "]")) ==== Okay(l) }
    def roundTripObjectsCase = prop { (l: java.util.List[TestObj]) => (objCoder.encode.toString(l) >>= objCoder.decode.fromString) ==== Okay(l) }
    def roundTripArraysCase = prop { (l: java.util.List[java.util.List[Int]]) => (aryCoder.encode.toString(l) >>= aryCoder.decode.fromString) ==== Okay(l) }

    def implicitCase = { import coders._; JsonCoder[java.util.List[Int]].encode.getClass must_== coder.encode.getClass }
}

class scalaMapAssocCodingTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import TestObjectFixture._
    import ResultFieldBehavior._

    def is = s2"""
        Coding of Map with a non-string key
            must encode lists correctly $encodeCase
            must decode lists correctly $decodeCase
            must be the implicit coder for Map[…] $implicitCase
    """

    val coder: JsonCoder[Map[TestObj, TestObj]] = container.jsonAssocArrayCoder(objectCoder(NoResultField), objectCoder(NoResultField))
    def toAssocJSON(kvp: (TestObj, TestObj)): String =
        s"""{"key":${kvp._1.json(NoResultField)},"value":${kvp._2.json(NoResultField)}}""".trim

    def encodeCase = prop { (m: Map[TestObj, TestObj]) => coder.encode.toString(m) ==== Okay(m.map(toAssocJSON).mkString("[", ",", "]")) }
    def decodeCase = prop { (m: Map[TestObj, TestObj]) => coder.decode.fromString(m.map(toAssocJSON).mkString("[", ",", "]")) ==== Okay(m) }

    def implicitCase = { import coders._; JsonCoder[Map[TestObj, TestObj]].encode.getClass must_== coder.encode.getClass }
}

class javaMapAssocCodingTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import TestObjectFixture._
    import ResultFieldBehavior._

    def is = s2"""
        Coding of java.util.Map with a non-string key
            must encode lists correctly $encodeCase
            must decode lists correctly $decodeCase
            must be the implicit coder for java.util.Map[…] $implicitCase
    """

    val coder: JsonCoder[java.util.Map[TestObj, TestObj]] = container.javaMapJsonCoder(objectCoder(NoResultField), objectCoder(NoResultField))
    implicit val arbJavaMap = Arbitrary(arbitrary[Map[TestObj, TestObj]].map(_.asJava))
    def toAssocJSON(kvp: (TestObj, TestObj)): String =
        s"""{"key":${kvp._1.json(NoResultField)},"value":${kvp._2.json(NoResultField)}}""".trim

    def encodeCase = prop { (m: java.util.Map[TestObj, TestObj]) => coder.encode.toString(m) ==== Okay(m.asScala.map(toAssocJSON).mkString("[", ",", "]")) }
    def decodeCase = prop { (m: java.util.Map[TestObj, TestObj]) => coder.decode.fromString(m.asScala.map(toAssocJSON).mkString("[", ",", "]")) ==== Okay(m) }

    def implicitCase = { import coders._; JsonCoder[java.util.Map[TestObj, TestObj]].encode.getClass must_== coder.encode.getClass }
}

class scalaMapStringCodingTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import TestObjectFixture._
    import ResultFieldBehavior._

    def is = s2"""
        Coding of Map with a string key
            must encode lists correctly $encodeCase
            must decode lists correctly $decodeCase
            must be the implicit coder for Map[…] $implicitCase
    """

    val coder: JsonCoder[Map[Int, TestObj]] = container.jsonObjectCoder(string.coders.intStringCoder, objectCoder(NoResultField))
    def toAssocJSON(kvp: (Int, TestObj)): String =
        s""" "${kvp._1}":${kvp._2.json(NoResultField)} """.trim

    def encodeCase = prop { (m: Map[Int, TestObj]) => coder.encode.toString(m) ==== Okay(m.map(toAssocJSON).mkString("{", ",", "}")) }
    def decodeCase = prop { (m: Map[Int, TestObj]) => coder.decode.fromString(m.map(toAssocJSON).mkString("{", ",", "}")) ==== Okay(m) }

    def implicitCase = { import coders._; import string.coders._; JsonCoder[Map[Int, TestObj]].encode.getClass must_== coder.encode.getClass }
}

class javaMapStringCodingTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    import TestObjectFixture._
    import ResultFieldBehavior._

    def is = s2"""
        Coding of java.util.Map with a string key
            must encode lists correctly $encodeCase
            must decode lists correctly $decodeCase
            must be the implicit coder for java.util.Map[…] $implicitCase
    """

    val coder: JsonCoder[java.util.Map[Int, TestObj]] = container.javaStringKeyedMapJsonCoder(string.coders.intStringCoder, objectCoder(NoResultField))
    implicit val arbJavaMap = Arbitrary(arbitrary[Map[Int, TestObj]].map(_.asJava))
    def toAssocJSON(kvp: (Int, TestObj)): String =
        s""" "${kvp._1}":${kvp._2.json(NoResultField)} """.trim

    def encodeCase = prop { (m: java.util.Map[Int, TestObj]) => coder.encode.toString(m) ==== Okay(m.asScala.map(toAssocJSON).mkString("{", ",", "}")) }
    def decodeCase = prop { (m: java.util.Map[Int, TestObj]) => coder.decode.fromString(m.asScala.map(toAssocJSON).mkString("{", ",", "}")) ==== Okay(m) }

    def implicitCase = { import coders._; import string.coders._; JsonCoder[java.util.Map[Int, TestObj]].encode.getClass must_== coder.encode.getClass }
}
