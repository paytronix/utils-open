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

import java.io.StringWriter

import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator, JsonParser, JsonToken}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.SpecificationWithJUnit
import org.specs2.execute.{Result => SpecsResult}
import org.specs2.matcher.Matcher

import com.paytronix.utils.interchange.base.{CoderFailure, CoderResult, Receiver, formatFailedPath}
import com.paytronix.utils.scala.result.{FailedG, Result, ResultG, unless}

import Arbitrary.arbitrary

object arbitraries {
    val nonnumericStr = Gen.frequency (
        (1, ""),
        (5, Gen.alphaStr),
        (5, Arbitrary.arbString.arbitrary.filter(s => !s.forall(Character.isDigit)))
    )

    val safeJavaBigDecimals = arbitrary[BigDecimal].map(_.bigDecimal).filter { bd =>
        try { new java.math.BigDecimal(bd.toString); true }
        catch { case nfe: NumberFormatException => false }
    }

    val safeScalaBigDecimals = arbitrary[BigDecimal].filter { bd =>
        try { new java.math.BigDecimal(bd.bigDecimal.toString); true }
        catch { case nfe: NumberFormatException => false }
    }

    implicit val arbJavaMathBigDecimals = Arbitrary(arbitrary[BigDecimal].map(_.bigDecimal))

    implicit val arbJavaMathBigInteger = Arbitrary(arbitrary[BigInt].map(_.bigInteger))
}

trait JsonMatchers { self: SpecificationWithJUnit =>
    def withParser[A](in: String)(f: InterchangeJsonParser => A): A = {
        val jp = new JsonFactory().createParser(in)
        val ijp = new InterchangeJsonParser(jp)
        f(ijp)
    }

    def withGenerator[A](f: InterchangeJsonGenerator => A): (A, String) = {
        val sw = new StringWriter
        val jg = new JsonFactory().createGenerator(sw)
        val ijg = new InterchangeJsonGenerator(jg)
        val res = f(ijg)
        jg.close()
        (res, sw.toString)
    }

    def encodeField[A](encoder: JsonEncoder[A], value: A): CoderResult[String] = {
        val (result, string) = withGenerator { ijg =>
            ijg.writeFieldName("field")
            encoder.run(value, ijg)
        }
        result.map { _ => string }
    }

    def decodeMissing[A](decoder: JsonDecoder[A]): CoderResult[A] =
        withParser("") { ijp =>
            ijp.currentValueIsMissing()
            val rec = new Receiver[A]
            decoder.run(ijp, rec) map { _ => rec.value }
        }

    def decode[A](decoder: JsonDecoder[A])(s: String): CoderResult[A] =
        withParser(s"[11111,$s,22222]") { ijp =>
            val rec = new Receiver[A]

            for {
                _ <- ijp.advanceTokenUnguarded()
                _ <- unless(ijp.currentToken == JsonToken.START_ARRAY)(FailedG("expected START_ARRAY not " + ijp.currentToken, CoderFailure.terminal) )
                _ <- ijp.advanceTokenUnguarded()
                _ <- unless(ijp.currentToken == JsonToken.VALUE_NUMBER_INT)(FailedG("expected VALUE_NUMBER_INT not " + ijp.currentToken, CoderFailure.terminal) )
                _ <- unless(ijp.intValue ==== 11111)(FailedG("expected number 11111 not " + ijp.intValue, CoderFailure.terminal) )

                _ <- ijp.advanceToken()
                _ <- decoder.run(ijp, rec)

                _ <- ijp.advanceTokenUnguarded()
                _ <- unless(ijp.currentToken == JsonToken.VALUE_NUMBER_INT)(FailedG("expected VALUE_NUMBER_INT not " + ijp.currentToken, CoderFailure.terminal) )
                _ <- unless(ijp.intValue ==== 22222)(FailedG("expected number 22222 not " + ijp.intValue, CoderFailure.terminal) )
                _ <- ijp.advanceTokenUnguarded()
                _ <- unless(ijp.currentToken == JsonToken.END_ARRAY)(FailedG("expected END_ARRAY not " + ijp.currentToken, CoderFailure.terminal) )
            } yield rec.value
        }

    def checkMissing[A](decoder: JsonDecoder[A]): SpecsResult =
        (decoder.fromString("null") must beMissingValue).updateMessage("explicit null: " + _) and
        (formatFailedPath(decodeMissing(decoder)) must beMissingValue).updateMessage("missing value: " + _) and
        (withParser("") { ijp =>
            val rec = new Receiver[A]
            ijp.advanceToken()
            decoder.run(ijp, rec) must beLike { case f@FailedG(_, _) =>
                f.message must beMatching("expected .*? but instead got EOF")
            }
        }).updateMessage("exhausted input: " + _)

    def beMissingValue[E, A]: Matcher[ResultG[E, A]] =
        beLike { case f@FailedG(_, _) =>
            f.message must beMatching("At source location \\d+:\\d+: required but missing")
        }

    def encodeString(s: String): String = {
        val sw = new StringWriter
        val jg = new JsonFactory().createGenerator(sw)
        jg.writeString(s)
        jg.close()
        sw.toString
    }
}
