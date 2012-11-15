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

import net.liftweb.json.JsonAST.{JNothing, JValue, render}
import net.liftweb.json.Printer.compact
import org.specs2.SpecificationFeatures
import org.specs2.execute.{Result => SpecsResult}
import org.specs2.matcher.Matcher
import org.specs2.specification.Around
import com.paytronix.utils.extendedreflection.Builder
import com.paytronix.utils.interchange.{Coder, CoderSettings, Coding, ComposableCoder}
import com.paytronix.utils.scala.result.{FailedG, Okay, Result, ResultG, optionOps}

object Helpers extends SpecificationFeatures {
    implicit val builder = new Builder(getClass.getClassLoader)

    trait TestValue[T] {
        val test: T

        def coder: Result[ComposableCoder[_]] =
            for {
                classR <- builder.classRFor(getClass)
                propR <- classR.properties.find(_.name == "test").toResult
                val typeR = propR.typeR
                coder <- Coding.forTypeComposable(getClass.getClassLoader, typeR)
            } yield coder

        if (getClass.getName contains "$anonfun")
            sys.error("extended reflection requires named classes, not anonymous ones, due to signature information being elided for those classes")
    }

    def printJSON(in: JValue): String = in match {
        case JNothing => "<nothing>"
        case other => compact(render(in))
    }
    def printJSON(in: ResultG[_, JValue]): String = in match {
        case Okay(jv) => "Okay(" + printJSON(jv) + ")"
        case other => other.toString
    }

    def matchEncodedJson(expected: JValue): Matcher[ResultG[_, JValue]] =
        (actualResult: ResultG[_, JValue]) =>
            if (actualResult.map(_ == expected) getOrElse false)
                ok
            else {
                val actualResultJSON = printJSON(actualResult)
                val expectedJSON = "Okay(" + printJSON(expected) + ")"
                if (actualResultJSON == expectedJSON)
                    ko(actualResult + " does not match Okay(" + expected + ")")
                else
                    ko(actualResultJSON + " does not match " + expectedJSON)
            }

    def avroRoundTrip[T](inst: ComposableCoder[T], value: T): SpecsResult =
        avroRoundTrip(Coder(getClass.getClassLoader, inst), value)

    def avroRoundTrip[T](inst: Coder[T], value: T): SpecsResult =
        avroRoundTripExpect[T](inst, value) {
            beLike {
                case Okay(null) if value == null => ok
                case Okay(v) => v must_== value
                case FailedG(throwable, _) =>
                    throwable.printStackTrace()
                    ko("decoding " + value + " from Avro failed (see log)")
            }
        }

    def avroRoundTripExpect[T](inst: ComposableCoder[T], value: T)(matcher: Matcher[Result[T]]): SpecsResult =
        avroRoundTripExpect(Coder(getClass.getClassLoader, inst), value)(matcher)

    def avroRoundTripExpect[T](inst: Coder[T], value: T)(matcher: Matcher[Result[T]]): SpecsResult =
        inst.encodeAvro(value) must beLike {
            case Okay(encoded) =>
                inst.decodeAvro(inst.avroSchema, encoded) must matcher
            case FailedG(throwable, _) =>
                throwable.printStackTrace()
                ko("encoding " + value + " to avro failed due to " + throwable.toString + " (see log)")
        }

    object resetCoding extends Around {
        def around[T <% SpecsResult](t: => T) = {
            Coding.reset
            val r = t
            Coding.reset
            r
        }
    }

    object resetCoderSettings extends Around {
        def around[T <% SpecsResult](t: => T) = {
            CoderSettings.reset
            val r = t
            CoderSettings.reset
            r
        }
    }
}
