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
import org.specs.matcher.Matcher
import com.paytronix.utils.extendedreflection.Builder
import com.paytronix.utils.interchange.{Coding, ComposableCoder}
import com.paytronix.utils.scala.result.{Okay, Result, ResultG, optionOps}

object Helpers {
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

    def json(in: JValue): String = in match {
        case JNothing => "<nothing>"
        case other => compact(render(in))
    }
    def json(in: ResultG[_, JValue]): String = in match {
        case Okay(jv) => "Okay(" + json(jv) + ")"
        case other => other.toString
    }

    case class matchEncodedJson(expected: JValue) extends Matcher[ResultG[_, JValue]] {
        def apply(actualResult: => ResultG[_, JValue]) =
            if (actualResult.map(_ == expected) getOrElse false)
                (true, "ok", "not ok")
            else {
                val actualResultJSON = json(actualResult)
                val expectedJSON = "Okay(" + json(expected) + ")"
                if (actualResultJSON == expectedJSON)
                    (false, "ok", actualResult + " does not match Okay(" + expected + ")")
                else
                    (false, "ok", actualResultJSON + " does not match " + expectedJSON)
            }
    }
}
