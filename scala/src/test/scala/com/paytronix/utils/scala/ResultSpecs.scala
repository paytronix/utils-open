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

package com.paytronix.utils.scala

import org.specs._
import org.specs.matcher.Matcher
import org.specs.runner.JUnit4

import result.{Failed, FailedG, Okay, Result, ResultG, catching, catchingException, catchingExceptions, eitherOps, eitherOpsG, optionOps}

class ResultTestSpecsAsTest extends JUnit4(ResultTestSpecs)

object ResultTestSpecs extends Specification {
    case class beFailedWith[E](message: String, parameter: E) extends Matcher[ResultG[E, _]] {
        def apply(actual: => ResultG[E, _]) = {
            val failedMessage = PartialFunction.condOpt(actual) {
                case FailedG(throwable, actualParameter) if message != throwable.getMessage && parameter != actualParameter =>
                    "expected message \"" + message + "\" does not match actual message \"" + throwable.getMessage + "\" and" +
                    " expected parameter \"" + String.valueOf(parameter) + "\" does not match actual parameter \"" + String.valueOf(actualParameter) + "\""

                case FailedG(throwable, _) if message != throwable.getMessage =>
                    "expected message \"" + message + "\" does not match actual message \"" + throwable.getMessage + "\""

                case FailedG(_, actualParameter) if parameter != actualParameter =>
                    "expected parameter \"" + String.valueOf(parameter) + "\" does not match actual parameter \"" + String.valueOf(actualParameter) + "\""

                case Okay(_) =>
                    "expected failure with message \"" + message + "\" and parameter \"" + String.valueOf(parameter) + "\", but got success"
            }

            (!failedMessage.isDefined, "ok", failedMessage getOrElse "")
        }
    }

    object beFailedWith {
        def apply(message: String): beFailedWith[Unit] = beFailedWith(message, ())
    }

    case class beFailedWithCause(message: String) extends Matcher[ResultG[_, _]] {
        def apply(actual: => ResultG[_, _]) = {
            val failedMessage = PartialFunction.condOpt(actual) {
                case FailedG(throwable, _) if throwable.getCause == null =>
                    "expected cause message \"" + message + "\", but failure has no cause (message is \"" + throwable.getMessage + "\")"

                case FailedG(throwable, _) if throwable.getCause.getMessage != message =>
                    "expected cause message \"" + message + "\" does not match actual cause message \"" + throwable.getCause.getMessage + "\""

                case Okay(_) =>
                    "expected failure with cause message \"" + message + "\", but got success"
            }

            (!failedMessage.isDefined, "ok", failedMessage getOrElse "")
        }
    }

    case object beFailedWithoutCause extends Matcher[ResultG[_, _]] {
        def apply(actual: => ResultG[_, _]) = {
            val failedMessage = PartialFunction.condOpt(actual) {
                case FailedG(throwable, _) if throwable.getCause != null =>
                    "expected no cause to failure, but got \"" + throwable.getCause + "\""

                case Okay(_) =>
                    "expected failure without cause, but got success"
            }

            (!failedMessage.isDefined, "ok", failedMessage getOrElse "")
        }
    }

    class ExceptionA(message: String) extends RuntimeException(message)
    class ExceptionB(message: String) extends RuntimeException(message)

    "Okay" should {
        val okay: ResultG[Nothing, String] = Okay("foo")

        "collect" in {
            okay.collect { case "foo" => 1 } must_== Okay(1)
            okay.collect { case "bar" => 1 } must beFailedWith("partial function did not apply to value")
        }

        "filter" in {
            okay.filter(_ == "foo") must_== okay
            okay.filter(_ == "bar") must beFailedWith("value did not pass filter")
        }

        "filterNot" in {
            okay.filterNot(_ == "foo") must beFailedWith("value did not pass filter")
            okay.filterNot(_ == "bar") must_== okay
        }

        "flatMap" in {
            okay.flatMap(s => Okay(s + "bar")) must_== Okay("foobar")
            okay.flatMap(s => Failed("foo")) must beFailedWith("foo")
        }

        "foreach" in {
            var isGood = false
            okay.foreach(s => isGood = s == "foo")
            isGood must beTrue
        }

        "getOrElse" in {
            okay getOrElse "bar" must_== "foo"
        }

        "isDefined" in {
            okay.isDefined must beTrue
        }

        "iterator" in {
            val iter = okay.iterator
            iter.hasNext must beTrue
            iter.next must_== "foo"
        }

        "map" in {
            okay.map(_ + "bar") must_== Okay("foobar")
        }

        "orElse" in {
            okay orElse Okay("bar") must_== okay
        }

        "orNull" in {
            okay.orNull must_== "foo"
        }

        "productPrefix" in {
            okay.productPrefix must_== "Okay"
        }

        "toList" in {
            okay.toList must_== List("foo")
        }

        // toOption tested in Result conversion
        // toBox tested in Result conversion

        "whenFailed" in {
            okay whenFailed "bar" must_== okay
            okay whenFailed ("bar", 1) must_== okay
        }

        "withFailureParameter" in {
            okay withFailureParameter 1 must_== okay
        }

        "then" in {
            okay then Okay("bar") must_== Okay("bar")
        }

        "replaceFailureWith" in {
            okay replaceFailureWith "bar" must_== okay
            okay replaceFailureWith ("bar", 1) must_== okay
        }

        "isA" in {
            okay.isA[String] must beTrue
            okay.isA[Int] must beFalse
            okay.isA[AnyRef] must beTrue
        }

        "asA" in {
            okay.asA[String] must_== okay
            okay.asA[Int] must beFailedWith("expected a Int but got a java.lang.String")
            okay.asA[AnyRef] must_== okay
        }

        "pass" in {
            var result: ResultG[Unit, String] = null
            okay pass (result = _) must_== okay
            result must_== okay
        }

        "flatten" in {
            Okay(okay).flatten must_== okay
            Okay(Failed("foo")).flatten must beFailedWith("foo")
        }

    }

    "FailedG" should {
        val failedUnit: ResultG[Unit, String] = Failed("failed message")
        val failedInt: ResultG[Int, String] = FailedG("failed message", 1)

        "collect" in {
            failedUnit.collect { case "foo" => 1 } must beFailedWith("failed message")
            failedUnit.collect { case "bar" => 1 } must beFailedWith("failed message")
            failedInt.collect { case "foo" => 1 } must beFailedWith("failed message")
            failedInt.collect { case "bar" => 1 } must beFailedWith("failed message")
        }

        "filter" in {
            failedUnit.filter(_ == "foo") must beFailedWith("failed message")
            failedUnit.filter(_ == "bar") must beFailedWith("failed message")
            failedInt.filter(_ == "foo") must beFailedWith("failed message")
            failedInt.filter(_ == "bar") must beFailedWith("failed message")
        }

        "filterNot" in {
            failedUnit.filterNot(_ == "foo") must beFailedWith("failed message")
            failedUnit.filterNot(_ == "bar") must beFailedWith("failed message")
            failedInt.filterNot(_ == "foo") must beFailedWith("failed message")
            failedInt.filterNot(_ == "bar") must beFailedWith("failed message")
        }

        "flatMap" in {
            failedUnit.flatMap(s => Okay(s + "bar")) must beFailedWith("failed message", ())
            failedUnit.flatMap(s => Failed("foo")) must beFailedWith("failed message", ())
            failedInt.flatMap(s => Okay(s + "bar")) must beFailedWith("failed message", 1)
            failedInt.flatMap(s => FailedG("foo", 2)) must beFailedWith("failed message", 1)
        }

        "foreach" in {
            var isGood = true
            failedUnit.foreach(s => isGood = false)
            isGood must_== true
            failedInt.foreach(s => isGood = false)
            isGood must_== true
        }

        "getOrElse" in {
            failedUnit getOrElse "bar" must_== "bar"
            failedInt getOrElse "bar" must_== "bar"
        }

        "isDefined" in {
            failedUnit.isDefined must beFalse
            failedInt.isDefined must beFalse
        }

        "iterator" in {
            val iter = failedUnit.iterator
            iter.hasNext must beFalse
        }

        "map" in {
            failedUnit.map(_ + "bar") must_== failedUnit
            failedInt.map(_ + "bar") must_== failedInt
        }

        "orElse" in {
            failedUnit orElse Okay("bar") must_== Okay("bar")
            failedInt orElse Okay("bar") must_== Okay("bar")
        }

        "orNull" in {
            failedUnit.orNull must beNull
            failedInt.orNull must beNull
        }

        "productPrefix" in {
            failedUnit.productPrefix must_== "Failed"
            failedInt.productPrefix must_== "Failed"
        }

        "toList" in {
            failedUnit.toList must_== Nil
            failedInt.toList must_== Nil
        }

        // toOption tested in Result conversion
        // toBox tested in Result conversion

        "whenFailed" in {
            failedUnit whenFailed "bar" must (beFailedWith("bar") and beFailedWithCause("failed message"))
            failedInt whenFailed "bar" must (beFailedWith("bar", 1) and beFailedWithCause("failed message"))
            failedUnit whenFailed ("bar", 2) must (beFailedWith("bar", 2) and beFailedWithCause("failed message"))
            failedInt whenFailed ("bar", 2) must (beFailedWith("bar", 2) and beFailedWithCause("failed message"))
        }

        "withFailureParameter" in {
            failedUnit withFailureParameter 2 must beFailedWith("failed message", 2)
            failedInt withFailureParameter 2 must beFailedWith("failed message", 2)
        }

        "then" in {
            failedUnit then Okay("bar") must_== failedUnit
            failedInt then Okay("bar") must_== failedInt
        }

        "replaceFailureWith" in {
            failedUnit replaceFailureWith "bar" must (beFailedWith("bar") and beFailedWithoutCause)
            failedInt replaceFailureWith "bar" must (beFailedWith("bar") and beFailedWithoutCause)
            failedUnit replaceFailureWith ("bar", 2) must (beFailedWith("bar", 2) and beFailedWithoutCause)
            failedInt replaceFailureWith ("bar", 2) must (beFailedWith("bar", 2) and beFailedWithoutCause)
        }

        "isA" in {
            failedUnit.isA[String] must beFalse
            failedUnit.isA[Int] must beFalse
            failedUnit.isA[AnyRef] must beFalse
            failedInt.isA[String] must beFalse
            failedInt.isA[Int] must beFalse
            failedInt.isA[AnyRef] must beFalse
        }

        "asA" in {
            failedUnit.asA[String] must beFailedWith("failed message")
            failedUnit.asA[Int] must beFailedWith("failed message")
            failedUnit.asA[AnyRef] must beFailedWith("failed message")
            failedInt.asA[String] must beFailedWith("failed message")
            failedInt.asA[Int] must beFailedWith("failed message")
            failedInt.asA[AnyRef] must beFailedWith("failed message")
        }

        "pass" in {
            var resultUnit: ResultG[Unit, String] = null
            var resultInt: ResultG[Int, String] = null
            failedUnit pass (resultUnit = _) must_== failedUnit
            resultUnit must_== failedUnit
            failedInt pass (resultInt = _) must_== failedInt
            resultInt must_== failedInt
        }

        "flatten" in {
            (Failed("foo"): Result[Result[String]]).flatten must beFailedWith("foo")
            Okay(failedUnit).flatten must_== failedUnit
            Okay(failedInt).flatten must_== failedInt
        }
    }

    "ResultG" should {
        "work in the simplest of for comprehensions (map only)" in {
            (for (str <- Okay("foo"))               yield str + "bar") must_== Okay("foobar")
            (for (str <- (Failed("foo"): Result[String])) yield str + "bar") must beFailedWith("foo")
        }

        "work in simple for comprehensions (map and flatMap)" in {
            (for (foo <- Okay("foo");               bar <- Okay("bar")) yield foo + bar) must_== Okay("foobar")
            (for (foo <- (Failed("foo"): Result[String]); bar <- Okay("bar")) yield foo + bar) must beFailedWith("foo")
            (for (foo <- Okay("foo");               bar <- Failed("bar"))    yield foo + bar) must beFailedWith("bar")
        }

        "work in complicated for comprehensions (map, flatMap, and filter)" in {
            {
                for {
                    foo <- Okay("foo")
                    if foo.length == 3
                    bar <- Okay("bar")
                } yield foo + bar
            } must_== Okay("foobar")

            {
                for {
                    foo <- Okay("foo")
                    if foo.length == 4
                    bar <- Okay("bar")
                } yield foo + bar
            } must beFailedWith("value did not pass filter")
        }

        "work in side effecting for comprehensions (foreach)" in {
            var result: String = null
            (for (foo <- Okay("foo")) { result = foo; foo }) must_== ()
            result must_== "foo"
        }
    }

    "Result conversion" should {
        "convert non-null to Okay" in {
            ResultG("foo") must_== Okay("foo")
        }

        "convert null to Failed(\"value was null\")" in {
            ResultG(null) must beFailedWith("value was null")
        }

        "convert None to Failed(\"option was none\")" in {
            None.toResult must beFailedWith("option was none")
        }

        "convert Some to Okay" in {
            Some("foo").toResult must_== Okay("foo")
        }

        "convert Okay to Some" in {
            Okay("foo").toOption must beSome("foo")
        }

        "convert Failed to None" in {
            Failed("foo").toOption must beNone
        }

        "convert Left(throwable) into Failed" in {
            val t = new RuntimeException("foo")
            Left(t).toResult must_== Failed(t)
        }

        "convert Left((throwable, param)) into FailedG" in {
            val t = new RuntimeException("foo")
            val p = "bar"
            Left((t, p)).toResult must_== FailedG(t, p)
        }

        "convert Right into Okay" in {
            Right("bar").toResult must_== Okay("bar")
        }

        "convert Okay into Right" in {
            Okay("bar").toEither must_== Right("bar")
        }

        "convert FailedG into Left" in {
            val t = new RuntimeException("foo")
            FailedG(t, "bar").toEither must_== Left(t, "bar")
        }
    }

    "Catching" should {
        "catch any Exception" in {
            catchingException { "foo" } must_== Okay("foo")
            catchingException { throw new RuntimeException("foo") } must beFailedWith("foo")
            catchingException { throw new Throwable("foo") } must throwA[Throwable]
        }

        "catch only a specific Exception" in {
            catching[ExceptionA] apply { "foo" } must_== Okay("foo")
            catching[ExceptionA] apply { throw new ExceptionA("foo") } must beFailedWith("foo")
            catching[ExceptionA] apply { throw new ExceptionB("foo") } must throwA[ExceptionB]
        }

        "catch only specific Exceptions" in {
            catchingExceptions(classOf[ExceptionA], classOf[ExceptionB]) { "foo" } must_== Okay("foo")
            catchingExceptions(classOf[ExceptionA], classOf[ExceptionB]) { throw new ExceptionA("foo") } must beFailedWith("foo")
            catchingExceptions(classOf[ExceptionA], classOf[ExceptionB]) { throw new ExceptionB("foo") } must beFailedWith("foo")
            catchingExceptions(classOf[ExceptionA], classOf[ExceptionB]) { throw new UnsupportedOperationException("foo") } must throwA[UnsupportedOperationException]
        }
    }
}
