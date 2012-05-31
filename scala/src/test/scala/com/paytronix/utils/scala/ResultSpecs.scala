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

import org.specs2.SpecificationWithJUnit
import org.specs2.matcher.{AnyMatchers, Matcher, StandardMatchResults}

import result.{
    Failed, FailedG, FailedParameter, FailedParameterDefault, Okay, Result, ResultG, eitherOps, eitherOpsG, optionOps, parameter, tryCatch, tryCatching
}

object ResultMatchers extends AnyMatchers with StandardMatchResults {

    def beFailedWith(message: String): Matcher[ResultG[Unit, _]] =
        beFailedWith(message, ())

    def beFailedWith[E](message: String, parameter: E): Matcher[ResultG[E, _]] =
        beLike {
            case FailedG(throwable, actualParameter) if message != throwable.getMessage && parameter != actualParameter =>
                ko (
                    "expected message \"" + message + "\" does not match actual message \"" + throwable.getMessage + "\" and" +
                    " expected parameter \"" + String.valueOf(parameter) + "\" does not match actual parameter \"" +
                    String.valueOf(actualParameter) + "\""
                )

            case FailedG(throwable, _) if message != throwable.getMessage =>
                ko("expected message \"" + message + "\" does not match actual message \"" + throwable.getMessage + "\"")

            case FailedG(_, actualParameter) if parameter != actualParameter =>
                ko (
                    "expected parameter \"" + String.valueOf(parameter) + "\" does not match actual parameter \"" +
                    String.valueOf(actualParameter) + "\""
                )

            case Okay(_) =>
                ko (
                    "expected failure with message \"" + message + "\" and parameter \"" + String.valueOf(parameter) +
                    "\", but got success"
                )

            case _ => ok
        }

    def beFailedWithCause(message: String): Matcher[ResultG[_, _]] =
        beLike {
            case FailedG(throwable, _) if throwable.getCause == null =>
                ko (
                    "expected cause message \"" + message + "\", but failure has no cause (message is \"" +
                    throwable.getMessage + "\")"
                )

            case FailedG(throwable, _) if throwable.getCause.getMessage != message =>
                ko (
                    "expected cause message \"" + message + "\" does not match actual cause message \"" +
                    throwable.getCause.getMessage + "\""
                )

            case Okay(_) =>
                ko("expected failure with cause message \"" + message + "\", but got success")

            case _ => ok
        }

    def beFailedWithoutCause: Matcher[ResultG[_, _]] =
        beLike {
            case FailedG(throwable, _) if throwable.getCause != null =>
                ko("expected no cause to failure, but got \"" + throwable.getCause + "\"")

            case Okay(_) =>
                ko("expected failure without cause, but got success")

            case _ => ok
        }
}

object ResultFixtures {
    val okay: ResultG[Nothing, String] = Okay("foo")
    val failedUnit: ResultG[Unit, String] = Failed("failed message")
    val failedInt: ResultG[Int, String] = FailedG("failed message", 1)

    class ExceptionA(message: String) extends RuntimeException(message)
    class ExceptionB(message: String) extends RuntimeException(message)

    final case class MyFailedParameter(s: String) extends FailedParameter
}

import ResultMatchers._
import ResultFixtures._

class OkaySpecTest extends SpecificationWithJUnit { def is =
    "Okay should" ^
    "collect" ! {
        { okay.withFailedType[Unit].collect { case "foo" => 1 } must_== Okay(1) } and
        { okay.withFailedType[Unit].collect { case "bar" => 1 } must beFailedWith("partial function did not apply to value") }
    } ^
    "filter" ! {
        { okay.withFailedType[Unit].filter(_ == "foo") must_== okay } and
        { okay.withFailedType[Unit].filter(_ == "bar") must beFailedWith("value did not pass filter") }
    } ^
    "filterNot" ! {
        { okay.withFailedType[Unit].filterNot(_ == "foo") must beFailedWith("value did not pass filter") } and
        { okay.withFailedType[Unit].filterNot(_ == "bar") must_== okay }
    } ^
    "flatMap" ! {
        { okay.flatMap(s => Okay(s + "bar")) must_== Okay("foobar") } and
        { okay.flatMap(s => Failed("foo")) must beFailedWith("foo") }
    } ^
    "foreach" ! {
        var isGood = false
        okay.foreach(s => isGood = s == "foo")
        isGood must beTrue
    } ^
    "getOrElse" ! {
        okay getOrElse "bar" must_== "foo"
    } ^
    "isDefined" ! {
        okay.isDefined must beTrue
    } ^
    "iterator" ! {
        val iter = okay.iterator

        { iter.hasNext must beTrue } and
        { iter.next must_== "foo" }
    } ^
    "map" ! {
        okay.map(_ + "bar") must_== Okay("foobar")
    } ^
    "| (orElse)" ! {
        { (okay | "bar") must_== okay }
        { (okay | parameter("bar")) must_== okay }
        { (okay | MyFailedParameter("bar")) must_== okay }
        { (okay | ("bar" -> Nil)) must_== okay }
        { (okay | Failed("bar")) must_== okay }
        { (okay | Okay("bar")) must_== okay }
        { (okay | { case Failed(throwable) => FailedG(throwable, "foo" + throwable.getMessage) }) must_== okay }
    } ^
    "orNull" ! {
        okay.orNull must_== "foo"
    } ^
    "toList" ! {
        okay.toList must_== List("foo")
    } ^
    // toOption tested in Result conversion
    // toEither tested in Result conversion
    "then" ! {
        okay then Okay("bar") must_== Okay("bar")
    } ^
    "isA" ! {
        { okay.isA[String] must beTrue  } and
        { okay.isA[Int]    must beFalse } and
        { okay.isA[AnyRef] must beTrue  }
    } ^
    "asA" ! {
        { okay.withFailedType[Unit].asA[String] must_== okay } and
        { okay.withFailedType[Unit].asA[Int]    must beFailedWith("expected a Int but got a java.lang.String") } and
        { okay.withFailedType[Unit].asA[AnyRef] must_== okay }
    } ^
    "pass" ! {
        var result: ResultG[Unit, String] = null

        { okay pass (result = _) must_== okay } and
        { result must_== okay }
    } ^
    "flatten" ! {
        { Okay(okay).flatten must_== okay } and
        { Okay(Failed("foo")).flatten must beFailedWith("foo") }
    }
}

class FailedGSpecTest extends SpecificationWithJUnit {
    implicit val fpdInt = new FailedParameterDefault[Int] { def default = -1 }

    def is =
        "FailedG should" ^
        "collect" ! {
            { failedUnit.collect { case "foo" => 1 } must beFailedWith("failed message") } and
            { failedUnit.collect { case "bar" => 1 } must beFailedWith("failed message") } and
            { failedInt.collect { case "foo" => 1 } must beFailedWith("failed message", 1) } and
            { failedInt.collect { case "bar" => 1 } must beFailedWith("failed message", 1) }
        } ^
        "filter" ! {
            { failedUnit.filter(_ == "foo") must beFailedWith("failed message") } and
            { failedUnit.filter(_ == "bar") must beFailedWith("failed message") } and
            { failedInt.filter(_ == "foo") must beFailedWith("failed message", 1) } and
            { failedInt.filter(_ == "bar") must beFailedWith("failed message", 1) }
        } ^
        "filterNot" ! {
            { failedUnit.filterNot(_ == "foo") must beFailedWith("failed message") } and
            { failedUnit.filterNot(_ == "bar") must beFailedWith("failed message") } and
            { failedInt.filterNot(_ == "foo") must beFailedWith("failed message", 1) } and
            { failedInt.filterNot(_ == "bar") must beFailedWith("failed message", 1) }
        } ^
        "flatMap" ! {
            { failedUnit.flatMap(s => Okay(s + "bar")) must beFailedWith("failed message", ()) } and
            { failedUnit.flatMap(s => Failed("foo")) must beFailedWith("failed message", ()) } and
            { failedInt.flatMap(s => Okay(s + "bar")) must beFailedWith("failed message", 1) } and
            { failedInt.flatMap(s => FailedG("foo", 2)) must beFailedWith("failed message", 1) }
        } ^
        "foreach" ! {
            var isGood = true

            { failedUnit.foreach(s => isGood = false); isGood must_== true } and
            { failedInt.foreach(s => isGood = false); isGood must_== true }
        } ^
        "getOrElse" ! {
            { failedUnit getOrElse "bar" must_== "bar" } and
            { failedInt getOrElse "bar" must_== "bar" }
        } ^
        "isDefined" ! {
            { failedUnit.isDefined must beFalse } and
            { failedInt.isDefined must beFalse }
        } ^
        "iterator" ! {
            failedUnit.iterator.hasNext must beFalse
        } ^
        "map" ! {
            { failedUnit.map(_ + "bar") must_== failedUnit } and
            { failedInt.map(_ + "bar") must_== failedInt }
        } ^
        "| (orElse)" ! {
            { (failedInt | "bar") must (beFailedWith("bar", 1) and beFailedWithCause("failed message")) } and
            { (failedInt | parameter("bar")) must (beFailedWith("failed message", "bar") and beFailedWithoutCause) } and
            { (failedInt | MyFailedParameter("bar")) must (beFailedWith("failed message", MyFailedParameter("bar")) and beFailedWithoutCause) } and
            { (failedInt | ("bar" -> Nil)) must (beFailedWith("bar", Nil) and beFailedWithCause("failed message")) } and
            { (failedInt | Failed("bar")) must (beFailedWith("bar") and beFailedWithoutCause) } and
            { (failedInt | Okay("bar")) must_== Okay("bar") } and
            { (failedInt | { case FailedG(throwable, _) => FailedG(throwable, "foo" + throwable.getMessage) }) must beFailedWith("failed message", "foofailed message") and beFailedWithoutCause }
        } ^
        "orNull" ! {
            { failedUnit.orNull must beNull } and
            { failedInt.orNull  must beNull }
        } ^
        "toList" ! {
            { failedUnit.toList must_== Nil } and
            { failedInt.toList  must_== Nil }
        } ^
        // toOption tested in Result conversion
        // toBox tested in Result conversion
        "then" ! {
            { failedUnit then Okay("bar") must_== failedUnit } and
            { failedInt then Okay("bar")  must_== failedInt }
        } ^
        "isA" ! {
            { failedUnit.isA[String] must beFalse } and
            { failedUnit.isA[Int]    must beFalse } and
            { failedUnit.isA[AnyRef] must beFalse } and
            { failedInt.isA[String]  must beFalse } and
            { failedInt.isA[Int]     must beFalse } and
            { failedInt.isA[AnyRef]  must beFalse }
        } ^
        "asA" ! {
            { failedUnit.asA[String] must beFailedWith("failed message") } and
            { failedUnit.asA[Int]    must beFailedWith("failed message") } and
            { failedUnit.asA[AnyRef] must beFailedWith("failed message") } and
            { failedInt.asA[String]  must beFailedWith("failed message", 1) } and
            { failedInt.asA[Int]     must beFailedWith("failed message", 1) } and
            { failedInt.asA[AnyRef]  must beFailedWith("failed message", 1) }
        } ^
        "pass" ! {
            var resultUnit: ResultG[Unit, String] = null
            var resultInt: ResultG[Int, String] = null

            { failedUnit pass (resultUnit = _) must_== failedUnit } and
            { resultUnit must_== failedUnit } and
            { failedInt pass (resultInt = _) must_== failedInt } and
            { resultInt must_== failedInt }
        } ^
        "flatten" ! {
            { (Failed("foo"): Result[Result[String]]).flatten must beFailedWith("foo") } and
            { Okay(failedUnit).flatten must_== failedUnit } and
            { Okay(failedInt).flatten must_== failedInt }
        }
}

class ResultGSpecTest extends SpecificationWithJUnit { def is =
    "ResultG" ^
    "work in the simplest of for comprehensions (map only)" ! {
        { (for (str <- Okay("foo"))               yield str + "bar") must_== Okay("foobar") } and
        { (for (str <- (Failed("foo"): Result[String])) yield str + "bar") must beFailedWith("foo") }
    } ^
    "work in simple for comprehensions (map and flatMap)" ! {
        { (for (foo <- Okay("foo");               bar <- Okay("bar"))       yield foo + bar) must_== Okay("foobar") } and
        { (for (foo <- (Failed("foo"): Result[String]); bar <- Okay("bar")) yield foo + bar) must beFailedWith("foo") } and
        { (for (foo <- Okay("foo");               bar <- Failed("bar"))     yield foo + bar) must beFailedWith("bar") }
    } ^
    "work in complicated for comprehensions (map, flatMap, and filter)" ! {
        { (for (foo <- Okay("foo").withFailedType[Unit]; if foo.length == 3; bar <- Okay("bar")) yield foo + bar) must_== Okay("foobar") } and
        { (for (foo <- Okay("foo").withFailedType[Unit]; if foo.length == 4; bar <- Okay("bar")) yield foo + bar) must beFailedWith("value did not pass filter") }
    } ^
    "work in for comprehensions with pattern matching" ! {
        val res = (for ((a, b) <- Okay((1,2)).withFailedType[Option[String]]; failed <- FailedG("test", Some("foo")) unless true) yield a + b)
        (res: ResultG[Option[String], Int]) // assert the type is not ResultG[Any, _]
        res must_== Okay(3)
    } ^
    "work in side effecting for comprehensions (foreach)" ! {
        var result: String = null

        { (for (foo <- Okay("foo")) { result = foo; foo }) must_== () } and
        { result must_== "foo" }
    }
}

class ResultConversionSpecTest extends SpecificationWithJUnit { def is =
    "Result conversion" ^
    "convert non-null to Okay" ! {
        ResultG("foo") must_== Okay("foo")
    } ^
    "convert null to Failed(\"value was null\")" ! {
        ResultG(null) must beFailedWith("value was null")
    } ^
    "convert None to Failed(\"option was none\")" ! {
        None.toResult must beFailedWith("option was none")
    } ^
    "convert Some to Okay" ! {
        Some("foo").toResult must_== Okay("foo")
    } ^
    "convert Okay to Some" ! {
        Okay("foo").toOption must beSome("foo")
    } ^
    "convert Failed to None" ! {
        Failed("foo").toOption must beNone
    } ^
    "convert Left(throwable) into Failed" ! {
        val t = new RuntimeException("foo")
        Left(t).toResult must_== Failed(t)
    } ^
    "convert Left((throwable, param)) into FailedG" ! {
        val t = new RuntimeException("foo")
        val p = "bar"
        Left((t, p)).toResult must_== FailedG(t, p)
    } ^
    "convert Right into Okay" ! {
        Right("bar").toResult must_== Okay("bar")
    } ^
    "convert Okay into Right" ! {
        Okay("bar").toEither must_== Right("bar")
    } ^
    "convert FailedG into Left" ! {
        val t = new RuntimeException("foo")
        FailedG(t, "bar").toEither must_== Left(t, "bar")
    }
}

class CatchingSpecTest extends SpecificationWithJUnit { def is =
    "Catching should" ^
    "catch any Exception" ! {
        { tryCatch.value { "foo" } must_== Okay("foo") } and
        { tryCatch.value { throw new RuntimeException("foo") } must beFailedWith("foo") } and
        { tryCatch.value { throw new Throwable("foo") } must throwA[Throwable] }
    } ^
    "catch only a specific Exception" ! {
        { tryCatching[ExceptionA].value { "foo" } must_== Okay("foo") } and
        { tryCatching[ExceptionA].value { throw new ExceptionA("foo") } must beFailedWith("foo") } and
        { tryCatching[ExceptionA].value { throw new ExceptionB("foo") } must throwA[ExceptionB] }
    } ^
    "catch only specific Exceptions" ! {
        { tryCatching(classOf[ExceptionA], classOf[ExceptionB]).value { "foo" } must_== Okay("foo") } and
        { tryCatching(classOf[ExceptionA], classOf[ExceptionB]).value { throw new ExceptionA("foo") } must beFailedWith("foo") } and
        { tryCatching(classOf[ExceptionA], classOf[ExceptionB]).value { throw new ExceptionB("foo") } must beFailedWith("foo") } and
        { tryCatching(classOf[ExceptionA], classOf[ExceptionB]).value { throw new UnsupportedOperationException("foo") } must throwA[UnsupportedOperationException] }
    }
}
