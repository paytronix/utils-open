//
// Copyright 2012-2014 Paytronix Systems, Inc.
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
import scalaz.{\/, -\/, \/-}

import result._

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

class OkayTest extends SpecificationWithJUnit {
    def is = s2"""
        Okay should
            collect Okay when the partial function applies           $collect1
            collect FailedG when the partial function doesn't apply  $collect2
            filter to Okay when the predicate allows                 $filter1
            filter to FailedG when the predicate rejects             $filter2
            filterNot to FailedG when the predicate allows           $filterNot1
            filterNot to Okay when the predicate rejects             $filterNot2
            flatMap to Okay when k yield Okay                        $flatMap1
            flatMap to FailedG when k yield FailedG                  $flatMap2
            apply a function with foreach                            $foreach1
            ignore the RHS of getOrElse                              $getOrElse1
            be okay                                                  $isOkay1
            be coercible to Okay                                     $asOkay1
            not be failed                                            $isFailed1
            not be coercible to Failed                               $asFailed1
            map a function over the contents                         $map1
            orElse "…" should have no effect                         $orElse1
            orElse parameter(…) should have no effect                $orElse2
            orElse FailedParameter should have no effect             $orElse3
            orElse ("…" -> …) should have no effect                  $orElse4
            orElse Failed(…) should have no effect                   $orElse5
            orElse Okay(…) should have no effect                     $orElse6
            orElse { partial function… } should have no effect       $orElse7
            orElse … should not evaluate the RHS                     $orElse8
            chain to another thing with >>                           $then1
            type test correctly                                      $isA1
            not lie about type tests                                 $isA2
            be covariant in type testing                             $isA3
            pass itself to a function with pass                      $pass1
            pass the contained value to a function for side effects  $sideEffect1
            flatten Okay(Okay(…)) to Okay(…)                         $flatten1
            flatten Okay(Failed(…)) to Failed(…)                     $flatten2
    """

    def collect1 = okay.withFailedType[Unit].collect { case "foo" => 1 } ==== Okay(1)
    def collect2 = okay.withFailedType[Unit].collect { case "bar" => 1 } must beFailedWith("partial function did not apply to value")
    def filter1 = okay.withFailedType[Unit].filter(_ == "foo") ==== okay
    def filter2 = okay.withFailedType[Unit].filter(_ == "bar") must beFailedWith("value did not pass filter")
    def filterNot1 = okay.withFailedType[Unit].filterNot(_ == "foo") must beFailedWith("value did not pass filter")
    def filterNot2 = okay.withFailedType[Unit].filterNot(_ == "bar") ==== okay
    def flatMap1 = okay.flatMap(s => Okay(s + "bar")) ==== Okay("foobar")
    def flatMap2 = okay.flatMap(s => Failed("foo")) must beFailedWith("foo")
    def foreach1 = { var isGood = false; okay.foreach(s => isGood = s == "foo"); isGood must beTrue }
    def getOrElse1 = okay.getOrElse(sys.error("not lazy enough!")) ==== "foo"
    def isOkay1 = okay.isOkay must beTrue
    def isFailed1 = okay.isFailed must beFalse
    def asOkay1 = (okay.asOkay: Result[String]) ==== okay
    def asFailed1 = okay.asFailed must throwAn[Exception]
    def map1 = okay.map(_ + "bar") ==== Okay("foobar")
    def orElse1 = (okay | "bar") ==== okay
    def orElse2 = (okay | parameter("bar")) ==== okay
    def orElse3 = (okay | MyFailedParameter("bar")) ==== okay
    def orElse4 = (okay | ("bar" -> Nil)) ==== okay
    def orElse5 = (okay | Failed("bar")) ==== okay
    def orElse6 = (okay | Okay("bar")) ==== okay
    def orElse7 = (okay | { case Failed(throwable) => FailedG(throwable, "foo" + throwable.getMessage) }) ==== okay
    def orElse8 = (okay | (sys.error("not lazy enough!"): String)) ==== okay
    def then1 = (okay >> Okay("bar")) ==== Okay("bar")
    def isA1 = okay.isA[String] must beTrue
    def isA2 = okay.isA[Int]    must beFalse
    def isA3 = okay.isA[AnyRef] must beTrue
    def asA1 = okay.withFailedType[Unit].asA[String] ==== okay
    def asA2 = okay.withFailedType[Unit].asA[Int]    must beFailedWith("expected a Int but got a java.lang.String")
    def asA3 = okay.withFailedType[Unit].asA[AnyRef] ==== okay
    def pass1 = { var result: ResultG[Unit, String] = null; { okay.pass { result = _ } ==== okay } and { result ==== okay } }
    def sideEffect1 = { var result: String = null; { okay.sideEffect { result = _ } ==== okay } and { result ==== "foo" } }
    def flatten1 = Okay(okay).flatten ==== okay
    def flatten2 = Okay(Failed("foo")).flatten must beFailedWith("foo")
}

class FailedGTest extends SpecificationWithJUnit {
    implicit val fpdInt = new FailedParameterDefault[Int] { def default = -1 }

    def is = s2"""
        FailedG should
            preserve the failure with collect                                        $collect1
            preserve the failure with filter                                         $filter1
            preserve the failure with filterNot                                      $filterNot1
            preserve the failure with flatMap                                        $flatMap1
            not evaluate the function passed to foreach                              $foreach1
            evaluate to the RHS of getOrElse                                         $getOrElse1
            not be okay                                                              $isOkay1
            not be coercible to Okay                                                 $asOkay1
            be failed                                                                $isFailed1
            be coercible to Failed                                                   $asFailed1
            preserve the failure with map                                            $map1
            orElse "…" should wrap an error message                                  $orElse1
            orElse parameter(…) should change the parameter                          $orElse2
            orElse FailedParameter should change the parameter                       $orElse3
            orElse ("…" -> …) should wrap an error message and change the parameter  $orElse4
            orElse Failed(…) should have no effect                                   $orElse5
            orElse Okay(…) should have no effect                                     $orElse6
            orElse { partial function… } should have no effect                       $orElse7
            preserve the failure with >>                                             $then1
            not type test even for the type ascribed                                 $isA1
            not type test for other types                                            $isA2
            not type test for AnyRef                                                 $isA3
            not type cast to the type ascribed                                       $asA1
            not type cast to other types                                             $asA2
            not type cast to AnyRef                                                  $asA3
            pass itself to a function with pass                                      $pass1
            not affect the failure when passing                                      $pass2
            preserve failure when flattening Failed(…)                               $flatten1
    """

    def collect1 = failedInt.collect { case "foo" => 1 } must beFailedWith("failed message", 1)
    def filter1 = failedInt.filter(_ == "foo") must beFailedWith("failed message", 1)
    def filterNot1 = failedInt.filterNot(_ == "foo") must beFailedWith("failed message", 1)
    def flatMap1 = failedInt.flatMap(s => Okay(s + "bar")) must beFailedWith("failed message", 1)
    def foreach1 = failedInt.foreach(s => sys.error(s"what the heck is $s?")) ==== (())
    def getOrElse1 = failedInt.getOrElse("bar") ==== "bar"
    def isOkay1 = failedInt.isOkay must beFalse
    def asOkay1 = failedInt.asOkay must throwA[Throwable]
    def isFailed1 = failedInt.isFailed must beTrue
    def asFailed1 = (failedInt.asFailed: ResultG[Int, String]) ==== failedInt
    def map1 = failedInt.map(_ + "bar") ==== failedInt
    def orElse1 = (failedInt | "bar")                    must (beFailedWith("bar", 1) and beFailedWithCause("failed message"))
    def orElse2 = (failedInt | parameter("bar"))         must (beFailedWith("failed message", "bar") and beFailedWithoutCause)
    def orElse3 = (failedInt | MyFailedParameter("bar")) must (beFailedWith("failed message", MyFailedParameter("bar")) and beFailedWithoutCause)
    def orElse4 = (failedInt | ("bar" -> Nil))           must (beFailedWith("bar", Nil) and beFailedWithCause("failed message"))
    def orElse5 = (failedInt | Failed("bar"))            must (beFailedWith("bar") and beFailedWithoutCause)
    def orElse6 = (failedInt | Okay("bar"))              ==== Okay("bar")
    def orElse7 = (failedInt | { case FailedG(throwable, _) => FailedG(throwable, "foo" + throwable.getMessage) }) must beFailedWith("failed message", "foofailed message") and beFailedWithoutCause
    def then1 = failedInt >> Okay("bar") ==== failedInt
    def isA1 = failedInt.isA[String]  must beFalse
    def isA2 = failedInt.isA[Int]     must beFalse
    def isA3 = failedInt.isA[AnyRef]  must beFalse
    def asA1 = failedInt.asA[String]  must beFailedWith("failed message", 1)
    def asA2 = failedInt.asA[Int]     must beFailedWith("failed message", 1)
    def asA3 = failedInt.asA[AnyRef]  must beFailedWith("failed message", 1)
    def pass1 = { var resultInt: ResultG[Int, String] = null; val _ = failedInt pass (resultInt = _); resultInt ==== failedInt }
    def pass2 = { failedInt.pass { _ => () } ==== failedInt }
    def flatten1 = (Failed("foo"): Result[Result[String]]).flatten must beFailedWith("foo")
}

class ResultGTest extends SpecificationWithJUnit {
    val okay: Result[String] = Okay("a")
    val okay2: Result[String] = Okay("b")
    val failed: Result[String] = Failed("foo")
    val failed2: Result[String] = Failed("bar")

    def is = s2"""
        ResultG for comprehensions
            for { … <-   Okay(…)                           } yield … $forG1Yield1
            for { … <- Failed(…)                           } yield … $forG1Yield2
            for { … <-   Okay(…);           … <-   Okay(…) } yield … $forG2Yield1
            for { … <- Failed(…);           … <-   Okay(…) } yield … $forG2Yield2
            for { … <-   Okay(…);           … <- Failed(…) } yield … $forG2Yield3
            for { … <- Failed(…);           … <- Failed(…) } yield … $forG2Yield4
            for { … <-   Okay(…); if true ; … <-   Okay(…) } yield … $forG3Yield1
            for { … <-   Okay(…); if false; … <-   Okay(…) } yield … $forG3Yield2
            for { … <-   Okay(…)                           } { … } $forG1SideEffect1
            support pattern matching binders in for-comprehensions that aren't refuted $patMatch1
            support pattern matching binders in for-comprehensions that are refuted $patMatch2
            properly compute the failed parameter type $paramType1
    """

    def forG1Yield1 = (for (a <- okay                        ) yield a + "bar") ==== Okay("abar")
    def forG1Yield2 = (for (a <- failed                      ) yield a + "bar") must beFailedWith("foo")
    def forG2Yield1 = (for (a <- okay;           b <- okay2  ) yield a + b    ) ==== Okay("ab")
    def forG2Yield2 = (for (a <- failed;         b <- okay2  ) yield a + b    ) must beFailedWith("foo")
    def forG2Yield3 = (for (a <- okay;           b <- failed2) yield a + b    ) must beFailedWith("bar")
    def forG2Yield4 = (for (a <- failed;         b <- failed2) yield a + b    ) must beFailedWith("foo")
    def forG3Yield1 = (for (a <- okay; if true;  b <- okay2  ) yield a + b    ) ==== Okay("ab")
    def forG3Yield2 = (for (a <- okay; if false; b <- okay2  ) yield a + b    ) must beFailedWith("value did not pass filter")
    def patMatch1 = (for ((a,b) <- Okay((1,2)): Result[(Int, Int)]) yield a+b) ==== Okay(3)
    def patMatch2 = (for ((0,b) <- Okay((1,2)): Result[(Int, Int)]) yield   b) must beFailedWith("value did not pass filter")
    def paramType1 = (for (a <- Okay(1): ResultG[Option[String], Int]; b <- FailedG("test", Some("foo")) unless true) yield a.toString+b) ==== Okay("1()")
    def forG1SideEffect1 = {
        var result: String = null

        { (for (foo <- Okay("foo")) { result = foo }) ==== (()) } and
        { result ==== "foo" }
    }
}

class ResultConversionTest extends SpecificationWithJUnit {
    val throwable = new RuntimeException("foo")

    def is = s2"""
        Result conversion should convert
            value          to Okay            $fromNonNull
            null           to Failed          $fromNull
            Okay           to value           $toNonNull
            Failed         to null            $toNull
            Some           to Okay            $fromSome
            None           to Failed          $fromNone
            Okay           to Some            $toSome
            Failed         to None            $toNone
            Right          to Okay            $fromRight
            Left w/o param to Failed          $fromLeftThrowable
            Left w/ param  to FailedG         $fromLeftThrowableG
            Okay           to Right           $toRight
            Failed         to Left w/ unit    $toLeftThrowable
            FailedG        to Left w/ param   $toLeftThrowableG
            \/-            to Okay            $fromDisjunctionRight
            -\/            to FailedG         $fromDisjunctionLeft
            Okay           to \/-             $toDisjunctionRight
            FailedG        to -\/             $toDisjunctionLeft
            Success (Try)  to Okay            $fromSuccess
            Failure (Try)  to Failed          $fromFailure
            Okay           to Success (Try)   $toSuccess
            Failed         to Failure (Try)   $toFailure
            Okay           to :: (List)       $toCons
            Failed         to Nil (List)      $toNil
            Okay           to Iterator.single $toIteratorSingle
            Failed         to Iterator.empty  $toIteratorEmpty
    """

    def fromNonNull = Result("foo") ==== Okay("foo")
    def fromNull = Result(null) must beFailedWith("value was null")
    def toNonNull = Okay("foo").orNull ==== "foo"
    def toNull = (Failed(""): Result[String]).orNull must beNull
    def fromSome = (Some("foo"): Option[String]).toResult ==== Okay("foo")
    def fromNone = (None: Option[String]).toResult must beFailedWith("option was none")
    def toSome = Okay("foo").toOption ==== Some("foo")
    def toNone = Failed("").toOption ==== None
    def fromRight = (Right("foo"): Either[Throwable, String]).toResult ==== Okay("foo")
    def fromLeftThrowable = Left(throwable).toResult ==== Failed(throwable)
    def fromLeftThrowableG = Left((throwable, 1)).toResult ==== FailedG(throwable, 1)
    def toRight = Okay("foo").toEither ==== Right("foo")
    def toLeftThrowable = Failed(throwable).toEither ==== Left((throwable, ()))
    def toLeftThrowableG = FailedG(throwable, 1).toEither ==== Left((throwable, 1))
    def fromDisjunctionRight = (\/-("foo"): (Throwable, Int) \/ String).toResult ==== Okay("foo")
    def fromDisjunctionLeft = (-\/((throwable, 1)): (Throwable, Int) \/ String).toResult ==== FailedG(throwable, 1)
    def toDisjunctionRight = Okay("foo").toDisjunction ==== \/-("foo")
    def toDisjunctionLeft = FailedG(throwable, 1).toDisjunction ==== -\/((throwable, 1))
    def fromSuccess = scala.util.Success("foo").toResult ==== Okay("foo")
    def fromFailure = scala.util.Failure(throwable).toResult ==== Failed(throwable)
    def toSuccess = Okay("foo").toTry ==== scala.util.Success("foo")
    def toFailure = Failed(throwable).toTry ==== scala.util.Failure(throwable)
    def toCons = Okay("foo").toList ==== List("foo")
    def toNil = Failed("").toList ==== List()
    def toIteratorSingle = Okay("foo").iterator.toList ==== List("foo")
    def toIteratorEmpty = Failed("").iterator.toList ==== List()
}

class ResultOptionTest extends SpecificationWithJUnit {
    def is = s2"""
        ResultG[Option[A]]
            Some mapResult to Okay(Some(…)) $e1
            Some mapResult to Failed(…)     $e2
            None mapResult to Okay(None)    $e3
            None mapResult to Failed(…)     $e4
    """

    def e1 = (Some("foo"): Option[String]).mapResult { s => Okay(s + "bar") } ==== Okay(Some("foobar"))
    def e2 = (Some("foo"): Option[String]).mapResult { _ => Failed("oh no!") } must beFailedWith("oh no!")
    def e3 = (None: Option[String]).mapResult { s => Okay(s + "bar") } ==== Okay(None)
    def e4 = (None: Option[String]).mapResult { _ => Failed("oh no!") } ==== Okay(None)
}

class CatchingSpecTest extends SpecificationWithJUnit {

    def is = s2"""
        Exception catching
            tryCatch.value success                                $tryCatch1
            tryCatch.value has correct failure exception          $tryCatch2
            tryCatch.value doesn't catch Throwable                $tryCatch3
            tryCatching.value[A] success                          $tryCatching1s
            tryCatching.value[A] catching A                       $tryCatching1c
            tryCatching.value[A] passing B                        $tryCatching1p
            tryCatching.value(classOf[A], classOf[B]) success     $tryCatching2s
            tryCatching.value(classOf[A], classOf[B]) catching A  $tryCatching2ac
            tryCatching.value(classOf[A], classOf[B]) catching B  $tryCatching2bc
            tryCatching.value(classOf[A], classOf[B]) passing C   $tryCatching2p
    """

    def tryCatch1 = tryCatch.value { "foo" } ==== Okay("foo")
    def tryCatch2 = tryCatch.value { throw new RuntimeException("foo") } must beFailedWith("foo")
    def tryCatch3 = tryCatch.value { throw new Throwable("foo") } must throwA[Throwable]
    def tryCatching1s = tryCatching[ExceptionA].value { "foo" } ==== Okay("foo")
    def tryCatching1c = tryCatching[ExceptionA].value { throw new ExceptionA("foo") } must beFailedWith("foo")
    def tryCatching1p = tryCatching[ExceptionA].value { throw new ExceptionB("foo") } must throwA[ExceptionB]
    def tryCatching2s = tryCatching(classOf[ExceptionA], classOf[ExceptionB]).value { "foo" } ==== Okay("foo")
    def tryCatching2ac = tryCatching(classOf[ExceptionA], classOf[ExceptionB]).value { throw new ExceptionA("foo") } must beFailedWith("foo")
    def tryCatching2bc = tryCatching(classOf[ExceptionA], classOf[ExceptionB]).value { throw new ExceptionB("foo") } must beFailedWith("foo")
    def tryCatching2p = tryCatching(classOf[ExceptionA], classOf[ExceptionB]).value { throw new UnsupportedOperationException("foo") } must throwA[UnsupportedOperationException]
}

class BindAndChainTest extends SpecificationWithJUnit {
    def is = s2"""
        ResultG macros
            okay >> okay             $chainOkayOkayCase
            okay >> failed           $chainOkayFailedCase
            failed >> okay           $chainFailedOkayCase
            failed >> failed         $chainFailedFailedCase
            failed >> error          $chainFailedLazyCase
            okay >>= { _ => okay }   $bindWildcardOkayCase
            failed >>= { _ => okay } $bindWildcardFailedCase
            okay >>= { a => rhs }    $bindIdentOkayCase
            failed >>= { a => rhs }  $bindIdentFailedCase
            okay >>= { case … }      $bindPFOkayCase
            failed >>= { case … }    $bindPFFailedCase
    """

    def chainOkayOkayCase =
        (Okay(1) >> Okay(2)) ==== Okay(2)
    def chainOkayFailedCase =
        (Okay(1) >> FailedG("foo", 1)) must beFailedWith("foo", 1)
    def chainFailedOkayCase =
        (FailedG("foo", 1) >> Okay(2)) must beFailedWith("foo", 1)
    def chainFailedFailedCase =
        (FailedG("foo", 1) >> FailedG("bar", 2)) must beFailedWith("foo", 1)
    def chainFailedLazyCase =
        (FailedG("foo", 1) >> sys.error("boom")) must beFailedWith("foo", 1)

    def bindWildcardOkayCase =
        ((Okay(1): ResultG[Int, Int]) >>= { _ => Okay(2) }) ==== Okay(2)
    def bindWildcardFailedCase =
        ((FailedG("foo", 1): ResultG[Int, Int]) >>= { _ => Okay(2) }) must beFailedWith("foo", 1)
    def bindIdentOkayCase =
        ((Okay(1): ResultG[Int, Int]) >>= { a => Okay(a+1) }) ==== Okay(2)
    def bindIdentFailedCase =
        ((FailedG("foo", 1): ResultG[Int, Int]) >>= { (a: Int) => Okay(a+1) }) must beFailedWith("foo", 1)
    def bindPFOkayCase =
        ((Okay(1): ResultG[Int, Int]) >>= { case 0 => Okay(2); case 1 => Okay(3); case _ => FailedG("nope", 999) }) ==== Okay(3)
    def bindPFFailedCase =
        ((FailedG("foo", 1): ResultG[Int, Int]) >>= { case 0 => Okay(2); case 1 => Okay(3); case _ => FailedG("nope", 999) }) must beFailedWith("foo", 1)
}
