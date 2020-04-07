//
// Copyright 2014-2020 Paytronix Systems, Inc.
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

package com.paytronix.utils.validation

import cats.data.NonEmptyList
import cats.data.Validated.Invalid
import org.specs2.SpecificationWithJUnit
import shapeless.HNil

import com.paytronix.utils.scala.result.{FailedException, FailedG, Okay}

import base.{Validated, ValidationError, field, validate}

object baseSpecFixtures {
    val nope = ValidationError("nope")
    val reallyNope = ValidationError("reallyNope")
}

import baseSpecFixtures._

class validationErrorTest extends SpecificationWithJUnit {
    def is = s2"""`ValidationError` should
        construct with just a message      $e1
        construct with a message and code  $e2
        construct with everything          $e3
        nest by adding a field segment     $e4
        attach invalid input when given    $e5
        convert to a string                $e6
    """

    def e1 = ValidationError.unapply(ValidationError("m")) ==== Some(("m", "m", Nil, None))
    def e2 = ValidationError.unapply(ValidationError("c", "m")) ==== Some(("c", "m", Nil, None))
    def e3 = ValidationError.unapply(ValidationError("c", "m", List("l"), Some("o"))) ==== Some(("c", "m", List("l"), Some("o")))
    def e4 = ValidationError("m").nest("foo").nest("bar").location ==== List("bar", "foo")
    def e5 = ValidationError("m").withInvalidInput("yo").invalidInput ==== Some("yo")
    def e6 = ValidationError("c", "m").nest("foo").nest("bar").toString ==== "At bar/foo: c: m"
}

class fieldTest extends SpecificationWithJUnit {
    def is = s2"""field validation support
        should attach field names with field("name", v)      $field1
        should attach field names with field("name", f)      $field2
        should attach field names with field("name", map, f) $field3

        should succeed when each field succeeded         $validate1
        should fail when any field is failed             $validate2
        should accumulate errors from all failed fields  $validate3
    """

    def field1 = field("g", field("f", base.failure(ValidationError("foo")))) must beLike {
        case Invalid(NonEmptyList(failure, Nil)) => failure.location ==== List("g", "f")
    }
    def field2 = field("g", field("f", (x: Int) => base.failure(ValidationError("foo"))))(123) must beLike {
        case Invalid(NonEmptyList(failure, Nil)) => failure.location ==== List("g", "f")
    }
    def field3 = field("f", Map("f" -> 1), base.success[Int] _ and { (x: Int) => base.failure(ValidationError("foo")) }) must beLike {
        case Invalid(NonEmptyList(failure, Nil)) => failure.location ==== List("f")
    }

    def validate1 = validate(base.success(1) :: base.success('a') :: base.success("c") :: HNil) ==== base.success(1 :: 'a' :: "c" :: HNil)
    def validate2 = validate(base.success(1) :: base.failure(nope) :: base.success("c") :: HNil) ==== base.failure(nope)
    def validate3 = validate(base.success(1) :: base.failure(nope) :: base.failure(reallyNope) :: HNil) ==== base.failure(nope, reallyNope)
}

class onlyAssertTest extends SpecificationWithJUnit {
    def is = s2"""`onlyAssert`
        should pass the value through unchanged  $e1
        but fail when the assertion fails        $e2
    """

    def e1 = base.onlyAssert[Int](a => base.success("foo"))(1) ==== base.success(1)
    def e2 = base.onlyAssert[Int](a => base.failure(nope))(1) ==== base.failure(nope)
}

class whenTest extends SpecificationWithJUnit {
    def is = s2"""`when`
        should not apply the validation function on false  $e1
        should apply the validation function on true       $e2
    """

    def e1 = base.when[Int](false)(a => sys.error("evaluated but shouldn't be"))(1) ==== base.success(1)
    def e2 = base.when[Int](true )(a => base.success(a+1))(1) ==== base.success(2)
}

class predicateETest extends SpecificationWithJUnit {
    def is = s2"""`predicateE`
        should succeed when the predicate is true $e1
        and fail when the predicate is not true   $e2
    """

    def e1 = base.predicateE[Int](nope)(_ => true )(1) ==== base.success(1)
    def e2 = base.predicateE[Int](nope)(_ => false)(1) ==== base.failure(nope)
}

class anyTest extends SpecificationWithJUnit {
    def is = s2"""`any[E]`
        should pick the first   $e1
        or the second           $e2
        or the default          $e3
        or the given default    $e4
    """

    val f: Int => Validated[String] = {
        case 1 => base.success("foo")
        case _ => base.failure(nope)
    }

    val g: Int => Validated[String] = {
        case 2 => base.success("bar")
        case _ => base.failure(nope)
    }

    def e1 = base.any(NonEmptyList.of(f,g))(1) ==== base.success("foo")
    def e2 = base.any(NonEmptyList.of(f,g))(2) ==== base.success("bar")
    def e3 = base.any(NonEmptyList.of(f,g))(3) ==== base.failure(base.generalError)
    def e4 = base.anyE(reallyNope)(NonEmptyList.of(f,g))(3) ==== base.failure(reallyNope)
}

class validatedTest extends SpecificationWithJUnit {
    def is = s2"""`Validated`
        should apply functions with `and`   $and1
        should convert to Okay (G)          $toResultG1
        should convert to FailedG           $toResultG2
        should convert to Okay              $toResult1
        should convert to Failed            $toResult2
        should convert to a value           $orThrow1
        should convert to an exception      $orThrow2
    """

    def and1 = (base.success("foo") and { s => base.success(s + "bar") }) ==== base.success("foobar")
    def toResultG1 = (base.success("foo"): Validated[String]).toResultG ==== Okay("foo")
    def toResultG2 = (base.failure(nope, reallyNope): Validated[String]).toResultG must beLike { case FailedG(_, p) => p ==== NonEmptyList.of(nope, reallyNope) }
    def toResult1 = (base.success("foo"): Validated[String]).toResult ==== Okay("foo")
    def toResult2 = (base.failure(nope, reallyNope): Validated[String]).toResult must beLike { case FailedG(t, ()) => t.getMessage ==== base.validationErrorsToString(NonEmptyList.of(nope, reallyNope)) }
    def orThrow1 = (base.success("foo"): Validated[String]).orThrow ==== "foo"
    def orThrow2 = (base.failure(nope): Validated[String]).orThrow must throwA[FailedException]
}
