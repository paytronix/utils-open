//
// Copyright 2012-2020 Paytronix Systems, Inc.
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

import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions

import scalaz.{NonEmptyList, Failure, Success}
import shapeless.HList

import base.{Validated, ValidationError, failure, field, predicateE, success}

object sequence {
    def tooShortError(i: Int) = ValidationError("too_short", s"must have at least $i element(s)")
    def tooLongError(i: Int)  = ValidationError("too_long", s"must have no more than $i element(s)")

    /** Assert that some iterable is at least some size */
    def noShorterThan[A <: Iterable[_]](limit: Int): A => Validated[A] =
        noShorterThanE(tooShortError)(limit)

    /** Assert that some iterable is at least some size */
    def noShorterThanE[A <: Iterable[_]](error: Int => ValidationError)(limit: Int): A => Validated[A] =
        predicateE(error(limit))(_.size >= limit)

    /** Assert that some iterable is no longer than some size */
    def noLongerThan[A <: Iterable[_]](limit: Int): A => Validated[A] =
        noLongerThanE(tooLongError)(limit)

    /** Assert that some iterable is no longer than some size */
    def noLongerThanE[A <: Iterable[_]](error: Int => ValidationError)(limit: Int): A => Validated[A] =
        predicateE(error(limit))(_.size <= limit)

    /** Split some input string by a delimiter */
    def delimitedBy(delimiter: String, strict: Boolean = false, maxSplits: Int = 0): String => Validated[List[String]] =
        s => {
            val builder = List.newBuilder[String]
            val in = if (strict) s else s.trim
            val limit = in.length
            val delimiterLength = delimiter.length
            var count = 0
            var start = 0

            while (start < limit) {
                in.indexOf(delimiter, start) match {
                    case -1 =>
                        builder += in.substring(start, limit)
                        start = limit

                    case pos if strict || pos > start =>
                        if (maxSplits > 0 && count == maxSplits) {
                            builder += in.substring(start, limit)
                            start = limit
                        } else {
                            builder += in.substring(start, pos)
                            count += 1
                            start = pos + delimiterLength
                        }

                    case pos =>
                        start = pos + delimiterLength
                }
            }

            success(builder.result())
        }

    /**
     * Apply some validation to every value in the given input collection.
     * This is the composable version that is intended to be used with "and".
     * See applyToEach for the version that's more sane to use when applying directly to values
     */
    def eachIs[A, B, InColl <: Iterable[A], OutColl](f: A => Validated[B])(implicit cbf: CanBuildFrom[InColl, B, OutColl]): InColl => Validated[OutColl] =
        in => {
            val results = cbf(in)
            val allErrors = List.newBuilder[ValidationError]
            val iter = in.iterator
            while (iter.hasNext) {
                f(iter.next) match {
                    case Failure(errors) => errors.foreach { error => allErrors += error; () }
                    case Success(result) => results += result
                }
            }
            allErrors.result() match {
                case Nil                     => success(results.result())
                case someError :: someErrors => failure(someError, someErrors: _*)
            }
        }
    /**
     * Apply some validation to every value in the given input collection.
     * This is the apply-able version that is not intended to be used in composition using "and".
     * See each for the composable version
     */
    def applyToEach[A, B, InColl <: Iterable[A], OutColl](in: InColl, f: A => Validated[B])(implicit cbf: CanBuildFrom[InColl, B, OutColl]): Validated[OutColl] =
        eachIs[A, B, InColl, OutColl](f)(cbf)(in)

    // undocumented territory! ask me later!

    import numeric.int
    import shapeless.{::, HList, HNil, Poly2}
    import shapeless.ops.hlist.{LeftFolder, Reverse}

    val exhaustedInput = ValidationError("exhausted_input", "exhausted input")
    def unexpectedInput(got: String) = ValidationError("unexpected_input", "expected end of input, but got: " + got)

    final case class ListParsingResult[A, +B](rest: List[A], result: B) {
        def mapResult[C](f: B => C): ListParsingResult[A, C] = copy(result = f(result))
    }
    type ListParsingFunction[A, +B] = List[A] => Validated[ListParsingResult[A, B]]

    implicit def validationToSingleListParser[A, B](f: A => Validated[B]): ListParsingFunction[A, B] = single(f)

    def single[A, B](f: A => Validated[B]): ListParsingFunction[A, B] = {
        case Nil => failure(exhaustedInput)
        case scala.::(a, rest) =>
            f(a) match {
                case Failure(errors) if a.isInstanceOf[String] =>
                    Failure(errors.map(_.withInvalidInput(a.asInstanceOf[String])))
                case Failure(errors) =>
                    Failure(errors)
                case Success(b) =>
                    success(ListParsingResult(rest, b))
            }
    }

    def fixed[A, B](count: Int)(f: ListParsingFunction[A, B]): ListParsingFunction[A, List[B]] =
        repeating[A, B]((index, _) => index >= count)(f)

    def rest[A, B](f: ListParsingFunction[A, B]): ListParsingFunction[A, List[B]] =
        repeating[A, B]((_, rest) => rest.isEmpty)(f)

    def counted[B](f: ListParsingFunction[String, B]): ListParsingFunction[String, List[B]] =
        field("count", single(int)) and { case ListParsingResult(rest, count) => fixed(count)(f)(rest) }

    def repeating[A, B](terminate: (Int, List[A]) => Boolean)(f: ListParsingFunction[A, B]): ListParsingFunction[A, List[B]] =
        in => {
            val results = List.newBuilder[B]

            var index = 0
            var rest = in
            var errors: Option[NonEmptyList[ValidationError]] = None
            while (!errors.isDefined && !terminate(index, rest)) {
                field(index.toString, f(rest)) match {
                    case Failure(errs) => errors = Some(errs)
                    case Success(ListParsingResult(newRest, result)) =>
                        results += result
                        rest = newRest
                }
                index += 1
            }

            errors map Failure.apply getOrElse success(ListParsingResult(rest, results.result()))
        }

    // these bits following are a bit clunky, but work. come scala 2.10, we'll have macros and then this mess can be cleaned up.

    object compositionFold extends Poly2 {
        def fold[A, B, L <: HList](prev: ListParsingFunction[A, L], f: ListParsingFunction[A, B]): ListParsingFunction[A, B :: L] =
            in => prev(in) and { case ListParsingResult(rest, result) =>
                f(rest).map { res => ListParsingResult(res.rest, res.result :: result) }
            }

        implicit def validationFunction[A, B, L <: HList] = at[ListParsingFunction[A, L], A => Validated[B]]((prev, f) => fold[A, B, L](prev, single(f)))
        implicit def listParsingFunction[A, B, L <: HList] = at[ListParsingFunction[A, L], ListParsingFunction[A, B]]((prev, f) => fold[A, B, L](prev, f))
    }

    def listOf[A]: ListParser[A] = new ListParser[A]
    final class ListParser[A] {
        def composedOf[L <: HList](fs: L)(implicit folder: LeftFolder[L, ListParsingFunction[A, HNil], compositionFold.type]): folder.Out =
            folder(fs, (in => Success(ListParsingResult(in, HNil))): ListParsingFunction[A, HNil])
    }

    implicit class resultReverser[A, B <: HList](val lpf: ListParsingFunction[A, B]) extends AnyVal {
        def reverseResult(implicit reverse: Reverse[B]): ListParsingFunction[A, reverse.Out] =
            in => lpf(in).map { _ mapResult (l => reverse(l)) }
    }

    implicit class fullyParse[A, B](val lpf: ListParsingFunction[A, B]) extends AnyVal {
        def mustFullyParse: List[A] => Validated[B] =
            in => lpf(in) and {
                case ListParsingResult(Nil, result) => success(result)
                case ListParsingResult(rest, _)     => failure(unexpectedInput(rest.toString))
            }
    }
}

