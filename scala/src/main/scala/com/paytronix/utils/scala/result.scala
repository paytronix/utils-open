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

import _root_.scala.collection.Iterator
import _root_.scala.collection.generic.CanBuild

trait resultLowPriorityImplicits {
    self: result.type =>
    // need low priority implicits so that Right("blah").toResult works.
    // if the implicits are both defined in result, then it's not known to the compiler which
    // of eitherOps or eitherOpsG applies, since Right("blah"): Either[Nothing, String], and
    // both Nothing <: Throwable (eitherOps) and Nothing <: (Throwable, E) (eitherOpsG)
    // so, make the default the more likely Result[A] case instead of ResultG[E, A]

    /** Enrich Either[(Throwable, E), A] with a .toResult method */
    implicit def eitherOpsG[E, A](in: Either[(Throwable, E), A]): EitherOps[E, A] =
        new EitherOps(in)
}

object result extends resultLowPriorityImplicits
{
    /** Usual type of ResultG used, where no additional error parameter is given */
    type Result[+A] = ResultG[Unit, A]

    /** Type of FailedG where no additional error parameter is given */
    type Failed = FailedG[Unit]

    /** Trait of things that can be attached to FailedG as parameters. Required to use the lightweight `result | param` syntax but not required otherwise. */
    trait FailedParameter

    /**
     * Like an Either that always carries a Throwable, or a Box without Empty that carries an error of type E or success of type A.
     * G suffix means "Generic".
     * Operates like an Option, Box, or right-biased Either for the most part
     */
    sealed abstract class ResultG[+E, +A] extends Product with Serializable
    {
        /**
         * If Okay and partial function applies, yields Okay with the result of the partial function,
         * otherwise Failed("partial function did not apply")
         */
        def collect[B](pf: PartialFunction[A, B]): Result[B]

        /**
         * If Okay and partial function applies, yields Okay with the result of the partial function,
         * otherwise FailedG("partial function did not apply", parameter)
         */
        def collectG[F >: E, B](parameter: F)(pf: PartialFunction[A, B]): ResultG[F, B]

        /**
         * If Okay and partial function applies, yields Okay with the result of the partial function,
         * otherwise the given result
         */
        def collectG[F >: E, B](otherwise: ResultG[F, B])(pf: PartialFunction[A, B]): ResultG[F, B]

        /**
         * If Okay and predicate yields true for the value, yields Okay again,
         * otherwise Failed("value did not pass filter")
         */
        def filter(p: A => Boolean): Result[A]

        /**
         * If Okay and predicate yields true for the value, yields Okay again,
         * otherwise FailedG("value did not pass filter", parameter)
         */
        def filterG[F >: E](parameter: F)(p: A => Boolean): ResultG[F, A]

        /** If Okay and predicate yields true for the value, yields Okay again, otherwise the given failure */
        def filterG[F >: E](otherwise: FailedG[F])(p: A => Boolean): ResultG[F, A]

        /** Converse of filter */
        def filterNot(p: A => Boolean): Result[A]

        /** Converse of filterG */
        def filterNotG[F >: E](parameter: F)(p: A => Boolean): ResultG[F, A]

        /** Converse of filterG */
        def filterNotG[F >: E](otherwise: FailedG[F])(p: A => Boolean): ResultG[F, A]

        /** If Okay, applies function to value and yields its result */
        def flatMap[F >: E, B](f: A => ResultG[F, B]): ResultG[F, B]

        /** If Okay, applies function to value and yields Unit */
        def foreach(f: A => Any): Unit

        /** If Okay, yields the value, otherwise the given default */
        def getOrElse[B >: A](default: => B): B

        /** Yields true iff Okay */
        def isDefined: Boolean

        /** Yields an Iterator of the value if Okay, an empty Iterator otherwise */
        def iterator: Iterator[A]

        /** If Okay, applies function to value and yields Okay with its result */
        def map[B](f: A => B): ResultG[E, B]

        /**
         * If Failed, then either modify the Failed somehow or replace with another Result.
         *
         * This operator can be used in a variety of ways:
         *   - wrap a failure with a new explanatory message: `rslt | "explanation"`
         *   - attach (or replace) a failure parameter that extends `FailedParameter`: `rslt | MyFailedParameter`
         *   - attach (or replace) a failure parameter: `rslt | parameter(1)`
         *   - wrap a failure with explanation and attach a parameter: `rslt | ("explanation" -> 1)`
         *   - replace a failued with some default Result: `rslt | Okay("it's fine")`
         *   - make a new failure based on the old one: `rslt | { case Failed(throwable) => FailedG(throwable, MyFailedParameter(throwable.getMessage)) }`
         */
        def | [F, B >: A] (f: FailedG[E] => ResultG[F, B]): ResultG[F, B]

        /** Alias for `|` operator when you want a named method instead of an operator */
        def orElse[F, B >: A] (f: FailedG[E] => ResultG[F, B]): ResultG[F, B] = this | f

        /** If Failed, yield null, otherwise yields the Okay value */
        def orNull[B >: A](implicit ev: Null <:< B): B

        /** If Failed, throw the enclosed exception, otherwise yield the Okay value */
        def orThrow: A

        /** Same as iterator */
        override def productIterator: Iterator[Any] = iterator

        /** Yields "Okay" */
        def productPrefix: String

        /** Yields a single element list with the Okay value, or Nil if Failed */
        def toList: List[A]

        /** Yields Some if Okay, None otherwise */
        def toOption: Option[A]

        /** Yields Right if Okay, Left otherwise */
        def toEither: Either[(Throwable, E), A]

        /** Lazy filtering for Scala 2.8 for comprehensions */
        def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

        final class WithFilter(p: A => Boolean) {
            def map[B](f: A => B): Result[B] = ResultG.this filter p map f
            def flatMap[B](f: A => Result[B]): Result[B] = ResultG.this filter p flatMap f
            def foreach[U](f: A => U): Unit = ResultG.this filter p foreach f
            def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
        }

        /** If Okay, replace the value with the given alternative. Equivalent to flatMap(_ => alternative) */
        def then[F >: E, B](alternative: ResultG[F, B]): ResultG[F, B]

        /** Yield true iff Okay and the value conforms to the given type */
        def isA[B](implicit m: Manifest[B]): Boolean

        /** Succeed with the value cast to the given type if it can be, otherwise fail with "expected a <given type>" */
        def asA[B](implicit m: Manifest[B]): Result[B]

        /** Succeed with the value cast to the given type if it can be, otherwise the given result */
        def asAG[F >: E, B](clazz: Class[B], parameter: F): ResultG[F, B]

        /** Succeed with the value cast to the given type if it can be, otherwise FailedG("expected a <given type>", parameter) */
        def asAG[F >: E, B](clazz: Class[B], otherwise: ResultG[F, B]): ResultG[F, B]

        /** Pass this to the function ignoring its result */
        def pass(f: ResultG[E, A] => Any): ResultG[E, A]

        /** If Okay with a Okay then yield Okay with the inner value. Equivalent to flatMap(x => x) */
        def flatten[F >: E, B](implicit ev: A => ResultG[F, B]): ResultG[F, B]

        /** "Right join" this result of iterable with some other iterable */
        def rightJoin[B, C, That](rhs: Iterable[C])(implicit cb: CanBuild[(ResultG[E, B], C), That], ev: A => Iterable[B]): That = {
            val builder = cb()
            val leftIteratorResult = this.map(a => ev(a).iterator)
            val rightIterator = rhs.iterator
            while (leftIteratorResult.map(_.hasNext).getOrElse(true) && rightIterator.hasNext) {
                builder += ((leftIteratorResult.map(_.next), rightIterator.next))
            }
            builder.result()
        }
    }

    object ResultG
    {
        /** Wrap the value with Okay if non-null, Failed("value was null") if null */
        def apply[A](in: A) = if (in != null) Okay(in) else Failed("value was null")

        /** Implicitly convert ResultG to Seq, for flatten and similar */
        implicit def resultAsSeq[E, A](in: ResultG[E, A]): Seq[A] =
            in match {
                case Okay(value) => Seq(value)
                case _ => Seq.empty
            }

        /** Allow a string to be used with ResultG's `|` operator to wrap a failure with a new explanatory message. For example: `rslt | "explanation"` */
        implicit def stringAsFailedMessage[E](s: String): FailedG[E] => FailedG[E] =
            failed => FailedG(s, failed.throwable, failed.parameter)

        /** Allow a FailedParameter to be used with ResultG's `|` operator to attach or replace a parameter. For example `rslt | MyFailedParameter("foo")` */
        implicit def failedParameterAsParameter[E <: FailedParameter](parameter: E): FailedG[Any] => FailedG[E] =
            failed => FailedG(failed.throwable, parameter)

        /**
         * Allow a pair (usually written with `->`) to be used with ResultG's `|` operator to wrap a failure with a new message and parameter.
         * For example `rslt | ("explanation" -> param)`
         */
        implicit def pairAsFailed[E](pair: (String, E)): FailedG[Any] => FailedG[E] =
            failed => FailedG(pair._1, failed.throwable, pair._2)

        /** Allow a ResultG to be used with ResultG's `|` to replace a failure with some alternate. For example `rslt | Okay("default value")` */
        implicit def resultAsReplacement[E, A](replacement: ResultG[E, A]): FailedG[Any] => ResultG[E, A] =
            _ => replacement
    }

    /** Allow any value to be used with ResultG's `|` to attach or replace a failure parameter. For example `rslt | parameter(1)` */
    def parameter[E](parameter: E): FailedG[Any] => FailedG[E] =
        failed => FailedG(failed.throwable, parameter)

    /** Result of a successful computation */
    final case class Okay[+A](result: A) extends ResultG[Nothing, A]
    {
        def collect[B](pf: PartialFunction[A, B]): Result[B] =
            if (pf.isDefinedAt(result)) Okay(pf(result)) else Failed("partial function did not apply to value")

        def collectG[F, B](otherwise: ResultG[F, B])(pf: PartialFunction[A, B]): ResultG[F, B] =
            if (pf.isDefinedAt(result)) Okay(pf(result)) else otherwise

        def collectG[F, B](parameter: F)(pf: PartialFunction[A, B]): ResultG[F, B] =
            if (pf.isDefinedAt(result)) Okay(pf(result)) else FailedG("partial function did not apply to value", parameter)

        def filter(p: A => Boolean): Result[A] =
            if (p(result)) Okay(result) else Failed("value did not pass filter")

        def filterG[F](parameter: F)(p: A => Boolean): ResultG[F, A] =
            if (p(result)) Okay(result) else FailedG("value did not pass filter", parameter)

        def filterG[F](otherwise: FailedG[F])(p: A => Boolean): ResultG[F, A] =
            if (p(result)) Okay(result) else otherwise

        def filterNot(p: A => Boolean): Result[A] =
            if (!p(result)) Okay(result) else Failed("value did not pass filter")

        def filterNotG[F](parameter: F)(p: A => Boolean): ResultG[F, A] =
            if (!p(result)) Okay(result) else FailedG("value did not pass filter", parameter)

        def filterNotG[F](otherwise: FailedG[F])(p: A => Boolean): ResultG[F, A] =
            if (!p(result)) Okay(result) else otherwise

        def asA[B](implicit m: Manifest[B]): Result[B] =
            tryCatching[ClassCastException].value {
                m.erasure.cast(result).asInstanceOf[B]
            } | Failed("expected a " + m + " but got a " + (result.asInstanceOf[AnyRef] match {
                case null => "null"
                case other => other.getClass.getName
            }))

        def asAG[F, B](clazz: Class[B], parameter: F): ResultG[F, B] =
            tryCatching[ClassCastException].value {
                clazz.cast(result).asInstanceOf[B]
            } | FailedG("expected a " + clazz.getName + " but got a " + (result.asInstanceOf[AnyRef] match {
                case null => "null"
                case other => other.getClass.getName
            }), parameter)

        def asAG[F, B](clazz: Class[B], otherwise: ResultG[F, B]): ResultG[F, B] =
            tryCatching[ClassCastException].value {
                clazz.cast(result).asInstanceOf[B]
            } orElse otherwise

        def flatMap[E, B](f: A => ResultG[E, B]): ResultG[E, B]             = f(result)
        def foreach(f: A => Any): Unit                                      = { f(result); () }
        def getOrElse[B >: A](default: => B): B                             = result
        def isDefined: Boolean                                              = true
        def iterator: Iterator[A]                                           = Iterator.single(result)
        def map[B](f: A => B): Okay[B]                                      = Okay(f(result))
        def | [F, B >: A] (f: FailedG[Nothing] => ResultG[F, B]): ResultG[F, B] = this
        def orNull[B >: A](implicit ev: Null <:< B): B                      = result
        def orThrow: A                                                      = result
        override def productPrefix: String                                  = "Okay"
        def toList: List[A]                                                 = result :: Nil
        def toOption: Option[A]                                             = Some(result)
        def toEither: Right[Nothing, A]                                     = Right(result)
        def then[E, B](alternative: ResultG[E, B]): ResultG[E, B]           = alternative
        def isA[B](implicit m: Manifest[B]): Boolean                        = m.erasure.isInstance(result)
        def pass(f: ResultG[Nothing, A] => Any): Okay[A]                    = { f(this); this }
        def flatten[E, B](implicit ev: A => ResultG[E, B]): ResultG[E, B]   = ev(result)

        override def toString =
            if (result == ()) "Okay"
            else "Okay(" + (try { String.valueOf(result) } catch { case _: Exception => "<failed .toString>" }) + ")"
    }

    /** Result of a failed computation */
    final case class FailedG[+E](throwable: Throwable, parameter: E) extends ResultG[E, Nothing]
    {
        def collect[B](pf: PartialFunction[Nothing, B]): Failed                   = Failed(throwable)
        def collectG[F >: E, B](parameter: F)(pf: PartialFunction[Nothing, B]): FailedG[F] = this
        def collectG[F >: E, B](otherwise: ResultG[F, B])(pf: PartialFunction[Nothing, B]): FailedG[F] = this
        def filter(p: Nothing => Boolean): Failed                                 = Failed(throwable)
        def filterG[F >: E](parameter: F)(p: Nothing => Boolean): FailedG[F]      = this
        def filterG[F >: E](otherwise: FailedG[F])(p: Nothing => Boolean): FailedG[F] = this
        def filterNot(p: Nothing => Boolean): Failed                              = Failed(throwable)
        def filterNotG[F >: E](parameter: F)(p: Nothing => Boolean): FailedG[F]   = this
        def filterNotG[F >: E](otherwise: FailedG[F])(p: Nothing => Boolean): FailedG[F] = this
        def flatMap[F >: E, B](f: Nothing => ResultG[F, B]): FailedG[F]           = this
        def foreach(f: Nothing => Any): Unit                                      = ()
        def getOrElse[B](default: => B): B                                        = default
        def isDefined: Boolean                                                    = false
        def iterator: Iterator[Nothing]                                           = Iterator.empty
        def map[B](f: Nothing => B): FailedG[E]                                   = this
        def | [F, B] (f: FailedG[E] => ResultG[F, B]): ResultG[F, B]              = f(this)
        def orNull[B](implicit ev: Null <:< B): B                                 = ev(null)
        override def productPrefix: String                                        = "Failed"
        def toList: List[Nothing]                                                 = Nil
        def toOption: Option[Nothing]                                             = None
        def toEither: Left[(Throwable, E), Nothing]                               = Left((throwable, parameter))
        def orThrow: Nothing                                                      = throw throwable
        def then[F >: E, B](alternative: ResultG[F, B]): FailedG[F]               = this
        def isA[B](implicit m: Manifest[B]): Boolean                              = false
        def asA[B](implicit m: Manifest[B]): Failed                               = Failed(throwable)
        def asAG[F >: E, B](clazz: Class[B], parameter: F): ResultG[F, B]         = this
        def asAG[F >: E, B](clazz: Class[B], otherwise: ResultG[F, B]): ResultG[F, B] = this
        def pass(f: ResultG[E, Nothing] => Any): FailedG[E]                       = { f(this); this }
        def flatten[F >: E, B](implicit ev: Nothing => ResultG[F, B]): FailedG[F] = this

        // need to override case class equality because throwables don't compare well
        override def equals(other: Any): Boolean = {
            def equalThrowable(a: Throwable, b: Throwable): Boolean =
                a == b || (
                    a != null && b != null &&
                    a.getClass == b.getClass &&
                    a.getMessage == b.getMessage &&
                    equalThrowable(a.getCause, b.getCause)
                )

            other match {
                case FailedG(otherThrowable, otherParameter) =>
                    parameter == otherParameter && equalThrowable(throwable, otherThrowable)
                case _ => false
            }
        }

        override def toString = {
            def causedBy(t: Throwable): String =
                if (t.getCause != null) " caused by " + t.getCause + causedBy(t.getCause)
                else ""

            if (parameter == ()) "Failed(" + throwable + causedBy(throwable) + ")"
            else                 "FailedG(" + throwable + causedBy(throwable) + ", " + parameter + ")"
        }
    }

    /** Exception generated when FailedG or Failed is given a message string rather than a Throwable. Formats slightly more cleanly */
    class FailedException(message: String, cause: Throwable = null) extends RuntimeException(message, cause) {
        override def toString: String = message
    }

    object FailedG
    {
        /** Construct a FailedG using a message rather than an exception */
        def apply[E](message: String, parameter: E): FailedG[E] =
            FailedG(new FailedException(message), parameter)

        /** Construct a FailedG using a message with a cause rather than an exception */
        def apply[E](message: String, cause: Throwable, parameter: E): FailedG[E] =
            FailedG(new FailedException(message, cause), parameter)

        /** Construct a FailedG using a message with a cause rather than an exception */
        def apply[E](message: String, cause: FailedG[_], parameter: E): FailedG[E] =
            FailedG(new FailedException(message, cause.throwable), parameter)
    }

    object Failed
    {
        /** Construct a FailedG using the given message and no argument (unit) */
        def apply(message: String): Failed = FailedG(message, ())

        /** Construct a FailedG using the given message with a cause and no argument (unit) */
        def apply(message: String, cause: Throwable): Failed = FailedG(new FailedException(message, cause), ())

        /** Construct a FailedG using the given message with a cause and no argument (unit) */
        def apply(message: String, cause: FailedG[_]): Failed = FailedG(new FailedException(message, cause.throwable), ())

        /** Construct a FailedG using the given throwable and no argument (unit) */
        def apply(throwable: Throwable): Failed = FailedG(throwable, ())

        /** Extract a cause throwable from a Result */
        def unapply(in: Result[_]): Option[Throwable] =
            in match {
                case FailedG(throwable, _) => Some(throwable)
                case _                     => None
            }

        object Message {
            /** Extract a failure message from a Result */
            def unapply(in: Result[_]): Option[String] =
                in match {
                    case FailedG(throwable, _) => Option(throwable.getMessage) orElse Some("unknown error")
                    case _                     => None
                }
        }
    }

    /** Enrich Options with a .toResult method */
    implicit def optionOps[A](in: Option[A]): OptionOps[A] = new OptionOps(in)

    /** Enrichment of Option that provides .toResult */
    class OptionOps[A](in: Option[A]) {
        /** Convert Some to Okay, and None to Failed("option was none") */
        def toResult: Result[A] =
            in match {
                case Some(a) => Okay(a)
                case None    => Failed("option was none")
            }
    }


    /** Enrich Either[Throwable, A] with a .toResult method */
    implicit def eitherOps[A](in: Either[Throwable, A]): EitherOps[Unit, A] =
        new EitherOps(in.left.map(t => (t, ())))

    /** Enrichment of Either with a .toResult method */
    class EitherOps[E, A](in: Either[(Throwable, E), A]) {
        /** Convert Left to Failed and Right to Okay */
        def toResult: ResultG[E, A] =
            in match {
                case Left((t, e)) => FailedG(t, e)
                case Right(a) => Okay(a)
            }
    }

    /**
     * Catch any [[java.lang.Exception]] in a block, yielding Failed when a [[java.lang.Exception]] is caught.
     * Use `tryCatch.value` when the function yields some A which should be wrapped in Okay on success, and `tryCatch.result` if the function yields a
     * [[com.paytronix.utils.scala.result.Result]].
     *
     * Examples:
     * {{{
     *     tryCatch.value { "foobar" }            == Okay("foobar")
     *     tryCatch.value { Okay("foobar") }      == Okay(Okay("foobar"))
     *     tryCatch.value { sys.error("oh no") }  == Failed(new RuntimeException("oh no"))
     *     tryCatch.result { "foobar" }           // type error since String is not <: Result[_]
     *     tryCatch.result { Okay("foobar") }     == Okay("foobar")
     *     tryCatch.result { Failed("why") }      == Failed(new FailedException("why"))
     *     tryCatch.result { sys.error("oh no") } == Failed(new RuntimeException("oh no"))
     * }}}
     */
    def tryCatch: TryCatch[Unit, Nothing] =
        tryCatching[Exception]

    /**
     * Catch throwable of type T in a block and yield Okay if no exception caught, Failed if an exception of the given type caught.
     * Use `tryCatching[E].value` when the function yields some A which should be wrapped in Okay on success, and `tryCatching[E].result` if the function yields a
     * [[com.paytronix.utils.scala.result.Result]].
     *
     * Examples:
     * {{{
     *     tryCatching[FailedException].value { "foobar" }                         == Okay("foobar")
     *     tryCatching[FailedException].value { Okay("foobar") }                   == Okay(Okay("foobar"))
     *     tryCatching[FailedException].value { throw new FailedException("why") } == Failed(new FailedException("why"))
     *     tryCatching[FailedException].value { sys.error("oh no") }               // throws RuntimeException("oh no")
     *     tryCatching[FailedException].result { "foobar" }                        // type error since String is not <: Result[_]
     *     tryCatching[FailedException].result { Okay("foobar") }                  == Okay("foobar")
     *     tryCatching[FailedException].result { Failed("why") }                   == Failed(new FailedException("why"))
     *     tryCatching[FailedException].result { sys.error("oh no") }              // throws RuntimeException("oh no")
     * }}}
     * Example: tryCatching[FooException].value { do some things }
     */
    def tryCatching[T <: Throwable](implicit m: Manifest[T]): TryCatch[Unit, Nothing] =
        TryCatch { case t if m.erasure.isInstance(t) => Failed(t) }

    /**
     * Catch only throwables of the given classes in a block, yielding Okay if no exception caught, Failed if one of those exception types caught.
     * Example: tryCatching(classOf[FooException], classOf[BarException]).value { ... }
     */

    def tryCatching(throwables: Class[_ <: Throwable]*): TryCatch[Unit, Nothing] =
        TryCatch { case t if throwables.exists(_.isInstance(t)) => Failed(t) }

    /** Generic catcher which turns exceptions into ResultGs via a partial function */
    final case class TryCatch[+E, +A](catchPF: PartialFunction[Throwable, ResultG[E, A]]) {
        def value[B >: A](f: => B): ResultG[E, B] =
            try { Okay(f) } catch catchPF
        def result[B >: A, F >: E](f: => ResultG[F, B]): ResultG[F, B] =
            try f catch catchPF
    }

    /**
     * Return the first application of the given function to each item from the given {@link Iterable} that is a {@link Okay} {@link ResultG}, or the last
     * application if none result in {@link Okay}
     */
    def firstOrLast[A, B](xs: Iterable[A])(f: (A) => Result[B]): Result[B] =
        firstOrLastG(Failed("no result to yield"), xs)(f)

    /**
     * Return the first application of the given function to each item from the given {@link Iterable} that is a {@link Okay} {@link ResultG}, or the last
     * application if none result in {@link Okay}
     */
    def firstOrLastG[A, B, E](default: ResultG[E, B], xs: Iterable[A])(f: (A) => ResultG[E, B]): ResultG[E, B] = {
        var last: ResultG[E, B] = default
        for (x <- xs) {
            f(x) match {
                case Okay(y) => return Okay(y)
                case other => last = other
            }
        }
        last
    }

    /** Implicitly convert an iterable to a IterableResultOps with various extended behaviors  */
    implicit def iterableResultOps[A](iterable: Iterable[A]): IterableResultOps[A] = IterableResultOps(iterable)

    /** Extension of Iterable that can perform various Result-oriented transforms or operations */
    final case class IterableResultOps[A](iterable: Iterable[A]) {
        /** Sequence each computation in the iterable, yielding Okay iff all computations yield Okay */
        def sequenceResult[E]()(implicit ev: A <:< (() => ResultG[E, Unit])): ResultG[E, Unit] =
            foreachResult(a => ev(a)())

        /**
         * Sequence a chain of possibly-failing computations with side effects by applying f to each element of seq in turn. If any application of
         * f results in Failed, then the chain is stopped there and that result returned. Otherwise Okay(()) is returned.
         */
        def foreachResult[E](f: A => ResultG[E, Unit]): ResultG[E, Unit] =
            foldLeftResult[E, Unit](())((_, a) => f(a))

        /**
         * Collect the result of a series of possibly-failing computations, returning Okay(seq) in the case where all succeed, or the first non-Okay
         * result otherwise.
         */
        def mapResult[E, B, That](f: A => ResultG[E, B])(implicit cb: CanBuild[B, That]): ResultG[E, That] = {
            val builder = cb()
            foreachResult(a => f(a).map(builder += _)).map(_ => builder.result())
        }

        /**
         * Fold a chain of possibly-failing computations, short circuiting the fold to Failed on the first computation that results in one.
         * Sequence a chain of possibly-failing computations with side effects by applying f to each element of seq in turn. If any application of
         * f results in Failed, then the chain is stopped there and that result returned. Otherwise Okay(result) is returned.
         */
        def foldLeftResult[E, B](init: B)(f: (B, A) => ResultG[E, B]): ResultG[E, B] = {
            // Manually implement rather than use fold so inlining, TCO, and early shortcut can apply
            val iterator = iterable.iterator
            def iterate(prev: ResultG[E, B]): ResultG[E, B] =
                prev match {
                    case Okay(b) if iterator.hasNext => iterate(f(b, iterator.next))
                    case _ => prev
                }
            iterate(Okay(init))
        }

        /**
         * Apply a possibly-failing computation to each element of the iterable, calling some function on non-Okays and collecting the Okays.
         * Usually the error handling function is some side-effecting function, e.g. logging.
         */
        def flattenResult[E, B, That](onError: (A, FailedG[E]) => Unit)(f: A => ResultG[E, B])(implicit cb: CanBuild[B, That]): That = {
            val builder = cb()
            val iterator = iterable.iterator
            while (iterator.hasNext) {
                val a = iterator.next
                f(a) match {
                    case Okay(b) => builder += b
                    case failed@FailedG(_, _) => onError(a, failed)
                }
            }
            builder.result()
        }

        /** "Left join" this iterable with a potentially missing right hand side. */
        def leftJoin[E, B, That](right: ResultG[E, Iterable[B]])(implicit cb: CanBuild[(A, ResultG[E, B]), That]): That = {
            val builder = cb()
            val leftIterator = iterable.iterator
            val rightIteratorResult = right.map(_.iterator)
            while (leftIterator.hasNext && rightIteratorResult.map(_.hasNext).getOrElse(true)) {
                builder += ((leftIterator.next, rightIteratorResult.map(_.next)))
            }
            builder.result()
        }
    }

}
