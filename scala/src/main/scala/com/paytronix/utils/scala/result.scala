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

package com.paytronix.utils.scala

import language.{higherKinds, implicitConversions}
import language.experimental.macros

import _root_.scala.annotation.implicitNotFound
import _root_.scala.collection.Iterator
import _root_.scala.collection.generic.CanBuild
import _root_.scala.reflect.{ClassTag, classTag}

import scalaz.{\/, -\/, \/-, Applicative, Foldable, Monad, Traverse, Functor, Scalaz}

// FIXME? gotta be a better way
import scala.annotation.unchecked.uncheckedVariance
import scala.language.higherKinds

object result {
    /** Usual type of ResultG used, where no additional error parameter is given */
    type Result[+A] = ResultG[Unit, A]

    /** Type of FailedG where no additional error parameter is given */
    type Failed = FailedG[Unit]

    /** Trait of things that can be attached to FailedG as parameters. Required to use the lightweight `result | param` syntax but not required otherwise. */
    trait FailedParameter

    val filterFailure: String = "value did not pass filter"

    /** Type class that provides a default failed parameter value, for functions that can't take one explicitly (like filter) */
    @implicitNotFound(msg = "Cannot find FailedParameterDefault for type ${E}. If ${E} is Nothing, this probably indicates you have something like Okay(...).filter (maybe automatically inserted for you by pattern matching), and you should add .withFailedType[Unit] to your Okay")
    trait FailedParameterDefault[E] {
        def default: E
    }

    object FailedParameterDefault {
        /** Construct a FailedParameterDefault */
        def value[E](v: E): FailedParameterDefault[E] = new FailedParameterDefault[E] { val default = v }

        /** Implicitly provide a Unit failed parameter */
        implicit val unitFailedParameterDefault: FailedParameterDefault[Unit] = new FailedParameterDefault[Unit] {
            def default = ()
        }

        /** Implicitly provide an Option failed parameter */
        implicit def optionFailedParameterDefault[A]: FailedParameterDefault[Option[A]] = new FailedParameterDefault[Option[A]] {
            def default = None
        }

        /** Implicitly provide a None failed parameter */
        implicit def noneFailedParameterDefault[A]: FailedParameterDefault[None.type] = new FailedParameterDefault[None.type] {
            def default = None
        }

        /** Implicitly provide a List failed parameter */
        implicit def listFailedParameterDefault[A]: FailedParameterDefault[List[A]] = new FailedParameterDefault[List[A]] {
            def default = Nil
        }

        /** Implicitly provide a Nil failed parameter */
        implicit val nilFailedParameterDefault: FailedParameterDefault[Nil.type] = new FailedParameterDefault[Nil.type] {
            def default = Nil
        }
    }

    /**
     * Like an Either that always carries a Throwable, or a Box without Empty that carries an error of type E or success of type A.
     * G suffix means "Generic".
     * Operates like an Option, Box, or right-biased Either (\/) for the most part
     */
    sealed abstract class ResultG[+E, +A] extends Product with Serializable {
        /**
         * Fix the failure type of a `ResultG`, since `Okay(value)` will be typed `Okay[A] <: ResultG[Nothing, A]` and you might
         * want some particular type instead of `Nothing`
         */
        def withFailedType[F](implicit ev: E @uncheckedVariance =:= Nothing): ResultG[F, A] =
            this.asInstanceOf[ResultG[F, A]]

        /**
         * If Okay and partial function applies, yields Okay with the result of the partial function,
         * otherwise Failed("partial function did not apply")
         */
        def collect[B, F >: E](pf: PartialFunction[A, B])(implicit fpd: FailedParameterDefault[F]): ResultG[F, B] =
            flatMap { v =>
                if (pf.isDefinedAt(v)) Okay(pf(v))
                else FailedG("partial function did not apply to value", fpd.default)
            }

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
         * otherwise Failed(filterFailure)
         */
        def filter[F >: E](p: A => Boolean)(implicit fpd: FailedParameterDefault[F]): ResultG[F, A] =
            flatMap { v =>
                if (p(v)) Okay(v)
                else FailedG(filterFailure, fpd.default)
            }

        /**
         * If Okay and predicate yields true for the value, yields Okay again,
         * otherwise FailedG(filterFailure, parameter)
         */
        def filterG[F >: E](parameter: F)(p: A => Boolean): ResultG[F, A]

        /** If Okay and predicate yields true for the value, yields Okay again, otherwise the given failure */
        def filterG[F >: E](otherwise: FailedG[F])(p: A => Boolean): ResultG[F, A]

        /** Converse of filter */
        def filterNot[F >: E](p: A => Boolean)(implicit fpd: FailedParameterDefault[F]): ResultG[F, A] =
            flatMap { v =>
                if (!p(v)) Okay(v)
                else FailedG(filterFailure, fpd.default)
            }

        /** Converse of filterG */
        def filterNotG[F >: E](parameter: F)(p: A => Boolean): ResultG[F, A]

        /** Converse of filterG */
        def filterNotG[F >: E](otherwise: FailedG[F])(p: A => Boolean): ResultG[F, A]

        /** If Okay, applies function to value and yields its result. Equivalent to `>>=` */
        final def flatMap[F >: E, B](f: A => ResultG[F, B]): ResultG[F, B] =
            if (this.isOkay) f(this.orThrow)
            else this.asFailed

        /** If Okay, applies function to value and yields its result. Equivalent to `flatMap` */
        final def >>= [F >: E, B](f: A => ResultG[F, B]): ResultG[F, B] =
            macro resultMacros.bind

        /** If Okay, applies function to value and yields Unit */
        final def foreach(f: A => Unit): Unit =
            if (this.isOkay) f(this.orThrow)

        /** If Okay, yields the value, otherwise the given default */
        final def getOrElse[B >: A](default: => B): B =
            if (this.isOkay) this.orThrow
            else default

        /** Yields true iff Okay */
        final def isOkay: Boolean =
            this.isInstanceOf[Okay[_]]

        /** Yields this cast as an Okay if it is one, throws exception otherwise. Intended for use by Java. */
        final def asOkay: Okay[A] =
            if (this.isOkay) this.asInstanceOf[Okay[A]]
            else this.orThrow.asInstanceOf[Okay[A]]

        /** Yields true iff failed */
        final def isFailed: Boolean =
            this.isInstanceOf[FailedG[_]]

        /** Yields this cast as a Failed if it is one, throws exception otherwise. Intended for use by Java. */
        final def asFailed: FailedG[E] =
            if (this.isFailed) this.asInstanceOf[FailedG[E]]
            else sys.error("expected " + this + " to be Failed")

        /** fascilitates continuation passing style over result */
        def cpsRes[X](failedCont: FailedG[E] => X, okCont: A => X): X

        /** Yields an Iterator of the value if Okay, an empty Iterator otherwise */
        def iterator: Iterator[A]

        /** If Okay, applies function to value and yields Okay with its result */
        final def map[B](f: A => B): ResultG[E, B] =
            if (this.isOkay) Okay(f(this.orThrow))
            else this.asFailed

        /** If `FailedG`, applies function to the parameter in the `FailedG` */
        final def mapFailure[F](f: E => F): ResultG[F, A] =
            if (this.isFailed) FailedG(this.asFailed.throwable, f(this.asFailed.parameter))
            else this.asOkay

        /** Replace any `Okay` value with `()` (`Unit`). Equivalent to `result.map(_ => ())` */
        final def unit: ResultG[E, Unit] = map(_ => ())

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
        final def orNull[B >: A](implicit ev: Null <:< B): B =
            if (this.isOkay) this.asOkay.result.asInstanceOf[B]
            else null.asInstanceOf[B]

        /** If Failed, throw the enclosed exception, otherwise yield the Okay value */
        final def orThrow: A =
            if (this.isOkay) this.asOkay.result
            else throw this.asFailed.throwable

        /** Same as iterator */
        override def productIterator: Iterator[Any] = iterator

        /** Yields "Okay" */
        def productPrefix: String

        /** Yields a single element list with the Okay value, or Nil if Failed */
        def toList: List[A]

        /** Yields `Some` if `Okay`, `None` otherwise */
        def toOption: Option[A]

        /** Yields `Right` if `Okay`, `Left` otherwise */
        def toEither: Either[(Throwable, E), A]

        /** Yields `\/-` if `Okay`, `-\/` otherwise */
        def toDisjunction: (Throwable, E) \/ A

        /** Yields `Success` if `Okay`, `Failure` otherwise */
        def toTry: scala.util.Try[A]

        /** Lazy filtering for Scala 2.8 for comprehensions */
        def withFilter[F >: E](p: A => Boolean)(implicit fpd: FailedParameterDefault[F]): WithFilter[F] = new WithFilter(p)

        final class WithFilter[F >: E](p: A => Boolean)(implicit fpd: FailedParameterDefault[F]) {
            def map[B](f: A => B): ResultG[F, B] = ResultG.this.filter[F](p)(fpd).map(f)
            def flatMap[G >: F, B](f: A => ResultG[G, B]): ResultG[G, B] = ResultG.this.filter[F](p)(fpd).flatMap(f)
            def foreach[U](f: A => Unit): Unit = ResultG.this.filter[F](p)(fpd).foreach(f)
            def withFilter(q: A => Boolean): WithFilter[F] = new WithFilter(x => p(x) && q(x))
        }

        /** If Okay, replace the value with the given alternative. Equivalent to >>= { _ => alternative } */
        final def >> [F >: E, B](consequent: => ResultG[F, B]): ResultG[F, B] =
            macro resultMacros.chain

        /**
         * Yield true iff Okay and the value's head type conforms to the given type.
         * <string>Warning:</strong> not type safe, as Okay(F(a)).isA[F[B]] only checks the type F and not B
         */
        def isA[B: ClassTag]: Boolean

        /**
         * Succeed with the value cast to the given type if it can be, otherwise fail with "expected a <given type>"
         * <string>Warning:</strong> not type safe, as Okay(F(a)).asA[F[B]] only checks the type F and not B
         */
        def asA[B](implicit tag: ClassTag[B], fpd: FailedParameterDefault[E /* haaaack */ @uncheckedVariance]): ResultG[E, B] =
            flatMap { v =>
                try Okay(classTag[B].runtimeClass.cast(v).asInstanceOf[B])
                catch { case e: ClassCastException =>
                    val what = v.asInstanceOf[AnyRef] match {
                        case null => "null"
                        case other => other.getClass.getName
                    }
                    FailedG(s"expected a $tag but got a $what", fpd.default)
                }
            }

        /** Succeed with the value cast to the given type if it can be, otherwise the given result */
        def asAG[F >: E, B](clazz: Class[B], parameter: F): ResultG[F, B]

        /** Succeed with the value cast to the given type if it can be, otherwise FailedG("expected a <given type>", parameter) */
        def asAG[F >: E, B](clazz: Class[B], otherwise: ResultG[F, B]): ResultG[F, B]

        /** Pass this to the function ignoring its result */
        def pass(f: ResultG[E, A] => Any): ResultG[E, A] =
            { f(this); this }

        /** Apply a function to a successful value  for its side effects yielding this ResultG unchanged */
        def sideEffect(f: A => Unit): ResultG[E, A]

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

    object Result {
        /** Wrap the value with Okay if non-null, Failed("value was null") if null */
        def apply[A](in: A): Result[A] = if (in != null) Okay(in) else Failed("value was null")
    }

    object ResultG extends FailedParameterImplicits {
        /** Implicitly convert ResultG to Seq, for flatten and similar */
        implicit def resultAsSeq[E, A](in: ResultG[E, A]): Seq[A] =
            in match {
                case Okay(value) => Seq(value)
                case _ => Seq.empty
            }

        /** Grant Scalaz powers to ResultG */
        implicit def resultGMonad[E] = new Traverse[({ type F[A] = ResultG[E, A] })#F] with Monad[({ type F[A] = ResultG[E, A] })#F] {
            def point[A](a: => A) =
                Okay(a)

            def bind[A, B](fa: ResultG[E, A])(f: A => ResultG[E, B]) =
                fa flatMap f

            def traverseImpl[G[_] : Applicative, A, B](fa: ResultG[E, A])(f: A => G[B]) =
                fa match {
                    case FailedG(throwable, x)  => Applicative[G].point(FailedG(throwable, x))
                    case Okay(x) => Applicative[G].map(f(x))(Okay(_))
                }

            override def foldRight[A, B](fa: ResultG[E, A], z: => B)(f: (A, => B) => B) =
                fa match {
                    case FailedG(_, _) => z
                    case Okay(a) => f(a, z)
                }
        }

        /** Allow unifying a `ResultG` where the failure parameter is compatible with the success type */
        implicit class Unifier[+A](val result: ResultG[A, A]) extends AnyVal {
            def unify: A =
                result match {
                    case Okay(a) => a
                    case FailedG(_, a) => a
                }
        }
    }

    /** Another way of writing `Failed("reason") unless predicate`; yield `Okay.unit` if `p` is `true`, `r` otherwise */
    def unless[E](p: Boolean)(r: ResultG[E, Unit]): ResultG[E, Unit] =
        if (p) Okay.unit else r

    /** Converse of `unless`; yield `Okay.unit` if `p` is `false`, `r` otherwise */
    def when[E](p: Boolean)(r: ResultG[E, Unit]): ResultG[E, Unit] =
        if (p) r else Okay.unit

    trait FailedParameterImplicits {
        /** Allow a string to be used with ResultG's `|` operator to wrap a failure with a new explanatory message. For example: `rslt | "explanation"` */
        implicit def stringAsFailedMessage[E](s: => String): FailedG[E] => FailedG[E] =
            failed => FailedG(s, failed.throwable, failed.parameter)

        /** Allow a FailedParameter to be used with ResultG's `|` operator to attach or replace a parameter. For example `rslt | MyFailedParameter("foo")` */
        implicit def failedParameterAsParameter[E <: FailedParameter](parameter: => E): FailedG[Any] => FailedG[E] =
            failed => FailedG(failed.throwable, parameter)

        /**
         * Allow a pair (usually written with `->`) to be used with ResultG's `|` operator to wrap a failure with a new message and parameter.
         * For example `rslt | ("explanation" -> param)`
         */
        implicit def pairAsFailed[E](pair: => (String, E)): FailedG[Any] => FailedG[E] =
            failed => FailedG(pair._1, failed.throwable, pair._2)

        /** Allow a ResultG to be used with ResultG's `|` to replace a failure with some alternate. For example `rslt | Okay("default value")` */
        implicit def resultAsReplacement[E, A](replacement: => ResultG[E, A]): FailedG[Any] => ResultG[E, A] =
            _ => replacement
    }

    /** Allow any value to be used with ResultG's `|` to attach or replace a failure parameter. For example `rslt | parameter(1)` */
    def parameter[E](parameter: E): FailedG[Any] => FailedG[E] =
        failed => FailedG(failed.throwable, parameter)

    object Okay {
        /** A single instance of `Okay(())` to save allocating useless `Okay`s */
        lazy val unit = Okay(())
    }

    /** Result of a successful computation */
    final case class Okay[+A](result: A) extends ResultG[Nothing, A] {
        def collectG[F, B](otherwise: ResultG[F, B])(pf: PartialFunction[A, B]): ResultG[F, B] =
            if (pf.isDefinedAt(result)) Okay(pf(result)) else otherwise

        def collectG[F, B](parameter: F)(pf: PartialFunction[A, B]): ResultG[F, B] =
            if (pf.isDefinedAt(result)) Okay(pf(result)) else FailedG("partial function did not apply to value", parameter)

        def filterG[F](parameter: F)(p: A => Boolean): ResultG[F, A] =
            if (p(result)) Okay(result) else FailedG(filterFailure, parameter)

        def filterG[F](otherwise: FailedG[F])(p: A => Boolean): ResultG[F, A] =
            if (p(result)) Okay(result) else otherwise

        def filterNotG[F](parameter: F)(p: A => Boolean): ResultG[F, A] =
            if (!p(result)) Okay(result) else FailedG(filterFailure, parameter)

        def filterNotG[F](otherwise: FailedG[F])(p: A => Boolean): ResultG[F, A] =
            if (!p(result)) Okay(result) else otherwise

        def asAG[F, B](clazz: Class[B], parameter: F): ResultG[F, B] =
            try Okay(clazz.cast(result).asInstanceOf[B])
            catch { case e: ClassCastException =>
                val what = result.asInstanceOf[AnyRef] match {
                    case null => "null"
                    case other => other.getClass.getName
                }
                FailedG(s"expected a ${clazz.getName} but got a $what", parameter)
            }

        def asAG[F, B](clazz: Class[B], otherwise: ResultG[F, B]): ResultG[F, B] =
            try Okay(clazz.cast(result).asInstanceOf[B])
            catch { case e: ClassCastException => otherwise }

        def iterator: Iterator[A] =
            Iterator.single(result)

        def | [F, B >: A] (f: FailedG[Nothing] => ResultG[F, B]): ResultG[F, B] =
            this

        override def productPrefix: String =
            "Okay"

        def toList: List[A] =
            result :: Nil

        def toOption: Option[A] =
            Some(result)

        def toEither: Either[Nothing, A] =
            Right(result)

        def toDisjunction: (Throwable, Nothing) \/ A =
            \/-(result)

        def toTry: scala.util.Try[A] =
            scala.util.Success(result)

        def isA[B: ClassTag]: Boolean =
            classTag[B].runtimeClass.isInstance(result)

        def sideEffect(f: A => Unit): Okay[A] =
            { f(result); this }

        def flatten[E, B](implicit ev: A => ResultG[E, B]): ResultG[E, B] =
            ev(result)

        def cpsRes[X](failedCont: FailedG[Nothing] => X, okCont: A => X): X = okCont(result)

        override def toString =
            "Okay(" + (try { String.valueOf(result) } catch { case _: Exception => "<failed .toString>" }) + ")"
    }

    /** Result of a failed computation */
    final case class FailedG[+E](throwable: Throwable, parameter: E) extends ResultG[E, Nothing] {
        lazy val message: String =
            if (throwable.getMessage != null) throwable.getMessage
            else                              throwable.toString

        def collectG[F >: E, B](parameter: F)(pf: PartialFunction[Nothing, B]): FailedG[F] =
            this

        def collectG[F >: E, B](otherwise: ResultG[F, B])(pf: PartialFunction[Nothing, B]): FailedG[F] =
            this

        def filterG[F >: E](parameter: F)(p: Nothing => Boolean): FailedG[F] =
            this

        def filterG[F >: E](otherwise: FailedG[F])(p: Nothing => Boolean): FailedG[F] =
            this

        def filterNotG[F >: E](parameter: F)(p: Nothing => Boolean): FailedG[F] =
            this

        def filterNotG[F >: E](otherwise: FailedG[F])(p: Nothing => Boolean): FailedG[F] =
            this

        def iterator: Iterator[Nothing] =
            Iterator.empty

        def | [F, B] (f: FailedG[E] => ResultG[F, B]): ResultG[F, B] =
            f(this)

        def toList: List[Nothing] =
            Nil

        def toOption: Option[Nothing] =
            None

        def toEither: Either[(Throwable, E), Nothing] =
            Left((throwable, parameter))

        def toDisjunction: (Throwable, E) \/ Nothing =
            -\/((throwable, parameter))

        def toTry: scala.util.Try[Nothing] =
            scala.util.Failure(throwable)

        def isA[B: ClassTag]: Boolean =
            false

        def asAG[F >: E, B](clazz: Class[B], parameter: F): ResultG[F, B] =
            this

        def asAG[F >: E, B](clazz: Class[B], otherwise: ResultG[F, B]): ResultG[F, B] =
            this

        def sideEffect(f: Nothing => Unit): FailedG[E] =
            this

        def flatten[F >: E, B](implicit ev: Nothing => ResultG[F, B]): FailedG[F] =
            this

        /** If the given boolean is `true`, yield `Okay.unit`, else this `FailedG` */
        def unless(b: Boolean): ResultG[E, Unit] =
            if (b) Okay.unit else this

        /** If the given boolean is `true`, yield this `FailedG`, else `Okay.unit` */
        def when(b: Boolean): ResultG[E, Unit] =
            if (b) this else Okay.unit

        def cpsRes[X](failedCont: FailedG[E] => X, okCont: Nothing => X): X = failedCont(this)

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

            "FailedG(" + throwable + causedBy(throwable) + ", " + parameter + ")"
        }
    }

    /** Exception generated when FailedG or Failed is given a message string rather than a Throwable. Formats slightly more cleanly */
    class FailedException(message: String, cause: Throwable = null) extends RuntimeException(message, cause) {
        override def toString: String = message
    }

    object FailedG {
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

    object Failed {
        /** Construct a FailedG using the given message and no argument (unit) */
        def apply(message: String): Failed = FailedG(message, ())

        /** Construct a FailedG using the given message with a cause and no argument (unit) */
        def apply(message: String, cause: Throwable): Failed = FailedG(new FailedException(message, cause), ())

        /** Construct a FailedG using the given message with a cause and no argument (unit) */
        def apply(message: String, cause: FailedG[_]): Failed = FailedG(new FailedException(message, cause.throwable), ())

        /** Construct a FailedG using the given throwable and no argument (unit) */
        def apply(throwable: Throwable): Failed = FailedG(throwable, ())

        /** Extract a cause throwable from a Result */
        def unapply(in: FailedG[_]): Option[Throwable] =
            in match {
                case FailedG(throwable, _) => Some(throwable)
                case _                     => None
            }

        object Message {
            /** Extract a failure message from a Result */
            def unapply(in: FailedG[_]): Option[String] =
                in match {
                    case failed@FailedG(_, _) => Some(failed.message)
                    case _                    => None
                }
        }
    }

    /** Enrichment of Option that provides ResultG related methods */
    implicit class optionOps[A](val in: Option[A]) extends AnyVal {
        /** Convert Some to Okay, and None to Failed("option was none") */
        def toResult: Result[A] =
            in match {
                case Some(a) => Okay(a)
                case None    => Failed("option was none")
            }

        /** Apply some possibly-failing computation on the value inside the `Option`, or yield `Okay(None)` if the option is `None` */
        def mapResult[E, B](f: A => ResultG[E, B]): ResultG[E, Option[B]] =
            in.map(a => f(a).map(Some.apply)).getOrElse(Okay(None))
    }

    /** Enrichment of Either with a .toResult method */
    implicit class eitherOps[A](val in: Either[Throwable, A]) extends AnyVal {
        /** Convert Left to Failed and Right to Okay */
        def toResult: Result[A] =
            in match {
                case Left(t)  => Failed(t)
                case Right(a) => Okay(a)
            }
    }

    /** Enrich `Either[(Throwable, E), A]` with a `.toResult` method */
    implicit class eitherOpsG[E, A](val in: Either[(Throwable, E), A]) extends AnyVal {
        /** Convert `Left` to `FailedG` and `Right` to `Okay` */
        def toResult: ResultG[E, A] =
            in match {
                case Left((t, e)) => FailedG(t, e)
                case Right(a) => Okay(a)
            }
    }

    /** Enrichment of Either with a .toResult method */
    implicit class disjunctionOps[A](val in: Throwable \/ A) extends AnyVal {
        /** Convert Left to Failed and Right to Okay */
        def toResult: Result[A] =
            in match {
                case -\/(t)  => Failed(t)
                case \/-(a) => Okay(a)
            }
    }

    /** Enrich `(Throwable, E) \/ A` with a `.toResult` method */
    implicit class disjunctionOpsG[E, A](val in: (Throwable, E) \/ A) extends AnyVal {
        /** Convert `-\/` to `FailedG` and `\/-` to `Okay` */
        def toResult: ResultG[E, A] =
            in match {
                case -\/((t, e)) => FailedG(t, e)
                case \/-(a)      => Okay(a)
            }
    }

    /** Enrichment of Try that provides ResultG related methods */
    implicit class tryOps[A](val in: scala.util.Try[A]) extends AnyVal {
        /** Convert `Success` to `Okay`, and `Failure` to `Failed` */
        def toResult: Result[A] =
            in match {
                case scala.util.Success(a) => Okay(a)
                case scala.util.Failure(t) => Failed(t)
            }
    }

    /** Cast value to given type and yield `Okay(castValue)` or `Failed` if value is not of the given (runtime) type */
    def cast[A: ClassTag](in: Any): Result[A] =
        Okay(in).withFailedType[Unit].asA[A]

    /** Cast value to given type and yield `Okay(castValue)` or `Failed` if value is not of the given (runtime) type */
    def cast[A](clazz: Class[A], in: Any): Result[A] =
        Okay(in).withFailedType[Unit].asAG(clazz, ())


    /** Evaluate `body` yielding the value wrapped in `Okay` and catch any `Exception` as a `Failed` */
    def tryCatchValue[A](body: => A): Result[A] =
        macro resultMacros.tryCatchValue

    /** Evaluate `body` yielding the value wrapped in `Okay` and catch any `Exception` as a `Failed`, transforming that failure with `ff` */
    def tryCatchValueG[E, A](ff: FailedG[Unit] => ResultG[E, A])(body: => A): ResultG[E, A] =
        macro resultMacros.tryCatchValueG

    /** Evaluate `body` yielding the value and catch any `Exception` as a `Failed` */
    def tryCatchResult[A](body: => Result[A]): Result[A] =
        macro resultMacros.tryCatchResult

    /** Evaluate `body` yielding the value and catch any `Exception` as a `Failed`, transforming that failure with `ff` */
    def tryCatchResultG[E, A](ff: FailedG[Unit] => ResultG[E, A])(body: => ResultG[E, A]): ResultG[E, A] =
        macro resultMacros.tryCatchResultG

    /** Evaluate `body` yielding the value wrapped in `Okay` and catch throwables of the given types as `Failed` */
    def tryCatchingValue[A](throwables: Class[_ <: Throwable]*)(body: => A): Result[A] =
        macro resultMacros.tryCatchingValue

    /** Evaluate `body` yielding the value wrapped in `Okay` and catch throwables of the given types as `Failed`, transforming that failure with `ff` */
    def tryCatchingValueG[E, A](throwables: Class[_ <: Throwable]*)(ff: FailedG[Unit] => ResultG[E, A])(body: => A): ResultG[E, A] =
        macro resultMacros.tryCatchingValueG

    /** Evaluate `body` yielding the value and catch throwables of the given types as `Failed` */
    def tryCatchingResult[A](throwables: Class[_ <: Throwable]*)(body: => Result[A]): Result[A] =
        macro resultMacros.tryCatchingResult

    /** Evaluate `body` yielding the value and catch throwables of the given types as `Failed`, transforming that failure with `ff` */
    def tryCatchingResultG[E, A](throwables: Class[_ <: Throwable]*)(ff: FailedG[Unit] => ResultG[E, A])(body: => ResultG[E, A]): ResultG[E, A] =
        macro resultMacros.tryCatchingResultG

    /**
     * Return the first application of the given function to each item from the given {@link Iterable} that is a {@link Okay} {@link ResultG}, or the last
     * application if none result in {@link Okay}
     */
    def firstOrLast[A, B](xs: Iterable[A])(f: A => Result[B]): Result[B] =
        firstOrLastG(Failed("no result to yield"), xs)(f)

    /**
     * Return the first application of the given function to each item from the given {@link Iterable} that is a {@link Okay} {@link ResultG}, or the last
     * application if none result in {@link Okay}
     */
    def firstOrLastG[A, B, E](default: ResultG[E, B], xs: Iterable[A])(f: A => ResultG[E, B]): ResultG[E, B] = {
        var last: ResultG[E, B] = default
        for (x <- xs) {
            f(x) match {
                case Okay(y) => return Okay(y)
                case other => last = other
            }
        }
        last
    }

    /**
     * Return the first application of the given function to each item from the given {@link Foldable} that is a {@link Okay} {@link ResultG}, or the last
     * application if none result in {@link Okay}
     */
    def firstOrLast[XS[_]: Foldable, A, B](xs: XS[A])(f: A => Result[B]): Result[B] =
        firstOrLastG(Failed("no result to yield"), xs)(f)

    /**
     * Return the first application of the given function to each item from the given {@link Foldable} that is a {@link Okay} {@link ResultG}, or the last
     * application if none result in {@link Okay}
     */
    def firstOrLastG[XS[_]: Foldable, A, B, E](default: ResultG[E, B], xs: XS[A])(f: A => ResultG[E, B]): ResultG[E, B] =
        Foldable[XS].foldLeft[A, ResultG[E, B] \/ Okay[B]](xs, -\/(default)) { (accum, a) =>
            accum match {
                case -\/(last) =>
                    f(a) match {
                        case success @ Okay(_) => \/-(success)
                        case failed            => -\/(failed)
                    }

                case success @ \/-(_) => success
            }
        }.fold(identity, identity)

    /** Extension of Iterable that can perform various Result-oriented transforms or operations */
    implicit class iterableResultOps[A](val iterable: Iterable[A]) extends AnyVal {
        /** Sequence each computation in the iterable, yielding Okay iff all computations yield Okay */
        def sequenceResult[E]()(implicit ev: A <:< (() => ResultG[E, Unit])): ResultG[E, Unit] =
            iterable.iterator.sequenceResult[E]()

        /**
         * Sequence a chain of possibly-failing computations with side effects by applying f to each element of seq in turn. If any application of
         * f results in Failed, then the chain is stopped there and that result returned. Otherwise Okay.unit is returned.
         */
        def foreachResult[E](f: A => ResultG[E, Unit]): ResultG[E, Unit] =
            iterable.iterator.foreachResult[E](f)

        /**
         * Collect the result of a series of possibly-failing computations, returning Okay(seq) in the case where all succeed, or the first non-Okay
         * result otherwise.
         */
        def mapResult[E, B, That](f: A => ResultG[E, B])(implicit cb: CanBuild[B, That]): ResultG[E, That] =
            iterable.iterator.mapResult[E, B, That](f)(cb)

        /**
         * Fold a chain of possibly-failing computations, short circuiting the fold to Failed on the first computation that results in one.
         * Sequence a chain of possibly-failing computations with side effects by applying f to each element of seq in turn. If any application of
         * f results in Failed, then the chain is stopped there and that result returned. Otherwise Okay(result) is returned.
         */
        def foldLeftResult[E, B](init: B)(f: (B, A) => ResultG[E, B]): ResultG[E, B] =
            iterable.iterator.foldLeftResult[E, B](init)(f)

        /**
         * Apply a possibly-failing computation to each element of the iterable, calling some function on non-Okays and collecting the Okays.
         * Usually the error handling function is some side-effecting function, e.g. logging.
         */
        def flattenResult[E, B, That](onError: (A, FailedG[E]) => Unit)(f: A => ResultG[E, B])(implicit cb: CanBuild[B, That]): That =
            iterable.iterator.flattenResult[E, B, That](onError)(f)(cb)

        /** "Left join" this iterable with a potentially missing right hand side. */
        def leftJoin[E, B, That](right: ResultG[E, Iterable[B]])(implicit cb: CanBuild[(A, ResultG[E, B]), That]): That =
            iterable.iterator.leftJoin[E, B, That](right)(cb)
    }

    /** Extension of Iterator that can perform various Result-oriented transforms or operations */
    implicit class iteratorResultOps[A](val iterator: Iterator[A]) extends AnyVal {
        /** Sequence each computation in the iterator, yielding Okay iff all computations yield Okay */
        def sequenceResult[E]()(implicit ev: A <:< (() => ResultG[E, Unit])): ResultG[E, Unit] =
            foreachResult(a => ev(a)())

        /**
         * Sequence a chain of possibly-failing computations with side effects by applying f to each element of seq in turn. If any application of
         * f results in Failed, then the chain is stopped there and that result returned. Otherwise Okay.unit is returned.
         */
        def foreachResult[E](f: A => ResultG[E, Unit]): ResultG[E, Unit] =
            foldLeftResult[E, Unit](())((_, a) => f(a))

        /**
         * Collect the result of a series of possibly-failing computations, returning Okay(seq) in the case where all succeed, or the first non-Okay
         * result otherwise.
         */
        def mapResult[E, B, That](f: A => ResultG[E, B])(implicit cb: CanBuild[B, That]): ResultG[E, That] = {
            val builder = cb()
            foreachResult { a =>
                f(a) match {
                    case Okay(b) => builder += b; Okay.unit
                    case failed@FailedG(_, _) => failed
                }
            }.map(_ => builder.result())
        }

        /**
         * Fold a chain of possibly-failing computations, short circuiting the fold to Failed on the first computation that results in one.
         * Sequence a chain of possibly-failing computations with side effects by applying f to each element of seq in turn. If any application of
         * f results in Failed, then the chain is stopped there and that result returned. Otherwise Okay(result) is returned.
         */
        def foldLeftResult[E, B](init: B)(f: (B, A) => ResultG[E, B]): ResultG[E, B] = {
            // Manually implement rather than use fold so inlining, TCO, and early shortcut can apply
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
        def leftJoin[E, B, That](right: ResultG[E, Iterable[B]])(implicit cb: CanBuild[(A, ResultG[E, B]), That]): That =
            leftJoinIterator[E, B, That](right.map(_.iterator))

        /** "Left join" this iterable with a potentially missing right hand side. */
        def leftJoinIterator[E, B, That](right: ResultG[E, Iterator[B]])(implicit cb: CanBuild[(A, ResultG[E, B]), That]): That = {
            val builder = cb()
            val leftIterator = iterator
            while (leftIterator.hasNext && right.map(_.hasNext).getOrElse(true)) {
                builder += ((leftIterator.next, right.map(_.next)))
            }
            builder.result()
        }
    }

    import Scalaz._

    /** monad transformer for ResultG */
    final case class ResultGT[F[_], E, A](run: F[ResultG[E, A]]) {

        def map[B](f: A => B)(implicit F: Functor[F]): ResultGT[F, E, B] =
            ResultGT(F.map(run)(_ map f))

        def mapF[B](f: A => F[B])(implicit M: Monad[F]): ResultGT[F, E, B] =
            flatMapF { f andThen (mb => M.map(mb)(Okay(_))) }

        def flatMap[B](f: A => ResultGT[F, E, B])(implicit F: Monad[F]): ResultGT[F, E, B] =
            ResultGT(F.bind(run)(_.cpsRes(F.point(_), f(_).run)))

        def flatMapF[B](f: A => F[ResultG[E, B]])(implicit F: Monad[F]): ResultGT[F, E, B] =
            ResultGT(F.bind(run)(_.cpsRes(F.point(_), f)))

        def | [D, B >: A](f: FailedG[E] => ResultG[D, B])(implicit F: Functor[F]): ResultGT[F, D, B] =
            ResultGT(F.map(run)(_ | f))

        /** alias for | */
        def orElse [D, B >: A](f: FailedG[E] => ResultG[D, B])(implicit F: Functor[F]): ResultGT[F, D, B] =
            this | f

        def foreach(f: A => Unit)(implicit F: Functor[F]): Unit = { F.map(run)(_ foreach f); () }

        def filter[D >: E](p: A => Boolean)(implicit fpd: FailedParameterDefault[D], F: Functor[F]): ResultGT[F, D, A] =
            ResultGT(F.map(run)(_.cpsRes(e => e, a => if (p(a)) Okay(a) else FailedG(filterFailure, fpd.default))))

        def withFilter[D >: E](p: A => Boolean)(implicit fdp: FailedParameterDefault[D]): WithFilter[D] = new WithFilter(p)

        final class WithFilter[D >: E](p: A => Boolean)(implicit fpd: FailedParameterDefault[D]) {
            def map[B](f: A => B)(implicit F: Functor[F]): ResultGT[F, D, B] =
                ResultGT.this.filter[D](p)(fpd, F).map(f)

            def flatMap[B](f: A => ResultGT[F, D, B])(implicit F: Monad[F]): ResultGT[F, D, B] =
                ResultGT.this.filter[D](p)(fpd, F).flatMap(f)

            def foreach[U](f: A => Unit)(implicit F: Functor[F]): Unit =
                ResultGT.this.filter[D](p)(fpd, F).foreach(f)

            def withFilter(q: A => Boolean): WithFilter[D] = new WithFilter(x => p(x) && q(x))
        }

        def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
            F.foldLeft(run, b)((b, res) => res.cpsRes(_ => b, a => f(b, a)))

        def bimap[D, B](fe: E => D, fa: A => B)(implicit F: Functor[F]): ResultGT[F, D, B] =
            ResultGT(F.map(run)(_.cpsRes(e => e.mapFailure(fe), a => Okay(fa(a)))))
    }

    trait ResultGTInstances {

        implicit def resultGTMonad[F[_], E](implicit F: Monad[F]) = new Monad[({ type M[A] = ResultGT[F, E, A] })#M] {

            def point[A](a: => A): ResultGT[F, E, A] =
                ResultGT(F.point(Okay(a)))

            def bind[A, B](fa: ResultGT[F, E, A])(f: A => ResultGT[F, E, B]): ResultGT[F, E, B] =
                fa flatMap f
        }
    }

    case object ResultGT extends ResultGTInstances {

        def fromResultG[F[_], E, A](a: ResultG[E, A])(implicit F: Monad[F]): ResultGT[F, E, A] =
            ResultGT[F, E, A](F.point(a))

        def fromResult[F[_], A](ra: Result[A])(implicit F: Monad[F]): ResultGT[F, Unit, A] =
            ResultGT[F, Unit, A](F.point(ra))

        def liftF[F[_], E, A](fa: F[A])(implicit F: Functor[F]): ResultGT[F, E, A] =
            ResultGT[F, E, A](fa map (Okay(_)))
    }
}
