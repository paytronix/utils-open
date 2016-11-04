//
// Copyright 2009-2012 Paytronix Systems, Inc.
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

package com.paytronix.utils.lift

import scala.collection.generic.CanBuild
import scala.collection.mutable.ArrayBuffer
import net.liftweb.common.{Box, Empty, EmptyBox, Failure, Full}
import org.slf4j.Logger

import log.loggerBoxOps

object box
{
    /** Path so that Java code can create an empty box */
    def empty[A](witness: Class[A]): Box[A] = Empty

    /** Path so that Java code can create an empty box for an int primitive type */
    def emptyInt(): Box[Int] = Empty

    /** Path so that Java code can create a full box */
    def full[A](a: A): Box[A] = Full(a)

    /** Helper to either return the value contained in a box or throw an exception using failureToException, primarily useful in java code */
    def openOrThrow[A](box: Box[A]): A = box.openOr { throw failureToException(box) }

    /** Exception used when a Failure Box is converted to an Error and has both a Throwable and Box cause */
    case class FailureBoxException(message: String, throwableCause: Throwable, failureCause: Failure)
        extends RuntimeException(message, throwableCause)

    /** Helper to transform some kind of failure box into an exception, primarily useful in java code */
    def failureToException[A](box: Box[A]): Exception =
        box match {
            case Full(_) => throw new IllegalArgumentException("failureToException called on a Full -- should only ever be called on Failure or Empty")
            case Empty                                        => new RuntimeException("Unknown cause")
            case Failure(msg, Full(throwable), Full(failure)) => new FailureBoxException(msg, throwable, failure)
            case Failure(msg, _,               Full(failure)) => new RuntimeException(msg, failureToException(failure))
            case Failure(msg, Full(throwable), _)             => new RuntimeException(msg, throwable)
            case Failure(msg, _, _)                           => new RuntimeException(msg)
        }

    /** Helper to transform some kind of failure box into an exception, primarily useful in java code */
    def failureToException[A](box: Box[A], message: String): Exception =
        box match {
            case Full(_) => throw new IllegalArgumentException("failureToException called on a Full -- should only ever be called on Failure or Empty")
            case Empty                                        => new RuntimeException(message + ": Unknown cause")
            case Failure(msg, _,               Full(failure)) => new RuntimeException(message + ": " + msg, failureToException(failure))
            case Failure(msg, Full(throwable), _)             => new RuntimeException(message + ": " + msg, throwable)
            case Failure(msg, _, _)                           => new RuntimeException(message + ": " + msg)
        }

    /**
     * Return the first application of the given function to each item from the given {@link Seq} that is a {@link Full} {@link Box}, or the last
     * application if none result in {@link Full}
     *
     * Similar to {@link net.liftweb.util.ListHelpers#first} except this one returns the last item in the Seq instead of Empty
     */
    def firstOrLast[A,B](xs: Seq[A])(f: (A) => Box[B]): Box[B] =
        xs.view.foldLeft[Box[B]](Empty) {
            (z, x) =>
                z match {
                    case Full(_) => z
                    case _       => f(x)
                }
        }

    /** Implicitly convert a box to a box with extended capabilities for error handling */
    implicit def boxToErrorHandlingBox[A](box: Box[A]): ErrorHandlingBox[A] = ErrorHandlingBox(box)

    /** Extension of Box that provides extended error handling capabilities */
    final case class ErrorHandlingBox[A](box: Box[A]) {
        /** Log an error and throw a better labelled NPE unless the covered box is Full (in which case return the value) */
        final def orAbort(logger: Logger, msg: String): A = box match {
            case Full(a) => a
            case other   => logger.errorBox(msg, box); throw new NullPointerException(msg)
        }

        /** Log a warn and throw a better labelled NPE unless the covered box is Full (in which case return the value) */
        final def orWarnAndAbort(logger: Logger, msg: String): A = box match {
            case Full(a) => a
            case other   => logger.warnBox(msg, box); throw new NullPointerException(msg)
        }

        /** For Empty or Failure boxes, log the given error text using debugBox */
        def orLogDebug(logger: Logger, msg: String): Box[A] = box match {
            case Full(_) => box
            case _       => logger.debugBox(msg, box); box
        }

        /** For Empty or Failure boxes, log the given error text using infoBox */
        def orLogInfo(logger: Logger, msg: String): Box[A] = box match {
            case Full(_) => box
            case _       => logger.infoBox(msg, box); box
        }

        /** For Empty or Failure boxes, log the given error text using warnBox */
        def orLogWarn(logger: Logger, msg: String): Box[A] = box match {
            case Full(_) => box
            case _       => logger.warnBox(msg, box); box
        }

        /** For Empty or Failure boxes, log the given error text using errorBox */
        def orLogError(logger: Logger, msg: String): Box[A] = box match {
            case Full(_) => box
            case _       => logger.errorBox(msg, box); box
        }

        /** For Empty or Failure boxes, log the given error text using errorBox after performing the specified cleanup behaviors */
        def orLogErrorWithCleanup(logger: Logger, msg: String, cleanup: () => Unit): Box[A] = box match {
            case Full(_) => box
            case _       => cleanup()
                            logger.errorBox(msg, box); box
        }

        // Aliases for orLog* that makes chaining orError and orLog* from SnippetHelpers more natural
        def andLogDebug(logger: Logger, msg: String): Box[A] = orLogDebug(logger, msg)
        def andLogInfo(logger: Logger, msg: String): Box[A] = orLogInfo(logger, msg)
        def andLogWarn(logger: Logger, msg: String): Box[A] = orLogWarn(logger, msg)
        def andLogError(logger: Logger, msg: String): Box[A] = orLogError(logger, msg)
    }

    /** Implicitly convert a box of box to a NestedBox with the ability to remove one layer of structure (monadic join) */
    implicit def boxToNestedBox[A](boxOfBox: Box[Box[A]]): NestedBox[A] = NestedBox(boxOfBox)

    /** Extension of Box that provides a monadic join operation */
    final case class NestedBox[A](boxOfBox: Box[Box[A]]) {
        /** Join the inner box to the outer box. Equivalent to <code>boxOfBox.flatMap(x => x)</code>. Known as "join" or &mu; in category theory */
        final def join: Box[A] = boxOfBox.flatMap(x => x)
    }

    /** Implicitly convert an iterable to a BoxAwareIterable with various extended behaviors  */
    implicit def iterableToBoxAwareIterable[A](iterable: Iterable[A]): BoxAwareIterable[A] = BoxAwareIterable(iterable)

    /** Extension of Iterable that can perform various Box-oriented transforms or operations */
    final case class BoxAwareIterable[A](iterable: Iterable[A]) {
        /**
         * Sequence a chain of possibly-failing computations with side effects by applying f to each element of seq in turn. If any application of
         * f results in Empty or Failure, then the chain is stopped there and that result returned. Otherwise Full(()) is returned.
         */
        def foreachBox(f: A => Box[Unit]): Box[Unit] = foldLeftBox[Unit](())((a, _) => f(a))

        /**
         * Collect the result of a series of possibly-failing computations, returning Full(Seq) in the case where all succeed, or the first non-Full
         * box otherwise.
         */
        def mapBox[B, That](f: A => Box[B])(implicit cb: CanBuild[B, That]): Box[That] = {
            val builder = cb()
            foreachBox(a => f(a).map(builder += _)).map(_ => builder.result())
        }

        /**
         * Fold a chain of possibly-failing computations, short circuiting the fold to Empty or Failure on the first computation that results in one
         * Sequence a chain of possibly-failing computations with side effects by applying f to each element of seq in turn. If any application of
         * f results in Empty or Failure, then the chain is stopped there and that result returned. Otherwise Full(()) is returned.
         */
        def foldLeftBox[B](init: B)(f: (A, B) => Box[B]): Box[B] = {
            // Manually implement rather than use fold so inlining, TCO, and early shortcut can apply
            val iterator = iterable.iterator
            def iterate(prev: Box[B]): Box[B] =
                prev match {
                    case Full(b) if iterator.hasNext => iterate(f(iterator.next, b))
                    case _ => prev
                }
            iterate(Full(init))
        }

        /**
         * Apply a possibly-failing computation to each element of the iterable, calling some function on non-Fulls and collecting the Fulls.
         * Usually the error handling function is some side-effecting function, e.g. logging.
         */
        def flattenBox[B, That](onError: (A, EmptyBox) => Unit)(f: A => Box[B])(implicit cb: CanBuild[B, That]): That = {
            val builder = cb()
            val iterator = iterable.iterator
            while (iterator.hasNext) {
                val a = iterator.next
                f(a) match {
                    case Full(b) => builder += b
                    case (emptyBox: EmptyBox) => onError(a, emptyBox)
                }
            }
            builder.result()
        }

        /** "Left join" this iterable with a potentially missing right hand side. */
        def leftJoin[B, That](right: Box[Iterable[B]])(implicit cb: CanBuild[(A, Box[B]), That]): That = {
            val builder = cb()
            val leftIterator = iterable.iterator
            val rightIteratorBox = right.map(_.iterator)
            while (leftIterator.hasNext && rightIteratorBox.map(_.hasNext).openOr(true)) {
                builder += ((leftIterator.next, rightIteratorBox.map(_.next)))
            }
            builder.result()
        }
    }

    /** Implicitly convert an iterable to a IterableAwareBox with various extended behaviors  */
    implicit def iterableToIterableAwareBox[A](box: Box[Iterable[A]]): IterableAwareBox[A] = IterableAwareBox(box)

    /** Extension of Box[Iterable] that can perform various Iterable-oriented transforms or operations */
    final case class IterableAwareBox[A](box: Box[Iterable[A]]) {
        /** "Right join" this box of iterable with some other iterable */
        def rightJoin[B, That](rhs: Iterable[B])(implicit cb: CanBuild[(Box[A], B), That]): That = {
            val builder = cb()
            val leftIteratorBox = box.map(_.iterator)
            val rightIterator = rhs.iterator
            while (leftIteratorBox.map(_.hasNext).openOr(true) && rightIterator.hasNext) {
                builder += ((leftIteratorBox.map(_.next), rightIterator.next))
            }
            builder.result()
        }
    }
}
