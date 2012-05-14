//
// Copyright 2010-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

import scala.collection.mutable.Queue
import org.slf4j.LoggerFactory
import com.paytronix.utils.scala.concurrent.ThreadLocal
import com.paytronix.utils.scala.log.loggerResultOps
import com.paytronix.utils.scala.result.{Failed, FailedG, FailedParameterDefault, Okay, ResultG}


/**
 * Rewindable result computations, where as each operation is executed it can register some action to take if later actions fail.
 *
 * Example:
 *     scala> def rewind(i: Int) = println("rewound " + i)
 *     rewind: (i: Int)Unit
 *
 *     scala>
 *
 *     scala> rewindable {
 *          |     for {
 *          |         a <- Okay(1) onRewind(rewind)
 *          |         b <- Okay(2) onRewind(rewind)
 *          |         c <- Okay(3)
 *          |         d <- Failed("oops")
 *          |     } yield a+b+c
 *          | }
 *     rewound 2
 *     rewound 1
 *     res0: com.paytronix.utils.scala.result.ResultG[Unit, Int] = FailedG(oops,())
 *
 *     scala> rewindable {
 *          |     for {
 *          |         a <- Okay(1) onRewind(rewind)
 *          |         b <- Okay(2) onRewind(rewind)
 *          |         c <- Okay(3)
 *          |     } yield a+b+c
 *          | }
 *     res1: com.paytronix.utils.scala.result.ResultG[Unit, Int] = Okay(6)
 */
object rewindable {
    private val logger = LoggerFactory.getLogger(getClass)

    private val rewinds = new ThreadLocal[Option[Queue[() => Unit]]] {
        protected val initial = None
    }

    /** Assert that there is a valid rewind context on the stack before executing the body */
    def inRewindableContext[A](f: => A): A = rewinds.get match {
        case Some(_) => f
        case _ => sys.error("not in rewindable context! this is a programming error not prevented by the type system -- " +
                            "there should have been a rewindable { } in scope")
    }

    /** Register a function to run in the case where a rewind occurs */
    def onRewind(f: => Unit) = rewinds.get.foreach(_ += (() => f))

    /** Begin a rewindable context. The body is evaluated and if is not Okay, any registered rewind actions will then occur for their side effects. */
    def rewindable[E, A](body: => RewindableResult[E, A]): ResultG[E, A] = {
        val queue: Queue[() => Unit] = new Queue

        rewinds.doWith(Some(queue))(body.result match {
            case result@Okay(_) => result
            case other =>
                queue.reverse.foreach { f =>
                    try {
                        f()
                    } catch {
                        case e: Exception =>
                            logger.errorResult("Failed to rewind", Failed("exception encountered while unwinding due to " + other, e))
                    }
                }
                other
        })
    }

    /** Implicitly convert a result computation to a RewindableResultOps which allows attachment of a rewind hook */
    implicit def rewindableResultOps[E, A](result: => ResultG[E, A]): RewindableResultOps[E, A] = new RewindableResultOps(result)

    /** Extension of a lazy ResultG that allows rewinds to be associated with forcing the result */
    final class RewindableResultOps[E, A](result: => ResultG[E, A]) {
        /**
         * First assert that there is a rollback context in scope, then force the result computation and register a rollback if the result is Okay.
         */
        def onRewind(f: A => Unit): RewindableResult[E, A] =
            result match {
                case result@Okay(a) =>
                    rewindable.this.onRewind(f(a))
                    new RewindableResult(result)

                case result => new RewindableResult(result)
            }
    }

    /** Implcitly wrap a ResultG[E, A] into a RewindableResult[E, A] so that plain results can be used in for-comprehensions alongside Rewindable ones */
    implicit def resultToRewindableResult[E, A](result: ResultG[E, A]): RewindableResult[E, A] = new RewindableResult(result)

    /** A wrapper around ResultG to prevent results with rewind actions from being used outside a rewindable { } context */
    class RewindableResult[+E, +A](private[scala] val result: ResultG[E, A]) {
        def map[B](f: A => B): RewindableResult[E, B] = new RewindableResult(result.map(f))
        def flatMap[F >: E, B](f: A => RewindableResult[F, B]): RewindableResult[F, B] = result match {
            case Okay(a)         => f(a)
            case f@FailedG(_, _) => f
        }
        def filter[F >: E](p: A => Boolean)(implicit fpd: FailedParameterDefault[F]): RewindableResult[F, A] = new RewindableResult(result.filter[F](p))
    }
}
