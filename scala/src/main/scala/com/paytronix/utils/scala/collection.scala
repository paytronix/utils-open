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

package com.paytronix.utils.scala

import scala.collection.generic.CanBuild

/**
 * Helpers for collections
 */
object collection
{
    /**
     * Construct a list by unfolding. The given function is repeatedly applied to some accumulator and until it yields None each value it yields is appended to a list.
     *
     * For example:
     *   unfold[Int, String](i =>
     *       if (i < 10) {
     *           Some((i+1, i.toString))
     *       } else {
     *           None
     *       })(0)
     *
     * Will emit the list:
     *     List[String](0, 1, 2 ... 9)
     */
    def unfold[A,B](init: A)(f: A => Option[(A,B)]): List[B] = {
        def iterate(a: A): List[B] =
            f(a) match {
                case Some((nextA,b)) => b :: iterate(nextA)
                case None            => Nil
            }
        iterate(init)
    }

    /** Standard 3-way zip */
    def zip[A, B, C, Out](as: Iterable[A], bs: Iterable[B], cs: Iterable[C])(implicit cb: CanBuild[(A,B,C), Out]): Out = {
        val aIter = as.iterator
        val bIter = bs.iterator
        val cIter = cs.iterator

        val builder = cb()

        def go: Out =
            if (aIter.hasNext && bIter.hasNext && cIter.hasNext) {
                builder += ((aIter.next, bIter.next, cIter.next))
                go
            } else builder.result()

        go
    }

    /** Standard 4-way zip */
    def zip[A, B, C, D, Out](as: Iterable[A], bs: Iterable[B], cs: Iterable[C], ds: Iterable[D])(implicit cb: CanBuild[(A,B,C,D), Out]): Out = {
        val aIter = as.iterator
        val bIter = bs.iterator
        val cIter = cs.iterator
        val dIter = ds.iterator

        val builder = cb()

        def go: Out =
            if (aIter.hasNext && bIter.hasNext && cIter.hasNext && dIter.hasNext) {
                builder += ((aIter.next, bIter.next, cIter.next, dIter.next))
                go
            } else builder.result()

        go
    }

    /** Standard 5-way zip */
    def zip[A, B, C, D, E, Out](as: Iterable[A], bs: Iterable[B], cs: Iterable[C], ds: Iterable[D], es: Iterable[E])(implicit cb: CanBuild[(A,B,C,D,E), Out]): Out = {
        val aIter = as.iterator
        val bIter = bs.iterator
        val cIter = cs.iterator
        val dIter = ds.iterator
        val eIter = es.iterator

        val builder = cb()

        def go: Out =
            if (aIter.hasNext && bIter.hasNext && cIter.hasNext && dIter.hasNext && eIter.hasNext) {
                builder += ((aIter.next, bIter.next, cIter.next, dIter.next, eIter.next))
                go
            } else builder.result()

        go
    }

    /** Implicitly convert an Iterable to an IterableEitherOps that has a partitioner */
    implicit def iterableEitherOps[A, B](in: Iterable[Either[A,B]]): IterableEitherOps[A,B] = IterableEitherOps(in)

    /** Implicitly add additional operations to IterableOnce */
    implicit def traversableOnceOps[A](in: TraversableOnce[A]): TraversableOnceOps[A] = TraversableOnceOps(in)

    final case class IterableEitherOps[A,B](iterable: Iterable[Either[A,B]]) {
        /** Partition the incoming iterable into two resulting sequences (a, b) composed of only the values inside Left or Right respectively */
        def partitionEither[ThatA, ThatB](implicit cba: CanBuild[A, ThatA], cbb: CanBuild[B, ThatB]): (ThatA, ThatB) = {
            val buildera = cba()
            val builderb = cbb()
            val iterator = iterable.iterator
            while (iterator.hasNext) {
                iterator.next match {
                    case Left(a) => buildera += a
                    case Right(b) => builderb += b
                }
            }
            (buildera.result(), builderb.result())
        }
    }

    final case class TraversableOnceOps[A](traversableOnce: TraversableOnce[A]) {
        /** Find the minimum value or yield None if the traversable is empty */
        def minOrNone(implicit cmp: Ordering[A]): Option[A] = traversableOnce.reduceOption(cmp.min)

        /** Find the minimum value or yield the specified defaultif the traversable is empty */
        def minOrElse(default: => A)(implicit cmp: Ordering[A]): A = minOrNone(cmp) getOrElse default

        /** Find the maximum value or yield None if the traversable is empty */
        def maxOrNone(implicit cmp: Ordering[A]): Option[A] = traversableOnce.reduceOption(cmp.max)

        /** Find the maximum value or yield the specified defaultif the traversable is empty */
        def maxOrElse(default: => A)(implicit cmp: Ordering[A]): A = maxOrNone(cmp) getOrElse default
    }

}
