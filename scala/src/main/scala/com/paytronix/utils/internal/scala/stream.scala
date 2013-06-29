//
// Copyright 2009-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

import scala.Stream
import scala.collection.mutable.Queue

object stream
{
    /** Produce a Stream from a java.util.Iterator */
    def stream[A](it: java.util.Iterator[A]): Stream[A] = it // triggers implicit javaIteratorToStream

    /** Produce a Stream from a java.util.Iterator */
    implicit def javaIteratorToStream[A](it: java.util.Iterator[A]): Stream[A] =
        if (it.hasNext) {
            Stream.cons(it.next, javaIteratorToStream(it))
        } else {
            Stream.empty
        }

    /** Produce a Stream from a java.util.Enumeration */
    implicit def javaEnumerationToStream[A](en: java.util.Enumeration[A]): Stream[A] =
        if (en.hasMoreElements) {
            Stream.cons(en.nextElement, javaEnumerationToStream(en))
        } else {
            Stream.empty
        }

    /**
     * Produce pairs of (key, list of values) from a stream of values by applying a function to extract the key
     * This method is strict by group (e.g. it will consume the input stream until it knows the group is completed)
     * but lazy between groups.
     *
     * For example, if you have a stream of tuples:
     *    (key, valueA, valueB, valueC), ...: Stream[(String, Int, Int, Int)]
     * But you really want a stream of keys with their values:
     *    (key, List((_, valueA1, valueB1, valueC1), (_, valueA2, valueB2, valueC2))), ...: Stream[(String, List[(String, Int, Int, Int)])]
     * Then you can use:
     *    streamGroupBy[String, (String, Int, Int, Int)](_._1)(vs)
     *
     * Example:
     * scala> val xs: Stream[(String, Int)] = Stream(("first", 1), ("first", 2), ("second", 3), ("third", 4), ("first", 5))
     * xs: Stream[(String, Int)] = Stream((first,1), ?)
     *
     * scala> val ys = streamGroupBy[String, (String, Int)](_._1)(xs)
     * ys: Stream[(String, List[(String, Int)])] = Stream((first,List((first,1), (first,2))), ?)
     *
     * scala> ys.toList
     * res0: List[(String, List[(String, Int)])] = List((first,List((first,1), (first,2))),
     *                                                  (second,List((second,3))),
     *                                                  (third,List((third,4))),
     *                                                  (first,List((first,5))))
     */
    def streamGroupBy[K, V](f: (V) => K)(vs: Stream[V]): Stream[(K, Seq[V])] = {
        def iterate(vs: Stream[V]): Stream[(K, Seq[V])] =
            if (vs.isEmpty) {
                Stream.empty
            } else {
                val firstV = vs.head
                val groupK = f(firstV)
                val q = new Queue[V]

                q += firstV

                def eat(vs: Stream[V]): Stream[V] =
                    if (vs.isEmpty) {
                        vs
                    } else {
                        val v = vs.head
                        val k = f(v)

                        if (k == groupK) {
                            q += v
                            eat(vs.tail)
                        } else {
                            vs
                        }
                    }
                val rest = eat(vs.tail)
                Stream.cons((groupK, q), iterate(rest))
            }
        iterate(vs)
    }

    implicit def streamGroupBy[A](s: Stream[A]): StreamOps[A] = StreamOps(s)

    final case class StreamOps[A](s: Stream[A]) {
        def streamGroupBy[B](f: A => B): Stream[(B, Seq[A])] =
            com.paytronix.utils.internal.scala.stream.streamGroupBy(f)(s)
    }
}
