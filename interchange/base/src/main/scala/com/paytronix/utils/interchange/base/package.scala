//
// Copyright 2014 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange

import scala.annotation.{Annotation, StaticAnnotation}
import scala.language.higherKinds

import scalaz.{BijectionT, Show}

import com.paytronix.utils.scala.concurrent.ThreadLocal
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, ResultG, parameter}

package base {
    /**
     * Type of functions which encode values to some kind of formatted output, e.g. JSON or Avro.
     * The output is some kind of receiver sink to make encoding in a streaming fashion possible and efficient
     */
    trait Encoder[A, F <: OutputFormat] {
        /** Encode the value `in` to the sink `out`, yielding `Okay.unit` on success and some other `FailedG` otherwise */
        def run(in: A, out: F#Sink): CoderResult[Unit]

        /** Map the input type of the encoder with a function that might fail */
        def mapKleisli[B](k: B => Result[A]): Encoder[B, F]

        /**
         * Map the input type of the encoder with a pure function.
         * Note: while you can implement map in terms of mapKleisli, this trait doesn't do so since often mapKleisli will have its return type
         * narrowed and map should match that
         */
        def map[B](f: B => A): Encoder[B, F]
    }

    /** Type of functions which produce values from some kind of formatted input, e.g. JSON or Avro */
    trait Decoder[A, F <: InputFormat]  {
        /** Decode from the source `in` to the receiver sink `out`, yielding `Okay.unit` on success and some other `FailedG` otherwise */
        def run(in: F#Source, out: Receiver[A]): CoderResult[Unit]

        /** Decode from the source `in` and yield `Okay(decoded)` on success */
        def apply(in: F#Source): CoderResult[A] = {
            val r = new Receiver[A]
            run(in, r) map { _ => r.value }
        }

        /** Map the output type of the decoder with a function that might fail */
        def mapKleisli[B](k: A => Result[B]): Decoder[B, F]

        /**
         * Map the output type of the decoder with a pure function.
         * Note: while you can implement map in terms of mapKleisli, this trait doesn't do so since often mapKleisli will have its return type
         * narrowed and map should match that
         */
        def map[B](f: A => B): Decoder[B, F]
    }

    /** Contains a matched pair of an `Encoder` and `Decoder` for some type */
    trait Coder[Enc[A] <: Encoder[A, F], Dec[A] <: Decoder[A, F], A, F <: Format] {
        /** Encoder for the type `A` using format `F` */
        val encode: Enc[A]

        /** Decoder for the type `A` using format `F` */
        val decode: Dec[A]

        /**
         * Wrap the Coder (pair) in a bijection, producing a derived `Coder` for type `B`.
         * The bijection is "to" the underlying type (`A`), that is `bijection(B => Result[A], A => Result[B])`
         */
        def mapBijection[B](bijection: BijectionT[Result, Result, B, A]): Coder[Enc, Dec, B, F]
    }

    /** Trait for a format that can be decoded from */
    trait InputFormat {
        type Source
    }

    /** Trait for a format that can be encoded to */
    trait OutputFormat {
        type Sink
    }

    /** Trait of output formats where encoding has no observable side effects, only the return value */
    trait ReceiverOutputFormat extends OutputFormat {
        type Sink = Receiver[Out]
        type Out
    }

    /**
     * Write cell for `ReceiverOutputFormat`s and `Decoder`s to "return" their value with.
     *
     * Nominally, instead of providing a writable cell of any sort the return type of an `Encoder` would be `A`, however
     * any kind of incremental sink-style output such as Avro or Jackson would then be heavily penalized by having to
     * construct an output closure of type Sink => Result[Unit] or similar which is unacceptably wasteful.
     *
     * So to avoid closure allocations, instead we use this awful hack of a writable cell. Since Scala is eager we can make the
     * social contract fairly easily that `Encoder`s for `ReceiverOutputFormat`s will not keep a reference
     * to a `Receiver` and will instead pass it down to subsidary `Encoder`s with its type magically changed by the magics
     * of unsafe type coercion.
     *
     * That way a minimum of allocations will occur, as a subsidiary encoder will just push the value into the cell where
     * the outside encoder will recover it. Once the outside encoder creates its value, it will then push that value into
     * the cell, and so on all the way out.
     */
    final class Receiver[A] {
        private var _value: A = _ // bleck
        @inline def value: A = _value
        @inline def apply(a: A): CoderResult[Unit] = { _value = a; Okay.unit }
    }

    /** Trait for a format that can be both decoded from and encoded to */
    trait Format extends InputFormat with OutputFormat

    /**
     * Trait for a format that can be both decoded from and encoded to, and encoding uses a `Receiver` not some
     * kind of writing sink
     */
    trait ReceiverFormat extends Format with ReceiverOutputFormat


    /** Annotation indicating that a property should have a separate name when encoded from the Scala/Java identifier */
    class name(name: String) extends StaticAnnotation

    /** Annotation indicating that a val property should be encoded even though it won't be decoded */
    class coded extends StaticAnnotation

    /** Annotation indicating that a property should not be coded, even if it would normally be */
    class notCoded extends StaticAnnotation

    /** Annotation indicating that a property can take the value `null` */
    class nullable extends StaticAnnotation

    /** Annotation indicating the default value a property should have when not present in the input */
    class default(value: Any) extends StaticAnnotation

    /**
     * Annotation that indicates some type is the base type of a union.
     *
     * Alternatives are identified by type using `alt[A]` and can be given an explicit string tag with `.tag`, for example:
     *
     *    union(alt[First].tag("explicit tag"), alt[Second])
     *
     * Some formats will use the tag and some will have some other way of identifying the union alternatives. In the case where the
     * tag is used but no explicit tag is given, the base name of the type (in the example, `"Second"`) will be used.
     */
    class union(alternatives: union.Alternative[Any]*) extends StaticAnnotation

    object union {
        class Alternative[+A] {
            def tag(tag: String) = this
        }

        def alt[A]: Alternative[A] = new Alternative[A]
    }


    /** Single component of a path through the data model for error reporting */
    sealed abstract class Segment

    /** Path component representing a named property of an object */
    final case class PropertySegment(prop: String) extends Segment { override def toString = "/" + prop }

    /** Path component representing an indexed value in a collection */
    final case class IntIndexSegment(idx: Int) extends Segment { override def toString = "[" + idx + "]" }

    /** Path component representing a keyed value in a mapping or object */
    final case class OtherIndexSegment[A: Show](idx: A) extends Segment { override def toString = "[" + Show[A].show(idx) + "]" }

    /**
     * Holds a boolean indicating whether or not the current decoding/encoding should be considered in a secure context
     * or not and either enables or disables insecure coding.
     */
    object InsecureContext extends ThreadLocal[Boolean] {
        protected val initial = true
    }

    /**
     * Holds a `ClassLoader` to when decoding classes by name.
     */
    final case class InterchangeClassLoader(classLoader: ClassLoader)

    object InterchangeClassLoader {
        implicit def contextClassLoader = InterchangeClassLoader(Thread.currentThread.getContextClassLoader)
    }
}

package object base {
     /** Result type for encoding and decoding which carries the path in the data model to the failure */
    type CoderResult[A] = ResultG[FailedPath, A]

    /** Path through the data model to a coding failure */
    type FailedPath = List[Segment]


    /** Replace some failed parameter with an empty `FailedPath` */
    val terminal: FailedG[Any] => FailedG[FailedPath] = { case FailedG(t, _) => FailedG(t, Nil) }

    /** Convert a Result[A] to CoderResult[A] */
    def atTerminal[A](f: Result[A]): CoderResult[A] =
        f | terminal

    /** Equivalent to trackFailedPath with an IntIndexSegment */
    def atIndex[A](idx: Int)(f: CoderResult[A]): CoderResult[A] =
        f mapFailure { IntIndexSegment(idx) :: _ }

    /** Equivalent to trackFailedPath with an OtherIndexSegment */
    def atIndex[A: Show, B](idx: A)(f: CoderResult[B]): CoderResult[B] =
        f mapFailure { OtherIndexSegment(idx) :: _ }

    /** Equivalent to trackFailedPath with a PropertySegment  */
    def atProperty[A](prop: String)(f: CoderResult[A]): CoderResult[A] = f mapFailure { PropertySegment(prop) :: _ }

    /** Convert a ParamFailed containing a path back to a "normal" Failed with a prefix to the message */
    def formatFailedPath[A](in: CoderResult[A]): Result[A] = in match {
        case Okay(value) => Okay(value)
        case FailedG(throwable, path) => {
            val formattedPath = path match {
                case PropertySegment(_) :: _ => path.mkString("").substring(1)
                case _ => path.mkString("")
            }
            val msg = throwable.getMessage match {
                case null => throwable.toString
                case s => s
            }
            Failed((if (formattedPath.isEmpty) "" else "At " + formattedPath + ": ") + msg, throwable)
        }
    }
}
