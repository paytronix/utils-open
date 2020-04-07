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

package com.paytronix.utils.interchange

import scala.annotation.StaticAnnotation
import scala.language.higherKinds

import cats.Show

import com.paytronix.utils.scala.concurrent.ThreadLocal
import com.paytronix.utils.scala.result.{Failed, FailedG, FailedParameter, FailedParameterDefault, Okay, Result, ResultG}

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
         * Run the Coder (pair) through a maybe-failing conversion between types, producing a derived `Coder` for type `B`.
         * The conversion is "to" the underlying type (`A`), that is `TypeConverter(B => Result[A], A => Result[B])`
         */
        def mapWithConverter[B](converter: TypeConverter[B, A]): Coder[Enc, Dec, B, F]
    }

    /**
      * Contains a pair of functions that convert from some input type to an output type and back again with no guarantee
      * that said conversions will be successful for any given input, nor that conversions will be lossless/reversible.
      */
    final case class TypeConverter[From, To](
        to:   From => Result[To],
        from: To   => Result[From]
    ) {
        def compose[NewTo](other: TypeConverter[To, NewTo]): TypeConverter[From, NewTo] = {
            def newTo(in: From): Result[NewTo] = this.to(in).flatMap(x => other.to(x))
            def newFrom(in: NewTo): Result[From] = other.from(in).flatMap(x => this.from(x))
            TypeConverter(newTo, newFrom)
        }

        def flip: TypeConverter[To, From] = TypeConverter(from, to)
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
    class name(val name: String) extends StaticAnnotation

    /** Annotation indicating the default value a property should have when not present in the input */
    class default(val value: Any) extends StaticAnnotation

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
        protected val initial = false
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
    type CoderResult[A] = ResultG[CoderFailure, A]

    /** Location of the coding failure in the source material, if known, along with the failure in the logical model */
    final case class CoderFailure(sourceLocation: Option[String], path: FailedPath) extends FailedParameter

    object CoderFailure {
        /** "Zero" for CoderFailure indicating failure at a leaf node in the logical model and an unknown source location */
        val terminal = CoderFailure(None, Nil)

        /** CoderFailure indicating failure at a leaf node in the logical model and a known source location */
        def terminalAt(location: String) = CoderFailure(Some(location), Nil)

        implicit val failedParameterDefault = new FailedParameterDefault[CoderFailure] {
            val default = terminal
        }
    }

    /** Path through the data model to a coding failure */
    type FailedPath = List[Segment]

    /** Replace some failed parameter with an empty `FailedPath` */
    val terminal: FailedG[Any] => FailedG[CoderFailure] = { case FailedG(t, _) => FailedG(t, CoderFailure.terminal) }

    /** Replace some failed parameter with an empty `FailedPath` but a known location */
    def terminalLocation(location: String): FailedG[Any] => FailedG[CoderFailure] =
        { case FailedG(t, _) => FailedG(t, CoderFailure.terminalAt(location)) }

    /** Convert a Result[A] to CoderResult[A] at a terminal node */
    def atTerminal[A](f: Result[A]): CoderResult[A] =
        f | terminal

    /** Convert a Result[A] to CoderResult[A] at a terminal node with a known location */
    def atTerminalLocation[A](location: String)(f: Result[A]): CoderResult[A] =
        f | terminalLocation(location)

    /** Equivalent to trackFailedPath with an IntIndexSegment */
    def atIndex[A](idx: Int)(f: CoderResult[A]): CoderResult[A] =
        f mapFailure { cf => cf.copy(path = IntIndexSegment(idx) :: cf.path) }

    /** Equivalent to trackFailedPath with an OtherIndexSegment */
    def atIndex[A: Show, B](idx: A)(f: CoderResult[B]): CoderResult[B] =
        f mapFailure { cf => cf.copy(path = OtherIndexSegment(idx) :: cf.path) }

    /** Equivalent to trackFailedPath with a PropertySegment  */
    def atProperty[A](prop: String)(f: CoderResult[A]): CoderResult[A] =
        f mapFailure { cf => cf.copy(path = PropertySegment(prop) :: cf.path) }

    /** Convert a ParamFailed containing a path back to a "normal" Failed with a prefix to the message */
    def formatFailedPath[A](in: CoderResult[A]): Result[A] = in match {
        case Okay(value) => Okay(value)
        case FailedG(throwable, cf) => {
            lazy val formattedPath = cf.path match {
                case PropertySegment(_) :: _ => cf.path.mkString("").substring(1)
                case                       _ => cf.path.mkString("")
            }
            lazy val msg = throwable.getMessage match {
                case null => throwable.toString
                case s    => s
            }

            cf.sourceLocation match {
                case Some(loc) if !formattedPath.isEmpty =>
                    Failed(s"At source location $loc, field $formattedPath: $msg", throwable)
                case Some(loc) =>
                    Failed(s"At source location $loc: $msg", throwable)
                case None if !formattedPath.isEmpty =>
                    Failed(s"At $formattedPath: $msg", throwable)
                case None =>
                    Failed(msg, throwable)
            }
        }
    }
}
