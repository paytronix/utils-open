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

package com.paytronix.utils.interchange.format.string

import scala.annotation.implicitNotFound

import com.paytronix.utils.interchange.base.{Coder, CoderResult, Decoder, Encoder, Receiver, ReceiverFormat, TypeConverter, atTerminal}
import com.paytronix.utils.scala.result.{FailedG, Okay, Result}

/**
 * Format for encoding or decoding from strings, usually only for scalar values. Used for some formats like JSON to
 * encode/decode map keys
 */
object StringFormat extends ReceiverFormat {
    type Source = String
    type Out = String
}

object StringEncoder extends StringEncoderLPI {
    /** Materializer for StringEncoder, just finds the StringEncoder in implicit scope. For example: StringEncoder[Int] */
    def apply[A](implicit encoder: StringEncoder[A]): StringEncoder[A] = implicitly
}

trait StringEncoderLPI {
    implicit def fromCoder[A](implicit coder: StringCoder[A]): StringEncoder[A] = coder.encode
}

/** Encoder which encodes values to strings */
@implicitNotFound(msg="No StringEncoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.string.coders")
trait StringEncoder[A] extends Encoder[A, StringFormat.type] { outer =>
    def apply(in: A): CoderResult[String] = {
        val r = new Receiver[String]
        run(in, r) map { _ => r.value }
    }

    def map[B](f: B => A): StringEncoder[B] = mapKleisli(b => Okay(f(b)))

    def mapKleisli[B](k: B => Result[A]): StringEncoder[B] = new StringEncoder[B] {
        def run(b: B, sink: Receiver[String]) = atTerminal(k(b)) >>= { outer.run(_, sink) }
    }
}

/** Materializer for StringDecoder, just finds the StringDecoder in implicit scope. For example: StringDecoder[Int] */
object StringDecoder extends StringDecoderLPI {
    def apply[A](implicit decoder: StringDecoder[A]): StringDecoder[A] = implicitly
}

trait StringDecoderLPI {
    implicit def fromCoder[A](implicit coder: StringCoder[A]): StringDecoder[A] = coder.decode
}

/** Decoder which decodes values from strings */
@implicitNotFound(msg="No StringDecoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.string.coders")
trait StringDecoder[A] extends Decoder[A, StringFormat.type] { outer =>
    def map[B](f: A => B): StringDecoder[B] = mapKleisli(a => Okay(f(a)))

    def mapKleisli[B](k: A => Result[B]): StringDecoder[B] = new StringDecoder[B] {
        def run(source: String, outB: Receiver[B]) = {
            val outA = new Receiver[A]
            outer.run(source, outA) match {
                case _: Okay[_] =>
                    k(outA.value) match {
                        case Okay(b) => outB(b)
                        case failed@FailedG(_, _) => atTerminal(failed)
                    }
                case failed: FailedG[_] => failed
            }
        }
    }
}

object StringCoder {
    def apply[A](implicit coder: StringCoder[A]): StringCoder[A] = implicitly

    def make[A](implicit encoder: StringEncoder[A], decoder: StringDecoder[A]): StringCoder[A] =
        new StringCoder[A] {
            val encode = encoder
            val decode = decoder
        }
}

/** Coder (encoder/decoder pair) which codes to/from strings */
@implicitNotFound(msg="No StringCoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.string.coders")
trait StringCoder[A] extends Coder[StringEncoder, StringDecoder, A, StringFormat.type] {
    val encode: StringEncoder[A]
    val decode: StringDecoder[A]

    def mapWithConverter[B](converter: TypeConverter[B, A]) =
        StringCoder.make(encode.mapKleisli(converter.to), decode.mapKleisli(converter.from))
}
