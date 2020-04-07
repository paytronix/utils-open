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

package com.paytronix.utils.interchange.format.json

import com.fasterxml.jackson.core.JsonToken

import com.paytronix.utils.interchange.base.Receiver

object tuple extends tuple

trait tuple {
    /** Coder for `Tuple1`, the rarely used single-element tuple type. Codes as a JSON array of one element */
    def tuple1JsonCoder[A](coderA: JsonCoder[A]): JsonCoder[Tuple1[A]] =
        tuple1JsonCoder(coderA.encode, coderA.decode)

    /** Coder for `Tuple1`, the rarely used single-element tuple type. Codes as a JSON array of one element */
    implicit def tuple1JsonCoder[A] (
        implicit encoderA: JsonEncoder[A],
                 decoderA: JsonDecoder[A]
    ): JsonCoder[Tuple1[A]] =
        JsonCoder.make(tuple1JsonEncoder(encoderA), tuple1JsonDecoder(decoderA))

    def tuple1JsonEncoder[A](implicit encoderA: JsonEncoder[A]): JsonEncoder[Tuple1[A]] = new JsonEncoder[Tuple1[A]] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: Tuple1[A], out: InterchangeJsonGenerator) =
            out.writeStartArray() >>
            encoderA.run(in._1, out) >>
            out.writeEndArray()
    }

    def tuple1JsonDecoder[A](implicit decoderA: JsonDecoder[A]): JsonDecoder[Tuple1[A]] = new JsonDecoder[Tuple1[A]] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: InterchangeJsonParser, out: Receiver[Tuple1[A]]) = {
            val receiverA = new Receiver[A]
            in.require(JsonToken.START_ARRAY) >>
            in.advanceToken() >> decoderA.run(in, receiverA) >>
            in.advanceToken() >> in.require(JsonToken.END_ARRAY) >>
            out(Tuple1(receiverA.value))
        }
    }

    /** Coder for 2-tuples. Codes as a JSON array of 2 elements */
    def tuple2JsonCoder[A, B](coderA: JsonCoder[A], coderB: JsonCoder[B]): JsonCoder[(A, B)] =
        tuple2JsonCoder(coderA.encode, coderA.decode, coderB.encode, coderB.decode)

    /** Coder for 2-tuples. Codes as a JSON array of 2 elements */
    implicit def tuple2JsonCoder[A, B] (
        implicit encoderA: JsonEncoder[A],
                 decoderA: JsonDecoder[A],
                 encoderB: JsonEncoder[B],
                 decoderB: JsonDecoder[B]
    ): JsonCoder[(A, B)] =
        JsonCoder.make(tuple2JsonEncoder(encoderA, encoderB), tuple2JsonDecoder(decoderA, decoderB))

    /** Encoder for 2-tuples. Encodes as a JSON array of 2 elements */
    def tuple2JsonEncoder[A, B](implicit encoderA: JsonEncoder[A], encoderB: JsonEncoder[B]): JsonEncoder[(A, B)] = new JsonEncoder[(A, B)] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: (A, B), out: InterchangeJsonGenerator) =
            out.writeStartArray() >>
            encoderA.run(in._1, out) >>
            encoderB.run(in._2, out) >>
            out.writeEndArray()
    }

    /** Decoder for 2-tuples. Decodes from a JSON array of 2 elements */
    def tuple2JsonDecoder[A, B](implicit decoderA: JsonDecoder[A], decoderB: JsonDecoder[B]): JsonDecoder[(A, B)] = new JsonDecoder[(A, B)] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: InterchangeJsonParser, out: Receiver[(A, B)]) = {
            val receiverA = new Receiver[A]
            val receiverB = new Receiver[B]
            in.require(JsonToken.START_ARRAY) >>
            in.advanceToken() >> decoderA.run(in, receiverA) >>
            in.advanceToken() >> decoderB.run(in, receiverB) >>
            in.advanceToken() >> in.require(JsonToken.END_ARRAY) >>
            out((receiverA.value, receiverB.value))
        }
    }

    /** Coder for 3-tuples. Codes as a JSON array of 3 elements */
    def tuple3JsonCoder[A, B, C](coderA: JsonCoder[A], coderB: JsonCoder[B], coderC: JsonCoder[C]): JsonCoder[(A, B, C)] =
        tuple3JsonCoder(coderA.encode, coderA.decode, coderB.encode, coderB.decode, coderC.encode, coderC.decode)

    /** Coder for 3-tuples. Codes as a JSON array of 3 elements */
    implicit def tuple3JsonCoder[A, B, C] (
        implicit encoderA: JsonEncoder[A],
                 decoderA: JsonDecoder[A],
                 encoderB: JsonEncoder[B],
                 decoderB: JsonDecoder[B],
                 encoderC: JsonEncoder[C],
                 decoderC: JsonDecoder[C]
    ): JsonCoder[(A, B, C)] =
        JsonCoder.make(tuple3JsonEncoder(encoderA, encoderB, encoderC), tuple3JsonDecoder(decoderA, decoderB, decoderC))

    /** Encoder for 3-tuples. Encodes as a JSON array of 3 elements */
    def tuple3JsonEncoder[A, B, C](implicit encoderA: JsonEncoder[A], encoderB: JsonEncoder[B], encoderC: JsonEncoder[C]): JsonEncoder[(A, B, C)] = new JsonEncoder[(A, B, C)] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: (A, B, C), out: InterchangeJsonGenerator) =
            out.writeStartArray() >>
            encoderA.run(in._1, out) >>
            encoderB.run(in._2, out) >>
            encoderC.run(in._3, out) >>
            out.writeEndArray()
    }

    /** Decoder for 3-tuples. Decodes from a JSON array of 3 elements */
    def tuple3JsonDecoder[A, B, C](implicit decoderA: JsonDecoder[A], decoderB: JsonDecoder[B], decoderC: JsonDecoder[C]): JsonDecoder[(A, B, C)] = new JsonDecoder[(A, B, C)] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: InterchangeJsonParser, out: Receiver[(A, B, C)]) = {
            val receiverA = new Receiver[A]
            val receiverB = new Receiver[B]
            val receiverC = new Receiver[C]
            in.require(JsonToken.START_ARRAY) >>
            in.advanceToken() >> decoderA.run(in, receiverA) >>
            in.advanceToken() >> decoderB.run(in, receiverB) >>
            in.advanceToken() >> decoderC.run(in, receiverC) >>
            in.advanceToken() >> in.require(JsonToken.END_ARRAY) >>
            out((receiverA.value, receiverB.value, receiverC.value))
        }
    }

    /** Coder for 4-tuples. Codes as a JSON array of 4 elements */
    def tuple4JsonCoder[A, B, C, D](coderA: JsonCoder[A], coderB: JsonCoder[B], coderC: JsonCoder[C], coderD: JsonCoder[D]): JsonCoder[(A, B, C, D)] =
        tuple4JsonCoder(coderA.encode, coderA.decode, coderB.encode, coderB.decode, coderC.encode, coderC.decode, coderD.encode, coderD.decode)

    /** Coder for 4-tuples. Codes as a JSON array of 4 elements */
    implicit def tuple4JsonCoder[A, B, C, D] (
        implicit encoderA: JsonEncoder[A],
                 decoderA: JsonDecoder[A],
                 encoderB: JsonEncoder[B],
                 decoderB: JsonDecoder[B],
                 encoderC: JsonEncoder[C],
                 decoderC: JsonDecoder[C],
                 encoderD: JsonEncoder[D],
                 decoderD: JsonDecoder[D]
    ): JsonCoder[(A, B, C, D)] =
        JsonCoder.make(tuple4JsonEncoder(encoderA, encoderB, encoderC, encoderD), tuple4JsonDecoder(decoderA, decoderB, decoderC, decoderD))

    /** Encoder for 4-tuples. Encodes as a JSON array of 4 elements */
    def tuple4JsonEncoder[A, B, C, D](implicit encoderA: JsonEncoder[A], encoderB: JsonEncoder[B], encoderC: JsonEncoder[C], encoderD: JsonEncoder[D]): JsonEncoder[(A, B, C, D)] = new JsonEncoder[(A, B, C, D)] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: (A, B, C, D), out: InterchangeJsonGenerator) =
            out.writeStartArray() >>
            encoderA.run(in._1, out) >>
            encoderB.run(in._2, out) >>
            encoderC.run(in._3, out) >>
            encoderD.run(in._4, out) >>
            out.writeEndArray()
    }

    /** Decoder for 4-tuples. Decodes from a JSON array of 4 elements */
    def tuple4JsonDecoder[A, B, C, D](implicit decoderA: JsonDecoder[A], decoderB: JsonDecoder[B], decoderC: JsonDecoder[C], decoderD: JsonDecoder[D]): JsonDecoder[(A, B, C, D)] = new JsonDecoder[(A, B, C, D)] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: InterchangeJsonParser, out: Receiver[(A, B, C, D)]) = {
            val receiverA = new Receiver[A]
            val receiverB = new Receiver[B]
            val receiverC = new Receiver[C]
            val receiverD = new Receiver[D]
            in.require(JsonToken.START_ARRAY) >>
            in.advanceToken() >> decoderA.run(in, receiverA) >>
            in.advanceToken() >> decoderB.run(in, receiverB) >>
            in.advanceToken() >> decoderC.run(in, receiverC) >>
            in.advanceToken() >> decoderD.run(in, receiverD) >>
            in.advanceToken() >> in.require(JsonToken.END_ARRAY) >>
            out((receiverA.value, receiverB.value, receiverC.value, receiverD.value))
        }
    }

    /** Coder for 5-tuples. Codes as a JSON array of 5 elements */
    def tuple5JsonCoder[A, B, C, D, E](coderA: JsonCoder[A], coderB: JsonCoder[B], coderC: JsonCoder[C], coderD: JsonCoder[D], coderE: JsonCoder[E]): JsonCoder[(A, B, C, D, E)] =
        tuple5JsonCoder(coderA.encode, coderA.decode, coderB.encode, coderB.decode, coderC.encode, coderC.decode, coderD.encode, coderD.decode, coderE.encode, coderE.decode)

    /** Coder for 5-tuples. Codes as a JSON array of 5 elements */
    implicit def tuple5JsonCoder[A, B, C, D, E] (
        implicit encoderA: JsonEncoder[A],
                 decoderA: JsonDecoder[A],
                 encoderB: JsonEncoder[B],
                 decoderB: JsonDecoder[B],
                 encoderC: JsonEncoder[C],
                 decoderC: JsonDecoder[C],
                 encoderD: JsonEncoder[D],
                 decoderD: JsonDecoder[D],
                 encoderE: JsonEncoder[E],
                 decoderE: JsonDecoder[E]
    ): JsonCoder[(A, B, C, D, E)] =
        JsonCoder.make(tuple5JsonEncoder(encoderA, encoderB, encoderC, encoderD, encoderE), tuple5JsonDecoder(decoderA, decoderB, decoderC, decoderD, decoderE))

    /** Encoder for 5-tuples. Encodes as a JSON array of 5 elements */
    def tuple5JsonEncoder[A, B, C, D, E](implicit encoderA: JsonEncoder[A], encoderB: JsonEncoder[B], encoderC: JsonEncoder[C], encoderD: JsonEncoder[D], encoderE: JsonEncoder[E]): JsonEncoder[(A, B, C, D, E)] = new JsonEncoder[(A, B, C, D, E)] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: (A, B, C, D, E), out: InterchangeJsonGenerator) =
            out.writeStartArray() >>
            encoderA.run(in._1, out) >>
            encoderB.run(in._2, out) >>
            encoderC.run(in._3, out) >>
            encoderD.run(in._4, out) >>
            encoderE.run(in._5, out) >>
            out.writeEndArray()
    }

    /** Decoder for 5-tuples. Decodes from a JSON array of 5 elements */
    def tuple5JsonDecoder[A, B, C, D, E](implicit decoderA: JsonDecoder[A], decoderB: JsonDecoder[B], decoderC: JsonDecoder[C], decoderD: JsonDecoder[D], decoderE: JsonDecoder[E]): JsonDecoder[(A, B, C, D, E)] = new JsonDecoder[(A, B, C, D, E)] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: InterchangeJsonParser, out: Receiver[(A, B, C, D, E)]) = {
            val receiverA = new Receiver[A]
            val receiverB = new Receiver[B]
            val receiverC = new Receiver[C]
            val receiverD = new Receiver[D]
            val receiverE = new Receiver[E]
            in.require(JsonToken.START_ARRAY) >>
            in.advanceToken() >> decoderA.run(in, receiverA) >>
            in.advanceToken() >> decoderB.run(in, receiverB) >>
            in.advanceToken() >> decoderC.run(in, receiverC) >>
            in.advanceToken() >> decoderD.run(in, receiverD) >>
            in.advanceToken() >> decoderE.run(in, receiverE) >>
            in.advanceToken() >> in.require(JsonToken.END_ARRAY) >>
            out((receiverA.value, receiverB.value, receiverC.value, receiverD.value, receiverE.value))
        }
    }

    /** Coder for 6-tuples. Codes as a JSON array of 6 elements */
    def tuple6JsonCoder[A, B, C, D, E, F](coderA: JsonCoder[A], coderB: JsonCoder[B], coderC: JsonCoder[C], coderD: JsonCoder[D], coderE: JsonCoder[E], coderF: JsonCoder[F]): JsonCoder[(A, B, C, D, E, F)] =
        tuple6JsonCoder(coderA.encode, coderA.decode, coderB.encode, coderB.decode, coderC.encode, coderC.decode, coderD.encode, coderD.decode, coderE.encode, coderE.decode, coderF.encode, coderF.decode)

    /** Coder for 6-tuples. Codes as a JSON array of 6 elements */
    implicit def tuple6JsonCoder[A, B, C, D, E, F] (
        implicit encoderA: JsonEncoder[A],
                 decoderA: JsonDecoder[A],
                 encoderB: JsonEncoder[B],
                 decoderB: JsonDecoder[B],
                 encoderC: JsonEncoder[C],
                 decoderC: JsonDecoder[C],
                 encoderD: JsonEncoder[D],
                 decoderD: JsonDecoder[D],
                 encoderE: JsonEncoder[E],
                 decoderE: JsonDecoder[E],
                 encoderF: JsonEncoder[F],
                 decoderF: JsonDecoder[F]
    ): JsonCoder[(A, B, C, D, E, F)] =
        JsonCoder.make(tuple6JsonEncoder(encoderA, encoderB, encoderC, encoderD, encoderE, encoderF),
                       tuple6JsonDecoder(decoderA, decoderB, decoderC, decoderD, decoderE, decoderF))

    /** Encoder for 6-tuples. Encodes as a JSON array of 6 elements */
    def tuple6JsonEncoder[A, B, C, D, E, F](implicit encoderA: JsonEncoder[A], encoderB: JsonEncoder[B], encoderC: JsonEncoder[C], encoderD: JsonEncoder[D], encoderE: JsonEncoder[E], encoderF: JsonEncoder[F]): JsonEncoder[(A, B, C, D, E, F)] = new JsonEncoder[(A, B, C, D, E, F)] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: (A, B, C, D, E, F), out: InterchangeJsonGenerator) =
            out.writeStartArray() >>
            encoderA.run(in._1, out) >>
            encoderB.run(in._2, out) >>
            encoderC.run(in._3, out) >>
            encoderD.run(in._4, out) >>
            encoderE.run(in._5, out) >>
            encoderF.run(in._6, out) >>
            out.writeEndArray()
    }

    /** Decoder for 6-tuples. Decodes from a JSON array of 6 elements */
    def tuple6JsonDecoder[A, B, C, D, E, F](implicit decoderA: JsonDecoder[A], decoderB: JsonDecoder[B], decoderC: JsonDecoder[C], decoderD: JsonDecoder[D], decoderE: JsonDecoder[E], decoderF: JsonDecoder[F]): JsonDecoder[(A, B, C, D, E, F)] = new JsonDecoder[(A, B, C, D, E, F)] {
        val mightBeNull = false
        val codesAsObject = false

        def run(in: InterchangeJsonParser, out: Receiver[(A, B, C, D, E, F)]) = {
            val receiverA = new Receiver[A]
            val receiverB = new Receiver[B]
            val receiverC = new Receiver[C]
            val receiverD = new Receiver[D]
            val receiverE = new Receiver[E]
            val receiverF = new Receiver[F]
            in.require(JsonToken.START_ARRAY) >>
            in.advanceToken() >> decoderA.run(in, receiverA) >>
            in.advanceToken() >> decoderB.run(in, receiverB) >>
            in.advanceToken() >> decoderC.run(in, receiverC) >>
            in.advanceToken() >> decoderD.run(in, receiverD) >>
            in.advanceToken() >> decoderE.run(in, receiverE) >>
            in.advanceToken() >> decoderF.run(in, receiverF) >>
            in.advanceToken() >> in.require(JsonToken.END_ARRAY) >>
            out((receiverA.value, receiverB.value, receiverC.value, receiverD.value, receiverE.value, receiverF.value))
        }
    }

    // if anybody wants more than 6, then they can work on the macro to generate the boilerplate automatically
}
