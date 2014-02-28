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

package com.paytronix.utils.interchange.format.avro

import scala.collection.JavaConverters.seqAsJavaListConverter

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}
import org.apache.avro.{Schema, io}

import com.paytronix.utils.interchange.base.{Receiver, terminal}
import com.paytronix.utils.scala.result.tryCatch

import utils.{encodeSchemaName, makeField}

object tuple extends tuple

trait tuple {
    private def tupleSchema(schemata: (Schema, Option[JsonNode])*): Schema = {
        val s = Schema.createRecord("Tuple" + schemata.size + "__" + schemata.map { case (schema, _) => encodeSchemaName(schema) }.mkString(""), "", "scala.lang", false)
        s.setFields(schemata.zipWithIndex.map { case ((schema, defaultJson), index) => makeField("_" + (index+1), schema, defaultJson) }.asJava)
        s
    }

    implicit def tuple1AvroCoder[A] (
        implicit encoderA: AvroEncoder[A],
                 decoderA: AvroDecoder[A]
    ): AvroCoder[Tuple1[A]] =
        AvroCoder.make(tuple1AvroEncoder(encoderA), tuple1AvroDecoder(decoderA))

    def tuple1AvroEncoder[A](implicit encoderA: AvroEncoder[A]): AvroEncoder[Tuple1[A]] = new AvroEncoder[Tuple1[A]] {
        val schema = tupleSchema((encoderA.schema, encoderA.defaultJson))
        val defaultJson = None

        def encodeDefaultJson(in: Tuple1[A]) =
            for {
                a <- encoderA.encodeDefaultJson(in._1)
            } yield {
                val obj = jsonNodeFactory.objectNode
                obj.put("_1", a)
                obj
            }

        def run(in: Tuple1[A], out: io.Encoder) =
            tryCatch.resultG(terminal) {
                encoderA.run(in._1, out)
            }
    }

    def tuple1AvroDecoder[A](implicit decoderA: AvroDecoder[A]): AvroDecoder[Tuple1[A]] = new AvroDecoder[Tuple1[A]] {
        val schema = tupleSchema((decoderA.schema, decoderA.defaultJson))
        val defaultJson = None

        def run(in: io.ResolvingDecoder, out: Receiver[Tuple1[A]]) =
            tryCatch.resultG(terminal) {
                val receiverA = new Receiver[A]
                decoderA.run(in, receiverA) >>
                out(Tuple1(receiverA.value))
            }
    }

    implicit def tuple2AvroCoder[A, B] (
        implicit encoderA: AvroEncoder[A],
                 decoderA: AvroDecoder[A],
                 encoderB: AvroEncoder[B],
                 decoderB: AvroDecoder[B]
    ): AvroCoder[(A, B)] =
        AvroCoder.make(tuple2AvroEncoder(encoderA, encoderB), tuple2AvroDecoder(decoderA, decoderB))

    implicit def tuple2AvroEncoder[A, B](implicit encoderA: AvroEncoder[A], encoderB: AvroEncoder[B]): AvroEncoder[(A, B)] = new AvroEncoder[(A, B)] {
        val schema = tupleSchema((encoderA.schema, encoderA.defaultJson), (encoderB.schema, encoderB.defaultJson))
        val defaultJson = None

        def encodeDefaultJson(in: (A, B)) =
            for {
                a <- encoderA.encodeDefaultJson(in._1)
                b <- encoderB.encodeDefaultJson(in._2)
            } yield {
                val obj = jsonNodeFactory.objectNode
                obj.put("_1", a)
                obj.put("_2", b)
                obj
            }

        def run(in: (A, B), out: io.Encoder) =
            tryCatch.resultG(terminal) {
                encoderA.run(in._1, out) >>
                encoderB.run(in._2, out)
            }
    }

    implicit def tuple2AvroDecoder[A, B](implicit decoderA: AvroDecoder[A], decoderB: AvroDecoder[B]): AvroDecoder[(A, B)] = new AvroDecoder[(A, B)] {
        val schema = tupleSchema((decoderA.schema, decoderA.defaultJson), (decoderB.schema, decoderB.defaultJson))
        val defaultJson = None

        def run(in: io.ResolvingDecoder, out: Receiver[(A, B)]) =
            tryCatch.resultG(terminal) {
                val receiverA = new Receiver[A]
                val receiverB = new Receiver[B]
                decoderA.run(in, receiverA) >>
                decoderB.run(in, receiverB) >>
                out((receiverA.value, receiverB.value))
            }
    }

    implicit def tuple3AvroCoder[A, B, C] (
        implicit encoderA: AvroEncoder[A],
                 decoderA: AvroDecoder[A],
                 encoderB: AvroEncoder[B],
                 decoderB: AvroDecoder[B],
                 encoderC: AvroEncoder[C],
                 decoderC: AvroDecoder[C]
    ): AvroCoder[(A, B, C)] =
        AvroCoder.make(tuple3AvroEncoder(encoderA, encoderB, encoderC), tuple3AvroDecoder(decoderA, decoderB, decoderC))

    implicit def tuple3AvroEncoder[A, B, C](implicit encoderA: AvroEncoder[A], encoderB: AvroEncoder[B], encoderC: AvroEncoder[C]): AvroEncoder[(A, B, C)] = new AvroEncoder[(A, B, C)] {
        val schema = tupleSchema((encoderA.schema, encoderA.defaultJson), (encoderB.schema, encoderB.defaultJson), (encoderC.schema, encoderC.defaultJson))
        val defaultJson = None

        def encodeDefaultJson(in: (A, B, C)) =
            for {
                a <- encoderA.encodeDefaultJson(in._1)
                b <- encoderB.encodeDefaultJson(in._2)
                c <- encoderC.encodeDefaultJson(in._3)
            } yield {
                val obj = jsonNodeFactory.objectNode
                obj.put("_1", a)
                obj.put("_2", b)
                obj.put("_3", c)
                obj
            }

        def run(in: (A, B, C), out: io.Encoder) =
            tryCatch.resultG(terminal) {
                encoderA.run(in._1, out) >>
                encoderB.run(in._2, out) >>
                encoderC.run(in._3, out)
            }
    }

    implicit def tuple3AvroDecoder[A, B, C](implicit decoderA: AvroDecoder[A], decoderB: AvroDecoder[B], decoderC: AvroDecoder[C]): AvroDecoder[(A, B, C)] = new AvroDecoder[(A, B, C)] {
        val schema = tupleSchema((decoderA.schema, decoderA.defaultJson), (decoderB.schema, decoderB.defaultJson), (decoderC.schema, decoderC.defaultJson))
        val defaultJson = None

        def run(in: io.ResolvingDecoder, out: Receiver[(A, B, C)]) =
            tryCatch.resultG(terminal) {
                val receiverA = new Receiver[A]
                val receiverB = new Receiver[B]
                val receiverC = new Receiver[C]
                decoderA.run(in, receiverA) >>
                decoderB.run(in, receiverB) >>
                decoderC.run(in, receiverC) >>
                out((receiverA.value, receiverB.value, receiverC.value))
            }
    }

    implicit def tuple4AvroCoder[A, B, C, D] (
        implicit encoderA: AvroEncoder[A],
                 decoderA: AvroDecoder[A],
                 encoderB: AvroEncoder[B],
                 decoderB: AvroDecoder[B],
                 encoderC: AvroEncoder[C],
                 decoderC: AvroDecoder[C],
                 encoderD: AvroEncoder[D],
                 decoderD: AvroDecoder[D]
    ): AvroCoder[(A, B, C, D)] =
        AvroCoder.make(tuple4AvroEncoder(encoderA, encoderB, encoderC, encoderD), tuple4AvroDecoder(decoderA, decoderB, decoderC, decoderD))

    implicit def tuple4AvroEncoder[A, B, C, D](implicit encoderA: AvroEncoder[A], encoderB: AvroEncoder[B], encoderC: AvroEncoder[C], encoderD: AvroEncoder[D]): AvroEncoder[(A, B, C, D)] = new AvroEncoder[(A, B, C, D)] {
        val schema = tupleSchema((encoderA.schema, encoderA.defaultJson), (encoderB.schema, encoderB.defaultJson), (encoderC.schema, encoderC.defaultJson), (encoderD.schema, encoderD.defaultJson))
        val defaultJson = None

        def encodeDefaultJson(in: (A, B, C, D)) =
            for {
                a <- encoderA.encodeDefaultJson(in._1)
                b <- encoderB.encodeDefaultJson(in._2)
                c <- encoderC.encodeDefaultJson(in._3)
                d <- encoderD.encodeDefaultJson(in._4)
            } yield {
                val obj = jsonNodeFactory.objectNode
                obj.put("_1", a)
                obj.put("_2", b)
                obj.put("_3", c)
                obj.put("_4", d)
                obj
            }

        def run(in: (A, B, C, D), out: io.Encoder) =
            tryCatch.resultG(terminal) {
                encoderA.run(in._1, out) >>
                encoderB.run(in._2, out) >>
                encoderC.run(in._3, out) >>
                encoderD.run(in._4, out)
            }
    }

    implicit def tuple4AvroDecoder[A, B, C, D](implicit decoderA: AvroDecoder[A], decoderB: AvroDecoder[B], decoderC: AvroDecoder[C], decoderD: AvroDecoder[D]): AvroDecoder[(A, B, C, D)] = new AvroDecoder[(A, B, C, D)] {
        val schema = tupleSchema((decoderA.schema, decoderA.defaultJson), (decoderB.schema, decoderB.defaultJson), (decoderC.schema, decoderC.defaultJson), (decoderD.schema, decoderD.defaultJson))
        val defaultJson = None

        def run(in: io.ResolvingDecoder, out: Receiver[(A, B, C, D)]) =
            tryCatch.resultG(terminal) {
                val receiverA = new Receiver[A]
                val receiverB = new Receiver[B]
                val receiverC = new Receiver[C]
                val receiverD = new Receiver[D]
                decoderA.run(in, receiverA) >>
                decoderB.run(in, receiverB) >>
                decoderC.run(in, receiverC) >>
                decoderD.run(in, receiverD) >>
                out((receiverA.value, receiverB.value, receiverC.value, receiverD.value))
            }
    }

    implicit def tuple5AvroCoder[A, B, C, D, E] (
        implicit encoderA: AvroEncoder[A],
                 decoderA: AvroDecoder[A],
                 encoderB: AvroEncoder[B],
                 decoderB: AvroDecoder[B],
                 encoderC: AvroEncoder[C],
                 decoderC: AvroDecoder[C],
                 encoderD: AvroEncoder[D],
                 decoderD: AvroDecoder[D],
                 encoderE: AvroEncoder[E],
                 decoderE: AvroDecoder[E]
    ): AvroCoder[(A, B, C, D, E)] =
        AvroCoder.make(tuple5AvroEncoder(encoderA, encoderB, encoderC, encoderD, encoderE), tuple5AvroDecoder(decoderA, decoderB, decoderC, decoderD, decoderE))

    implicit def tuple5AvroEncoder[A, B, C, D, E](implicit encoderA: AvroEncoder[A], encoderB: AvroEncoder[B], encoderC: AvroEncoder[C], encoderD: AvroEncoder[D], encoderE: AvroEncoder[E]): AvroEncoder[(A, B, C, D, E)] = new AvroEncoder[(A, B, C, D, E)] {
        val schema = tupleSchema((encoderA.schema, encoderA.defaultJson), (encoderB.schema, encoderB.defaultJson), (encoderC.schema, encoderC.defaultJson), (encoderD.schema, encoderD.defaultJson), (encoderE.schema, encoderE.defaultJson))
        val defaultJson = None

        def encodeDefaultJson(in: (A, B, C, D, E)) =
            for {
                a <- encoderA.encodeDefaultJson(in._1)
                b <- encoderB.encodeDefaultJson(in._2)
                c <- encoderC.encodeDefaultJson(in._3)
                d <- encoderD.encodeDefaultJson(in._4)
                e <- encoderE.encodeDefaultJson(in._5)
            } yield {
                val obj = jsonNodeFactory.objectNode
                obj.put("_1", a)
                obj.put("_2", b)
                obj.put("_3", c)
                obj.put("_4", d)
                obj.put("_5", e)
                obj
            }

        def run(in: (A, B, C, D, E), out: io.Encoder) =
            tryCatch.resultG(terminal) {
                encoderA.run(in._1, out) >>
                encoderB.run(in._2, out) >>
                encoderC.run(in._3, out) >>
                encoderD.run(in._4, out) >>
                encoderE.run(in._5, out)
            }
    }

    implicit def tuple5AvroDecoder[A, B, C, D, E](implicit decoderA: AvroDecoder[A], decoderB: AvroDecoder[B], decoderC: AvroDecoder[C], decoderD: AvroDecoder[D], decoderE: AvroDecoder[E]): AvroDecoder[(A, B, C, D, E)] = new AvroDecoder[(A, B, C, D, E)] {
        val schema = tupleSchema((decoderA.schema, decoderA.defaultJson), (decoderB.schema, decoderB.defaultJson), (decoderC.schema, decoderC.defaultJson), (decoderD.schema, decoderD.defaultJson), (decoderE.schema, decoderE.defaultJson))
        val defaultJson = None

        def run(in: io.ResolvingDecoder, out: Receiver[(A, B, C, D, E)]) =
            tryCatch.resultG(terminal) {
                val receiverA = new Receiver[A]
                val receiverB = new Receiver[B]
                val receiverC = new Receiver[C]
                val receiverD = new Receiver[D]
                val receiverE = new Receiver[E]
                decoderA.run(in, receiverA) >>
                decoderB.run(in, receiverB) >>
                decoderC.run(in, receiverC) >>
                decoderD.run(in, receiverD) >>
                decoderE.run(in, receiverE) >>
                out((receiverA.value, receiverB.value, receiverC.value, receiverD.value, receiverE.value))
            }
    }

    implicit def tuple6AvroCoder[A, B, C, D, E, F] (
        implicit encoderA: AvroEncoder[A],
                 decoderA: AvroDecoder[A],
                 encoderB: AvroEncoder[B],
                 decoderB: AvroDecoder[B],
                 encoderC: AvroEncoder[C],
                 decoderC: AvroDecoder[C],
                 encoderD: AvroEncoder[D],
                 decoderD: AvroDecoder[D],
                 encoderE: AvroEncoder[E],
                 decoderE: AvroDecoder[E],
                 encoderF: AvroEncoder[F],
                 decoderF: AvroDecoder[F]
    ): AvroCoder[(A, B, C, D, E, F)] =
        AvroCoder.make(tuple6AvroEncoder(encoderA, encoderB, encoderC, encoderD, encoderE, encoderF),
                       tuple6AvroDecoder(decoderA, decoderB, decoderC, decoderD, decoderE, decoderF))

    implicit def tuple6AvroEncoder[A, B, C, D, E, F](implicit encoderA: AvroEncoder[A], encoderB: AvroEncoder[B], encoderC: AvroEncoder[C], encoderD: AvroEncoder[D], encoderE: AvroEncoder[E], encoderF: AvroEncoder[F]): AvroEncoder[(A, B, C, D, E, F)] = new AvroEncoder[(A, B, C, D, E, F)] {
        val schema = tupleSchema((encoderA.schema, encoderA.defaultJson), (encoderB.schema, encoderB.defaultJson), (encoderC.schema, encoderC.defaultJson), (encoderD.schema, encoderD.defaultJson), (encoderE.schema, encoderE.defaultJson), (encoderF.schema, encoderF.defaultJson))
        val defaultJson = None

        def encodeDefaultJson(in: (A, B, C, D, E, F)) =
            for {
                a <- encoderA.encodeDefaultJson(in._1)
                b <- encoderB.encodeDefaultJson(in._2)
                c <- encoderC.encodeDefaultJson(in._3)
                d <- encoderD.encodeDefaultJson(in._4)
                e <- encoderE.encodeDefaultJson(in._5)
                f <- encoderF.encodeDefaultJson(in._6)
            } yield {
                val obj = jsonNodeFactory.objectNode
                obj.put("_1", a)
                obj.put("_2", b)
                obj.put("_3", c)
                obj.put("_4", d)
                obj.put("_5", e)
                obj.put("_6", f)
                obj
            }

        def run(in: (A, B, C, D, E, F), out: io.Encoder) =
            tryCatch.resultG(terminal) {
                encoderA.run(in._1, out) >>
                encoderB.run(in._2, out) >>
                encoderC.run(in._3, out) >>
                encoderD.run(in._4, out) >>
                encoderE.run(in._5, out) >>
                encoderF.run(in._6, out)
            }
    }

    implicit def tuple6AvroDecoder[A, B, C, D, E, F](implicit decoderA: AvroDecoder[A], decoderB: AvroDecoder[B], decoderC: AvroDecoder[C], decoderD: AvroDecoder[D], decoderE: AvroDecoder[E], decoderF: AvroDecoder[F]): AvroDecoder[(A, B, C, D, E, F)] = new AvroDecoder[(A, B, C, D, E, F)] {
        val schema = tupleSchema((decoderA.schema, decoderA.defaultJson), (decoderB.schema, decoderB.defaultJson), (decoderC.schema, decoderC.defaultJson), (decoderD.schema, decoderD.defaultJson), (decoderE.schema, decoderE.defaultJson), (decoderF.schema, decoderF.defaultJson))
        val defaultJson = None

        def run(in: io.ResolvingDecoder, out: Receiver[(A, B, C, D, E, F)]) =
            tryCatch.resultG(terminal) {
                val receiverA = new Receiver[A]
                val receiverB = new Receiver[B]
                val receiverC = new Receiver[C]
                val receiverD = new Receiver[D]
                val receiverE = new Receiver[E]
                val receiverF = new Receiver[F]
                decoderA.run(in, receiverA) >>
                decoderB.run(in, receiverB) >>
                decoderC.run(in, receiverC) >>
                decoderD.run(in, receiverD) >>
                decoderE.run(in, receiverE) >>
                decoderF.run(in, receiverF) >>
                out((receiverA.value, receiverB.value, receiverC.value, receiverD.value, receiverE.value, receiverF.value))
            }
    }

    // if anybody wants more than 6, then they can work on the macro to generate the boilerplate automatically
}