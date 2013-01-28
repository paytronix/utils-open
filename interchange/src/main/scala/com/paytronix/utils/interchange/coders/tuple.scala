//
// Copyright 2013 Paytronix Systems, Inc.
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

import java.util.{Collection => JavaCollection}
import scala.collection.JavaConverters.{asScalaBufferConverter, seqAsJavaListConverter}

import net.liftweb.json.JsonAST.{JArray, JNothing, JNull, JObject, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, iterableResultOps, parameter}

/** Trait that implements Avro coding of tuples by storing as a record with _1, _2, _3, etc. and mongo coding as an array */
trait TupleCoder {
    self: ComposableCoder[_ <: Product] =>

    import ComposableCoder.{CoderResult, catchingCoderException}

    val arity: Int
    protected val coders: Array[ComposableCoder[AnyRef]]
    protected def makeValue(in: Array[AnyRef]): Result[ValueType]

    lazy val avroSchema = {
        val s = Schema.createRecord("Tuple" + arity + "__" + coders.map(c => AvroUtils.encodeSchemaName(c.avroSchema._1)).mkString(""), "", "scala.lang", false)
        s.setFields((1 to arity).map(n => AvroUtils.makeField("_" + n, coders(n-1).avroSchema)).asJava)
        (s, None)
    }

    def decodeAvroTuple(classLoader: ClassLoader, in: ResolvingDecoder): CoderResult[ValueType] =
        catchingCoderException {
            val a = Array.ofDim[AnyRef](arity)
            in.readFieldOrder.toSeq.foreachResult (
                f => coders(f.pos).decodeAvro(classLoader, in).map(a(f.pos) = _)
            ) then (makeValue(a) | parameter(Nil))
        }

    def encodeAvroTuple(classLoader: ClassLoader, in: ValueType, out: Encoder): CoderResult[Unit] =
        avroSchema._1.getFields.asScala.foreachResult (
            f => coders(f.pos).encodeAvro(classLoader, in.productElement(f.pos).asInstanceOf[AnyRef], out) then Okay(())
        )

    def encodeAvroTupleDefaultJson(classLoader: ClassLoader, in: ValueType) = {
        val objNode = jsonNodeFactory.objectNode
        avroSchema._1.getFields.asScala.foreachResult { f =>
            coders(f.pos).encodeAvroDefaultJson(classLoader, in.productElement(f.pos).asInstanceOf[AnyRef]) map { v =>
                objNode.put("_" + (f.pos+1), v)
                ()
            }
        } then Okay(objNode)
    }

    def decodeMongoDBTuple(classLoader: ClassLoader, in: AnyRef): CoderResult[ValueType] =
        in match {
            case coll: JavaCollection[_] if coll.size == coders.length => {
                val a = Array.ofDim[AnyRef](arity)
                val iter = coll.iterator()
                (1 to arity) foreachResult (
                    (i: Int) => coders(i).decodeMongoDB(classLoader, iter.next().asInstanceOf[AnyRef]).map(a(i) = _)
                ) then (makeValue(a) | parameter(Nil))
            }

            case coll: JavaCollection[_] => FailedG("expected a collection of " + coders.length + " elements but got " + coll.size + " elements", Nil)
            case null                    => FailedG("required but missing", Nil)
            case _                       => FailedG("not a collection", Nil)
        }

    def encodeMongoDBTuple(classLoader: ClassLoader, in: ValueType): CoderResult[AnyRef] = {
        val a = Array.ofDim[AnyRef](arity)
        (1 to arity) foreachResult (
            (i: Int) => coders(i).encodeMongoDB(classLoader, in.productElement(i).asInstanceOf[AnyRef]).map(a(i) = _)
        ) then Okay(a)
    }
}

/** Map a 1-tuple */
case class Tuple1Coder[A] (
    coder: ComposableCoder[A]
) extends ComposableCoder[Tuple1[A]] with TupleCoder
{
    val mostSpecificClass = classOf[Tuple1[A]]

    val arity = 1

    protected val coders = Array(coder.asCoderFor[AnyRef])

    protected def makeValue(in: Array[AnyRef]) =
        in match {
            case Array(a) => Okay(new Tuple1(a.asInstanceOf[A]))
            case _        => FailedG("expected 1 elements from tuple decode, got " + in.length, Nil)
        }

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JArray(jvalue :: Nil) => coder.decode(classLoader, jvalue).map(new Tuple1(_))
            case JArray(_)             => FailedG("expected an array with exactly one element", Nil)
            case _                     => coder.decode(classLoader, in).map(new Tuple1(_))
        }

    def encode(classLoader: ClassLoader, in: Tuple1[A]) =
        for {
            jvalue <- coder.encode(classLoader,in._1)
        } yield JArray(jvalue :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple1[A], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Tuple1[A]) =
        encodeAvroTupleDefaultJson(classLoader, in)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple1[A]) =
        encodeMongoDBTuple(classLoader, in)
}

/** Map a 2-tuple */
case class Tuple2Coder[A,B] (
    coder1: ComposableCoder[A],
    coder2: ComposableCoder[B]
) extends ComposableCoder[(A,B)] with TupleCoder
{
    val mostSpecificClass = classOf[(A,B)]

    val arity = 2

    protected val coders = Array(coder1.asCoderFor[AnyRef], coder2.asCoderFor[AnyRef])
    protected def makeValue(in: Array[AnyRef]) =
        in match {
            case Array(a, b) => Okay((a.asInstanceOf[A], b.asInstanceOf[B]))
            case _ => Failed("expected 2 elements from tuple decode, got " + in.length)
        }

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JArray(jvalue1 :: jvalue2 :: Nil) =>
                for {
                    value1 <- coder1.decode(classLoader, jvalue1)
                    value2 <- coder2.decode(classLoader, jvalue2)
                } yield (value1, value2)

            case JArray(_) => FailedG("expected an array with exactly two elements", Nil)
            case _         => FailedG("not an array", Nil)
        }

    def encode(classLoader: ClassLoader, in: (A,B)) =
        for {
            jvalue1 <- coder1.encode(classLoader, in._1)
            jvalue2 <- coder2.encode(classLoader, in._2)
        } yield JArray(jvalue1 :: jvalue2 :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple2[A, B], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Tuple2[A, B]) =
        encodeAvroTupleDefaultJson(classLoader, in)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple2[A, B]) =
        encodeMongoDBTuple(classLoader, in)
}

/** Map a 3-tuple */
case class Tuple3Coder[A,B,C] (
    coder1: ComposableCoder[A],
    coder2: ComposableCoder[B],
    coder3: ComposableCoder[C]
) extends ComposableCoder[(A,B,C)] with TupleCoder
{
    val mostSpecificClass = classOf[(A,B,C)]

    val arity = 3

    protected val coders = Array(coder1.asCoderFor[AnyRef], coder2.asCoderFor[AnyRef], coder3.asCoderFor[AnyRef])
    protected def makeValue(in: Array[AnyRef]) = in match {
        case Array(a, b, c) => Okay((a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C]))
        case _ => FailedG("expected 3 elements from tuple decode, got " + in.length, Nil)
    }

    def decode(classLoader: ClassLoader, in: JValue) = in match {
        case JArray(jvalue1 :: jvalue2 :: jvalue3 :: Nil) =>
            for {
                value1 <- coder1.decode(classLoader, jvalue1)
                value2 <- coder2.decode(classLoader, jvalue2)
                value3 <- coder3.decode(classLoader, jvalue3)
            } yield (value1, value2, value3)

        case JArray(_) => FailedG("expected an array with exactly three elements", Nil)
        case _         => FailedG("not an array", Nil)
    }

    def encode(classLoader: ClassLoader, in: (A,B,C)) =
        for {
            jvalue1 <- coder1.encode(classLoader, in._1)
            jvalue2 <- coder2.encode(classLoader, in._2)
            jvalue3 <- coder3.encode(classLoader, in._3)
        } yield JArray(jvalue1 :: jvalue2 :: jvalue3 :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple3[A, B, C], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Tuple3[A, B, C]) =
        encodeAvroTupleDefaultJson(classLoader, in)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple3[A, B, C]) =
        encodeMongoDBTuple(classLoader, in)
}

/** Map a 4-tuple */
case class Tuple4Coder[A,B,C,D] (
    coder1: ComposableCoder[A],
    coder2: ComposableCoder[B],
    coder3: ComposableCoder[C],
    coder4: ComposableCoder[D]
) extends ComposableCoder[(A,B,C,D)] with TupleCoder
{
    val mostSpecificClass = classOf[(A,B,C,D)]

    val arity = 4

    protected val coders =
        Array(coder1.asCoderFor[AnyRef], coder2.asCoderFor[AnyRef], coder3.asCoderFor[AnyRef], coder4.asCoderFor[AnyRef])
    protected def makeValue(in: Array[AnyRef]) = in match {
        case Array(a, b, c, d) => Okay((a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C], d.asInstanceOf[D]))
        case _ => FailedG("expected 4 elements from tuple decode, got " + in.length, Nil)
    }

    def decode(classLoader: ClassLoader, in: JValue) = in match {
        case JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: Nil) =>
            for {
                value1 <- coder1.decode(classLoader, jvalue1)
                value2 <- coder2.decode(classLoader, jvalue2)
                value3 <- coder3.decode(classLoader, jvalue3)
                value4 <- coder4.decode(classLoader, jvalue4)
            } yield (value1, value2, value3, value4)

        case JArray(_) => FailedG("expected an array with exactly four elements", Nil)
        case _         => FailedG("not an array", Nil)
    }

    def encode(classLoader: ClassLoader, in: (A,B,C,D)) =
        for {
            jvalue1 <- coder1.encode(classLoader, in._1)
            jvalue2 <- coder2.encode(classLoader, in._2)
            jvalue3 <- coder3.encode(classLoader, in._3)
            jvalue4 <- coder4.encode(classLoader, in._4)
        } yield JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple4[A, B, C, D], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Tuple4[A, B, C, D]) =
        encodeAvroTupleDefaultJson(classLoader, in)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple4[A, B, C, D]) =
        encodeMongoDBTuple(classLoader, in)
}

/** Map a 5-tuple */
case class Tuple5Coder[A,B,C,D,E] (
    coder1: ComposableCoder[A],
    coder2: ComposableCoder[B],
    coder3: ComposableCoder[C],
    coder4: ComposableCoder[D],
    coder5: ComposableCoder[E]
) extends ComposableCoder[(A,B,C,D,E)] with TupleCoder
{
    val mostSpecificClass = classOf[(A,B,C,D,E)]

    val arity = 5

    protected val coders = Array (
        coder1.asCoderFor[AnyRef],
        coder2.asCoderFor[AnyRef],
        coder3.asCoderFor[AnyRef],
        coder4.asCoderFor[AnyRef],
        coder5.asCoderFor[AnyRef]
    )
    protected def makeValue(in: Array[AnyRef]) = in match {
        case Array(a, b, c, d, e) => Okay((a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C],
                                           d.asInstanceOf[D], e.asInstanceOf[E]))
        case _ => FailedG("expected 5 elements from tuple decode, got " + in.length, Nil)
    }

    def decode(classLoader: ClassLoader, in: JValue) = in match {
        case JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: jvalue5 :: Nil) =>
            for {
                value1 <- coder1.decode(classLoader, jvalue1)
                value2 <- coder2.decode(classLoader, jvalue2)
                value3 <- coder3.decode(classLoader, jvalue3)
                value4 <- coder4.decode(classLoader, jvalue4)
                value5 <- coder5.decode(classLoader, jvalue5)
            } yield (value1, value2, value3, value4, value5)

        case JArray(_) => FailedG("expected an array with exactly five elements", Nil)
        case _         => FailedG("not an array", Nil)
    }

    def encode(classLoader: ClassLoader, in: (A,B,C,D,E)) =
        for {
            jvalue1 <- coder1.encode(classLoader, in._1)
            jvalue2 <- coder2.encode(classLoader, in._2)
            jvalue3 <- coder3.encode(classLoader, in._3)
            jvalue4 <- coder4.encode(classLoader, in._4)
            jvalue5 <- coder5.encode(classLoader, in._5)
        } yield JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: jvalue5 :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple5[A, B, C, D, E], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Tuple5[A, B, C, D, E]) =
        encodeAvroTupleDefaultJson(classLoader, in)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple5[A, B, C, D, E]) =
        encodeMongoDBTuple(classLoader, in)
}

/** Map a 6-tuple */
case class Tuple6Coder[A,B,C,D,E,F] (
    coder1: ComposableCoder[A],
    coder2: ComposableCoder[B],
    coder3: ComposableCoder[C],
    coder4: ComposableCoder[D],
    coder5: ComposableCoder[E],
    coder6: ComposableCoder[F]
) extends ComposableCoder[(A,B,C,D,E,F)] with TupleCoder
{
    val mostSpecificClass = classOf[(A,B,C,D,E,F)]

    val arity = 6

    protected val coders = Array (
        coder1.asCoderFor[AnyRef],
        coder2.asCoderFor[AnyRef],
        coder3.asCoderFor[AnyRef],
        coder4.asCoderFor[AnyRef],
        coder5.asCoderFor[AnyRef],
        coder6.asCoderFor[AnyRef]
    )
    protected def makeValue(in: Array[AnyRef]) = in match {
        case Array(a, b, c, d, e, f) => Okay((a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C],
                                              d.asInstanceOf[D], e.asInstanceOf[E], f.asInstanceOf[F]))
        case _ => FailedG("expected 6 elements from tuple decode, got " + in.length, Nil)
    }

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: jvalue5 :: jvalue6 :: Nil) =>
                for {
                    value1 <- coder1.decode(classLoader, jvalue1)
                    value2 <- coder2.decode(classLoader, jvalue2)
                    value3 <- coder3.decode(classLoader, jvalue3)
                    value4 <- coder4.decode(classLoader, jvalue4)
                    value5 <- coder5.decode(classLoader, jvalue5)
                    value6 <- coder6.decode(classLoader, jvalue6)
                } yield (value1, value2, value3, value4, value5, value6)

            case JArray(_) => FailedG("expected an array with exactly six elements", Nil)
            case _         => FailedG("not an array", Nil)
        }

    def encode(classLoader: ClassLoader, in: (A,B,C,D,E,F)) =
        for {
            jvalue1 <- coder1.encode(classLoader, in._1)
            jvalue2 <- coder2.encode(classLoader, in._2)
            jvalue3 <- coder3.encode(classLoader, in._3)
            jvalue4 <- coder4.encode(classLoader, in._4)
            jvalue5 <- coder5.encode(classLoader, in._5)
            jvalue6 <- coder6.encode(classLoader, in._6)
        } yield JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: jvalue5 :: jvalue6 :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple6[A, B, C, D, E, F], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Tuple6[A, B, C, D, E, F]) =
        encodeAvroTupleDefaultJson(classLoader, in)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple6[A, B, C, D, E, F]) =
        encodeMongoDBTuple(classLoader, in)
}
