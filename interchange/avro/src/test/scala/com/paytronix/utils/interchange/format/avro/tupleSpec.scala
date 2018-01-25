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

import java.nio.ByteBuffer

import org.apache.avro.Schema
import org.specs2.{ScalaCheck, SpecificationWithJUnit}
import org.specs2.matcher.Matcher

import com.paytronix.utils.scala.result.{Okay, Result}

import scalar.{intAvroCoder, stringAvroCoder}
import tuple._


abstract class avroTupleSpecBase extends SpecificationWithJUnit with AvroMatchers {
    val arity: Int

    def beCorrectSchema: Matcher[Schema] = beLike { case schema: Schema =>
        (schema.getType ==== Schema.Type.RECORD).updateMessage("schema type: " + _) and
        (schema.getFields.size ==== arity).updateMessage("schema arity: " + _) and
        (0 until arity).map { n =>
            val expectedType = if (n % 2 == 0) Schema.Type.INT else Schema.Type.STRING
            (schema.getFields.get(n).schema.getType ==== expectedType).updateMessage("field " + n + ": " + _)
        }.reduce(_ and _)
    }

    def encode(product: Product): Array[Byte] = {
        val parts = (0 until arity).map { n =>
            if (n % 2 == 0) zigZagEncode(product.productElement(n).asInstanceOf[Int])
            else            makeAvroString(product.productElement(n).asInstanceOf[String])
        }
        val bb = ByteBuffer.allocate(parts.map(_.length).sum)
        parts.foreach(bb.put)
        bb.array
    }

    def beValidEncodingOf(product: Product): Matcher[Result[Array[Byte]]] =
        beLike { case Okay(a) => a must beEqualToArray(encode(product)) }
}

class tuple1AvroCoderTest extends avroTupleSpecBase with ScalaCheck {
    def is = s2"""
        tuple1AvroCoder
            must have correct schema $eschema
            must encode values correctly $eencode
            must decode values correctly $edecode
            must decode defaults correctly $edefault
    """

    val arity = 1
    lazy val coder = AvroCoder[Tuple1[Int]]

    def eschema = coder.schema must beCorrectSchema

    def eencode = prop { (i: Int) => val t = Tuple1(i); coder.encode.toBytes(t) must beValidEncodingOf(t) }
    def edecode = prop { (i: Int) => val t = Tuple1(i); coder.decode.fromBytes(coder.schema)(encode(t)) ==== Okay(t) }
    def edefault = prop { (i: Int) => val t = Tuple1(i); decodeDefault(coder.default(t)) ==== Okay(t) }
}

class tuple2AvroCoderTest extends avroTupleSpecBase with ScalaCheck {
    def is = s2"""
        tuple2AvroCoder
            must have correct schema $eschema
            must encode values correctly $eencode
            must decode values correctly $edecode
            must decode defaults correctly $edefault
    """

    val arity = 2
    lazy val coder = AvroCoder[(Int, String)]

    def eschema = coder.schema must beCorrectSchema

    def eencode = prop { (t: (Int, String)) => coder.encode.toBytes(t) must beValidEncodingOf(t) }
    def edecode = prop { (t: (Int, String)) => coder.decode.fromBytes(coder.schema)(encode(t)) ==== Okay(t) }
    def edefault = prop { (t: (Int, String)) => decodeDefault(coder.default(t)) ==== Okay(t) }
}

class tuple3AvroCoderTest extends avroTupleSpecBase with ScalaCheck {
    def is = s2"""
        tuple3AvroCoder
            must have correct schema $eschema
            must encode values correctly $eencode
            must decode values correctly $edecode
            must decode defaults correctly $edefault
    """

    val arity = 3
    lazy val coder = AvroCoder[(Int, String, Int)]

    def eschema = coder.schema must beCorrectSchema

    def eencode = prop { (t: (Int, String, Int)) => coder.encode.toBytes(t) must beValidEncodingOf(t) }
    def edecode = prop { (t: (Int, String, Int)) => coder.decode.fromBytes(coder.schema)(encode(t)) ==== Okay(t) }
    def edefault = prop { (t: (Int, String, Int)) => decodeDefault(coder.default(t)) ==== Okay(t) }
}

class tuple4AvroCoderTest extends avroTupleSpecBase with ScalaCheck {
    def is = s2"""
        tuple4AvroCoder
            must have correct schema $eschema
            must encode values correctly $eencode
            must decode values correctly $edecode
            must decode defaults correctly $edefault
    """

    val arity = 4
    lazy val coder = AvroCoder[(Int, String, Int, String)]

    def eschema = coder.schema must beCorrectSchema

    def eencode = prop { (t: (Int, String, Int, String)) => coder.encode.toBytes(t) must beValidEncodingOf(t) }
    def edecode = prop { (t: (Int, String, Int, String)) => coder.decode.fromBytes(coder.schema)(encode(t)) ==== Okay(t) }
    def edefault = prop { (t: (Int, String, Int, String)) => decodeDefault(coder.default(t)) ==== Okay(t) }
}

class tuple5AvroCoderTest extends avroTupleSpecBase with ScalaCheck {
    def is = s2"""
        tuple5AvroCoder
            must have correct schema $eschema
            must encode values correctly $eencode
            must decode values correctly $edecode
            must decode defaults correctly $edefault
    """

    val arity = 5
    lazy val coder = AvroCoder[(Int, String, Int, String, Int)]

    def eschema = coder.schema must beCorrectSchema

    def eencode = prop { (t: (Int, String, Int, String, Int)) => coder.encode.toBytes(t) must beValidEncodingOf(t) }
    def edecode = prop { (t: (Int, String, Int, String, Int)) => coder.decode.fromBytes(coder.schema)(encode(t)) ==== Okay(t) }
    def edefault = prop { (t: (Int, String, Int, String, Int)) => decodeDefault(coder.default(t)) ==== Okay(t) }
}

class tuple6AvroCoderTest extends avroTupleSpecBase with ScalaCheck {
    def is = s2"""
        tuple6AvroCoder
            must have correct schema $eschema
            must encode values correctly $eencode
            must decode values correctly $edecode
            must decode defaults correctly $edefault
    """

    val arity = 6
    lazy val coder = AvroCoder[(Int, String, Int, String, Int, String)]

    def eschema = coder.schema must beCorrectSchema

    def eencode = prop { (t: (Int, String, Int, String, Int, String)) => coder.encode.toBytes(t) must beValidEncodingOf(t) }
    def edecode = prop { (t: (Int, String, Int, String, Int, String)) => coder.decode.fromBytes(coder.schema)(encode(t)) ==== Okay(t) }
    def edefault = prop { (t: (Int, String, Int, String, Int, String)) => decodeDefault(coder.default(t)) ==== Okay(t) }
}
