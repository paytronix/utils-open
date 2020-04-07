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

package com.paytronix.utils.interchange.format.avro

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.util.Arrays

import org.apache.avro.Schema
import org.apache.avro.io.{BinaryData, DecoderFactory}
import org.specs2.SpecificationWithJUnit
import org.specs2.matcher.Matcher

import com.paytronix.utils.interchange.base.{Receiver, formatFailedPath}
import com.paytronix.utils.scala.result.Result

import utils.makeField

trait AvroTestUtils {
    lazy val utf8 = Charset.forName("UTF-8")

    def zigZagEncode(l: Long): Array[Byte] = {
        val buf = Array.ofDim[Byte](10);
        val len = BinaryData.encodeLong(l, buf, 0)
        val out = Array.ofDim[Byte](len);
        System.arraycopy(buf, 0, out, 0, len)
        out
    }

    def makeAvroString(s: String): Array[Byte] =
        makeAvroBytes(utf8.encode(s))

    def makeAvroBytes(bb: ByteBuffer): Array[Byte] = {
        val lengthBytes = zigZagEncode(bb.remaining)
        val out: ByteBuffer = ByteBuffer.allocate(lengthBytes.length + bb.remaining)
        out.put(lengthBytes)
        bb.mark()
        out.put(bb)
        bb.reset()
        out.array
    }

    def makeAvroArray(elements: Seq[Array[Byte]], blockSize: Option[Int] = None): Array[Byte] =
        if (elements.size == 0) {
            zigZagEncode(0)
        } else {
            val fullBlockSize = blockSize getOrElse elements.size
            val fullBlocks = elements.size / fullBlockSize
            val trailingBlockSize = elements.size - fullBlocks*fullBlockSize
            val totalElementSize = elements.map(_.length).sum
            val fullBlockSizeBytes = zigZagEncode(fullBlockSize)
            val trailingBlockSizeBytes = if (trailingBlockSize > 0) zigZagEncode(trailingBlockSize) else Array[Byte]()
            val blockHeadersSize = fullBlocks*fullBlockSizeBytes.length + trailingBlockSizeBytes.length

            val bb = ByteBuffer.allocate(totalElementSize + blockHeadersSize + 1)

            var toGo = elements
            while (toGo.nonEmpty) {
                if (toGo.size >= fullBlockSize) {
                    bb.put(fullBlockSizeBytes)
                    toGo.take(fullBlockSize).foreach(bb.put)
                    toGo = toGo.drop(fullBlockSize)
                } else {
                    bb.put(trailingBlockSizeBytes)
                    toGo.foreach(bb.put)
                    toGo = Nil
                }
            }

            bb.put(0: Byte)

            bb.array
        }

    def decodeDefault[A](coder: AvroCoder[A]): Result[A] = {
        val oldSchema = Schema.createRecord("DefaultWrapper", "", "", false)
        oldSchema.setFields(Arrays.asList())
        val newSchema = Schema.createRecord("DefaultWrapper", "", "", false)
        newSchema.setFields(Arrays.asList(makeField("value", coder.schema, coder.defaultJson, "")))
        val in = DecoderFactory.get.binaryDecoder(Array[Byte](), 0, 0, null)
        val resolver = utils.ResolvingDecoderCache(newSchema, oldSchema)
        resolver.configure(in)
        resolver.readFieldOrder // need this side effect to occur, otherwise the decoder will continue to decode other stuff but report incorrect field orders
        val receiver = new Receiver[A]
        val result = coder.decode.run(resolver, receiver)
        resolver.drain()
        formatFailedPath(result).map { _ => receiver.value }
    }
}

object AvroTestUtils extends AvroTestUtils

trait AvroMatchers extends AvroTestUtils { self: SpecificationWithJUnit =>

    def beEqualToArray[A](expected: Array[A]) =
        ((_: Array[A]).mkString("[", ",", "]")) ^^ be_===(expected.mkString("[", ",", "]"))

    def beAvroString(s: String) =
        beEqualToArray(makeAvroString(s))

    def beAvroBytes(bb: ByteBuffer) =
        beEqualToArray(makeAvroBytes(bb))

    def beAvroInt(i: Int) =
        beEqualToArray(zigZagEncode(i))

    def beAvroLong(l: Long) =
        beEqualToArray(zigZagEncode(l))

    def beAvroArray(elements: Seq[Array[Byte]], blockSize: Option[Int] = None) =
        beEqualToArray(makeAvroArray(elements, blockSize))

    def fieldLike(name: String, typ: Schema.Type, unionTypes: Schema.Type*): Matcher[Schema.Field] =
        beLike { case f =>
            (f.name ==== name).updateMessage(name + " name: " + _) and
            (f.schema.getType ==== typ).updateMessage(name + " type: " + _) and
            (
                typ match {
                    case Schema.Type.UNION if unionTypes.nonEmpty =>
                        (f.schema.getTypes.size ==== unionTypes.size) and
                        unionTypes.zipWithIndex.map { case (utyp, idx) =>
                            f.schema.getTypes.get(idx).getType ==== utyp
                        }.reduce(_ and _)
                    case _ => ok
                }
            )
        }
}
