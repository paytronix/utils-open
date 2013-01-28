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

import java.nio.ByteBuffer
import javax.xml.bind.DatatypeConverter

import net.liftweb.json.JsonAST.{JNothing, JNull, JString, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.bson.BSON
import org.bson.types.Binary
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.result.{FailedG, Okay, parameter, tryCatch}

object ByteArrayCoder extends StringSafeCoder[Array[Byte]] {
    val mostSpecificClass = classOf[Array[Byte]]

    def decode(classLoader: ClassLoader, in: JValue) =
        ByteBufferCoder.decode(classLoader, in) map { _.array }

    def encode(classLoader: ClassLoader, in: Array[Byte]) =
        ByteBufferCoder.encode(classLoader, ByteBuffer.wrap(in))

    def decodeString(classLoader: ClassLoader, in: String) =
        ByteBufferCoder.decodeString(classLoader, in) map { _.array }

    def encodeString(classLoader: ClassLoader, in: Array[Byte]) =
        ByteBufferCoder.encodeString(classLoader, ByteBuffer.wrap(in))

    def avroSchema =
        ByteBufferCoder.avroSchema

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        ByteBufferCoder.decodeAvro(classLoader, in) map { _.array }

    def encodeAvro(classLoader: ClassLoader, in: Array[Byte], out: Encoder) =
        ByteBufferCoder.encodeAvro(classLoader, ByteBuffer.wrap(in), out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Array[Byte]) =
        tryCatch.value(jsonNodeFactory.textNode(new String(in, "ISO-8859-1"))) | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        ByteBufferCoder.decodeMongoDB(classLoader, in) map { _.array }

    def encodeMongoDB(classLoader: ClassLoader, in: Array[Byte]) =
        ByteBufferCoder.encodeMongoDB(classLoader, ByteBuffer.wrap(in))

    override def toString = "ByteArrayCoder"
}

object ByteBufferCoder extends StringSafeCoder[ByteBuffer] {
    import ComposableCoder.{atTerminal, catchingCoderException}

    val mostSpecificClass = classOf[ByteBuffer]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: ByteBuffer) =
        encodeString(classLoader, in) map JString.apply

    def decodeString(classLoader: ClassLoader, in: String) =
        catchingCoderException {
            Okay(ByteBuffer.wrap(DatatypeConverter.parseBase64Binary(in)))
        }

    def encodeString(classLoader: ClassLoader, in: ByteBuffer) =
        catchingCoderException {
            Okay {
                if (in.hasArray && in.capacity == in.limit && in.position == 0)
                    DatatypeConverter.printBase64Binary(in.array)
                else {
                    // FIXME not the most efficient, better to chop it up into blocks divisible by 3
                    val a = Array.ofDim[Byte](in.remaining)
                    in.get(a)
                    DatatypeConverter.printBase64Binary(a)
                }
            }
        }

    lazy val avroSchema = (Schema.create(Schema.Type.BYTES), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        atTerminal(tryCatch.value(in.readBytes(null)))

    def encodeAvro(classLoader: ClassLoader, in: ByteBuffer, out: Encoder) =
        atTerminal(tryCatch.value(out.writeBytes(in)))

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: ByteBuffer) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](in.remaining)
            in.mark()
            in.get(bytes)
            in.reset()
            jsonNodeFactory.textNode(new String(bytes, "ISO-8859-1"))
        } | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case b: Binary            => Okay(ByteBuffer.wrap(b.getData))
            case s: String            => decodeString(classLoader, s)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not a string", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: ByteBuffer) =
        atTerminal {
            tryCatch.value {
                val a = Array.ofDim[Byte](in.remaining)
                in.get(a)
                new Binary(BSON.BINARY, a)
            }
        }

    override def toString = "ByteBufferCoder"
}
