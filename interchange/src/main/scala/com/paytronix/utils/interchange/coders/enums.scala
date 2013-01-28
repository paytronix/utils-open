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

import scala.collection.JavaConverters.seqAsJavaListConverter

import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}
import net.liftweb.json.JsonAST.{JNothing, JNull, JString, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}

import com.paytronix.utils.scala.result.{FailedG, Okay, optionOps, parameter, tryCatch, tryCatching}

/** Coder for Java enumerations */
case class JavaEnumCoder[T <: Enum[T]](enumClass: Class[T]) extends StringSafeCoder[T]
{
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = enumClass

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T) =
        Okay(JString(in.toString))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatching[IllegalArgumentException].value(Enum.valueOf(enumClass, in)) | ("\"" + in + "\" is not a valid enumeration value") | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: T) =
        Okay(in.toString)

    private def enumValues =
        enumClass.getMethod("values").invoke(null).asInstanceOf[Array[T]]

    lazy val avroSchema = {
        import AvroUtils.nameAndNamespaceFromClass
        val (namespace, name) = nameAndNamespaceFromClass(enumClass)
        (Schema.createEnum(name, "", namespace, enumValues.map(_.toString.replaceAll("[^_a-zA-Z0-9]", "")).toSeq.asJava), None)
    }

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            val values = enumValues
            in.readEnum() match {
                case i if i < 0              => FailedG("read negative enum index " + i + " from Avro", Nil)
                case i if i >= values.length => FailedG("read overflow enum index " + i + " from Avro", Nil)
                case i                       => Okay(enumValues(i))
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        tryCatch.value {
            out.writeEnum(in.ordinal)
        } | parameter(Nil)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        encodeString(classLoader, in).map(jsonNodeFactory.textNode)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String => decodeString(classLoader, s)
            case null      => FailedG("required but missing", Nil)
            case _         => FailedG("not a string", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        encodeString(classLoader, in)
}

/** Coder for Scala enumerations */
case class ScalaEnumCoder[T <: Enumeration](enum: T) extends StringSafeCoder[T#Value]
{
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[T#Value]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T#Value) =
        Okay(JString(in.toString))

    def decodeString(classLoader: ClassLoader, in: String) =
        enum.values.find(_.toString == in).toResult | ("\"" + in + "\" is not a valid enumeration value") | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: T#Value) =
        Okay(in.toString)

    private lazy val enumsByOrdinal: Map[Int, T#Value] = Map(enum.values.zipWithIndex.map(_.swap).toSeq: _*)
    private lazy val ordinalsByEnum: Map[T#Value, Int] = Map(enum.values.zipWithIndex.toSeq            : _*)

    lazy val avroSchema = {
        import AvroUtils.nameAndNamespaceFromClass
        val (namespace, name) = nameAndNamespaceFromClass(enum.getClass)
        (Schema.createEnum(name, "", namespace, enum.values.map(_.toString.replaceAll("[^_a-zA-Z0-9]", "")).toSeq.asJava), None)
    }

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            in.readEnum() match {
                case i if i < 0                    => FailedG("read negative enum index " + i + " from Avro", Nil)
                case i if i >= enumsByOrdinal.size => FailedG("read overflow enum index " + i + " from Avro", Nil)
                case i                             => Okay(enumsByOrdinal(i))
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: T#Value, out: Encoder) =
        tryCatch.value {
            out.writeEnum(ordinalsByEnum(in))
        } | parameter(Nil)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T#Value) =
        encodeString(classLoader, in).map(jsonNodeFactory.textNode)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String => decodeString(classLoader, s)
            case null      => FailedG("required but missing", Nil)
            case _         => FailedG("not a string", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T#Value) =
        encodeString(classLoader, in)
}
