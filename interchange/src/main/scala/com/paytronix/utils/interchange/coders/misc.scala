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

import com.mongodb.BasicDBObject
import net.liftweb.json.JsonAST.{JField, JNull, JObject, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result}

/** Singleton coder, which always encodes as an empty object and always decodes a particular value regardless of the input JSON */
case class SingletonCoder[T](inst: T, encodedFields: List[JField] = Nil) extends ComposableCoder[T]
{
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = inst.asInstanceOf[AnyRef].getClass.asInstanceOf[Class[T]]

    def decode(classLoader: ClassLoader, in: JValue) = Okay(inst)
    def encode(classLoader: ClassLoader, in: T) = Okay(JObject(encodedFields))

    lazy val avroSchema = {
        import AvroUtils.nameAndNamespaceFromClass
        val (namespace, name) = nameAndNamespaceFromClass(inst.asInstanceOf[AnyRef].getClass)
        val s = Schema.createRecord(name, "", namespace, false)
        s.setFields(new java.util.ArrayList())
        (s, Some(jsonNodeFactory.objectNode))
    }

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = Okay(inst)
    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) = Okay(())
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) = Okay(jsonNodeFactory.nullNode)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) = Okay(inst)
    def encodeMongoDB(classLoader: ClassLoader, in: T) = catchingCoderException(Okay(MongoUtils.fromJValue(encodedFields)))
}

/**
 * Coder which always returns the given Failed when decoding or encoding is attempted. Usually used in places where a Coder is
 * required, but couldn't be built and there is no acceptable place to give back a Result until later (e.g. during initialization of
 * an object.
 */
case class FailCoder[T](failed: Result[T] = Failed("cannot encode/decode"))(implicit m: Manifest[T]) extends ComposableCoder[T]
{
    import ComposableCoder.atTerminal

    val mostSpecificClass = m.erasure.asInstanceOf[Class[T]]

    def decode(classLoader: ClassLoader, in: JValue) = atTerminal(failed)
    def encode(classLoader: ClassLoader, in: T) = atTerminal(failed.asA[JValue])

    lazy val avroSchema = (Schema.create(Schema.Type.NULL), Some(jsonNodeFactory.nullNode))
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = atTerminal(failed)
    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) = atTerminal(failed.asA[Unit])
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) = atTerminal(failed.asA[JsonNode])

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) = atTerminal(failed)
    def encodeMongoDB(classLoader: ClassLoader, in: T) = atTerminal(failed.asA[AnyRef])
}

object FailCoder {
    def unless[T](in: Result[Coder[T]])(implicit m: Manifest[T]): Coder[T] =
        in match {
            case Okay(coder) => coder
            case failed => Coder(getClass.getClassLoader, FailCoder[T](failed.asA[T]))
        }
}

/** Code a single field in a JObject. Typically used for single-field extraction */
case class FieldCoder[T](field: String, coder: ComposableCoder[T]) extends ComposableCoder[T] {
    import ComposableCoder.atProperty

    val mostSpecificClass = coder.mostSpecificClass

    def decode(classLoader: ClassLoader, in: JValue) =
        atProperty(field) {
            coder.decode(classLoader, in \ field)
        }

    def encode(classLoader: ClassLoader, in: T) =
        atProperty(field) {
            coder.encode(classLoader, in).map(jv => JObject(List(JField(field, jv))))
        }

    def avroSchema = coder.avroSchema

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        coder.decodeAvro(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        coder.encodeAvro(classLoader, in, out)

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        coder.encodeAvroDefaultJson(classLoader, in)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case m: java.util.Map[_, _] =>
                atProperty(field)(coder.decodeMongoDB(classLoader, m.asInstanceOf[java.util.Map[String, AnyRef]].get(field)))
            case null => FailedG("required but expected", Nil)
            case _    => FailedG("not an object", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        atProperty(field)(coder.encodeMongoDB(classLoader, in)) map { encoded =>
            val obj = new BasicDBObject
            obj.put(field, encoded)
        }
}
