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

import com.mongodb.DBObject
import net.liftweb.json.JsonAST.{JNothing, JNull, JString, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.bson.types.ObjectId
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.result.{FailedG, Okay}

/** Coder for MongoDB ObjectId fields */
object ObjectIdCoder extends StringSafeCoder[ObjectId] {
    val mostSpecificClass = classOf[ObjectId]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s) => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _ => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: ObjectId) = encodeString(classLoader, in).map(JString.apply)

    def decodeString(classLoader: ClassLoader, in: String) = if (ObjectId.isValid(in)) Okay(new ObjectId(in)) else FailedG("not a valid object id", Nil)
    def encodeString(classLoader: ClassLoader, in: ObjectId) = Okay(in.toString)

    lazy val avroSchema = (Schema.create(Schema.Type.STRING), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = decodeString(classLoader, in.readString(null).toString)
    def encodeAvro(classLoader: ClassLoader, in: ObjectId, out: Encoder) = encodeString(classLoader, in).map(out.writeString)
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: ObjectId) = encodeString(classLoader, in).map(jsonNodeFactory.textNode)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case oid: ObjectId => Okay(oid)
            case s: String     => decodeString(classLoader, s)
            case null          => FailedG("required but missing", Nil)
            case _             => FailedG("not a string", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: ObjectId) =
        Okay(in)

    override def toString = "ObjectIdCoder"
}

/** Coder for MongoDB literal DBObjects */
object DBObjectCoder extends ComposableCoder[DBObject] {
    val mostSpecificClass = classOf[DBObject]

    def decode(classLoader: ClassLoader, in: JValue) = FailedG("DBObject not codable in JSON", Nil)
    def encode(classLoader: ClassLoader, in: DBObject) = FailedG("DBObject not codable in JSON", Nil)

    lazy val avroSchema = (Schema.create(Schema.Type.NULL), Some(jsonNodeFactory.nullNode))
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = FailedG("DBObject not codable in Avro", Nil)
    def encodeAvro(classLoader: ClassLoader, in: DBObject, out: Encoder) = FailedG("DBObject not codable in Avro", Nil)
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: DBObject) = FailedG("DBObject not codable in Avro", Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case dbo: DBObject => Okay(dbo)
            case _             => FailedG("not a DBObject", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: DBObject) =
        Okay(in)

    override def toString = "DBObjectCoder"
}
