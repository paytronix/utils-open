//
// Copyright 2012 Paytronix Systems, Inc.
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

import scala.collection.JavaConverters.{asScalaBufferConverter, bufferAsJavaListConverter, mapAsScalaMapConverter}
import scala.collection.mutable.ArrayBuffer

import com.mongodb.{BasicDBObject, DBObject}
import net.liftweb.json.JsonAST.{JArray, JBool, JDouble, JInt, JField, JNothing, JNull, JObject, JString, JValue}

import com.paytronix.utils.scala.result.Result

trait CodedMongoObject[T] {
    implicit val manifest: Manifest[T]
    lazy val coder = Coding.forClass[T]

    /** Implicitly convert a coded object to a DBObject, throwing an exception if coding fails */
    implicit def toDBObjectOrThrow(obj: T): DBObject =
        toDBObject(obj).orThrow

    /** Implicitly convert a coded object to a DBObject */
    implicit def toDBObject(obj: T): Result[DBObject] =
        coder.flatMap(_.encodeMongoDB(obj)).asA[DBObject]

    /** Implicitly convert a coded object from a DBObject, throwing an exception if coding fails */
    implicit def fromDBObjectOrThrow(dbo: DBObject): T =
        fromDBObject(dbo).orElse("failed to decode " + dbo).orThrow

    /** Implicitly convert a coded object from a DBObject */
    implicit def fromDBObject(dbo: DBObject): Result[T] =
        coder.flatMap(_.decodeMongoDB(dbo)).orElse("failed to decode " + dbo).asA[T]
}

object MongoUtils {
    def toJValue(a: Any): JValue =
        a match {
            case null => JNull
            case l: java.util.List[_]     => JArray(l.asScala.map(toJValue).toList)
            case b: Boolean               => JBool(b)
            case d: Double                => JDouble(d)
            case bd: java.math.BigDecimal => JDouble(bd.doubleValue)
            case bd: BigDecimal           => JDouble(bd.doubleValue)
            case bi: java.math.BigInteger => JInt(new BigInt(bi))
            case bi: BigInt               => JInt(bi)
            case i: Int                   => JInt(BigInt(i))
            case m: java.util.Map[_, _] =>
                JObject(m.asScala.map {
                    case (k: String, v) => JField(k, toJValue(v))
                    case kvp => sys.error("expected map with string keys, but got " + kvp)
                }.toList)
            case s: String => JString(s)
        }

    def fromJValue(in: JObject): DBObject =
        fromJValue(in.obj)

    def fromJValue(in: List[JField]): DBObject = {
        val dbObject = new BasicDBObject

        for (JField(n, v) <- in) {
            dbObject.put(n, fromJValue(v))
        }

        dbObject
    }

    def fromJValue(in: JValue): AnyRef =
        in match {
            case JArray(a)      => ArrayBuffer(a.map(fromJValue)).asJava
            case JBool(b)       => b.asInstanceOf[AnyRef]
            case JDouble(d)     => d.asInstanceOf[AnyRef]
            case JField(n, v)   => sys.error("should not have called fromJValue on a JField!")
            case JInt(i)        =>
                if (i > BigInt(java.lang.Integer.MAX_VALUE.toString) || i < BigInt(java.lang.Integer.MIN_VALUE.toString)) i.toString
                else i.intValue.asInstanceOf[AnyRef]
            case JObject(jfs)   => fromJValue(jfs)
            case JString(s)     => s
            case JNothing|JNull => null
        }
}
