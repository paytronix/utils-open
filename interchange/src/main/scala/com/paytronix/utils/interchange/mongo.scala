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

import java.nio.ByteBuffer
import java.nio.charset.Charset;
import java.util.regex.Pattern
import scala.collection.JavaConverters.{asScalaBufferConverter, asScalaSetConverter, bufferAsJavaListConverter, mapAsScalaMapConverter}
import scala.collection.mutable.ArrayBuffer

import com.mongodb.{BasicDBList, BasicDBObject, DBObject}
import net.liftweb.json.JsonAST.{JArray, JBool, JDouble, JInt, JField, JNothing, JNull, JObject, JString, JValue}
import org.bson.BSONObject
import org.bson.types.{Binary, BSONTimestamp, ObjectId}

import com.paytronix.utils.scala.result.Result

trait CodedMongoObject[T] {
    def objectManifest: Manifest[T]
    private implicit def codedMongoObjectManifest = objectManifest

    lazy val coder = Coding.forClass[T](objectManifest)

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
    // I've chosen to use this field name because spaces are not allowed in Java field names, and so it should never conflict with an actual field name on an object being encoded/decoded
    private val StringWithNullFieldName = "String With Null"

    // FIXME very exceptionful
    def toJValue(a: Any): JValue =
        a match {
            case null                     => JNull
            case l: java.util.List[_]     => JArray(l.asScala.map(toJValue).toList)
            case b: Boolean               => JBool(b)
            case d: Double                => JDouble(d)
            case bd: java.math.BigDecimal => JDouble(bd.doubleValue)
            case bd: BigDecimal           => JDouble(bd.doubleValue)
            case bi: java.math.BigInteger => JInt(new BigInt(bi))
            case bi: BigInt               => JInt(bi)
            case b: Byte                  => JInt(BigInt(b))
            case s: Short                 => JInt(BigInt(s))
            case i: Int                   => JInt(BigInt(i))
            case l: Long                  => JInt(BigInt(l))
            case s: String                => JString(s)

            case m: java.util.Map[_, _] if m.containsKey(StringWithNullFieldName) =>
                JString(new String(m.get(StringWithNullFieldName).asInstanceOf[Array[Byte]], Charset.forName("UTF-8")))

            case m: java.util.Map[_, _] =>
                JObject(m.asScala.map {
                    case (k: String, v) => JField(k, toJValue(v))
                    case kvp => sys.error("expected map with string keys, but got " + kvp)
                }.toList)

            case bso: BSONObject if bso.containsField(StringWithNullFieldName) =>
                JString(new String(bso.get(StringWithNullFieldName).asInstanceOf[Array[Byte]], Charset.forName("UTF-8")))

            case bso: BSONObject =>
                JObject(bso.keySet.asScala.map {
                    case k => JField(k, toJValue(bso.get(k)))
                }.toList)

            case p: Pattern =>
                var fls = ""
                if ((p.flags & Pattern.CASE_INSENSITIVE) != 0) fls += "i"
                if ((p.flags & Pattern.MULTILINE) != 0) fls += "m"
                if ((p.flags & Pattern.DOTALL) != 0) fls += "s"
                JObject(JField("$regex", JString(p.toString)) :: JField("$options", JString(fls)) :: Nil)
            case r: scala.util.matching.Regex =>
                toJValue(r.pattern)
            case jud: java.util.Date =>
                JObject(JField("$date", JInt(BigInt(jud.getTime))) :: Nil)
            case jod: org.joda.time.ReadableInstant =>
                JObject(JField("$date", JInt(BigInt(jod.getMillis))) :: Nil)
            case b: Binary =>
                val base64 = ByteBufferCoder.encodeString(getClass.getClassLoader, ByteBuffer.wrap(b.getData).limit(b.length).asInstanceOf[ByteBuffer]).orThrow
                JObject(JField("$binary", JString(base64)) :: JField("$type", JString("%02x".format(b.getType))) :: Nil)
            case t: BSONTimestamp =>
                JObject(JField("$timestamp", JObject(JField("t", JInt(BigInt(t.getTime))) :: JField("i", JInt(BigInt(t.getInc))) :: Nil)) :: Nil)
            case oid: ObjectId =>
                JObject(JField("$oid", JString(oid.toString)) :: Nil)

            case _ =>
                sys.error("unhandled type in MongoDB data: " + a.getClass.getName + " (" + (try String.valueOf(a) catch { case e: Exception => "<" + e.toString + ">" }) + ")")
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

    def fromJValues(in: Seq[JValue]): DBObject = {
        val dbList = new BasicDBList

        for (x <- in)
            dbList.add(fromJValue(x))

        dbList
    }

    private def fieldOrThrow(fls: List[JField], s: String): JValue =
        fls.find(_.name == s).getOrElse(sys.error("required field " + s + " was missing"))

    // FIXME very exceptionful
    def fromJValue(in: JValue): AnyRef =
        in match {
            case JObject(jfs) =>
                jfs.find {
                    case JField("$regex"|"$date"|"$binary"|"$timestamp"|"$oid", _) => true
                    case _ => false
                } match {
                    case Some(JField("$regex", v)) =>
                        val pattern = v.asInstanceOf[JString].s
                        val flags = jfs.find(_.name == "$options").map(_.asInstanceOf[JString].s).map { s =>
                            (if (s contains 'i') Pattern.CASE_INSENSITIVE else 0) | (if (s contains 'm') Pattern.MULTILINE else 0) | (if (s contains 's') Pattern.DOTALL else 0)
                        }.getOrElse(0)
                        java.util.regex.Pattern.compile(pattern, flags)
                    case Some(JField("$date", v)) =>
                        val millis = v.asInstanceOf[JInt].num.intValue
                        new java.util.Date(millis)
                    case Some(JField("$binary", v)) =>
                        val typ = Integer.parseInt(fieldOrThrow(jfs, "$type").asInstanceOf[JString].s, 16)
                        val bytes = ByteBufferCoder.decodeString(getClass.getClassLoader, v.asInstanceOf[JString].s).orThrow.array
                        new Binary(typ.asInstanceOf[Byte], bytes)
                    case Some(JField("$timestamp", v)) =>
                        val sub = v.asInstanceOf[JObject].obj
                        val time = fieldOrThrow(sub, "time").asInstanceOf[JInt].num.intValue
                        val inc = fieldOrThrow(sub, "inc").asInstanceOf[JInt].num.intValue
                        new BSONTimestamp(time, inc)
                    case Some(JField("$oid", v)) =>
                        val id = v.asInstanceOf[JString].s
                        new ObjectId(id)
                    case _ =>
                        fromJValue(jfs)
                }

            case JArray(a)      => fromJValues(a)
            case JBool(b)       => b.asInstanceOf[AnyRef]
            case JDouble(d)     => d.asInstanceOf[AnyRef]
            case JField(n, v)   => sys.error("should not have called fromJValue on a JField!")
            case JInt(i)        =>
                if (i >= BigInt(java.lang.Integer.MIN_VALUE.toString) && i <= BigInt(java.lang.Integer.MAX_VALUE.toString)) i.longValue.asInstanceOf[AnyRef]
                else if (i >= BigInt(java.lang.Integer.MIN_VALUE.toString) && i <= BigInt(java.lang.Integer.MAX_VALUE.toString)) i.intValue.asInstanceOf[AnyRef]
                else i.toString
            case JString(s)     =>
                s.indexOf('\u0000') match {
                    case -1 => s
                    case _  => new BasicDBObject(StringWithNullFieldName, new Binary(s.getBytes(Charset.forName("UTF-8"))))
                }
            case JNothing|JNull => null
        }
}
