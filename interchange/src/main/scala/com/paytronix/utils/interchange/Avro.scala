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

import java.security.MessageDigest
import java.util.Arrays
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable.WeakHashMap
import scala.util.MurmurHash.stringHash

import net.liftweb.json.JsonAST.{JField, JNull, JObject, JValue, render}
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.Printer.compact
import org.apache.avro.Schema
import org.apache.avro.io.{DecoderFactory, ResolvingDecoder}
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.reflection.splitFullyQualifiedName
import com.paytronix.utils.scala.result.{Result, tryCatch}

/** Object that holds a thread-local cache for Avro ResolvingDecoders */
private object ResolvingDecoderCache {
    private val _cache = new ThreadLocal[WeakHashMap[(Schema, Schema), ResolvingDecoder]]

    private def cache = _cache.get match {
        case null => {
            val m = WeakHashMap.empty[(Schema, Schema), ResolvingDecoder]
            _cache.set(m)
            m
        }

        case m => m
    }

    def apply(reader: Schema, writer: Schema): ResolvingDecoder =
        cache.getOrElseUpdate (
            (reader, writer),
            DecoderFactory.get.resolvingDecoder(Schema.applyAliases(writer, reader), reader, null)
        )
}

/** Object with helpers for doing Avro work */
object AvroUtils {
    /** Create a union schema representing a nullable value of the given input schema, with index 1 meaning value present and index 0 meaning null / not present */
    def nullable(in: Schema): (Schema, Option[JsonNode]) =
        /* defaults for unions in Avro are not configurable, and default to the first union alternative. however, avro still checks the nullosity of the default, so we have to provide something */
        (Schema.createUnion(Arrays.asList(Schema.create(Schema.Type.NULL), in)), Some(jsonNodeFactory.nullNode))

    /** Create a new Avro schema field using the `(Schema, Option[JsonNode])` pair that Interchange passes around */
    def makeField(name: String, schemaAndDefault: (Schema, Option[JsonNode]), doc: String = ""): Schema.Field =
        new Schema.Field(name, schemaAndDefault._1, doc, schemaAndDefault._2.orNull)

    /** Split a Class formal name into a pair containing namespace and unqualified name */
    def nameAndNamespaceFromClass(clazz: Class[_]): (String, String) = {
        val rawName = clazz.getName
        splitFullyQualifiedName (
            if (rawName.endsWith("$")) rawName.substring(0, rawName.length() - 1).replace('$', '.')
            else                       rawName.replace('$', '.')
        )
    }

    /** Encode the name of a schema for use in generating various automatically generated record types, e.g. for tuples */
    def encodeSchemaName(in: Schema): String = {
        import Schema.Type._

        lazy val encodeName = {
            val chars = in.getFullName.toCharArray
            val out = new StringBuilder
            var i = 0
            var start = 0
            while (i < chars.length) {
                val ch = chars(i)
                if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9'))) {
                    if (start != i) {
                        out.appendAll(chars, start, i - start)
                        start = i+1
                    }
                    out.append('_').append(String.format("%04x", (ch: Int).asInstanceOf[AnyRef]))
                }
                i = i+1
            }
            out.appendAll(chars, start, chars.length-start)
            out.toString
        }

        in.getType match {
            case ARRAY   => return "A" + encodeSchemaName(in.getElementType)
            case BOOLEAN => return "z"
            case BYTES   => return "B"
            case DOUBLE  => return "d"
            case ENUM    => return "E" + encodeName
            case FIXED   => return "F" + encodeName
            case FLOAT   => return "f"
            case INT     => return "i"
            case LONG    => return "l"
            case MAP     => return "M" + encodeSchemaName(in.getValueType)
            case NULL    => return "N"
            case RECORD  => return "R" + encodeName
            case STRING  => return "S"
            case UNION   => return "U" + in.getTypes.size + "__" + in.getTypes.asScala.map(encodeSchemaName).mkString("")
        }
    }

    /** Convert a `JValue` into an Avro `Schema` */
    def schemaFromJValue(in: JValue): Result[Schema] =
        tryCatch.value(new Schema.Parser().parse(compact(render(in))))

    /** Convert a `Schema` into an Avro `JValue` */
    def schemaToJValue(s: Schema): Result[JValue] =
        tryCatch.value(parse(s.toString))

    /** Produce a hash from a canonical version of the given schema JSON, for addressing the schema without transmitting it in its entirety */
    def hashSchema(in: JValue): Result[Array[Byte]] =
        tryCatch.value {
            val s = compact(render(in.transform {
                case JObject(fields) => JObject(fields.sortBy(_.name))
            }))

            val md = MessageDigest.getInstance("SHA-512")
            md.digest(s.getBytes("UTF-8"))
        }

    /** Produce a hash from a canonical version of the given `Schema`, for addressing the schema without transmitting it in its entirety */
    def hashSchema(in: Schema): Result[Array[Byte]] =
        schemaToJValue(in).flatMap(hashSchema)

    /** Convert a Lift JSON JValue into a Jackson JsonNode, which Avro expects for default values */
    def jvalueToJsonNode(in: JValue): JsonNode = {
        import net.liftweb.json.JsonAST._

        in match {
            case JBool(b)     => jsonNodeFactory.booleanNode(b)
            case JDouble(d)   => jsonNodeFactory.numberNode(d)
            case JField(_, _) => sys.error("jvalueToJsonNode should never have encountered a JField")
            case JInt(bi)     => jsonNodeFactory.numberNode(bi.bigInteger)
            case JNothing     => jsonNodeFactory.nullNode
            case JNull        => jsonNodeFactory.nullNode
            case JString(s)   => jsonNodeFactory.textNode(s)

            case JArray(els) =>
                val node = jsonNodeFactory.arrayNode
                for (jv <- els) node.add(jvalueToJsonNode(jv))
                node

            case JObject(fs) =>
                val node = jsonNodeFactory.objectNode
                for (JField(s, jv) <- fs) node.put(s, jvalueToJsonNode(jv))
                node
        }
    }
}
