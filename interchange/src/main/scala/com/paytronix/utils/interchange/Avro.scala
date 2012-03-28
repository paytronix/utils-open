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

import java.util.Arrays
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable.WeakHashMap
import org.apache.avro.Schema
import org.apache.avro.io.{DecoderFactory, ResolvingDecoder}
import com.paytronix.utils.scala.reflection.splitFullyQualifiedName

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
    def nullable(in: Schema): Schema = Schema.createUnion(Arrays.asList(in, Schema.create(Schema.Type.NULL)))

    def nameAndNamespaceFromClass(clazz: Class[_]): (String, String) = {
        val rawName = clazz.getName
        splitFullyQualifiedName (
            if (rawName.endsWith("$")) rawName.substring(0, rawName.length() - 1).replace('$', '.')
            else                       rawName.replace('$', '.')
        )
    }

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
}
