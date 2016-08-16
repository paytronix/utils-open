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

import java.util.Arrays
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable.WeakHashMap
import scala.reflect.runtime.universe.{Constant, TypeTag, typeOf, typeTag}
import scala.util.MurmurHash.stringHash

import org.apache.avro
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.reflection.splitFullyQualifiedName
import com.paytronix.utils.scala.result.{Result, tryCatchValue}


object utils {
    /** Object that holds a thread-local cache for Avro ResolvingDecoders */
    object ResolvingDecoderCache {
        private val _cache = new ThreadLocal[WeakHashMap[(avro.Schema, avro.Schema), avro.io.ResolvingDecoder]]

        private def cache = _cache.get match {
            case null => {
                val m = WeakHashMap.empty[(avro.Schema, avro.Schema), avro.io.ResolvingDecoder]
                _cache.set(m)
                m
            }

            case m => m
        }

        def apply(reader: avro.Schema, writer: avro.Schema): avro.io.ResolvingDecoder =
            cache.getOrElseUpdate (
                (reader, writer),
                avro.io.DecoderFactory.get.resolvingDecoder(avro.Schema.applyAliases(writer, reader), reader, null)
            )
    }

    /** Create a union schema representing a nullable value of the given input schema, with index 1 meaning value present and index 0 meaning null / not present */
    def nullable(in: avro.Schema): avro.Schema =
        /* defaults for unions in Avro are not configurable, and default to the first union alternative. however, avro still checks the nullosity of the default, so we have to provide something */
        avro.Schema.createUnion(Arrays.asList(avro.Schema.create(avro.Schema.Type.NULL), in))

    /** Create a new Avro schema field */
    def makeField(name: String, schema: avro.Schema, defaultJson: Option[JsonNode], doc: String = ""): avro.Schema.Field =
        new avro.Schema.Field(name, schema, doc, defaultJson.orNull)

    /** Results of analyzing a class name and annotations for Avro schema naming information */
    final case class AvroTypeNaming(namespace: String, name: String, aliases: Set[String])

    /** Split a Class formal name into a pair containing namespace and unqualified name */
    def typeNaming[A: TypeTag]: AvroTypeNaming = {
        val avroNameTpe = typeOf[com.paytronix.utils.interchange.format.avro.name]
        val avroAliasesTpe = typeOf[com.paytronix.utils.interchange.format.avro.aliases]

        val rawName = typeTag[A].tpe.typeSymbol.fullName
        val (namespace, defaultName) = splitFullyQualifiedName (
            if (rawName.endsWith("$")) rawName.substring(0, rawName.length() - 1).replace('$', '.')
            else                       rawName.replace('$', '.')
        )

        val avroName = typeTag[A].tpe.typeSymbol.annotations
            .collectFirst { case annot if annot.tree.tpe =:= avroNameTpe && annot.tree.children.size > 1 => annot }
            .map { annot => annot.tree.children.tail.head }
            // this next bit is especially awful
            .map { tree => tree.productElement(0).asInstanceOf[Constant].value.asInstanceOf[String] }
            .getOrElse { defaultName }
        val avroAliases = typeTag.tpe.typeSymbol.annotations
            .collectFirst { case annot if annot.tree.tpe =:= avroAliasesTpe && annot.tree.children.size > 1 => annot }
            .map { annot => annot.tree.children.tail }
            .map { trees => trees.map { _.productElement(0).asInstanceOf[Constant].value.asInstanceOf[String] } }
            .map { strings => Set.empty ++ strings }
            .getOrElse { Set.empty }

        AvroTypeNaming(namespace, avroName, avroAliases)
    }

    /** Sanitize a name to a format acceptable as an identifier, replacing invalid characters with _hhhh where hhhh is the unicode value */
    def sanitizeSchemaName(in: String): String = {
        val chars = in.toCharArray
        val out = new StringBuilder
        var i = 0
        var start = 0
        while (i < chars.length) {
            val ch = chars(i)
            if (i == 0 && !(Character.isLetter(ch) || ch == '_') || !(Character.isLetterOrDigit(ch) || ch == '_')) {
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

    /** Encode the name of a schema for use in generating various automatically generated record types, e.g. for tuples */
    def encodeSchemaName(in: avro.Schema): String = {
        import avro.Schema.Type._

        lazy val encodeName = sanitizeSchemaName(in.getFullName)

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

    /** Produce a 128-bit MD5 fingerprint from a canonical version of the given `Schema`, for addressing the schema without transmitting it in its entirety */
    def fingerprintSchema(s: avro.Schema): Result[Array[Byte]] =
        tryCatchValue(avro.SchemaNormalization.parsingFingerprint("MD5", s))

}
