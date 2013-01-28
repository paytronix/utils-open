//
// Copyright 2010-2012 Paytronix Systems, Inc.
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

import java.io.{ByteArrayOutputStream, InputStream, IOException, OutputStream}
import java.lang.reflect.{Constructor, Method}
import java.math.BigInteger
import java.nio.ByteBuffer
import java.util.{
    Arrays,
    ArrayList  => JavaArrayList,
    Collection => JavaCollection,
    Date       => JavaDate,
    HashMap    => JavaHashMap,
    List       => JavaList,
    Map        => JavaMap
}
import javax.xml.bind.DatatypeConverter
import scala.annotation.tailrec
import scala.collection.JavaConverters.{iterableAsScalaIterableConverter, mapAsScalaMapConverter, seqAsJavaListConverter}
import scala.collection.generic.{CanBuild, CanBuildFrom}
import scala.collection.immutable.{Map => ImmutableMap, Set => ImmutableSet}
import scala.collection.mutable.{ArrayBuffer, Buffer, Builder, Map => MutableMap, Set => MutableSet}
import scala.reflect.Manifest

import com.mongodb.{BasicDBObject, DBObject}
import net.liftweb.json.Implicits.string2jvalue
import net.liftweb.json.JsonAST.{JArray, JBool, JDouble, JField, JInt, JNothing, JNull, JObject, JString, JValue, render}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.Printer.compact
import org.apache.avro.Schema
import org.apache.avro.io.{DatumReader, DatumWriter, Decoder, DecoderFactory, Encoder, EncoderFactory, ResolvingDecoder}
import org.bson.{BSON, BSONObject}
import org.bson.types.{Binary, ObjectId}
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}
import org.joda.time.{DateTime, Duration, LocalDate, LocalDateTime, LocalTime}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.slf4j.LoggerFactory

import com.paytronix.utils.extendedreflection
import com.paytronix.utils.scala.concurrent.ThreadLocal
import com.paytronix.utils.scala.log.resultLoggerOps
import com.paytronix.utils.scala.reflection.{classByName, paranamer, splitFullyQualifiedName}
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, ResultG, cast, firstOrLastG, iterableResultOps, optionOps, parameter, tryCatch, tryCatching}


