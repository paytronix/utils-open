//
// Copyright 2016 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange.format.liftjson

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer
import com.paytronix.utils.scala.reflection.classByName
import java.lang.ClassLoader

import com.fasterxml.jackson.core.JsonToken
import net.liftweb.json.JsonAST.{JValue, JArray, JBool, JDouble, JField, JInt, JNothing, JNull, JObject, JString}

import com.paytronix.utils.interchange
import com.paytronix.utils.interchange.base.{CoderFailure, CoderResult, Receiver,InsecureContext,InterchangeClassLoader}
import com.paytronix.utils.interchange.format.{json, string}
import com.paytronix.utils.interchange.format.json.{JsonCoder, JsonDecoder, JsonEncoder, InterchangeJsonGenerator, InterchangeJsonParser}
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, tryCatchResultG,ResultG}
import com.paytronix.utils.interchange.base.container.result.instantiateThrowable
import json.coders.{booleanJsonCoder, doubleJsonCoder, intJsonCoder, jsonObjectCoder, jsonObjectEncoder, jsonObjectDecoder, jsonArrayCoder, scalaBigIntJsonCoder, stringJsonCoder}
import string.coders.stringCoder


/** `JsonCoder`s to convert raw JSON into happy `JValue` types */
object coders {
    // This could've just used a derived adHocUnion coder except there's an infinitely recursive call stack
    // involved with the adHocUnion logic and the wrapper coder logic used for JArrayCoder
    implicit object jValueCoder extends JsonCoder[JValue] {
        object decode extends JsonDecoder[JValue] {
            val mightBeNull = true
            val codesAsObject = false

            def run(parser: InterchangeJsonParser, receiver: Receiver[JValue]): CoderResult[Unit] = {
                def tryParse[A <: JValue](decoder: JsonDecoder[A]): CoderResult[Unit] = {
                    val subReceiver = new Receiver[A]
                    parser.excursion(decoder.run(parser, subReceiver)) >>
                    parser.skipToEndOfValue() >>
                    receiver(subReceiver.value)
                }

                if (!parser.hasValue || parser.currentToken == JsonToken.VALUE_NULL) {
                    receiver(JNull)
                } else {
                    // JString is first because it will only decode actual JSON strings, but the int, boolean, and double
                    // coders will accept strings if they contain data that can be converted to the appropriate type
                    tryParse(jStringCoder.decode)
                    .orElse(tryParse(jIntCoder.decode))
                    .orElse(tryParse(jBoolCoder.decode))
                    .orElse(tryParse(jDoubleCoder.decode))
                    .orElse(tryParse(jArrayCoder.decode))
                    .orElse(tryParse(jObjectCoder.decode))
                    .orElse(parser.unexpectedToken("a valid JValue"))
                }
            }
        }

        object encode extends JsonEncoder[JValue] {
            val mightBeNull = true
            val codesAsObject = false
            def run(jValue: JValue, generator: json.InterchangeJsonGenerator): CoderResult[Unit] = {
                def tryEncode[A <: JValue](value: A, encoder: JsonEncoder[A]): CoderResult[Unit] = tryCatchResultG(interchange.base.terminal)(encoder.run(value, generator))

                jValue match {
                    case jInt: JInt       => tryEncode(jInt,    jIntCoder.encode)
                    case jBool: JBool     => tryEncode(jBool,   jBoolCoder.encode)
                    case jDouble: JDouble => tryEncode(jDouble, jDoubleCoder.encode)
                    case jString: JString => tryEncode(jString, jStringCoder.encode)
                    case jArray: JArray   => tryEncode(jArray,  jArrayCoder.encode)
                    case jObject: JObject => tryEncode(jObject, jObjectCoder.encode)
                    case JNull|JNothing   => generator.writeNull()
                    case _: JField        => FailedG(s"JFields cannot be encoded by this coder", interchange.base.CoderFailure.terminal)
                }
            }
        }
    }

    implicit val jArrayCoder:  JsonCoder[JArray]  = json.derive.wrapper.coder[JArray]
    implicit val jBoolCoder:   JsonCoder[JBool]   = json.derive.wrapper.coder[JBool]
    implicit val jDoubleCoder: JsonCoder[JDouble] = json.derive.wrapper.coder[JDouble]
    implicit val jIntCoder:    JsonCoder[JInt]    = json.derive.wrapper.coder[JInt]
    implicit val jStringCoder: JsonCoder[JString] = json.derive.wrapper.coder[JString]

    /** `CanBuildFrom` for `JObject` to allow us to use jsonObjectDecoder so we don't have to roll our own */
    def canBuildJObject = new CanBuildFrom[Nothing, (String, JValue), JObject] {
        def apply() = new Builder[(String, JValue), JObject] {
            val buf = new ListBuffer[JField]
            def clear() = buf.clear()
            def result() = JObject(buf.toList)
            def += (p: (String, JValue)) = {
                buf += JField(p._1, p._2)
                this
            }
        }

        def apply(from: Nothing) = apply()
    }

    implicit val jObjectCoder: JsonCoder[JObject] = {
        val encoder = jsonObjectEncoder[String, JValue, JObject](_.obj.map(f => f.name -> f.value), string.StringEncoder[String], jValueCoder.encode)
        val decoder = jsonObjectDecoder[String, JValue, JObject](canBuildJObject, string.StringDecoder[String], jValueCoder.decode)
        json.JsonCoder.make(encoder, decoder)
    }
}