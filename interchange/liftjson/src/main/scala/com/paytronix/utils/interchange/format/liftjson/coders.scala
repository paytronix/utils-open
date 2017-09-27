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
                    case JNull|JNothing   => generator.writeNothingOrNull()
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

    import net.liftweb.common.{Box, Full, Empty,Failure, ParamFailure,EmptyBox}
    /**
     * Coder for `Box[A]`.
     *
     * Encodes as `null`/nothing or the value if the value will not be null, `null`/nothing or a
     * single element array if the value might be encoded as `null`.
     * See the discussion of `mightBeNull` on `JsonEncoderOrDecoder` for more about the specifics of nested `null`-like codings.
     */
    def boxJsonCoder[A](valueCoder: JsonCoder[A]): JsonCoder[Box[A]] =
        boxJsonCoder[A](valueCoder.encode, valueCoder.decode)

    /**
     * Coder for `Box[A]`.
     *
     * Encodes as `null`/nothing or the value if the value will not be null, `null`/nothing or a
     * single element array if the value might be encoded as `null`.
     * See the discussion of `mightBeNull` on `JsonEncoderOrDecoder` for more about the specifics of nested `null`-like codings.
     */
    implicit def boxJsonCoder[A](implicit valueEncoder: JsonEncoder[A], valueDecoder: JsonDecoder[A]): JsonCoder[Box[A]] =
        JsonCoder.make(boxJsonEncoder(valueEncoder), boxJsonDecoder(valueDecoder))

    /**
     * Encoder for `Box[A]`.
     *
     * Encodes as `null`/nothing or the value if the value will not be null, `null`/nothing or a
     * single element array if the value might be encoded as `null`.
     * See the discussion of `mightBeNull` on `JsonEncoderOrDecoder` for more about the specifics of nested `null`-like codings.
     */
    def boxJsonEncoder[A](implicit valueEncoder: JsonEncoder[A]): JsonEncoder[Box[A]] =
            new JsonEncoder[Box[A]] {
                val mightBeNull = true
                val codesAsObject = valueEncoder.codesAsObject

                def run(in: Box[A], out: InterchangeJsonGenerator) =
                    in match {
                        case Full(a) =>
                            if(codesAsObject){
                                out.writeStartObject() >>
                                out.writeFieldName("result") >> out.writeString("success")
                                out.writeFieldName("value") >> {
                                    if (valueEncoder.mightBeNull){
                                        out.writeStartArray() >> {
                                            out.omitNextMissing()
                                            valueEncoder.run(a, out)
                                        } >> out.writeEndArray()
                                    } else {
                                            out.omitNextMissing()
                                            valueEncoder.run(a, out)
                                    }
                                } >>
                                out.writeEndObject()
                            }
                            else {
                                if (valueEncoder.mightBeNull){
                                    out.writeStartArray() >> {
                                        out.omitNextMissing()
                                        valueEncoder.run(a, out)
                                    } >> out.writeEndArray()
                                }
                                else {
                                    out.omitNextMissing()
                                    valueEncoder.run(a, out)
                                }
                            }
                        case Failure(msg,exception,chain) =>
                            out.writeStartObject() >> {
                                out.omitNextMissing()
                                out.writeFieldName("result") >> out.writeString("failed") >>
                                out.writeFieldName("errorCode") >> out.writeString("system.error") >>
                                out.writeFieldName("errorMessage") >> out.writeString(msg) >>
                                //if the exception is Full, print out the exception, otherwise
                                //leave the exception field out of the result
                                exception.map{(in:Throwable) => 
                                    out.writeFieldName("exception") >> out.writeStartObject() >>
                                    out.writeFieldName("isA") >> out.writeString(in.getClass().getName())
                                    if(in.getMessage()!=null){
                                        out.writeFieldName("message") >> out.writeString(in.getMessage())
                                    }
                                    out.writeEndObject()
                                }.openOr(Okay.unit) >> {
                                    chain.foreach {
                                        out.writeFieldName("chain") >> run(_, out)
                                    }
                                    Okay.unit
                                }
                                   
                            } >> out.writeEndObject()
                        case Empty =>
                            out.writeNothingOrNull()
                    }
            }

    protected final def readThrowable(in: InterchangeJsonParser, out: Receiver[Throwable]): CoderResult[Unit] = {
        class State {
            var isA: String = null
            var message: String = null
        }

        val state = new State
        out(null)

        in.foreachFields {
            case "isA"     => state.isA = in.stringValue; Okay.unit
            case "message" => state.message = in.stringValue; Okay.unit
            case "chain"   => readThrowable(in, out)
            case _         => in.skipToEndOfValue()
        } >>
        {
            if (state.isA == null) FailedG("expecting \"isA\"", CoderFailure.terminal)
            else if (state.message == null) FailedG("expecting \"message\"", CoderFailure.terminal)
            else instantiateThrowable(state.isA, state.message, Option(out.value)).mapFailure(_ => CoderFailure.terminal) >>= out.apply
        }
    }

    /**
     * Decoder for `Box[A]`.
     *
     * Decodes from `null`/nothing or the value if the value will not be null, `null`/nothing or a
     * single element array if the value might be encoded as `null`.
     * See the discussion of `mightBeNull` on `JsonEncoderOrDecoder` for more about the specifics of nested `null`-like codings.
     */
    def boxJsonDecoder[A](implicit valueDecoder: JsonDecoder[A]): JsonDecoder[Box[A]] =
        if (valueDecoder.mightBeNull)
            new JsonDecoder[Box[A]] {
                val mightBeNull = true
                val codesAsObject = valueDecoder.codesAsObject

                def run(in: InterchangeJsonParser, out: Receiver[Box[A]]) =
                    if (!in.hasValue || in.currentToken == JsonToken.VALUE_NULL){
                        out(Empty)
                    }
                    else if (in.currentToken == JsonToken.START_ARRAY) {
                        val rec = new Receiver[A]
                        in.advanceTokenUnguarded() >> {
                            in.currentToken match {
                                case JsonToken.END_ARRAY =>
                                    in.currentValueIsMissing()
                                    valueDecoder.run(in, rec) >> out(Full(rec.value))
                                case null =>
                                    in.unexpectedToken("value in one-element array")
                                case _ =>
                                    valueDecoder.run(in, rec) >> in.advanceTokenUnguarded() >> {
                                        in.currentToken match {
                                            case JsonToken.END_ARRAY =>
                                                out(Full(rec.value))
                                            case other =>
                                                in.unexpectedToken("end of a single element array")
                                        }
                                    }
                            }
                        }
                    } 
                    else{
                        in.unexpectedToken("single element array or null")
                    }
            }
        else
            new JsonDecoder[Box[A]] {
                val mightBeNull = false
                val codesAsObject = valueDecoder.codesAsObject

                class State {
                    var throwable: Receiver[Throwable] = new Receiver[Throwable]
                    var chain:     Box[A]              = null
                    var message:   String              = null
                }
                var toBeReturned: Box[A] = Empty

                def runHelper(in: InterchangeJsonParser, out: Receiver[Box[A]]): Box[A] =
                    if (!in.hasValue || in.currentToken == JsonToken.VALUE_NULL){
                        Empty
                    }
                    else if (in.currentToken == JsonToken.START_OBJECT) {
                        in.peekFields(Array("result")) >>= {
                            case Array(Some(resultString)) if (resultString == "success" || resultString == "failed") => {
                                if (resultString == "success") {

                                    in.foreachFields {
                                        case "value" => {
                                            val rec = new Receiver[A]
                                            valueDecoder.run(in, rec)
                                            toBeReturned = Full(rec.value)
                                            Okay.unit
                                        }
                                        case _       => in.skipToEndOfValue()
                                    }
                                    Okay.unit
                                } else {
                                    val state = new State

                                    in.foreachFields {
                                        case "errorMessage" => state.message = in.stringValue
                                                               Okay.unit
                                        case "exception"    => readThrowable(in, state.throwable)
                                        case "chain"        => state.chain = runHelper(in,out)
                                                               Okay.unit
                                        case _              => in.skipToEndOfValue()
                                    }

                                    toBeReturned = Failure (
                                           state.message,
                                           if(state.throwable.value!=null) Full(state.throwable.value) else Empty,
                                           state.chain match {
                                                case f:Failure => Full(f)
                                                case _ => Empty
                                           })

                                    Okay.unit
                                }
                            }
                            case _ => in.unexpectedToken("one of success or failed")
                        }

                        toBeReturned
                    }
                    else {
                        val rec = new Receiver[A]
                        valueDecoder.run(in, rec)
                        Full(rec.value)
                    }

                def run(in: InterchangeJsonParser, out: Receiver[Box[A]]) =
                    out(runHelper(in,out))
            }
}