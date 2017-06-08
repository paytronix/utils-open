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

package com.paytronix.utils.interchange.format.json

import scala.annotation.tailrec
import scala.collection.JavaConverters.{asJavaCollectionConverter, asScalaBufferConverter, asScalaSetConverter, collectionAsScalaIterableConverter, mapAsScalaMapConverter}
import scala.collection.generic.CanBuildFrom

import com.fasterxml.jackson.core.JsonToken

import com.paytronix.utils.interchange.base.{CoderFailure, CoderResult, InsecureContext, InterchangeClassLoader, Receiver, terminal}
import com.paytronix.utils.interchange.base.container.javaCollections.{canBuildJavaList, canBuildJavaMap, canBuildJavaSet, canBuildJavaSortedMap}
import com.paytronix.utils.interchange.base.container.scalaIterable.canBuildScalaList
import com.paytronix.utils.interchange.base.container.result.instantiateThrowable
import com.paytronix.utils.interchange.format.string.{StringCoder, StringDecoder, StringEncoder}
import com.paytronix.utils.scala.concurrent.ThreadLocal
import com.paytronix.utils.scala.result.{FailedG, FailedParameterDefault, Okay, ResultG, iterableResultOps}

object container extends container

trait container extends containerLPI {
    /** Wrapper coding for nullable values. Decodes explicit `null` or missing values as `null` and omits the value on output if `null` */
    def nullableJsonCoder[A >: Null](valueCoder: JsonCoder[A]): JsonCoder[A] =
        nullableJsonCoder[A](valueCoder.encode, valueCoder.decode)

    /** Wrapper coding for nullable values. Decodes explicit `null` or missing values as `null` and omits the value on output if `null` */
    def nullableJsonCoder[A >: Null](implicit valueEncoder: JsonEncoder[A], valueDecoder: JsonDecoder[A]): JsonCoder[A] =
        JsonCoder.make(nullableJsonEncoder(valueEncoder), nullableJsonDecoder(valueDecoder))

    /** Wrapper encoder for nullable values. Encodes `null` as an Avro `null`, uses the wrapped encoder otherwise */
    def nullableJsonEncoder[A >: Null](implicit valueEncoder: JsonEncoder[A]) =
        new JsonEncoder[A] {
            val mightBeNull = true
            val codesAsObject = valueEncoder.codesAsObject

            def run(in: A, out: InterchangeJsonGenerator) =
                in match {
                    case null => out.writeNothingOrNull()
                    case _    => valueEncoder.run(in, out)
                }
        }

    /** Wrapper decoder for nullable values. Decodes `null` and passes through to the wrapped coder otherwise */
    def nullableJsonDecoder[A >: Null](implicit valueDecoder: JsonDecoder[A]) =
        new JsonDecoder[A] {
            val mightBeNull = true
            val codesAsObject = valueDecoder.codesAsObject

            def run(in: InterchangeJsonParser, out: Receiver[A]) =
                if (!in.hasValue)
                    out(null)
                else if (in.currentToken == JsonToken.VALUE_NULL)
                    out(null)
                else
                    valueDecoder.run(in, out)
        }

    /**
     * Wrapper decoder which substitutes some default value where the input value is missing. Not usually used directly, instead used via
     * @default annotation or `JsonCoder#default`
     */
    def defaultJsonDecoder[A](value: A)(implicit valueDecoder: JsonDecoder[A]) =
        new JsonDecoder[A] {
            val mightBeNull = valueDecoder.mightBeNull // defaulting never implies by itself that null will be produced
            val codesAsObject = valueDecoder.codesAsObject

            def run(in: InterchangeJsonParser, out: Receiver[A]) =
                if (in.hasValue && in.currentToken != JsonToken.VALUE_NULL) valueDecoder.run(in, out)
                else out(value)
        }

    /**
     * Coder for `Option[A]`.
     *
     * Encodes as `null`/nothing or the value if the value will not be null, `null`/nothing or a
     * single element array if the value might be encoded as `null`.
     * See the discussion of `mightBeNull` on `JsonEncoderOrDecoder` for more about the specifics of nested `null`-like codings.
     */
    def optionJsonCoder[A](valueCoder: JsonCoder[A]): JsonCoder[Option[A]] =
        optionJsonCoder[A](valueCoder.encode, valueCoder.decode)

    /**
     * Coder for `Option[A]`.
     *
     * Encodes as `null`/nothing or the value if the value will not be null, `null`/nothing or a
     * single element array if the value might be encoded as `null`.
     * See the discussion of `mightBeNull` on `JsonEncoderOrDecoder` for more about the specifics of nested `null`-like codings.
     */
    implicit def optionJsonCoder[A](implicit valueEncoder: JsonEncoder[A], valueDecoder: JsonDecoder[A]): JsonCoder[Option[A]] =
        JsonCoder.make(optionJsonEncoder(valueEncoder), optionJsonDecoder(valueDecoder))

    /**
     * Encoder for `Option[A]`.
     *
     * Encodes as `null`/nothing or the value if the value will not be null, `null`/nothing or a
     * single element array if the value might be encoded as `null`.
     * See the discussion of `mightBeNull` on `JsonEncoderOrDecoder` for more about the specifics of nested `null`-like codings.
     */
    def optionJsonEncoder[A](implicit valueEncoder: JsonEncoder[A]): JsonEncoder[Option[A]] =
        if (valueEncoder.mightBeNull)
            new JsonEncoder[Option[A]] {
                val mightBeNull = true
                val codesAsObject = valueEncoder.codesAsObject

                def run(in: Option[A], out: InterchangeJsonGenerator) =
                    in match {
                        case Some(a) =>
                            out.writeStartArray() >> {
                                out.omitNextMissing()
                                valueEncoder.run(a, out)
                            } >> out.writeEndArray()
                        case None =>
                            out.writeNothingOrNull()
                    }
            }
        else
            new JsonEncoder[Option[A]] {
                val mightBeNull = true
                val codesAsObject = valueEncoder.codesAsObject

                def run(in: Option[A], out: InterchangeJsonGenerator) =
                    in match {
                        case Some(a) => valueEncoder.run(a, out)
                        case None    => out.writeNothingOrNull()
                    }
            }

    /**
     * Decoder for `Option[A]`.
     *
     * Decodes from `null`/nothing or the value if the value will not be null, `null`/nothing or a
     * single element array if the value might be encoded as `null`.
     * See the discussion of `mightBeNull` on `JsonEncoderOrDecoder` for more about the specifics of nested `null`-like codings.
     */
    def optionJsonDecoder[A](implicit valueDecoder: JsonDecoder[A]): JsonDecoder[Option[A]] =
        if (valueDecoder.mightBeNull)
            new JsonDecoder[Option[A]] {
                val mightBeNull = true
                val codesAsObject = valueDecoder.codesAsObject

                def run(in: InterchangeJsonParser, out: Receiver[Option[A]]) =
                    if (!in.hasValue || in.currentToken == JsonToken.VALUE_NULL)
                        out(None)
                    else if (in.currentToken == JsonToken.START_ARRAY) {
                        val rec = new Receiver[A]
                        in.advanceTokenUnguarded() >> {
                            in.currentToken match {
                                case JsonToken.END_ARRAY =>
                                    in.currentValueIsMissing()
                                    valueDecoder.run(in, rec) >> out(Some(rec.value))
                                case null =>
                                    in.unexpectedToken("value in one-element array")
                                case _ =>
                                    valueDecoder.run(in, rec) >> in.advanceTokenUnguarded() >> {
                                        in.currentToken match {
                                            case JsonToken.END_ARRAY =>
                                                out(Some(rec.value))
                                            case other =>
                                                in.unexpectedToken("end of a single element array")
                                        }
                                    }
                            }
                        }
                    } else
                        in.unexpectedToken("single element array or null")
            }
        else
            new JsonDecoder[Option[A]] {
                val mightBeNull = true
                val codesAsObject = valueDecoder.codesAsObject

                def run(in: InterchangeJsonParser, out: Receiver[Option[A]]) =
                    if (!in.hasValue || in.currentToken == JsonToken.VALUE_NULL)
                        out(None)
                    else {
                        val rec = new Receiver[A]
                        valueDecoder.run(in, rec) >> out(Some(rec.value))
                    }
            }

    /**
     * Coder for `Either[A, B]` that operates like a implicit union of `A` and `B` with configurable left
     * and right field names.
     *
     * For example, `eitherJsonCoder[String, Int]` encodes as `{ "left": "foo" }` or `{ "right": 123 }`
     */
    def eitherJsonCoder[A, B] (
        leftField: String, leftCoder: JsonCoder[A],
        rightField: String, rightCoder: JsonCoder[B]
    ): JsonCoder[Either[A, B]] =
        eitherJsonCoder(leftField, rightField)(leftCoder.encode, rightCoder.encode, leftCoder.decode, rightCoder.decode)

    /**
     * Coder for `Either[A, B]` that operates like a implicit union of `A` and `B` with configurable left
     * and right field names.
     *
     * For example, `eitherJsonCoder[String, Int]` encodes as `{ "left": "foo" }` or `{ "right": 123 }`
     */
    implicit def eitherJsonCoder[A, B]
        (leftField: String = "left", rightField: String = "right")
        (implicit leftEncoder: JsonEncoder[A], rightEncoder: JsonEncoder[B],
                  leftDecoder: JsonDecoder[A], rightDecoder: JsonDecoder[B])
        : JsonCoder[Either[A, B]] =
        JsonCoder.make(eitherJsonEncoder(leftField, rightField), eitherJsonDecoder(leftField, rightField))

    /**
     * Encoder for `Either[A, B]` that operates like a implicit union of `A` and `B` with configurable left
     * and right field names.
     *
     * For example, `eitherJsonCoder[String, Int]` encodes as `{ "left": "foo" }` or `{ "right": 123 }`
     */
    def eitherJsonEncoder[A, B]
        (leftField: String, rightField: String)
        (implicit leftEncoder: JsonEncoder[A], rightEncoder: JsonEncoder[B]) =
        new JsonEncoder[Either[A, B]] {
            val mightBeNull = false
            val codesAsObject = true

            def run(in: Either[A, B], out: InterchangeJsonGenerator) =
                in match {
                    case Left(a) =>
                        out.writeStartObject() >>
                        out.writeFieldName(leftField) >>
                        leftEncoder.run(a, out) >>
                        out.writeEndObject()
                    case Right(b) =>
                        out.writeStartObject() >>
                        out.writeFieldName(rightField) >>
                        rightEncoder.run(b, out) >>
                        out.writeEndObject()
                }
        }

    /**
     * Decoder for `Either[A, B]` that operates like a implicit union of `A` and `B` with configurable left
     * and right field names.
     *
     * For example, `eitherJsonCoder[String, Int]` decodes from `{ "left": "foo" }` or `{ "right": 123 }`
     */
    def eitherJsonDecoder[A, B]
        (leftField: String, rightField: String)
        (implicit leftDecoder: JsonDecoder[A], rightDecoder: JsonDecoder[B]) =
        new JsonDecoder[Either[A, B]] {
            val mightBeNull = false
            val codesAsObject = true

            def run(in: InterchangeJsonParser, out: Receiver[Either[A, B]]) =
                in.requireValue >> {
                    in.currentToken match {
                        case JsonToken.VALUE_NULL => in.unexpectedMissingValue
                        case JsonToken.START_OBJECT =>
                            var seenField = false
                            in.foreachFields {
                                case `leftField`|`rightField` if seenField =>
                                    in.unexpectedToken(s"expected only one field named $leftField or $rightField")
                                case `leftField` =>
                                    seenField = true
                                    val rec = new Receiver[A]
                                    leftDecoder.run(in, rec) >> out(Left(rec.value))
                                case `rightField` =>
                                    seenField = true
                                    val rec = new Receiver[B]
                                    rightDecoder.run(in, rec) >> out(Right(rec.value))
                                case _ =>
                                    in.skipToEndOfValue()
                            } >> {
                                if (seenField) Okay.unit
                                else FailedG(s"expected an object with either $leftField or $rightField", in.terminal)
                            }
                        case other =>
                            in.unexpectedToken(s"start of object containing either $leftField or $rightField")
                    }
                }
        }

    /**
     * Coder for `ResultG[E, A]`.
     *
     * Encodes in different ways depending on whether the `Okay` value will encode as an object or not, whether the encoded value could be `null`,
     * and whether the `hideFailures` boolean is `true` or `false` (defaults to `false`)
     *
     * In the case where the `Okay` value might encode as `null`, encodes `Okay` as a single-element array containing the encoded value, e.g. `[123]`
     *
     * In the case where the `Okay` value encodes as an object and will not be `null`, encodes `Okay` as that object with an additional `result` field
     * added with the value `success` if no `result` field is already present.
     *
     * In the case where the Okay value doesn't encode as an object and will not be `null`, encodes `Okay` as the value itself, e.g. `123`.
     *
     * For `FailedG` if `hideFailures` is `true`, encodes as `null` or nothing.
     *
     * For `FailedG` if `hideFailures` is `false`, encodes as an object regardless of how the `Okay` value encodes:
     *
     *    {
     *        "result": "failed",               // always failed
     *        "errorCode": "system.error",      // always system.error
     *        "errorMessage": failed message,
     *        "param": encoded FailedG param value,
     *        "throwable": {                    // only if InsecureContext is false
     *            "isA": java class name,
     *            "message": throwable message,
     *            "cause": {throwable}
     *        }
     *    }
     */
    def resultGJsonCoder[E, A]
        (paramCoder: JsonCoder[E], valueCoder: JsonCoder[A])
        (implicit paramDefault: FailedParameterDefault[E], interchangeClassLoader: InterchangeClassLoader)
        : JsonCoder[ResultG[E, A]] =
        resultGJsonCoder[E, A](paramCoder.encode, valueCoder.encode, paramCoder.decode, valueCoder.decode, paramDefault, interchangeClassLoader)

    /** Holds a boolean indicating whether or not the current ResultG decoding/encoding should hide details of failures or not. */
    object ResultGHideFailures extends ThreadLocal[Boolean] {
        protected val initial = false
    }

    /**
     * Coder for `ResultG[E, A]`.
     *
     * Encodes in different ways depending on whether the `Okay` value will encode as an object or not, whether the encoded value could be `null`,
     * and whether the `ResultGHideFailures` boolean is `true` or `false` (defaults to `false`)
     *
     * In the case where the `Okay` value might encode as `null`, encodes `Okay` as a single-element array containing the encoded value, e.g. `[123]`
     *
     * In the case where the `Okay` value encodes as an object and will not be `null`, encodes `Okay` as that object with an additional `result` field
     * added with the value `success` if no `result` field is already present.
     *
     * In the case where the Okay value doesn't encode as an object and will not be `null`, encodes `Okay` as the value itself, e.g. `123`.
     *
     * For `FailedG` if `ResultGHideFailures` is `true`, encodes as `null` or nothing.
     *
     * For `FailedG` if `ResultGHideFailures` is `false`, encodes as an object regardless of how the `Okay` value encodes:
     *
     *    {
     *        "result": "failed",               // always failed
     *        "errorCode": "system.error",      // always system.error
     *        "errorMessage": failed message,
     *        "param": encoded FailedG param value,
     *        "throwable": {                    // only if InsecureContext is false
     *            "isA": java class name,
     *            "message": throwable message,
     *            "cause": {throwable}
     *        }
     *    }
     */
    implicit def resultGJsonCoder[E, A](
        implicit paramEncoder: JsonEncoder[E],
                 valueEncoder: JsonEncoder[A],
                 paramDecoder: JsonDecoder[E],
                 valueDecoder: JsonDecoder[A],
                 paramDefault: FailedParameterDefault[E],
                 interchangeClassLoader: InterchangeClassLoader
    ): JsonCoder[ResultG[E, A]] =
        JsonCoder.make(resultGJsonEncoder[E, A], resultGJsonDecoder[E, A])

    private trait ResultGJsonEncoder[E, A] extends JsonEncoder[ResultG[E, A]] {
        val mightBeNull = true
        val codesAsObject = false
        implicit val paramEncoder: JsonEncoder[E]
        implicit val valueEncoder: JsonEncoder[A]

        private def writeThrowable(in: Throwable, out: InterchangeJsonGenerator): CoderResult[Unit] =
            out.writeStartObject() >>
            out.writeFieldName("isA") >> out.writeString(in.getClass.getName) >>
            out.writeFieldName("message") >>
            {
                if (in.getMessage != null) out.writeString(in.getMessage)
                else out.writeString(in.toString)
            } >>
            {
                if (in.getCause == null) Okay.unit
                else out.writeFieldName("cause") >> writeThrowable(in.getCause, out)
            } >>
            out.writeEndObject()

        protected def writeFailedG(in: FailedG[E], out: InterchangeJsonGenerator) =
            out.writeStartObject() >>
            out.writeFieldName("errorCode") >> out.writeString("system.error") >>
            out.writeFieldName("errorMessage") >> out.writeString(in.message) >>
            out.writeFieldName("result") >> out.writeString("failed") >>
            out.writeFieldName("param") >> paramEncoder.run(in.parameter, out) >>
            {
                if (InsecureContext.get) Okay.unit
                else out.writeFieldName("throwable") >> writeThrowable(in.throwable, out)
            } >>
            out.writeEndObject()
    }

    /** Encoder for `ResultG[E, A]`. See `resultGJsonCoder` for details of encoding */
    def resultGJsonEncoder[E, A](implicit paramEncoder: JsonEncoder[E], valueEncoder: JsonEncoder[A]): JsonEncoder[ResultG[E, A]] = {
        // alias to avoid val paramEncoder = paramEncoder
        val _paramEncoder = paramEncoder
        val _valueEncoder = valueEncoder

        if (valueEncoder.mightBeNull) {
            new ResultGJsonEncoder[E, A] {
                implicit val paramEncoder = _paramEncoder
                implicit val valueEncoder = _valueEncoder

                def run(in: ResultG[E, A], out: InterchangeJsonGenerator) =
                    in match {
                        case Okay(a) =>
                            out.writeStartArray() >> { out.omitNextMissing(); valueEncoder.run(a, out) } >> out.writeEndArray()

                        case failed: FailedG[_] =>
                            if (ResultGHideFailures.get) out.writeNothingOrNull()
                            else writeFailedG(failed, out)
                    }
            }
        } else if (valueEncoder.codesAsObject) {
            new ResultGJsonEncoder[E, A] {
                implicit val paramEncoder = _paramEncoder
                implicit val valueEncoder = _valueEncoder

                def run(in: ResultG[E, A], out: InterchangeJsonGenerator) =
                    in match {
                        case Okay(a) =>
                            out.filterNextObject(new InterchangeJsonGenerator.ObjectFilter {
                                var seenFields = Set.empty[String]
                                override def fieldName(name: String) = {
                                    seenFields += name
                                    Okay.unit
                                }
                                override def end() = {
                                    if (seenFields.contains("result")) Okay.unit
                                    else out.writeFieldName("result") >> out.writeString("success")
                                }
                            })
                            valueEncoder.run(a, out)

                        case failed: FailedG[_] =>
                            if (ResultGHideFailures.get) out.writeNothingOrNull()
                            else writeFailedG(failed, out)
                    }
            }
         } else {
            new ResultGJsonEncoder[E, A] {
                implicit val paramEncoder = _paramEncoder
                implicit val valueEncoder = _valueEncoder

                def run(in: ResultG[E, A], out: InterchangeJsonGenerator) =
                    in match {
                        case Okay(a) =>
                            valueEncoder.run(a, out)

                        case failed: FailedG[_] =>
                            if (ResultGHideFailures.get) out.writeNothingOrNull()
                            else writeFailedG(failed, out)
                    }
            }
        }
    }

    trait ResultGJsonDecoder[E, A] extends JsonDecoder[ResultG[E, A]] {
        val mightBeNull = true
        val codesAsObject = false
        implicit val paramDefault: FailedParameterDefault[E]
        implicit val interchangeClassLoader: InterchangeClassLoader
        implicit val paramDecoder: JsonDecoder[E]
        implicit val valueDecoder: JsonDecoder[A]

        protected def unknownFailure: FailedG[E] = FailedG("unknown failure", paramDefault.default)

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
                case "cause"   => readThrowable(in, out)
                case _         => in.skipToEndOfValue()
            } >>
            {
                if (state.isA == null) FailedG("expecting \"isA\"", CoderFailure.terminal)
                else if (state.message == null) FailedG("expecting \"message\"", CoderFailure.terminal)
                else instantiateThrowable(state.isA, state.message, Option(out.value)).mapFailure(_ => CoderFailure.terminal) >>= out.apply
            }
        }

        protected final def readFailure(in: InterchangeJsonParser, out: Receiver[ResultG[E, A]]): CoderResult[Unit] =
            if (ResultGHideFailures.get) {
                in.skipToEndOfValue() >> out(FailedG("unknown cause", paramDefault.default))
            } else {
                class State {
                    var gotResult = false
                    var gotParam = false // can't be sure that null means no parameter read, since it could be nullable
                    var param = new Receiver[E]
                    var throwable = new Receiver[Throwable]
                    var message: String = null
                }

                val state = new State

                in.foreachFields {
                    case "result"       => if (in.stringValue == "failed") { state.gotResult = true; Okay.unit }
                                           else FailedG("expected result \"failed\" not \"" + in.stringValue + "\"", CoderFailure.terminal)
                    case "param"        => state.gotParam = true
                                           paramDecoder.run(in, state.param)
                    case "throwable"    => // don't read throwables in insecure contexts due to instantiating a class on demand
                                           if (InsecureContext.get) in.skipToEndOfValue()
                                           else readThrowable(in, state.throwable)
                    case "errorMessage" => state.message = in.stringValue
                                           Okay.unit
                    case _              => in.skipToEndOfValue()
                } >>
                {
                    if (!state.gotResult) FailedG("expecting \"result\": \"failed\" but found none", CoderFailure.terminal)
                    else if (!state.gotParam) FailedG("expecting \"param\"", CoderFailure.terminal)
                    else if (state.throwable.value == null && state.message == null) FailedG("missing \"throwable\" or \"errorMessage\"", CoderFailure.terminal)
                    else if (state.throwable.value == null)
                        out(FailedG(state.message, state.param.value))
                    else
                        out(FailedG(state.throwable.value, state.param.value))
                }
            }
    }

    /** Decoder for `ResultG[E, A]`. See `resultGJsonCoder` for details of encoding */
    def resultGJsonDecoder[E, A] (
        implicit paramDecoder: JsonDecoder[E],
                 valueDecoder: JsonDecoder[A],
                 paramDefault: FailedParameterDefault[E],
                 interchangeClassLoader: InterchangeClassLoader
    ): JsonDecoder[ResultG[E, A]] = {
        // alias to avoid val paramDecoder = paramDecoder
        val _paramDefault           = paramDefault
        val _interchangeClassLoader = interchangeClassLoader
        val _paramDecoder           = paramDecoder
        val _valueDecoder           = valueDecoder

        if (valueDecoder.mightBeNull) {
            new ResultGJsonDecoder[E, A] {
                implicit val paramDefault           = _paramDefault
                implicit val interchangeClassLoader = _interchangeClassLoader
                implicit val paramDecoder           = _paramDecoder
                implicit val valueDecoder           = _valueDecoder

                def run(in: InterchangeJsonParser, out: Receiver[ResultG[E, A]]) =
                    if (!in.hasValue || in.currentToken == JsonToken.VALUE_NULL)
                        out(unknownFailure)
                    else if (in.currentToken == JsonToken.START_ARRAY)
                        in.advanceTokenUnguarded() >>
                        {
                            if (in.currentToken == JsonToken.END_ARRAY) {
                                in.currentValueIsMissing()
                                val rec = new Receiver[A]

                                valueDecoder.run(in, rec) >> out(Okay(rec.value))
                            } else {
                                val rec = new Receiver[A]
                                valueDecoder.run(in, rec) >>
                                in.advanceTokenUnguarded() >>
                                {
                                    if (in.currentToken == JsonToken.END_ARRAY)
                                        out(Okay(rec.value))
                                    else
                                        in.unexpectedToken("empty array or array with exactly one element")
                                }
                            }
                        }
                    else if (in.currentToken == JsonToken.START_OBJECT)
                        readFailure(in, out)
                    else
                        in.unexpectedToken("start of object describing a failure, a single element array, or null")
            }
        } else if (valueDecoder.codesAsObject) {
            new ResultGJsonDecoder[E, A] {
                implicit val paramDefault           = _paramDefault
                implicit val interchangeClassLoader = _interchangeClassLoader
                implicit val paramDecoder           = _paramDecoder
                implicit val valueDecoder           = _valueDecoder

                def run(in: InterchangeJsonParser, out: Receiver[ResultG[E, A]]) =
                    if (!in.hasValue || in.currentToken == JsonToken.VALUE_NULL)
                        out(unknownFailure)
                    else if (in.currentToken == JsonToken.START_OBJECT)
                        in.peekFields(Array("result", "param", "errorCode", "errorMessage")) >>= {
                            case Array(Some("failed"), Some(_), Some(_), Some(_)) =>
                                readFailure(in, out)
                            case _ =>
                                val rec = new Receiver[A]
                                valueDecoder.run(in, rec) >> out(Okay(rec.value))
                        }
                    else
                        in.unexpectedToken("object or null")
            }
        } else {
            new ResultGJsonDecoder[E, A] {
                implicit val paramDefault           = _paramDefault
                implicit val interchangeClassLoader = _interchangeClassLoader
                implicit val paramDecoder           = _paramDecoder
                implicit val valueDecoder           = _valueDecoder

                def run(in: InterchangeJsonParser, out: Receiver[ResultG[E, A]]) =
                    if (!in.hasValue || in.currentToken == JsonToken.VALUE_NULL)
                        out(unknownFailure)
                    else if (in.currentToken == JsonToken.START_OBJECT)
                        readFailure(in, out)
                    else {
                        val rec = new Receiver[A]
                        valueDecoder.run(in, rec) >> out(Okay(rec.value))
                    }
            }
        }
    }

    /** Coder which will only successfully encode or decode when in a secure context */
    def secureOnlyJsonCoder[A](valueCoder: JsonCoder[A]): JsonCoder[A] =
        secureOnlyJsonCoder(valueCoder.encode, valueCoder.decode)

    /** Coder which will only successfully encode or decode when in a secure context */
    def secureOnlyJsonCoder[A](implicit valueEncoder: JsonEncoder[A], valueDecoder: JsonDecoder[A]): JsonCoder[A] =
        JsonCoder.make(insecureJsonEncoder[A], secureOnlyJsonDecoder[A])

    def secureOnlyJsonDecoder[A](implicit valueDecoder: JsonDecoder[A]) =
        new JsonDecoder[A] {
            val mightBeNull = valueDecoder.mightBeNull
            val codesAsObject = valueDecoder.codesAsObject

            def run(in: InterchangeJsonParser, out: Receiver[A]) =
                if (InsecureContext.get) FailedG("Invalid in insecure context", in.terminal)
                else valueDecoder.run(in, out)
        }

    /** Coder for values that should not be coded when in an insecure context and instead decode as a substitute value */
    def insecureJsonCoder[A](valueCoder: JsonCoder[A], substitute: A): JsonCoder[A] =
        insecureJsonCoder(substitute)(valueCoder.encode, valueCoder.decode)

    /** Coder for values that should not be coded when in an insecure context and instead decode as a substitute value */
    def insecureJsonCoder[A](substitute: A)(implicit valueEncoder: JsonEncoder[A], valueDecoder: JsonDecoder[A]): JsonCoder[A] =
        JsonCoder.make(insecureJsonEncoder[A], insecureJsonDecoder[A](substitute))

    /** Encoder for values that should not be encoded when in an insecure context */
    def insecureJsonEncoder[A](implicit valueEncoder: JsonEncoder[A]) =
        new JsonEncoder[A] {
            val mightBeNull = valueEncoder.mightBeNull
            val codesAsObject = valueEncoder.codesAsObject

            def run(in: A, out: InterchangeJsonGenerator) =
                if (InsecureContext.get) out.writeNothingOrNull()
                else valueEncoder.run(in, out)
        }

    /** Decoder for values that should not be decoded when in an insecure context and instead use a substitute value */
    def insecureJsonDecoder[A](substitute: A)(implicit valueDecoder: JsonDecoder[A]) =
        new JsonDecoder[A] {
            val mightBeNull = valueDecoder.mightBeNull
            val codesAsObject = valueDecoder.codesAsObject

            def run(in: InterchangeJsonParser, out: Receiver[A]) =
                if (InsecureContext.get) (if (in.hasValue) in.skipToEndOfValue() else Okay.unit) >> out(substitute)
                else valueDecoder.run(in, out)
        }

    /** Coder for `java.util.List`. Encodes as a JSON array */
    def javaListJsonCoder[E](elemCoder: JsonCoder[E]): JsonCoder[java.util.List[E]] =
        javaListJsonCoder(elemCoder.encode, elemCoder.decode)

    /** Coder for `java.util.List`. Encodes as a JSON array */
    implicit def javaListJsonCoder[E](implicit elemEncoder: JsonEncoder[E], elemDecoder: JsonDecoder[E]): JsonCoder[java.util.List[E]] =
        JsonCoder.make(javaListJsonEncoder[E], javaListJsonDecoder[E])

    /** Encoder for `java.util.List`. Encodes as a JSON array */
    def javaListJsonEncoder[E](implicit elemEncoder: JsonEncoder[E]): JsonEncoder[java.util.List[E]] =
        jsonArrayEncoder[E, java.util.List[E]](_.asScala, elemEncoder)

    /** Decoder for `java.util.List`. Decodes from a JSON array */
    def javaListJsonDecoder[E](implicit elemDecoder: JsonDecoder[E]): JsonDecoder[java.util.List[E]] =
        jsonArrayDecoder[E, java.util.List[E]](canBuildJavaList, elemDecoder)


     /** Coder for `java.util.List`. Encodes as a JSON array */
    def javaSetJsonCoder[E](elemCoder: JsonCoder[E]): JsonCoder[java.util.Set[E]] =
        javaSetJsonCoder(elemCoder.encode, elemCoder.decode)

    /** Coder for `java.util.Set`. Encodes as a JSON array */
    implicit def javaSetJsonCoder[E](implicit elemEncoder: JsonEncoder[E], elemDecoder: JsonDecoder[E]): JsonCoder[java.util.Set[E]] =
        JsonCoder.make(javaSetJsonEncoder[E], javaSetJsonDecoder[E])

    /** Encoder for `java.util.Set`. Encodes as a JSON array */
    def javaSetJsonEncoder[E](implicit elemEncoder: JsonEncoder[E]): JsonEncoder[java.util.Set[E]] =
        jsonArrayEncoder[E, java.util.Set[E]](_.asScala, elemEncoder)

    /** Decoder for `java.util.Set`. Decodes from a JSON array */
    def javaSetJsonDecoder[E](implicit elemDecoder: JsonDecoder[E]): JsonDecoder[java.util.Set[E]] =
        jsonArrayDecoder[E, java.util.Set[E]](canBuildJavaSet, elemDecoder)

    /** Coder for `java.util.Collection`. Encodes as a JSON array */
    def javaCollectionJsonCoder[E](elemCoder: JsonCoder[E]): JsonCoder[java.util.Collection[E]] =
        javaCollectionJsonCoder(elemCoder.encode, elemCoder.decode)

    /** Coder for `java.util.Collection`. Encodes as a JSON array */
    implicit def javaCollectionJsonCoder[E](implicit elemEncoder: JsonEncoder[E], elemDecoder: JsonDecoder[E]): JsonCoder[java.util.Collection[E]] =
        JsonCoder.make(javaCollectionJsonEncoder[E], javaCollectionJsonDecoder[E])

    /** Encoder for `java.util.Collection`. Encodes as a JSON array */
    def javaCollectionJsonEncoder[E](implicit elemEncoder: JsonEncoder[E]): JsonEncoder[java.util.Collection[E]] =
        jsonArrayEncoder[E, java.util.Collection[E]](_.asScala, elemEncoder)

    /** Decoder for `java.util.Collection`. Decodes from a JSON array */
    def javaCollectionJsonDecoder[E](implicit elemDecoder: JsonDecoder[E]): JsonDecoder[java.util.Collection[E]] =
        jsonArrayDecoder[E, java.util.Collection[E]](canBuildJavaSet, elemDecoder)

    /** Coder for `Iterable`. Encodes as a JSON array */
    def iterableJsonCoder[E](elemCoder: JsonCoder[E]): JsonCoder[Iterable[E]] =
        iterableJsonCoder(elemCoder.encode, elemCoder.decode)

    /** Coder for `Iterable`. Encodes as a JSON array */
    implicit def iterableJsonCoder[E](implicit elemEncoder: JsonEncoder[E], elemDecoder: JsonDecoder[E]): JsonCoder[Iterable[E]] =
        JsonCoder.make(iterableJsonEncoder[E], iterableJsonDecoder[E])

    /** Encoder for `Iterable`. Encodes as a JSON array */
    def iterableJsonEncoder[E](implicit elemEncoder: JsonEncoder[E]): JsonEncoder[Iterable[E]] =
        jsonArrayEncoder[E, Iterable[E]](_.iterable, elemEncoder)

    /** Decoder for `Iterable`. Decodes from a JSON array */
    def iterableJsonDecoder[E](implicit elemDecoder: JsonDecoder[E]): JsonDecoder[Iterable[E]] =
        jsonArrayDecoder[E, Iterable[E]](canBuildScalaList, elemDecoder)

    /** Coder for `List`. Encodes as a JSON array */
    def listJsonCoder[E](elemCoder: JsonCoder[E]): JsonCoder[List[E]] =
        listJsonCoder(elemCoder.encode, elemCoder.decode)

    /** Coder for `List`. Encodes as a JSON array */
    implicit def listJsonCoder[E](implicit elemEncoder: JsonEncoder[E], elemDecoder: JsonDecoder[E]): JsonCoder[List[E]] =
        JsonCoder.make(listJsonEncoder[E], listJsonDecoder[E])

    /** Encoder for `List`. Encodes as a JSON array */
    def listJsonEncoder[E](implicit elemEncoder: JsonEncoder[E]): JsonEncoder[List[E]] =
        jsonArrayEncoder[E, List[E]](_.toList, elemEncoder)

    /** Decoder for `List`. Decodes from a JSON array */
    def listJsonDecoder[E](implicit elemDecoder: JsonDecoder[E]): JsonDecoder[List[E]] =
        jsonArrayDecoder[E, List[E]](canBuildScalaList, elemDecoder)

    // FIXME? map encoding allocates 2-tuples for each element as it iterates

    /** Coder for any map with keys that can be coded as strings, expressed as a collection of key/value pairs. Encodes as a JSON object */
    implicit def jsonObjectCoder[K, V, M]
        (keyCoder: StringCoder[K], valueCoder: JsonCoder[V])
        (implicit asIterable: M => Iterable[(K, V)], canBuildFrom: CanBuildFrom[Nothing, (K, V), M])
        : JsonCoder[M] =
        jsonObjectCoder(asIterable, canBuildFrom, keyCoder.encode, valueCoder.encode, keyCoder.decode, valueCoder.decode)

    /** Coder for any map with keys that can be coded as strings, expressed as a collection of key/value pairs. Encodes as a JSON object */
    implicit def jsonObjectCoder[K, V, M] (
        implicit asIterable:   M => Iterable[(K, V)],
                 canBuildFrom: CanBuildFrom[Nothing, (K, V), M],
                 keyEncoder:   StringEncoder[K],
                 valueEncoder: JsonEncoder[V],
                 keyDecoder:   StringDecoder[K],
                 valueDecoder: JsonDecoder[V]
    ): JsonCoder[M] =
        JsonCoder.make(jsonObjectEncoder(asIterable, keyEncoder, valueEncoder), jsonObjectDecoder(canBuildFrom, keyDecoder, valueDecoder))

    /** Encoder for any map with keys that can be coded as strings, expressed as a collection of key/value pairs. Encodes as a JSON object */
    def jsonObjectEncoder[K, V, M](implicit asIterable: M => Iterable[(K, V)], keyEncoder: StringEncoder[K], valueEncoder: JsonEncoder[V]) =
        new JsonEncoder[M] {
            val mightBeNull = false
            val codesAsObject = true

            def run(in: M, out: InterchangeJsonGenerator) = {
                val keyReceiver = new Receiver[String]

                out.writeStartObject() >>
                asIterable(in).foreachResult { case (k, v) =>
                    keyEncoder.run(k, keyReceiver) >>
                    out.writeFieldName(keyReceiver.value) >>
                    valueEncoder.run(v, out)
                } >>
                out.writeEndObject()
            }
        }

    /** Decoder for any map with keys that can be coded as strings, expressed as a collection of key/value pairs. Encodes as a JSON object */
    def jsonObjectDecoder[K, V, M](implicit canBuildFrom: CanBuildFrom[Nothing, (K, V), M], keyDecoder: StringDecoder[K], valueDecoder: JsonDecoder[V]) =
        new JsonDecoder[M] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[M]) = {
                val builder = canBuildFrom()
                val keyReceiver = new Receiver[K]
                val valueReceiver = new Receiver[V]

                in.require(JsonToken.START_OBJECT) >>
                in.foreachFields { field =>
                    keyDecoder.run(field, keyReceiver) >>
                    valueDecoder.run(in, valueReceiver) >> {
                        builder += ((keyReceiver.value, valueReceiver.value))
                        Okay.unit
                    }
                } >>
                out(builder.result())
            }
        }

    /** Coder for `java.util.Map` with keys that can be coded as strings. Encodes as a JSON object */
    def javaStringKeyedMapJsonCoder[K, V](keyCoder: StringCoder[K], valueCoder: JsonCoder[V]): JsonCoder[java.util.Map[K, V]] =
        javaStringKeyedMapJsonCoder(keyCoder.encode, valueCoder.encode, keyCoder.decode, valueCoder.decode)

    /** Coder for `java.util.Map` with keys that can be coded as strings. Encodes as a JSON object */
    implicit def javaStringKeyedMapJsonCoder[K, V] (
        implicit keyEncoder: StringEncoder[K],
                 valueEncoder: JsonEncoder[V],
                 keyDecoder: StringDecoder[K],
                 valueDecoder: JsonDecoder[V]
    ): JsonCoder[java.util.Map[K, V]] =
        JsonCoder.make(javaStringKeyedMapJsonEncoder(keyEncoder, valueEncoder), javaStringKeyedMapJsonDecoder(keyDecoder, valueDecoder))

    /** Encoder for `java.util.Map` with keys that can be coded as strings. Encodes as a JSON object */
    def javaStringKeyedMapJsonEncoder[K, V](implicit keyEncoder: StringEncoder[K], valueEncoder: JsonEncoder[V]) =
        jsonObjectEncoder[K, V, java.util.Map[K, V]](_.asScala, keyEncoder, valueEncoder)

    /** Decoder for `java.util.Map` with keys that can be coded as strings. Decodes from a JSON object */
    def javaStringKeyedMapJsonDecoder[K, V](implicit keyDecoder: StringDecoder[K], valueDecoder: JsonDecoder[V]) =
        jsonObjectDecoder[K, V, java.util.Map[K, V]](canBuildJavaMap, keyDecoder, valueDecoder)
}

trait containerLPI extends containerLPI2 {
    /** Coder for map-like collections where the key is complex and cannot be coded as a String. Encodes as a JSON array of `{"key":…, "value":…}` objects */
    def jsonAssocArrayCoder[K, V, M]
        (keyCoder: JsonCoder[K], valueCoder: JsonCoder[V])
        (implicit asIterable: M => Iterable[(K, V)], canBuildFrom: CanBuildFrom[Nothing, (K, V), M])
        : JsonCoder[M] =
        jsonAssocArrayCoder(asIterable, canBuildFrom, keyCoder.encode, valueCoder.encode, keyCoder.decode, valueCoder.decode)

    /** Coder for map-like collections where the key is complex and cannot be coded as a String. Encodes as a JSON array of `{"key":…, "value":…}` objects */
    implicit def jsonAssocArrayCoder[K, V, M] (
        implicit asIterable:   M => Iterable[(K, V)],
                 canBuildFrom: CanBuildFrom[Nothing, (K, V), M],
                 keyEncoder:   JsonEncoder[K],
                 valueEncoder: JsonEncoder[V],
                 keyDecoder:   JsonDecoder[K],
                 valueDecoder: JsonDecoder[V]
    ): JsonCoder[M] =
        JsonCoder.make(jsonAssocArrayEncoder(asIterable, keyEncoder, valueEncoder), jsonAssocArrayDecoder(canBuildFrom, keyDecoder, valueDecoder))

    /** Encoder for map-like collections where the key is complex and cannot be encoded as a String. Encodes as a JSON array of `{"key":…, "value":…}` objects */
    def jsonAssocArrayEncoder[K, V, M](implicit asIterable: M => Iterable[(K, V)], keyEncoder: JsonEncoder[K], valueEncoder: JsonEncoder[V]) =
        new JsonEncoder[M] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: M, out: InterchangeJsonGenerator) =
                out.writeStartArray() >>
                asIterable(in).foreachResult { case (k, v) =>
                    out.writeStartObject() >>
                    out.writeFieldNameNotMissing("key") >> keyEncoder.run(k, out) >>
                    out.writeFieldNameNotMissing("value") >> valueEncoder.run(v, out) >>
                    out.writeEndObject()
                } >>
                out.writeEndArray()
        }

    /** Decoder for map-like collections where the key is complex and cannot be decoded from a String. Decodes from a JSON array of pairs */
    def jsonAssocArrayDecoder[K, V, M](implicit canBuildFrom: CanBuildFrom[Nothing, (K, V), M], keyDecoder: JsonDecoder[K], valueDecoder: JsonDecoder[V]) =
        new JsonDecoder[M] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[M]) =
                in.requireValue >> {
                    val builder = canBuildFrom()
                    val keyReceiver = new Receiver[K]
                    val valueReceiver = new Receiver[V]

                    @tailrec
                    def iterate(): CoderResult[Unit] = {
                        var done = false
                        val res = in.advanceTokenUnguarded() >> {
                            in.currentToken match {
                                case JsonToken.END_ARRAY => done = true; Okay.unit
                                case JsonToken.START_OBJECT =>
                                    var seenKey = false
                                    var seenValue = false

                                    in.foreachFields {
                                        case "key"   => seenKey = true; keyDecoder.run(in, keyReceiver)
                                        case "value" => seenValue = true; valueDecoder.run(in, valueReceiver)
                                        case _ => in.skipToEndOfValue()
                                    } >> {
                                        if (!seenKey) FailedG("missing \"key\" field for associative array", in.terminal)
                                        else if (!seenValue) FailedG("missing \"value\" field for associative array", in.terminal)
                                        else {
                                            builder += ((keyReceiver.value, valueReceiver.value))
                                            Okay.unit
                                        }
                                    }
                                case _ =>
                                    in.unexpectedToken("end of array or start of object")
                            }
                        }

                        res match {
                            case _: Okay[_] if done => Okay.unit
                            case _: Okay[_]         => iterate()
                            case failed: FailedG[_] => failed
                        }
                    }

                    in.require(JsonToken.START_ARRAY) >> iterate() >> out(builder.result())
                }
        }

    /** Coder for `java.util.Map` with keys that cannot be coded as Strings. Encodes as a JSON array of `{"key":…, "value":…}` objects */
    def javaMapJsonCoder[K, V](keyCoder: JsonCoder[K], valueCoder: JsonCoder[V]): JsonCoder[java.util.Map[K, V]] =
        javaMapJsonCoder(keyCoder.encode, valueCoder.encode, keyCoder.decode, valueCoder.decode)

    /** Coder for `java.util.Map` with keys that cannot be coded as Strings. Encodes as a JSON array of `{"key":…, "value":…}` objects */
    implicit def javaMapJsonCoder[K, V] (
        implicit keyEncoder: JsonEncoder[K],
                 valueEncoder: JsonEncoder[V],
                 keyDecoder: JsonDecoder[K],
                 valueDecoder: JsonDecoder[V]
    ): JsonCoder[java.util.Map[K, V]] =
        JsonCoder.make(javaMapJsonEncoder(keyEncoder, valueEncoder), javaMapJsonDecoder(keyDecoder, valueDecoder))

    /** Encoder for `java.util.Map` with keys that cannot be encoded as Strings. Encodes as a JSON array of `{"key":…, "value":…}` objects */
    def javaMapJsonEncoder[K, V](implicit keyEncoder: JsonEncoder[K], valueEncoder: JsonEncoder[V]) =
        jsonAssocArrayEncoder[K, V, java.util.Map[K, V]](_.asScala, keyEncoder, valueEncoder)

    /** Decoder for `java.util.Map` with keys that cannot be decoded from Strings. Decodes from a JSON array of `{"key":…, "value":…}` objects */
    def javaMapJsonDecoder[K, V](implicit keyDecoder: JsonDecoder[K], valueDecoder: JsonDecoder[V]) =
        jsonAssocArrayDecoder[K, V, java.util.Map[K, V]](canBuildJavaMap, keyDecoder, valueDecoder)
}

trait containerLPI2 {
    /** Coder for sequence type `S` comprised of element `E` coding as a JSON array */
    def jsonArrayCoder[E, S]
        (elemCoder: JsonCoder[E])
        (implicit asIterable: S => Iterable[E], canBuildFrom: CanBuildFrom[Nothing, E, S])
        : JsonCoder[S] =
        jsonArrayCoder(asIterable, canBuildFrom, elemCoder.encode, elemCoder.decode)

    /** Coder for sequence type `S` comprised of element `E` coding as a JSON array */
    implicit def jsonArrayCoder[E, S] (
        implicit asIterable: S => Iterable[E],
                 canBuildFrom: CanBuildFrom[Nothing, E, S],
                 elemEncoder: JsonEncoder[E],
                 elemDecoder: JsonDecoder[E]
     ): JsonCoder[S] =
        JsonCoder.make(jsonArrayEncoder(asIterable, elemEncoder), jsonArrayDecoder(canBuildFrom, elemDecoder))

    /** Encoder for sequence type `S` comprised of element `E` encoding as a JSON array */
    def jsonArrayEncoder[E, S](implicit asIterable: S => Iterable[E], elemEncoder: JsonEncoder[E]) =
        new JsonEncoder[S] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: S, out: InterchangeJsonGenerator) =
                out.writeStartArray() >>
                asIterable(in).foreachResult { e =>
                    elemEncoder.run(e, out)
                } >>
                out.writeEndArray()
        }

    /** Decoder for sequence type `S` comprised of element `E` decoding from a JSON array */
    def jsonArrayDecoder[E, S](implicit canBuildFrom: CanBuildFrom[Nothing, E, S], elemDecoder: JsonDecoder[E]) =
        new JsonDecoder[S] {
            val mightBeNull = false
            val codesAsObject = false

            def run(in: InterchangeJsonParser, out: Receiver[S]) =
                in.requireValue >> {
                    val builder = canBuildFrom()
                    val receiver = new Receiver[E]

                    @tailrec
                    def iterate(): CoderResult[Unit] = {
                        var done = false
                        val res = in.advanceTokenUnguarded() >> {
                            in.currentToken match {
                                case JsonToken.END_ARRAY => done = true; Okay.unit
                                case _ =>
                                    elemDecoder.run(in, receiver) >> {
                                        builder += receiver.value
                                        Okay.unit
                                    }
                            }
                        }

                        res match {
                            case _: Okay[_] if done => Okay.unit
                            case _: Okay[_]         => iterate()
                            case failed: FailedG[_] => failed
                        }
                    }

                    in.require(JsonToken.START_ARRAY) >> iterate() >> out(builder.result())
                }
        }
}
