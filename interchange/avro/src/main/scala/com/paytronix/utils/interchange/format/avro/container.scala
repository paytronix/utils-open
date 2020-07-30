//
// Copyright 2014-2020 Paytronix Systems, Inc.
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
import scala.annotation.tailrec
import scala.collection.JavaConverters.{asScalaBufferConverter, mapAsScalaMapConverter}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

import org.apache.avro.{Schema, io}
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}
import scalaz.syntax.apply.^

import com.paytronix.utils.interchange.base.{CoderFailure, CoderResult, InterchangeClassLoader, InsecureContext, Receiver, atIndex, atProperty, atTerminal, terminal}
import com.paytronix.utils.interchange.base.container.javaCollections.{canBuildJavaList, canBuildJavaMap, canBuildJavaSortedMap}
import com.paytronix.utils.interchange.base.container.result.instantiateThrowable
import com.paytronix.utils.interchange.format.string.{StringCoder, StringDecoder, StringEncoder}
import com.paytronix.utils.scala.result.{FailedG, FailedParameterDefault, Okay, ResultG, iterableResultOps, tryCatchValueG, tryCatchResultG}

import utils.{encodeSchemaName, makeField, nullable}

object container extends container

trait container extends containerLPI {

    // FIXME: a known bug with nullableAvroCoder is that you can't use it with a union type, e.g. @nullable X where X is a union blows up at runtime

    /**
     * Wrapper coding for nullable values. Encodes as a union between Avro null and the actual type.
     * <strong>Warning:</strong> that this doesn't work for union types on account of Avro's limitation on not having nested unions.
     */
    def nullableAvroCoder[A >: Null](implicit valueCoder: AvroCoder[A]): AvroCoder[A] =
        nullableAvroCoder(valueCoder.encode, valueCoder.decode)

    /**
     * Wrapper coding for nullable values. Encodes as a union between Avro null and the actual type.
     * <strong>Warning:</strong> that this doesn't work for union types on account of Avro's limitation on not having nested unions.
     */
    def nullableAvroCoder[A >: Null](implicit valueEncoder: AvroEncoder[A], valueDecoder: AvroDecoder[A]): AvroCoder[A] =
        AvroCoder.make(nullableAvroEncoder(valueEncoder), nullableAvroDecoder(valueDecoder))

    /** Wrapper encoder for nullable values. Encodes `null` as an Avro `null`, uses the wrapped encoder otherwise */
    def nullableAvroEncoder[A >: Null](implicit valueEncoder: AvroEncoder[A]) = new AvroEncoder[A] {
        val schema = nullable(valueEncoder.schema)
        val defaultJson = Some(jsonNodeFactory.nullNode)

        def encodeDefaultJson(in: A) =
            if (in == null) Okay(jsonNodeFactory.nullNode)
            else FailedG("nullable fields must default to null due to Avro restrictions", CoderFailure.terminal)

        def run(in: A, out: io.Encoder) =
            tryCatchResultG(terminal) {
                if (in == null) {
                    out.writeIndex(0)
                    out.writeNull()
                    Okay.unit
                } else {
                    out.writeIndex(1)
                    valueEncoder.run(in, out)
                }
            }
    }

    /** Wrapper decoder for nullable values. Decodes `null` and passes through to the wrapped coder otherwise */
    def nullableAvroDecoder[A >: Null](implicit valueDecoder: AvroDecoder[A]) = new AvroDecoder[A] {
        val schema = nullable(valueDecoder.schema)
        val defaultJson = Some(jsonNodeFactory.nullNode)

        def run(in: io.ResolvingDecoder, out: Receiver[A]) =
            tryCatchResultG(terminal) {
                in.readIndex() match {
                    case 0 =>
                        in.readNull()
                        out(null)
                    case 1 =>
                        valueDecoder.run(in, out)
                }
            }
    }

    /** Coder for `Option[A]`. Encodes as a union of Avro null and a `Some` structure. */
    def optionAvroCoder[A](valueCoder: AvroCoder[A]): AvroCoder[Option[A]] =
        optionAvroCoder(valueCoder.encode, valueCoder.decode)

    /** Coder for `Option[A]`. Encodes as a union of Avro null and a `Some` structure. */
    implicit def optionAvroCoder[A](implicit valueEncoder: AvroEncoder[A], valueDecoder: AvroDecoder[A]): AvroCoder[Option[A]] =
        AvroCoder.make(optionAvroEncoder(valueEncoder), optionAvroDecoder(valueDecoder))

    private def optionSchema(valueSchema: Schema, valueDefault: Option[JsonNode]): Schema =
        utils.nullable {
            if (valueSchema.getType != Schema.Type.UNION) valueSchema
            else {
                val someSchema = Schema.createRecord("Some__" + encodeSchemaName(valueSchema), "", "scala", false)
                someSchema.setFields(Arrays.asList(makeField("value", valueSchema, valueDefault)))
                someSchema
            }
        }

    /** Encoder for `Option[A]`. Encodes as a union of Avro null and a `Some` structure. */
    def optionAvroEncoder[A](implicit valueEncoder: AvroEncoder[A]) = new AvroEncoder[Option[A]] {
        val schema = optionSchema(valueEncoder.schema, valueEncoder.defaultJson)
        val defaultJson = Some(jsonNodeFactory.nullNode)

        def encodeDefaultJson(in: Option[A]) =
            in match {
                case None => Okay(jsonNodeFactory.nullNode)
                case Some(a) => FailedG("Options must default to None due to Avro restrictions", CoderFailure.terminal)
            }

        def run(in: Option[A], out: io.Encoder) =
            tryCatchResultG(terminal) {
                in match {
                    case None =>
                        out.writeIndex(0)
                        out.writeNull()
                        Okay.unit
                    case Some(a) =>
                        out.writeIndex(1)
                        valueEncoder.run(a, out)
                }
            }
    }

    /** Decoder for `Option[A]`. Decodes from a union of Avro null and a `Some` structure. */
    def optionAvroDecoder[A](implicit valueDecoder: AvroDecoder[A]) = new AvroDecoder[Option[A]] {
        val schema = optionSchema(valueDecoder.schema, valueDecoder.defaultJson)
        val defaultJson = Some(jsonNodeFactory.nullNode)

        def run(in: io.ResolvingDecoder, out: Receiver[Option[A]]) =
            tryCatchResultG(terminal) {
                in.readIndex() match {
                    case 0 =>
                        in.readNull()
                        out(None)
                    case 1 =>
                        if (valueDecoder.schema.getType == Schema.Type.UNION) in.readFieldOrder()
                        val r = new Receiver[A]
                        valueDecoder.run(in, r) >> {
                            out(Some(r.value))
                        }
                }
            }
    }

    /** Coder for `Either[A, B]`. Encodes as a union of a `Left` record and `Right` record. */
    def eitherAvroCoder[A, B](leftCoder: AvroCoder[A], rightCoder: AvroCoder[B]): AvroCoder[Either[A, B]] =
        eitherAvroCoder(leftCoder.encode, rightCoder.encode, leftCoder.decode, rightCoder.decode)

    /** Coder for `Either[A, B]`. Encodes as a union of a `Left` record and `Right` record. */
    implicit def eitherAvroCoder[A, B] (
        implicit leftEncoder: AvroEncoder[A],
                 rightEncoder: AvroEncoder[B],
                 leftDecoder: AvroDecoder[A],
                 rightDecoder: AvroDecoder[B]
    ): AvroCoder[Either[A, B]] =
        AvroCoder.make(eitherAvroEncoder(leftEncoder, rightEncoder), eitherAvroDecoder(leftDecoder, rightDecoder))

    private def eitherSchema(leftSchema: Schema, leftDefault: Option[JsonNode], rightSchema: Schema, rightDefault: Option[JsonNode]): Schema = {
        val l = Schema.createRecord("Left__" + encodeSchemaName(leftSchema), "", "scala", false)
        l.setFields(Arrays.asList(makeField("value", leftSchema, leftDefault)))

        val r = Schema.createRecord("Right__" + encodeSchemaName(rightSchema), "", "scala", false)
        r.setFields(Arrays.asList(makeField("value", rightSchema, rightDefault)))

        Schema.createUnion(Arrays.asList(l, r))
    }


    /** Encoder for `Either[A, B]`. Encodes as a union of a `Left` record and `Right` record. */
    def eitherAvroEncoder[A, B](implicit leftEncoder: AvroEncoder[A], rightEncoder: AvroEncoder[B]) = new AvroEncoder[Either[A, B]] {
        val schema = eitherSchema(leftEncoder.schema, leftEncoder.defaultJson, rightEncoder.schema, rightEncoder.defaultJson)
        val defaultJson = None

        def encodeDefaultJson(in: Either[A, B]) =
            in match {
                case Left(value) =>
                    leftEncoder.encodeDefaultJson(value) >>= { json =>
                        val obj = jsonNodeFactory.objectNode
                        obj.put("value", json)
                        Okay(obj)
                    }
                case Right(_) =>
                    FailedG("can't encode a default value for Avro that uses Right, since Avro doesn't allow union defaults using anything but the first branch", CoderFailure.terminal)
            }

        def run(in: Either[A, B], out: io.Encoder) =
            in match {
                case Left(value) =>
                    tryCatchResultG(terminal) {
                        out.writeIndex(0)
                        leftEncoder.run(value, out)
                    }

                case Right(value) =>
                    tryCatchResultG(terminal) {
                        out.writeIndex(1)
                        rightEncoder.run(value, out)
                    }
            }
    }

    /** Decoder for `Either[A, B]`. Decodes a union of a `Left` record and `Right` record. */
    def eitherAvroDecoder[A, B](implicit leftDecoder: AvroDecoder[A], rightDecoder: AvroDecoder[B]) = new AvroDecoder[Either[A, B]] {
        val schema = eitherSchema(leftDecoder.schema, leftDecoder.defaultJson, rightDecoder.schema, rightDecoder.defaultJson)
        val defaultJson = None

        def run(in: io.ResolvingDecoder, out: Receiver[Either[A, B]]) =
            tryCatchResultG(terminal) {
                in.readIndex() match {
                    case 0     => val r = new Receiver[A]; leftDecoder.run(in, r) >> { out(Left(r.value)) }
                    case 1     => val r = new Receiver[B]; rightDecoder.run(in, r) >> { out(Right(r.value)) }
                    case other => FailedG("read unknown union index " + other + " from Avro for Either", CoderFailure.terminal)
                }
            }
    }

    private val throwableSchema: Schema = {
        val s = Schema.createRecord("Throwable", "", "com.paytronix.utils.scala.result", false)
        s.setFields(Arrays.asList (
            makeField("isA", Schema.create(Schema.Type.STRING), None, ""),
            makeField("message", nullable(Schema.create(Schema.Type.STRING)), None, ""),
            makeField("cause", nullable(s), Some(jsonNodeFactory.nullNode), "")
        ))
        s
    }

    private val missingSchema = {
        val s = Schema.createRecord("__Missing", "", "com.paytronix.utils.scala.result", false)
        s.setFields(Arrays.asList())
        s
    }

    private def okaySchema(value: Schema, valueDefault: Option[JsonNode]): Schema = {
        val s = Schema.createRecord("Okay__" + encodeSchemaName(value), "", "com.paytronix.utils.scala.result", false)
        s.setFields(Arrays.asList(makeField("value", value, valueDefault, "")))
        s
    }

    private def failedSchema(param: Schema, paramDefault: Option[JsonNode]): Schema = {
        val s = Schema.createRecord (
            if (param.getType == Schema.Type.NULL) "Failed"
            else "FailedG__" + encodeSchemaName(param),
            "", "com.paytronix.utils.scala.result", false
        )

        s.setFields(Arrays.asList (
            makeField("throwable", throwableSchema, None, ""),
            makeField("param", param, paramDefault, "")
        ))

        s
    }

    private def resultGSchema (
        param: Schema, paramDefault: Option[JsonNode],
        value: Schema, valueDefault: Option[JsonNode]
    ): Schema =
        Schema.createUnion(Arrays.asList(missingSchema, okaySchema(value, valueDefault), failedSchema(param, paramDefault)))

    /** Coder for `ResultG[E, A]`. Encodes as a union of [missing, failed, okay] with each a structure */
    def resultGAvroCoder[E, A]
        (paramCoder: AvroCoder[E], valueCoder: AvroCoder[A])
        (implicit paramDefault: FailedParameterDefault[E], interchangeClassLoader: InterchangeClassLoader)
        : AvroCoder[ResultG[E, A]] =
        resultGAvroCoder(paramCoder.encode, valueCoder.encode, paramCoder.decode, valueCoder.decode, paramDefault, interchangeClassLoader)

    /** Coder for `ResultG[E, A]`. Encodes as a union of [missing, failed, okay] with each a structure */
    implicit def resultGAvroCoder[E, A] (
        implicit paramEncoder: AvroEncoder[E],
                 valueEncoder: AvroEncoder[A],
                 paramDecoder: AvroDecoder[E],
                 valueDecoder: AvroDecoder[A],
                 paramDefault: FailedParameterDefault[E],
                 interchangeClassLoader: InterchangeClassLoader
    ): AvroCoder[ResultG[E, A]] =
        AvroCoder.make(resultGAvroEncoder[E, A], resultGAvroDecoder[E, A])

    /** Encoder for `ResultG[E, A]`. Encodes as a union of [missing, failed, okay] with each a structure */
    def resultGAvroEncoder[E, A] (
        implicit paramEncoder: AvroEncoder[E],
                 valueEncoder: AvroEncoder[A]
    ) = new AvroEncoder[ResultG[E, A]] {
        val schema = resultGSchema (
            paramEncoder.schema, paramEncoder.defaultJson,
            valueEncoder.schema, valueEncoder.defaultJson
        )
        val defaultJson = Some(jsonNodeFactory.objectNode)

        def encodeDefaultJson(in: ResultG[E, A]) =
            in match {
                case Okay(_) =>
                    FailedG("cannot have an Avro default of Okay(...) for ResultG as Avro only supports defaulting to the first branch of a union", CoderFailure.terminal)

                case _ =>
                    Okay(jsonNodeFactory.objectNode)
            }

        def run(in: ResultG[E, A], out: io.Encoder) = {
            def encodeFailed(in: FailedG[E]): CoderResult[Unit] =
                encodeThrowable(in.throwable) >> paramEncoder.run(in.parameter, out)

            def encodeThrowable(in: Throwable): CoderResult[Unit] =
                tryCatchResultG(terminal) {
                    out.writeString(in.getClass.getName)
                    in.getMessage match {
                        case null    => out.writeIndex(0); out.writeNull()
                        case message => out.writeIndex(1); out.writeString(message)
                    }
                    in.getCause match {
                        case null =>
                            out.writeIndex(0)
                            out.writeNull()
                            Okay.unit

                        case cause =>
                            out.writeIndex(1)
                            encodeThrowable(cause)
                    }
                }

            tryCatchResultG(terminal) {
                in match {
                    case Okay(value) =>
                        out.writeIndex(1)
                        valueEncoder.run(value, out)

                    case failed: FailedG[_] =>
                        out.writeIndex(2)
                        encodeFailed(failed)
                }
            }
        }
    }

    /** Decoder for `ResultG[E, A]`. Decodes from a union of [missing, failed, okay] with each a structure */
    def resultGAvroDecoder[E, A] (
        implicit paramDecoder: AvroDecoder[E],
                 valueDecoder: AvroDecoder[A],
                 paramDefault: FailedParameterDefault[E],
                 interchangeClassLoader: InterchangeClassLoader
    ) = new AvroDecoder[ResultG[E, A]] {
        val schema = resultGSchema (
            paramDecoder.schema, paramDecoder.defaultJson,
            valueDecoder.schema, valueDecoder.defaultJson
        )
        val defaultJson = Some(jsonNodeFactory.nullNode)

        def run(in: io.ResolvingDecoder, out: Receiver[ResultG[E, A]]) = {
            def decodeFailed(): CoderResult[FailedG[E]] =
                decodeThrowable() >>= { throwable =>
                    val receiver = new Receiver[E]
                    paramDecoder.run(in, receiver) map { _ => FailedG(throwable, receiver.value) }
                }

            def decodeThrowable(): CoderResult[Throwable] =
                for {
                    isA      <- atProperty("isA")(tryCatchValueG(terminal)(in.readString(null).toString))
                    message  <- atProperty("message") {
                        tryCatchValueG(terminal) {
                            in.readIndex() match {
                                case 0 => in.readNull(); null
                                case 1 => in.readString(null).toString
                            }
                        }
                    }
                    causeOpt <- atProperty("cause") {
                        tryCatchResultG(terminal) {
                            in.readIndex() match {
                                case 0 => in.readNull(); Okay(None)
                                case 1 => decodeThrowable() map Some.apply
                                case other => FailedG("read unknown union index " + other + " from Avro for cause of throwable", CoderFailure.terminal)
                            }
                        }
                    }

                    instance <- atTerminal(instantiateThrowable(isA, message, causeOpt))
                } yield instance

            tryCatchResultG(terminal) {
                in.readIndex() match {
                    case 0     => out(FailedG("unknown failure", paramDefault.default))
                    case 1     => val receiver = new Receiver[A]
                                  valueDecoder.run(in, receiver) >> { out(Okay(receiver.value)) }
                    case 2     => decodeFailed() >>= out.apply
                    case other => FailedG("read unknown union index " + other + " from Avro for ResultG", CoderFailure.terminal)
                }
            }
        }
    }

    /** Coder for values that should not be coded when in an insecure context and instead encode/decode as a substitute value */
    def insecureAvroCoder[A](valueCoder: AvroCoder[A], substitute: A): AvroCoder[A] =
        insecureAvroCoder(substitute)(valueCoder.encode, valueCoder.decode)

    /** Coder for values that should not be coded when in an insecure context and instead encode/decode as a substitute value */
    def insecureAvroCoder[A](substitute: A)(implicit valueEncoder: AvroEncoder[A], valueDecoder: AvroDecoder[A]): AvroCoder[A] =
        AvroCoder.make(insecureAvroEncoder(substitute), insecureAvroDecoder(substitute))

    /** Encoder for values that should not be encoded when in an insecure context and instead encode a substitute value */
    def insecureAvroEncoder[A](substitute: A)(implicit valueEncoder: AvroEncoder[A]) =
        new AvroEncoder[A] {
            val schema = valueEncoder.schema
            val defaultJson = valueEncoder.defaultJson

            def encodeDefaultJson(a: A) =
                valueEncoder.encodeDefaultJson(a)

            def run(in: A, out: io.Encoder) =
                valueEncoder.run(if (InsecureContext.get) substitute else in, out)
        }

    /** Decoder for values that should not be decoded when in an insecure context and instead use a substitute value */
    def insecureAvroDecoder[A](substitute: A)(implicit valueDecoder: AvroDecoder[A]) =
        new AvroDecoder[A] {
            val schema = valueDecoder.schema
            val defaultJson = valueDecoder.defaultJson

            def run(in: io.ResolvingDecoder, out: Receiver[A]) =
                // ideally and for most safety you wouldn't even run the value coder in an insecure context, but we need to do drive the
                // Avro ResolvingDecoder since the schema still specifies. this is perhaps not ideal, but otherwise we'd have to generate
                // different schemas for insecure and secure contexts which is out of scope right now
                try valueDecoder.run(in, out) finally {
                    if (InsecureContext.get) {
                        val _ = out(substitute)
                    } else ()
                }
        }

    /** Coder for `java.util.List`. Encodes as an Avro array */
    def javaListAvroCoder[E](elemCoder: AvroCoder[E]): AvroCoder[java.util.List[E]] =
        javaListAvroCoder(elemCoder.encode, elemCoder.decode)

    /** Coder for `java.util.List`. Encodes as an Avro array */
    implicit def javaListAvroCoder[E](implicit elemEncoder: AvroEncoder[E], elemDecoder: AvroDecoder[E]): AvroCoder[java.util.List[E]] =
        AvroCoder.make(javaListAvroEncoder(elemEncoder), javaListAvroDecoder(elemDecoder))

    /** Encoder for `java.util.List`. Encodes as an Avro array */
    def javaListAvroEncoder[E](implicit elemEncoder: AvroEncoder[E]): AvroEncoder[java.util.List[E]] =
        avroArrayEncoder[E, java.util.List[E]](_.asScala, elemEncoder)

    /** Decoder for `java.util.List`. Decodes from an Avro array */
    def javaListAvroDecoder[E](implicit elemDecoder: AvroDecoder[E]): AvroDecoder[java.util.List[E]] =
        avroArrayDecoder[E, java.util.List[E]](canBuildJavaList, elemDecoder)

    /** Coder for any map with keys that can be coded as strings, expressed as a collection of key/value pairs. Encodes as an Avro map */
    def avroMapCoder[K, V, M]
        (keyCoder: StringCoder[K], valueCoder: AvroCoder[V])
        (implicit asIterable: M => Iterable[(K, V)], canBuildFrom: CanBuildFrom[Nothing, (K, V), M])
        : AvroCoder[M] =
        avroMapCoder(asIterable, canBuildFrom, keyCoder.encode, valueCoder.encode, keyCoder.decode, valueCoder.decode)

    /** Coder for any map with keys that can be coded as strings, expressed as a collection of key/value pairs. Encodes as an Avro map */
    implicit def avroMapCoder[K, V, M] (
        implicit asIterable:   M => Iterable[(K, V)],
                 canBuildFrom: CanBuildFrom[Nothing, (K, V), M],
                 keyEncoder:   StringEncoder[K],
                 valueEncoder: AvroEncoder[V],
                 keyDecoder:   StringDecoder[K],
                 valueDecoder: AvroDecoder[V]
    ): AvroCoder[M] =
        AvroCoder.make(avroMapEncoder(asIterable, keyEncoder, valueEncoder), avroMapDecoder(canBuildFrom, keyDecoder, valueDecoder))

    /** Encoder for any map with keys that can be coded as strings, expressed as a collection of key/value pairs. Encodes as an Avro map */
    def avroMapEncoder[K, V, M](implicit asIterable: M => Iterable[(K, V)], keyEncoder: StringEncoder[K], valueEncoder: AvroEncoder[V]) =
        new AvroEncoder[M] {
            val schema = Schema.createMap(valueEncoder.schema)
            val defaultJson = None

            def encodeDefaultJson(in: M) = tryCatchResultG(terminal) {
                val obj = jsonNodeFactory.objectNode
                val keyReceiver = new Receiver[String]
                asIterable(in).foreachResult { case (k, v) =>
                    keyEncoder.run(k, keyReceiver) >> {
                        valueEncoder.encodeDefaultJson(v) >>= { value =>
                            obj.put(keyReceiver.value, value)
                            Okay.unit
                        }
                    }
                } >> Okay(obj)
            }

            def run(in: M, out: io.Encoder) = tryCatchResultG(terminal) {
                val keyReceiver = new Receiver[String]
                out.writeMapStart()
                out.setItemCount(in.size)
                asIterable(in).foreachResult { case (k, v) =>
                    out.startItem()
                    keyEncoder.run(k, keyReceiver) >> {
                        out.writeString(keyReceiver.value)
                        valueEncoder.run(v, out)
                    }
                } >> {
                    out.writeMapEnd()
                    Okay.unit
                }
            }
        }

    /** Decoder for any map with keys that can be coded as strings, expressed as a collection of key/value pairs. Decodes as an Avro map */
    def avroMapDecoder[K, V, M](implicit canBuildFrom: CanBuildFrom[Nothing, (K, V), M], keyDecoder: StringDecoder[K], valueDecoder: AvroDecoder[V]) =
        new AvroDecoder[M] {
            val schema = Schema.createMap(valueDecoder.schema)
            val defaultJson = None

            def run(in: io.ResolvingDecoder, out: Receiver[M]) = tryCatchResultG(terminal) {
                val builder = canBuildFrom()
                val keyReceiver = new Receiver[K]
                val valueReceiver = new Receiver[V]

                @tailrec def accumulate(limit: Long): CoderResult[Unit] =
                    if (limit == 0) Okay.unit
                    else {
                        var l: Long = 0
                        while (l < limit) {
                            keyDecoder.run(in.readString(null).toString, keyReceiver) match {
                                case _: Okay[_] =>
                                    valueDecoder.run(in, valueReceiver) match {
                                        case _: Okay[_] => builder += ((keyReceiver.value, valueReceiver.value))
                                        case failed => return failed
                                    }
                                case failed => return failed
                            }
                            l += 1
                        }
                        accumulate(in.mapNext())
                    }

                val initial = in.readMapStart()
                builder.sizeHint(initial match {
                    case l if l > Integer.MAX_VALUE => Integer.MAX_VALUE
                    case l => l.asInstanceOf[Int]
                })

                accumulate(initial) >> out(builder.result())
            }
        }

    /** Coder for `java.util.Map` with keys that can be coded as strings. Encodes as an Avro map */
    def javaStringKeyedMapAvroCoder[K, V](keyCoder: StringCoder[K], valueCoder: AvroCoder[V]): AvroCoder[java.util.Map[K, V]] =
        javaStringKeyedMapAvroCoder(keyCoder.encode, valueCoder.encode, keyCoder.decode, valueCoder.decode)

    /** Coder for `java.util.Map` with keys that can be coded as strings. Encodes as an Avro map */
    implicit def javaStringKeyedMapAvroCoder[K, V] (
        implicit keyEncoder: StringEncoder[K],
                 valueEncoder: AvroEncoder[V],
                 keyDecoder: StringDecoder[K],
                 valueDecoder: AvroDecoder[V]
    ): AvroCoder[java.util.Map[K, V]] =
        AvroCoder.make(javaStringKeyedMapAvroEncoder(keyEncoder, valueEncoder), javaStringKeyedMapAvroDecoder(keyDecoder, valueDecoder))

    /** Encoder for `java.util.Map` with keys that can be encoded as strings. Encodes as an Avro map */
    def javaStringKeyedMapAvroEncoder[K, V](implicit keyEncoder: StringEncoder[K], valueEncoder: AvroEncoder[V]) =
        avroMapEncoder[K, V, java.util.Map[K, V]](_.asScala, keyEncoder, valueEncoder)

    /** Decoder for `java.util.Map` with keys that can be decoded from strings. Decodes as an Avro map */
    def javaStringKeyedMapAvroDecoder[K, V](implicit keyDecoder: StringDecoder[K], valueDecoder: AvroDecoder[V]) =
        avroMapDecoder[K, V, java.util.Map[K, V]](canBuildJavaMap, keyDecoder, valueDecoder)
}

trait containerLPI extends containerLPI2 {
    /** Coder for map-like collections where the key is complex and cannot be coded as a String. Encodes as an Avro array of pairs */
    def avroAssocArrayCoder[K, V, M]
        (keyCoder: AvroCoder[K], valueCoder: AvroCoder[V])
        (implicit asIterable: M => Iterable[(K, V)], canBuildFrom: CanBuildFrom[Nothing, (K, V), M])
        : AvroCoder[M] =
        avroAssocArrayCoder(asIterable, canBuildFrom, keyCoder.encode, valueCoder.encode, keyCoder.decode, valueCoder.decode)

    /** Coder for map-like collections where the key is complex and cannot be coded as a String. Encodes as an Avro array of pairs */
    implicit def avroAssocArrayCoder[K, V, M] (
        implicit asIterable:   M => Iterable[(K, V)],
                 canBuildFrom: CanBuildFrom[Nothing, (K, V), M],
                 keyEncoder:   AvroEncoder[K],
                 valueEncoder: AvroEncoder[V],
                 keyDecoder:   AvroDecoder[K],
                 valueDecoder: AvroDecoder[V]
    ): AvroCoder[M] =
        AvroCoder.make(avroAssocArrayEncoder(asIterable, keyEncoder, valueEncoder),
                       avroAssocArrayDecoder(canBuildFrom, keyDecoder, valueDecoder))

    private def assocArraySchema(key: AvroEncoderOrDecoder, value: AvroEncoderOrDecoder) =
        Schema.createArray {
            val recordName = "kvpair__" + encodeSchemaName(key.schema) + encodeSchemaName(value.schema)
            val pairSchema = Schema.createRecord(recordName, "", "", false)
            pairSchema.setFields(Arrays.asList (
                utils.makeField("key",   key.schema, None, ""),
                utils.makeField("value", value.schema, None, "")
            ))
            pairSchema
        }

    /** Encoder for map-like collections where the key is complex and cannot be encoded as a String. Encodes as an Avro array of pairs */
    def avroAssocArrayEncoder[K, V, M](implicit asIterable: M => Iterable[(K, V)], keyEncoder: AvroEncoder[K], valueEncoder: AvroEncoder[V]) =
        new AvroEncoder[M] {
            val schema = assocArraySchema(keyEncoder, valueEncoder)
            val defaultJson = None

            def encodeDefaultJson(in: M) = tryCatchResultG(terminal) {
                val array = jsonNodeFactory.arrayNode
                asIterable(in).foreachResult { case (k, v) =>
                    keyEncoder.encodeDefaultJson(k) >>= { key =>
                        valueEncoder.encodeDefaultJson(v) >>= { value =>
                            val item = jsonNodeFactory.objectNode
                            item.put("key", key)
                            item.put("value", value)
                            array.add(item)
                            Okay.unit
                        }
                    }
                } >> Okay(array)
            }

            def run(in: M, out: io.Encoder) = tryCatchResultG(terminal) {
                out.writeArrayStart()
                out.setItemCount(in.size)
                asIterable(in).foreachResult { case (k, v) =>
                    out.startItem()
                    keyEncoder.run(k, out) match {
                        case _: Okay[_] => valueEncoder.run(v, out)
                        case failed     => failed
                    }
                } >> {
                    out.writeArrayEnd()
                    Okay.unit
                }
            }
        }

    /** Decoder for map-like collections where the key is complex and cannot be decoded from a String. Decodes from an Avro array of pairs */
    def avroAssocArrayDecoder[K, V, M](implicit canBuildFrom: CanBuildFrom[Nothing, (K, V), M], keyDecoder: AvroDecoder[K], valueDecoder: AvroDecoder[V]) =
        new AvroDecoder[M] {
            val schema = assocArraySchema(keyDecoder, valueDecoder)
            val defaultJson = None

            def run(in: io.ResolvingDecoder, out: Receiver[M]) = tryCatchResultG(terminal) {
                val builder = canBuildFrom()
                val keyReceiver = new Receiver[K]
                val valueReceiver = new Receiver[V]

                @tailrec def accumulate(limit: Long): CoderResult[Unit] =
                    if (limit == 0) Okay.unit
                    else {
                        var l: Long = 0
                        while (l < limit) {
                            // FIXME doesn't bother to handle variances in the schema (does not use in.readFieldOrder())
                            keyDecoder.run(in, keyReceiver) match {
                                case _: Okay[_] =>
                                    valueDecoder.run(in, valueReceiver) match {
                                        case _: Okay[_] => builder += ((keyReceiver.value, valueReceiver.value))
                                        case failed     => return failed
                                    }
                                case failed => return failed
                            }

                            l += 1
                        }
                        accumulate(in.arrayNext())
                    }

                val initial = in.readArrayStart()
                builder.sizeHint(initial match {
                    case l if l > Integer.MAX_VALUE => Integer.MAX_VALUE
                    case l => l.asInstanceOf[Int]
                })

                accumulate(initial) >> out(builder.result())
            }
        }

    /** Coder for `java.util.Map` with keys that cannot be coded as Strings. Encodes as an Avro array of pairs */
    def javaMapAvroCoder[K, V](keyCoder: AvroCoder[K], valueCoder: AvroCoder[V]): AvroCoder[java.util.Map[K, V]] =
        javaMapAvroCoder(keyCoder.encode, valueCoder.encode, keyCoder.decode, valueCoder.decode)

    /** Coder for `java.util.Map` with keys that cannot be coded as Strings. Encodes as an Avro array of pairs */
    implicit def javaMapAvroCoder[K, V] (
        implicit keyEncoder: AvroEncoder[K],
                 valueEncoder: AvroEncoder[V],
                 keyDecoder: AvroDecoder[K],
                 valueDecoder: AvroDecoder[V]
    ): AvroCoder[java.util.Map[K, V]] =
        AvroCoder.make(javaMapAvroEncoder(keyEncoder, valueEncoder), javaMapAvroDecoder(keyDecoder, valueDecoder))

    /** Encoder for `java.util.Map` with keys that cannot be encoded as Strings. Encodes as an Avro array of pairs */
    def javaMapAvroEncoder[K, V](implicit keyEncoder: AvroEncoder[K], valueEncoder: AvroEncoder[V]) =
        avroAssocArrayEncoder[K, V, java.util.Map[K, V]](_.asScala, keyEncoder, valueEncoder)

    /** Decoder for `java.util.Map` with keys that cannot be decoded from Strings. Decodes from an Avro array of pairs */
    def javaMapAvroDecoder[K, V](implicit keyDecoder: AvroDecoder[K], valueDecoder: AvroDecoder[V]) =
        avroAssocArrayDecoder[K, V, java.util.Map[K, V]](canBuildJavaMap, keyDecoder, valueDecoder)
}

trait containerLPI2 {
    /** Coder for sequence type `S` comprised of element `E` coding as an Avro array */
    def avroArrayCoder[E, S]
        (elemCoder: AvroCoder[E])
        (implicit asIterable: S => Iterable[E], canBuildFrom: CanBuildFrom[Nothing, E, S])
        : AvroCoder[S] =
        avroArrayCoder(asIterable, canBuildFrom, elemCoder.encode, elemCoder.decode)

    /** Coder for sequence type `S` comprised of element `E` coding as an Avro array */
    implicit def avroArrayCoder[E, S] (
        implicit asIterable: S => Iterable[E],
                 canBuildFrom: CanBuildFrom[Nothing, E, S],
                 elemEncoder: AvroEncoder[E],
                 elemDecoder: AvroDecoder[E]
     ): AvroCoder[S] =
        AvroCoder.make(avroArrayEncoder(asIterable, elemEncoder), avroArrayDecoder(canBuildFrom, elemDecoder))

    /** Encoder for sequence type `S` comprised of element `E` encoding as an Avro array */
    def avroArrayEncoder[E, S](implicit asIterable: S => Iterable[E], elemEncoder: AvroEncoder[E]) =
        new AvroEncoder[S] {
            val schema = Schema.createArray(elemEncoder.schema)
            val defaultJson = None

            def encodeDefaultJson(s: S) = {
                val array = jsonNodeFactory.arrayNode
                asIterable(s).foreachResult { e =>
                    elemEncoder.encodeDefaultJson(e) >>= { e =>
                        array.add(e)
                        Okay.unit
                    }
                } >> Okay(array)
            }

            def run(in: S, out: io.Encoder) =
                tryCatchResultG(terminal) {
                    out.writeArrayStart()
                    val it = asIterable(in)
                    var index = 0
                    out.setItemCount(it.size)
                    it.foreachResult { e =>
                        out.startItem()
                        atIndex(index) {
                            index += 1
                            elemEncoder.run(e, out)
                        }
                    } >> Okay(out.writeArrayEnd())
                }
        }

    /** Decoder for sequence type `S` comprised of element `E` decoding from an Avro array */
    def avroArrayDecoder[E, S](implicit canBuildFrom: CanBuildFrom[Nothing, E, S], elemDecoder: AvroDecoder[E]) =
        new AvroDecoder[S] {
            val schema = Schema.createArray(elemDecoder.schema)
            val defaultJson = None

            def run(in: io.ResolvingDecoder, out: Receiver[S]) =
                tryCatchResultG(terminal) {
                    val builder = canBuildFrom()
                    val receiver = new Receiver[E]
                    var index = 0

                    @tailrec def accumulate(limit: Long): CoderResult[Unit] =
                        if (limit == 0) Okay.unit
                        else {
                            var l: Long = 0
                            while (l < limit) {
                                atIndex(index)(elemDecoder.run(in, receiver)) match {
                                    case _: Okay[_]         => builder += receiver.value
                                    case failed: FailedG[_] => return failed
                                }
                                index += 1
                                l += 1
                            }
                            accumulate(in.arrayNext())
                        }

                    val initial = in.readArrayStart()
                    builder.sizeHint(initial match {
                        case l if l > Integer.MAX_VALUE => Integer.MAX_VALUE
                        case l                          => l.asInstanceOf[Int]
                    })
                    accumulate(initial) >> out(builder.result())
                }
        }
}
