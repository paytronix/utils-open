//
// Copyright 2013 Paytronix Systems, Inc.
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

import java.util.{Arrays, Collection => JavaCollection, Map => JavaMap}

import com.mongodb.BasicDBObject
import net.liftweb.json.JsonAST.{JArray, JField, JNothing, JNull, JObject, JString, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.reflection.classByName
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, ResultG, cast, parameter, tryCatch, tryCatching}

object ResultCoder {
    val throwableSchema = Schema.createRecord("Throwable", "", "com.paytronix.utils.scala.result", false)
    throwableSchema.setFields(Arrays.asList (
        AvroUtils.makeField("isA", (Schema.create(Schema.Type.STRING), None)),
        AvroUtils.makeField("message", (Schema.create(Schema.Type.STRING), None)),
        AvroUtils.makeField("cause", AvroUtils.nullable(throwableSchema))
    ))
    val missingSchema = Schema.createRecord("__Missing", "", "com.paytronix.utils.scala.result", false)
    missingSchema.setFields(Arrays.asList())
}

/** Coder for ResultG[E, A] */
case class ResultCoder[E, A] (
    failedParamCoder:   ComposableCoder[E],
    valueCoder:         ComposableCoder[A],
    hideFailures:       Option[Boolean] = None
) extends OptionLikeCoder[ResultG[E, A]] {
    import ComposableCoder.{CoderResult, atProperty, catchingCoderException}

    val mostSpecificClass = classOf[ResultG[E, A]]

    def shouldHideFailures: Boolean = hideFailures getOrElse CoderSettings.hideFailures.get

    private def instThrowable(classLoader: ClassLoader, className: String, message: String, causeOpt: Option[Throwable]): Result[Throwable] =
        for {
            clazz <- classByName[Throwable](classLoader, className)
            inst <- causeOpt match {
                case Some(cause) =>
                    tryCatching[NoSuchMethodException].value(clazz.getConstructor(classOf[String], classOf[Throwable]))
                        .orElse("throwable class " + className + " does not have (String, Throwable) constructor")
                        .flatMap { ctor => tryCatch.value(ctor.newInstance(message, cause)) }

                case None =>
                    (
                        tryCatching[NoSuchMethodException].value(clazz.getConstructor(classOf[String]))
                            .orElse("throwable class " + className + " does not have a (String) constructor")
                            .flatMap { ctor => tryCatch.value(ctor.newInstance(message)) }
                    ) orElse (
                        tryCatching[NoSuchMethodException].value(clazz.getConstructor(classOf[String], classOf[Throwable]))
                            .orElse("throwable class " + className + " does not have a (String, Throwable) constructor")
                            .flatMap { ctor => tryCatch.value(ctor.newInstance(message, null)) }
                    )
            }
        } yield inst

    def decode(classLoader: ClassLoader, in: JValue) = {
        def decodeThrowable(in: JObject): CoderResult[Throwable] =
            (in \ "isA", in \ "message", in \ "cause") match {
                case (JString(className), JString(message), nestedCauseObj: JObject) =>
                    atProperty("cause") {
                        decodeThrowable(nestedCauseObj) flatMap { nestedCause =>
                            instThrowable(classLoader, className, message, Some(nestedCause)) | parameter(Nil)
                        }
                    }

                case (JString(className), JString(message), JNothing|JNull) =>
                    instThrowable(classLoader, className, message, None) | parameter(Nil)

                case (JString(_), JString(_), _) =>
                    atProperty("cause")(FailedG("expected an object or null", Nil))

                case (JString(_), _, _) =>
                    atProperty("message")(FailedG("expected a string", Nil))

                case (_, _, _) =>
                    atProperty("isA")(FailedG("expected a string", Nil))
            }

        def decodeFailed(messageOrThrowable: Either[JValue, JValue], param: JValue): CoderResult[FailedG[E]] =
            atProperty("param")(failedParamCoder.decode(classLoader, param)) flatMap { param =>
                messageOrThrowable.fold (
                    _ match {
                        case message: JString =>
                            Okay(FailedG(message.s, param))
                        case _ =>
                            FailedG("expected \"errorMessage\" to be a string describing failure cause", Nil)
                    },
                    _ match {
                        case throwable: JObject =>
                            atProperty("throwable")(decodeThrowable(throwable)) map { FailedG(_, param) }
                        case _ =>
                            FailedG("expected \"throwable\" to be an object containing failure cause", Nil)
                    }
                )
            }

        object EncodedFailure {
            def unapply(in: JValue): Option[CoderResult[FailedG[E]]] =
                in match {
                    case JObject(fields) =>
                        fields.sortBy(_.name) match {
                            case List(JField("errorCode", _), JField("errorMessage", _), JField("param", param), JField("result", JString("failed")), JField("throwable", throwable))  =>
                                Some(decodeFailed(Right(throwable), param))
                            case List(JField("errorCode", _), JField("errorMessage", message), JField("param", param), JField("result", JString("failed"))) =>
                                Some(decodeFailed(Left(message), param))
                            case _ => None
                        }
                    case _ => None
                }
        }

        valueCoder match {
            case (_: OptionLikeCoder[_])|(_: UnitCoder.type) =>
                in match {
                    case null|JNull|JNothing =>
                        failedParamCoder.decode(classLoader, JNothing) map { param => FailedG("unknown failure", param) }
                    case EncodedFailure(decodingResult) =>
                        decodingResult
                    case jobject: JObject =>
                        FailedG("got an object, which was expected to be an encoded failure, but didn't conform to the expected schema", Nil)
                    case JArray(Nil) =>
                        valueCoder.decode(classLoader, JNothing) map Okay.apply
                    case JArray(jv :: Nil) =>
                        valueCoder.decode(classLoader, jv) map Okay.apply
                    case JArray(_) =>
                        FailedG("expected an empty array or one with exactly one element", Nil)
                    case _ =>
                        FailedG("not an object or array", Nil)
                }

            case _ =>
                in match {
                    case null|JNull|JNothing =>
                        failedParamCoder.decode(classLoader, JNothing) map { param => FailedG("unknown failure", param) }
                    case EncodedFailure(decodingResult) =>
                        decodingResult
                    case jv =>
                        valueCoder.decode(classLoader, jv) map Okay.apply
                }
        }
    }

    def encode(classLoader: ClassLoader, in: ResultG[E, A]) = {
        def encodeFailed(failed: FailedG[E]): CoderResult[JValue] =
            for {
                throwableFieldOption <-
                    if (CoderSettings.isInsecureContext.get) Okay(None)
                    else atProperty("throwable")(encodeThrowable(failed.throwable)) map { obj => Some(JField("throwable", obj)) }
                paramField <- atProperty("param")(failedParamCoder.encode(classLoader, failed.parameter)) map { JField("param", _) }
            } yield {
                var fields: List[JField] = paramField :: Nil
                throwableFieldOption.foreach(fields ::= _)
                fields ::= JField("errorCode", JString("system.error"))
                fields ::= JField("errorMessage", JString(failed.message))
                fields ::= JField("result", JString("failed"))
                JObject(fields)
            }

        def encodeThrowable(throwable: Throwable): CoderResult[JObject] =
            throwable.getCause match {
                case null =>
                    Okay(JObject (
                        JField("isA", JString(throwable.getClass.getName)) ::
                        JField("message",
                            Option(throwable.getMessage) map JString getOrElse JString(throwable.toString)) ::
                        Nil
                    ))

                case cause =>
                    encodeThrowable(cause) map { causeObj =>
                        JObject (
                            JField("isA", JString(throwable.getClass.getName)) ::
                            JField("message",
                                Option(throwable.getMessage) map JString getOrElse JString(throwable.getClass.getName)) ::
                            JField("cause", causeObj) ::
                            Nil
                        )
                    }
            }

        def addResultSuccess(in: JValue): JValue =
            in match {
                case JObject(fields) if !fields.exists(_.name == "result") =>
                    JObject(JField("result", JString("success")) :: fields)
                case _ => in
            }

        catchingCoderException {
            valueCoder match {
                case (_: OptionLikeCoder[_])|(_: UnitCoder.type) =>
                    in match {
                        case Okay(value) =>
                            valueCoder.encode(classLoader, value) map { jv => JArray(jv :: Nil) }

                        case _ if shouldHideFailures =>
                            Okay(JNothing)

                        case failed: FailedG[_] =>
                            encodeFailed(failed)
                    }

                case _ =>
                    in match {
                        case Okay(value) =>
                            valueCoder.encode(classLoader, value) map addResultSuccess

                        case _ if shouldHideFailures =>
                            Okay(JNothing)

                        case failed@FailedG(_, _) =>
                            encodeFailed(failed)
                    }
            }
        }
    }

    // Avro encoding

    import ResultCoder.{missingSchema, throwableSchema}

    val okaySchema = Schema.createRecord("Okay__" + AvroUtils.encodeSchemaName(valueCoder.avroSchema._1),
                                         "", "com.paytronix.utils.scala.result", false)

    okaySchema.setFields(Arrays.asList (
        AvroUtils.makeField("value", valueCoder.avroSchema)
    ))

    val failedSchema = Schema.createRecord (
        if (valueCoder == UnitCoder) "Failed"
        else "FailedG__" + AvroUtils.encodeSchemaName(valueCoder.avroSchema._1),
        "", "com.paytronix.utils.scala.result", false
    )

    failedSchema.setFields(Arrays.asList (
        AvroUtils.makeField("throwable", (throwableSchema, None)),
        AvroUtils.makeField("param", failedParamCoder.avroSchema)
    ))

    lazy val avroSchema =
        (Schema.createUnion(Arrays.asList(missingSchema, okaySchema, failedSchema)), Some(jsonNodeFactory.nullNode))

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = {
        def decodeFailed(): CoderResult[FailedG[E]] =
            decodeThrowable() flatMap { throwable =>
                failedParamCoder.decodeAvro(classLoader, in) map { FailedG(throwable, _) }
            }

        def decodeThrowable(): CoderResult[Throwable] =
            {
                for {
                    isA <- atProperty("isA")(tryCatch.value(in.readString(null).toString) | parameter(Nil))
                    message <- atProperty("message")(tryCatch.value(in.readString(null).toString) | parameter(Nil))
                    causeOpt <- atProperty("cause") {
                        in.readIndex() match {
                            case 0 => in.readNull(); Okay(None)
                            case 1 => decodeThrowable() map Some.apply
                            case other => FailedG("read unknown union index " + other + " from Avro for cause of throwable", Nil)
                        }
                    }

                    instance <- instThrowable(classLoader, isA, message, causeOpt) | parameter(Nil)
                } yield instance
            }

        catchingCoderException {
            in.readIndex() match {
                case 0     => failedParamCoder.decode(classLoader, JNothing) map { param => FailedG("unknown failure", param) }
                case 1     => valueCoder.decodeAvro(classLoader, in) map Okay.apply
                case 2     => decodeFailed()
                case other => FailedG("read unknown union index " + other + " from Avro for ResultG", Nil)
            }
        }
    }

    def encodeAvro(classLoader: ClassLoader, in: ResultG[E, A], out: Encoder) = {
        def encodeFailed(in: FailedG[E]): CoderResult[Unit] =
            encodeThrowable(in.throwable) then failedParamCoder.encodeAvro(classLoader, in.parameter, out)

        def encodeThrowable(in: Throwable): CoderResult[Unit] =
            catchingCoderException {
                out.writeString(in.getClass.getName)
                out.writeString(in.getMessage() match { case null => in.toString(); case s => s })
                in.getCause match {
                    case null =>
                        out.writeIndex(0)
                        out.writeNull()
                        Okay(())

                    case cause =>
                        out.writeIndex(1)
                        encodeThrowable(cause)
                }
            }

        catchingCoderException {
            in match {
                case Okay(value) =>
                    out.writeIndex(1)
                    valueCoder.encodeAvro(classLoader, value, out)

                case failed: FailedG[_] =>
                    out.writeIndex(2)
                    encodeFailed(failed)
            }
        }
    }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: ResultG[E, A]) =
        in match {
            case Okay(_) =>
                FailedG("cannot have an Avro default of Okay(...) for ResultG as Avro only supports defaulting to the first branch of a union", Nil)

            case _ =>
                Okay(jsonNodeFactory.nullNode)
        }

    // MongoDB encoding

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) = {
        def decodeFailed(in: JavaMap[String, AnyRef]): CoderResult[FailedG[E]] =
            for {
                throwable <- atProperty("throwable")(Result(in.get("throwable")).asA[JavaMap[String, AnyRef]] | parameter(Nil) flatMap decodeThrowable)
                param <- atProperty("param")(Result(in.get("param")) | parameter(Nil) flatMap { failedParamCoder.decodeMongoDB(classLoader, _) })
            } yield FailedG(throwable, param)

        def decodeThrowable(in: JavaMap[String, AnyRef]): CoderResult[Throwable] =
            for {
                isA     <- atProperty("isA")(Result(in.get("isA")).asA[String] | parameter(Nil))
                message <- atProperty("message")(Result(in.get("message")).asA[String] | parameter(Nil))

                causeOpt <- atProperty("cause") {
                    Option(in.get("cause")) map { cause =>
                        cast[JavaMap[String, AnyRef]](cause) | parameter(Nil) flatMap decodeThrowable map Some.apply
                    } getOrElse Okay(None)
                }

                instance <- instThrowable(classLoader, isA, message, causeOpt) | parameter(Nil)
            } yield instance

        valueCoder match {
            case (_: OptionLikeCoder[_])|(_: UnitCoder.type) =>
                in match {
                    case null =>
                        failedParamCoder.decode(classLoader, JNothing) map { param => FailedG("unknown failure", param) }
                    case m: JavaMap[_, _] if m.get("throwable") != null =>
                        decodeFailed(m.asInstanceOf[JavaMap[String, AnyRef]])
                    case m: JavaMap[_, _] =>
                        FailedG("got an object, which was expected to be an encoded failure, but was something else (throwable field missing)", Nil)
                    case coll: JavaCollection[_] if coll.isEmpty =>
                        valueCoder.decodeMongoDB(classLoader, null) map Okay.apply
                    case coll: JavaCollection[_] if coll.size == 1 =>
                        valueCoder.decodeMongoDB(classLoader, coll.asInstanceOf[JavaCollection[AnyRef]].iterator().next()) map Okay.apply
                    case coll: JavaCollection[_] =>
                        FailedG("expected an empty array or one with exactly one element", Nil)
                    case _ =>
                        FailedG("not an object or array", Nil)
                }

            case _ =>
                in match {
                    case null =>
                        failedParamCoder.decode(classLoader, JNothing) map { FailedG("unknown failure", _) }
                    case m: JavaMap[_, _] if m.get("throwable") != null =>
                        decodeFailed(m.asInstanceOf[JavaMap[String, AnyRef]])
                    case other =>
                        valueCoder.decodeMongoDB(classLoader, other) map Okay.apply
                }
        }
    }

    def encodeMongoDB(classLoader: ClassLoader, in: ResultG[E, A]) = {
        def encodeFailed(in: FailedG[E]): CoderResult[AnyRef] =
            for {
                throwable <- encodeThrowable(in.throwable)
                param <- failedParamCoder.encodeMongoDB(classLoader, in.parameter)
            } yield {
                val obj = new BasicDBObject
                obj.put("throwable", throwable)
                obj.put("param", param)
                obj
            }

        def encodeThrowable(in: Throwable): CoderResult[AnyRef] =
            Option(in.getCause) map { cause => encodeThrowable(cause) map Some.apply } getOrElse Okay(None) map {
                causeOpt =>
                val obj = new BasicDBObject
                obj.put("isA", in.getClass.getName)
                obj.put("message", in.getMessage match { case null => in.toString; case s => s })
                obj.put("cause", causeOpt.orNull)
                obj
            }

        in match {
            case Okay(value) =>
                valueCoder match {
                    case (_: OptionLikeCoder[_])|(_: UnitCoder.type) =>
                        valueCoder.encodeMongoDB(classLoader, value) map { java.util.Arrays.asList(_) }

                    case _ =>
                        valueCoder.encodeMongoDB(classLoader, value)
                }

            case _ if shouldHideFailures =>
                Okay(null)

            case failed: FailedG[_] =>
                encodeFailed(failed)
        }
    }
}
