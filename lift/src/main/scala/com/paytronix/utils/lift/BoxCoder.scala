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

package com.paytronix.utils.lift

import java.util.Arrays

import com.mongodb.BasicDBObject
import net.liftweb.common.{Box, Empty, EmptyBox, Failure, Full, ParamFailure}
import net.liftweb.json.Implicits.string2jvalue
import net.liftweb.json.JsonAST.{JArray, JBool, JDouble, JField, JInt, JNothing, JNull, JObject, JString, JValue, render}
import net.liftweb.util.ControlHelpers.tryo
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}
import org.slf4j.{Logger, LoggerFactory}

import com.paytronix.utils.extendedreflection
import com.paytronix.utils.interchange.{AvroUtils, CoderSettings, Coding, ComposableCoder, OptionLikeCoder, UnitCoder}
import com.paytronix.utils.scala.log.{loggerResultOps, resultLoggerOps}
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, parameter, tryCatch}

import box.{boxToErrorHandlingBox, boxToNestedBox}
import log.loggerBoxOps
import result.resultBoxOps

/** Coder for Box[T] */
case class BoxCoder[T](valueCoder: ComposableCoder[T], hideFailures: Option[Boolean] = None) extends OptionLikeCoder[Box[T]]
{
    import BoxCoder.{allFailureFields, logger, requiredFailureFields}
    import ComposableCoder.{CoderResult, atProperty}

    val mostSpecificClass = classOf[Box[T]]

    def shouldHideFailures: Boolean = hideFailures getOrElse CoderSettings.hideFailures.get

    // JSON encoding

    def decode(classLoader: ClassLoader, in: JValue) = {
        def decodeFailure(in: JValue): Failure = {
            val failure = Failure(
                Full(in \ "failure").asA[JString].map(_.s) openOr "",
                Empty,
                Full(in \ "chain").asA[JObject].map(jobj => Full(decodeFailure(jobj))) openOr Empty
            )

            (in \ "paramIsA", in \ "param") match {
                case (JNothing, _) =>
                    failure

                case (JString(isAClassName), toDecode) =>
                    implicit val builder = new extendedreflection.Builder(classLoader)

                    (for {
                        typeR <- tryCatch.result(builder.typeRFor(Class.forName(isAClassName, true, classLoader)))
                        coder <- Coding.forType(classLoader, typeR)
                        param <- coder.decode(toDecode)
                    } yield param) match {
                        case Okay(param) => failure ~> param
                        case paramDecodeFailure =>
                            logger.warnResult("Failed to decode incoming ParamFailure of type " + isAClassName, paramDecodeFailure)
                            failure
                    }

                case (_, _) =>
                    logger.warn("Failed to decode incoming ParamFailure -- paramIsA not a string")
                    failure
            }
        }

        object EncodedFailure {
            def unapply(in: JValue): Option[Failure] =
                in match {
                    case JObject(fields) =>
                        val fieldNames = Set(fields.map(_.name): _*)
                        if (requiredFailureFields.subsetOf(fieldNames) && fieldNames.subsetOf(allFailureFields) && (in \ "result") == JString("failed"))
                            Some(decodeFailure(in))
                        else
                            None
                    case _ => None
                }
        }

        valueCoder match {
            case (_: OptionLikeCoder[_])|(_: UnitCoder.type) =>
                in match {
                    case null|JNothing|JNull =>
                        Okay(Empty)
                    case EncodedFailure(decodingResult) =>
                        Okay(decodingResult)
                    case jobject: JObject =>
                        FailedG("got an object, which was expected to be an encoded failure, but didn't match the expected schema", Nil)
                    case JArray(Nil) =>
                        valueCoder.decode(classLoader, JNothing).map(Full.apply)
                    case JArray(jv :: Nil) =>
                        valueCoder.decode(classLoader, jv).map(Full.apply)
                    case JArray(_) =>
                        FailedG("expected an empty array or one with exactly one element", Nil)
                    case _ =>
                        FailedG("not an object or array", Nil)
                }

            case _ =>
                in match {
                    case null|JNothing|JNull =>
                        Okay(Empty)
                    case EncodedFailure(decodingResult) =>
                        Okay(decodingResult)
                    case jv =>
                        valueCoder.decode(classLoader, jv).map(Full.apply)
                }
        }
    }

    def encode(classLoader: ClassLoader, in: Box[T]) = {
        def encodeFailure(failure: Failure): JValue = {
            var fields: List[JField] = Nil

            failure match {
                case ParamFailure(_, _, _, null) =>
                    logger.warn("Ignoring null parameter to ParamFailure")

                case ParamFailure(_, _, _, param) => {
                    implicit val builder = new extendedreflection.Builder(classLoader)
                    (for {
                        typeR <- builder.typeRFor(param.asInstanceOf[AnyRef].getClass)
                        coder <- Coding.forType(classLoader, typeR)
                        encoded <- coder.forceEncode(param)
                    } yield {
                        fields ::= JField("param", encoded)
                        fields ::= JField("paramIsA", param.asInstanceOf[AnyRef].getClass.getName)
                    }).logWarn("Failed to encode outgoing ParamFailure")
                }

                case _ => {}
            }

            failure.chain.foreach(
                cause => fields ::= JField("chain", encodeFailure(cause))
            )

            fields ::= JField("failure", JString(failure.msg))
            fields ::= JField("errorCode", JString("system.error"))
            fields ::= JField("errorMessage", JString(failure.msg))
            fields ::= JField("result", JString("failed"))

            JObject(fields)
        }

        def addResultSuccess(in: JValue): JValue =
            in match {
                case JObject(fields) if !fields.exists(_.name == "result") =>
                    JObject(JField("result", "success") :: fields)
                case _ => in
            }

        tryCatch.value {
            valueCoder match {
                case (_: OptionLikeCoder[_])|(_: UnitCoder.type) =>
                    in match {
                        case _: EmptyBox if shouldHideFailures => Okay(JNothing)
                        case Empty                             => Okay(JNothing)
                        case Full(value)                       => valueCoder.encode(classLoader, value).map(jv => JArray(jv :: Nil))
                        case f: Failure                        => Okay(encodeFailure(f))
                    }

                case _ =>
                    in match {
                        case _: EmptyBox if shouldHideFailures => Okay(JNothing)
                        case Empty                             => Okay(JNothing)
                        case Full(value)                       => valueCoder.encode(classLoader, value) map addResultSuccess
                        case f: Failure                        => Okay(encodeFailure(f))
                    }
            }
        }.orElse(parameter(Nil)).flatten
    }

    // Avro encoding

    val fullSchema = Schema.createRecord("Full__" + AvroUtils.encodeSchemaName(valueCoder.avroSchema._1),
                                         "", "net.liftweb.common", false)

    fullSchema.setFields(Arrays.asList (
        AvroUtils.makeField("value", valueCoder.avroSchema)
    ))

    val avroSchema = (Schema.createUnion(Arrays.asList (
        Schema.create(Schema.Type.NULL),
        fullSchema,
        BoxCoder.failureSchema
    )), Some(jsonNodeFactory.nullNode))

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = {
        def decodeFailure(): CoderResult[Failure] = {
            def decodeBaseFailure() = {
                val message = in.readString(null).toString
                in.readIndex() match {
                    case 0 =>
                        in.readNull()
                        Okay(Failure(message))
                    case 1 => decodeFailure() map { chain => Failure(message, Empty, Full(chain)) }
                    case other => FailedG("read unknown union index " + other + " from Avro for Failure chain", Nil)
                }
            }

            try {
                in.readIndex() match {
                    case 0 => decodeBaseFailure()

                    case 1 =>
                        implicit val builder = new extendedreflection.Builder(classLoader)
                        decodeBaseFailure flatMap { baseFailure =>
                            val isAClassName = in.readString(null).toString
                            val schemaJSON   = in.readString(null).toString
                            val byteBuffer   = in.readBytes(null)
                            val bytes = Array.ofDim[Byte](byteBuffer.remaining)
                            byteBuffer.get(bytes)

                            {
                                for {
                                    schema <- tryCatch.value(new Schema.Parser().parse(schemaJSON))
                                    typeR  <- tryCatch.result(builder.typeRFor(Class.forName(isAClassName, true, classLoader)))
                                    coder  <- Coding.forType(classLoader, typeR)
                                    param  <- coder.decodeAvro(schema, bytes)
                                } yield baseFailure ~> param
                            } | parameter(Nil)
                        }

                    case other => FailedG("read unknown union index " + other + " from Avro for Failure", Nil)
                }
            } catch {
                case e: Exception => FailedG(e, Nil)
            }
        }

        tryCatch.value {
            in.readIndex() match {
                case 0 => Okay(Empty)
                case 1 => valueCoder.decodeAvro(classLoader, in).map(Full.apply)
                case 2 => decodeFailure()
                case other => FailedG("read unknown union index " + other + " from Avro for Box", Nil)
            }
        }.orElse(parameter(Nil)).flatten
    }

    def encodeAvro(classLoader: ClassLoader, in: Box[T], out: Encoder) = {
        def encodeFailure(in: Failure): CoderResult[Unit] =
            try {
                def encodeBaseFailure(message: String, chain: Box[Failure]): CoderResult[Unit] = {
                    out.writeString(message)
                    chain match {
                        case Full(nested) => {
                            out.writeIndex(1)
                            encodeFailure(nested)
                        }

                        case _ =>
                            Okay(out.writeIndex(0))
                    }
                }

                in match {
                    case ParamFailure(message, _, chain, param) if param.asInstanceOf[AnyRef] != null =>
                        implicit val builder = new extendedreflection.Builder(classLoader)
                        val paramClass = param.asInstanceOf[AnyRef].getClass()

                        {
                            for {
                                typeR   <- builder.typeRFor(paramClass)
                                coder   <- Coding.forType(classLoader, typeR)
                                encoded <- coder.forceEncodeAvro(param)

                                baseFailureOk <- {
                                    out.writeIndex(1)
                                    encodeBaseFailure(message, chain)
                                }
                            } yield {
                                out.writeString(paramClass.getName)
                                out.writeString(coder.avroSchema.toString)
                                out.writeBytes(encoded)
                            }
                        } match {
                            case Okay(_) => Okay(())
                            case failed =>
                                failed.logWarn("Failed to encode outgoing ParamFailure")
                                out.writeIndex(0)
                                encodeBaseFailure(message, chain)
                        }

                    case Failure(message, _, chain) =>
                        out.writeIndex(0)
                        encodeBaseFailure(message, chain)
                }
            } catch {
                case e: Exception => FailedG(e, Nil)
            }

        tryCatch.value {
            in match {
                case Empty =>
                    out.writeIndex(0)
                    Okay(())

                case Full(v) =>
                    out.writeIndex(1)
                    valueCoder.encodeAvro(classLoader, v, out)

                case failure: Failure =>
                    out.writeIndex(2)
                    encodeFailure(failure)
            }
        }.orElse(parameter(Nil)).flatten
    }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Box[T]) =
        in match {
            case Empty => Okay(jsonNodeFactory.nullNode)
            case _ => FailedG("Avro cannot encode non-Empty Boxes", Nil)
        }

    // MongoDB encoding

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) = {
        def decodeFailure(in: java.util.Map[String, AnyRef]): Failure = {
            val failure = Failure(
                (Box!!in.get("failure")).asA[String] openOr "",
                Empty,
                (Box!!in.get("chain")).asA[java.util.Map[_, _]].map (
                    m => Full(decodeFailure(m.asInstanceOf[java.util.Map[String, AnyRef]]))
                ) openOr Empty
            )

            (in.get("paramIsA"), in.get("param")) match {
                case (null, _) =>
                    failure

                case (isAClassName: String, toDecode) =>
                    implicit val builder = new extendedreflection.Builder(classLoader)

                    {
                        for {
                            typeR <- tryCatch.result(builder.typeRFor(Class.forName(isAClassName, true, classLoader)))
                            coder <- Coding.forType(classLoader, typeR)
                            param <- coder.decodeMongoDB(toDecode)
                        } yield param
                    } match {
                        case Okay(param) => failure ~> param
                        case paramDecodeFailed =>
                            logger.warnResult("Failed to decode incoming ParamFailure of type " + isAClassName, paramDecodeFailed)
                            failure
                    }

                case (_, _) =>
                    logger.warn("Failed to decode incoming ParamFailure -- paramIsA not a string")
                    failure
            }
        }

        valueCoder match {
            case (_: OptionLikeCoder[_])|(_: UnitCoder.type) =>
                in match {
                    case null =>
                        Okay(Empty)
                    case m: java.util.Map[_, _] if m.get("failure") != null =>
                        Okay(decodeFailure(m.asInstanceOf[java.util.Map[String, AnyRef]]))
                    case m: java.util.Map[_, _] =>
                        FailedG("got an object, which was expected to be an encoded failure, but was something else (failure field missing)", Nil)
                    case coll: java.util.Collection[_] if coll.isEmpty =>
                        valueCoder.decodeMongoDB(classLoader, null).map(Full.apply)
                    case coll: java.util.Collection[_] if coll.size == 1 =>
                        valueCoder.decodeMongoDB(classLoader, coll.asInstanceOf[java.util.Collection[AnyRef]].iterator().next()).map(Full.apply)
                    case coll: java.util.Collection[_] =>
                        FailedG("expected an empty array or one with exactly one element", Nil)
                    case _ =>
                        FailedG("not an object or array", Nil)
                }

            case _ =>
                in match {
                    case null =>
                        Okay(Empty)
                    case m: java.util.Map[_, _] if m.get("failure") != null =>
                        Okay(decodeFailure(m.asInstanceOf[java.util.Map[String, AnyRef]]))
                    case other =>
                        valueCoder.decodeMongoDB(classLoader, other).map(Full.apply)
                }
        }
    }


    def encodeMongoDB(classLoader: ClassLoader, in: Box[T]) = {
        def encodeFailure(failure: Failure): AnyRef = {
            val obj = new BasicDBObject

            failure match {
                case ParamFailure(_, _, _, null) =>
                    logger.warn("Ignoring null parameter to ParamFailure")

                case ParamFailure(_, _, _, param) =>
                    implicit val builder = new extendedreflection.Builder(classLoader)
                    val paramClass = param.asInstanceOf[AnyRef].getClass()

                    {
                        for {
                            typeR <- builder.typeRFor(paramClass)
                            coder <- Coding.forType(classLoader, typeR)
                            encoded <- coder.forceEncodeMongoDB(param)
                        } yield {
                            obj.put("param", encoded)
                            obj.put("paramIsA", paramClass.getName)
                        }
                    }.logWarn("Failed to encode outgoing ParamFailure")

                case _ => {}
            }

            failure.chain.foreach { cause => obj.put("chain", encodeFailure(cause)) }

            obj.put("failure", failure.msg)

            obj
        }

        valueCoder match {
            case (_: OptionLikeCoder[_])|(_: UnitCoder.type) =>
                in match {
                    case _: EmptyBox if shouldHideFailures => Okay(null)
                    case Empty                             => Okay(null)
                    case Full(value)                       => valueCoder.encodeMongoDB(classLoader, value).map(java.util.Arrays.asList(_))
                    case f: Failure                        => Okay(encodeFailure(f))
                }

            case _ =>
                in match {
                    case _: EmptyBox if shouldHideFailures => Okay(null)
                    case Empty                             => Okay(null)
                    case Full(value)                       => valueCoder.encodeMongoDB(classLoader, value)
                    case f: Failure                        => Okay(encodeFailure(f))
                }
        }
    }
}

object BoxCoder {
    implicit val logger = LoggerFactory.getLogger(getClass)

    val requiredFailureFields = Set("errorCode", "errorMessage", "failure", "result")
    val optionalFailureFields = Set("chain", "paramIsA", "param")
    val allFailureFields = requiredFailureFields union optionalFailureFields

    Coding.register(classOf[Box[_]], Coding.oneArgRegistration("Box", BoxCoder(_)))

    val plainFailureSchema = Schema.createRecord("PlainFailure", "", "net.liftweb.common", false)
    val paramFailureSchema = Schema.createRecord("ParamFailure", "", "net.liftweb.common", false)
    val failureSchema      = Schema.createRecord("Failure", "", "net.liftweb.common", false)

    // wrapper since you can't nest unions
    failureSchema.setFields(Arrays.asList (
        AvroUtils.makeField("failure", (Schema.createUnion(Arrays.asList(plainFailureSchema, paramFailureSchema)), None))
    ))

    plainFailureSchema.setFields(Arrays.asList (
        AvroUtils.makeField("failure", (Schema.create(Schema.Type.STRING), None)),
        AvroUtils.makeField("chain",   AvroUtils.nullable(failureSchema))
    ))

    paramFailureSchema.setFields(Arrays.asList (
        AvroUtils.makeField("failure",     (Schema.create(Schema.Type.STRING), None)),
        AvroUtils.makeField("chain",       AvroUtils.nullable(failureSchema)),
        AvroUtils.makeField("paramIsA",    (Schema.create(Schema.Type.STRING), None)),
        AvroUtils.makeField("paramSchema", (Schema.create(Schema.Type.STRING), None)),
        AvroUtils.makeField("paramData",   (Schema.create(Schema.Type.BYTES), None))
    ))
}


