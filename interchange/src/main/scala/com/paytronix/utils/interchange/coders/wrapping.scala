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

import java.util.Arrays

import com.mongodb.BasicDBObject
import net.liftweb.json.JsonAST.{JArray, JNothing, JNull, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.result.{Okay, Failed, FailedG, Result, parameter}

/** Coder that uses a lens to extract/inject the value the be coded from some other thing */
class MappedCoder[T, U](val injector: U => Result[T], val extractor: T => Result[U], val underlying: ComposableCoder[U])(implicit m: Manifest[T]) extends ComposableCoder[T] {
    import ComposableCoder.CoderResult

    val mostSpecificClass = m.erasure.asInstanceOf[Class[T]]

    protected def inject(u: U): CoderResult[T] = injector(u) | parameter(Nil)
    protected def extract(t: T): CoderResult[U] = extractor(t) | parameter(Nil)

    def decode(classLoader: ClassLoader, in: JValue): CoderResult[T] =
        underlying.decode(classLoader, in) flatMap inject
    def encode(classLoader: ClassLoader, in: T): CoderResult[JValue] =
        extract(in) flatMap { underlying.encode(classLoader, _) }

    def avroSchema = underlying.avroSchema
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder): CoderResult[T] =
        underlying.decodeAvro(classLoader, in) flatMap inject
    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder): CoderResult[Unit] =
        extract(in) flatMap { underlying.encodeAvro(classLoader, _, out) }
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T): CoderResult[JsonNode] =
        extract(in) flatMap { underlying.encodeAvroDefaultJson(classLoader, _) }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef): CoderResult[T] =
        underlying.decodeMongoDB(classLoader, in) flatMap inject
    def encodeMongoDB(classLoader: ClassLoader, in: T): CoderResult[AnyRef] =
        extract(in) flatMap { underlying.encodeMongoDB(classLoader, _) }
}

/** Coder that uses a lens to extract/inject the value the be coded from some other thing */
class StringSafeMappedCoder[T, U](injector: U => Result[T], extractor: T => Result[U], underlying: StringSafeCoder[U])(implicit m: Manifest[T]) extends MappedCoder[T, U](injector, extractor, underlying)(m) with StringSafeCoder[T] {
    import ComposableCoder.CoderResult

    def decodeString(classLoader: ClassLoader, in: String): CoderResult[T] =
        underlying.decodeString(classLoader, in) flatMap inject
    def encodeString(classLoader: ClassLoader, in: T): CoderResult[String] =
        extract(in) flatMap { underlying.encodeString(classLoader, _) }
}

/** Insecure coder which always decodes a default value and encodes to nothing in an insecure context, and operates normally in a secure one */
case class InsecureCoder[T](coder: ComposableCoder[T], substitute: Result[T]) extends ComposableCoder[T]
{
    import ComposableCoder.atTerminal

    val mostSpecificClass = coder.mostSpecificClass

    def decode(classLoader: ClassLoader, in: JValue) =
        if (CoderSettings.isInsecureContext.get) atTerminal(substitute)
        else coder.decode(classLoader, in)
    def encode(classLoader: ClassLoader, in: T) =
        if (CoderSettings.isInsecureContext.get) Okay(JNothing)
        else coder.encode(classLoader, in)

    def avroSchema = coder.avroSchema
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        if (CoderSettings.isInsecureContext.get) atTerminal(substitute)
        else coder.decodeAvro(classLoader, in)
    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        if (CoderSettings.isInsecureContext.get) Okay(())
        else coder.encodeAvro(classLoader, in, out)
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        coder.encodeAvroDefaultJson(classLoader, in)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (CoderSettings.isInsecureContext.get) atTerminal(substitute)
        else coder.decodeMongoDB(classLoader, in)
    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        if (CoderSettings.isInsecureContext.get) Okay(null)
        else coder.encodeMongoDB(classLoader, in)
}

/** Coder which substitutes some default values for nulls/missing */
case class DefaultingCoder[T](coder: ComposableCoder[T], default: T) extends ComposableCoder[T]
{
    val mostSpecificClass = coder.mostSpecificClass

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JNull|JNothing => Okay(default)
            case _              => coder.decode(classLoader, in)
        }
    def encode(classLoader: ClassLoader, in: T) =
        coder.encode(classLoader, in)

    lazy val avroSchema = {
        val defaultJson =
            coder.encodeAvroDefaultJson(mostSpecificClass.getClassLoader, default)
            .orElse("Failed to encode Avro default to Avro-specific JSON for default of type " + mostSpecificClass + ": " + default)
            .orThrow
        (coder.avroSchema._1, Some(defaultJson))
    }

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        coder.decodeAvro(classLoader, in)
    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        coder.encodeAvro(classLoader, in, out)
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        coder.encodeAvroDefaultJson(classLoader, in)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case null => Okay(default)
            case _    => coder.decodeMongoDB(classLoader, in)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        coder.encodeMongoDB(classLoader, in)
}

/**
 * Coder that allows for null -- if JNothing or JNull is decoded null is decoded, otherwise the nested coder is delegated to.
 * Ideally, the type argument would be <: AnyRef, but that requires too many forced downcasts everywhere.
 */
case class NullCoder[T](valueCoder: ComposableCoder[T]) extends ComposableCoder[T]
{
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = valueCoder.mostSpecificClass

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case null|JNothing|JNull => Okay(null.asInstanceOf[T])
            case other               => valueCoder.decode(classLoader, other)
        }

    def encode(classLoader: ClassLoader, in: T) =
        in match {
            case null  => Okay(JNothing)
            case other => valueCoder.encode(classLoader, other)
        }

    lazy val avroSchema = AvroUtils.nullable(valueCoder.avroSchema._1)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            in.readIndex() match {
                case 0 =>
                    in.readNull()
                    Okay(null.asInstanceOf[T])
                case 1 => valueCoder.decodeAvro(classLoader, in)
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        catchingCoderException {
            in match {
                case null =>
                    out.writeIndex(0)
                    out.writeNull()
                    Okay(())

                case _ =>
                    out.writeIndex(1)
                    valueCoder.encodeAvro(classLoader, in, out)
            }
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        if (in == null) Okay(jsonNodeFactory.nullNode)
        else FailedG("Avro cannot encode non-null values as defaults", Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case null  => Okay(null.asInstanceOf[T])
            case other => valueCoder.decodeMongoDB(classLoader, in)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        in match {
            case null  => Okay(null)
            case other => valueCoder.encodeMongoDB(classLoader, in)
        }
}

/** Coder for Option[T] */
case class OptionCoder[T](valueCoder: ComposableCoder[T]) extends OptionLikeCoder[Option[T]]
{
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[Option[T]]

    def decode(classLoader: ClassLoader, in: JValue) =
        valueCoder match {
            case (_: OptionLikeCoder[_])|(_: UnitCoder.type) =>
                in match {
                    case null|JNothing|JNull => Okay(None)
                    case JArray(Nil)         => valueCoder.decode(classLoader, JNothing).map(Some.apply)
                    case JArray(jv :: Nil)   => valueCoder.decode(classLoader, jv).map(Some.apply)
                    case JArray(_)           => FailedG("expected an empty array or one with exactly one element", Nil)
                    case _                   => FailedG("not an object or array", Nil)
                }

            case _ =>
                in match {
                    case null|JNothing|JNull => Okay(None)
                    case jv                  => valueCoder.decode(classLoader, jv) map Some.apply
                }
        }

    def encode(classLoader: ClassLoader, in: Option[T]) =
        valueCoder match {
            case (_: OptionLikeCoder[_])|(_: UnitCoder.type) =>
                in match {
                    case Some(value) => valueCoder.encode(classLoader, value).map(jv => JArray(jv :: Nil))
                    case None => Okay(JNothing)
                }

            case _ =>
                in match {
                    case Some(value) => valueCoder.encode(classLoader, value)
                    case None => Okay(JNothing)
                }
        }

    lazy val avroSchema = AvroUtils.nullable(valueCoder.avroSchema match {
        case (schema, None) if schema.getType != Schema.Type.UNION => schema
        case otherSchemaAndDefault@(otherSchema, _) =>
            val someSchema = Schema.createRecord("Some__" + AvroUtils.encodeSchemaName(valueCoder.avroSchema._1), "", "scala", false)

            someSchema.setFields(Arrays.asList (
                AvroUtils.makeField("value", otherSchemaAndDefault)
            ))

            someSchema
    })

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            in.readIndex() match {
                case 0     => { in.readNull(); Okay(None) }
                case 1     => valueCoder.decodeAvro(classLoader, in).map(Some.apply)
                case other => FailedG("read unknown union index " + other + " from Avro for Option", Nil)
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: Option[T], out: Encoder) =
        catchingCoderException {
            in match {
                case None => {
                    out.writeIndex(0)
                    out.writeNull()
                    Okay(())
                }

                case Some(v) => {
                    out.writeIndex(1)
                    valueCoder.encodeAvro(classLoader, v, out)
                }
            }
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Option[T]) =
        in match {
            case None    => Okay(jsonNodeFactory.nullNode)
            case Some(_) => FailedG("Avro cannot encode a non-None Option as a default", Nil)
        }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case null => Okay(None)
            case other => valueCoder.decodeMongoDB(classLoader, in).map(Some.apply)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Option[T]) =
        in match {
            case Some(value) => valueCoder.encodeMongoDB(classLoader, value)
            case None => Okay(null)
        }
}

