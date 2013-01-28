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

import java.util.{Arrays, Map => JavaMap}
import scala.collection.JavaConverters.{asScalaBufferConverter, seqAsJavaListConverter}

import com.mongodb.BasicDBObject
import net.liftweb.json.JsonAST.{JField, JNothing, JNull, JObject, JString, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.result.{FailedG, Okay, Result, cast, firstOrLastG, optionOps}

/** Coder for Either[A, B] */
case class EitherCoder[A, B] (
    leftValueCoder:  ComposableCoder[A],
    rightValueCoder: ComposableCoder[B],
    leftLabel: String = "left",
    rightLabel: String = "right"
) extends ComposableCoder[Either[A, B]]
{
    import ComposableCoder.{CoderResult, atProperty, catchingCoderException}

    val mostSpecificClass = classOf[Either[A, B]]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case jobj: JObject =>
                (jobj \ leftLabel, jobj \ rightLabel) match {
                    case (JNothing|JNull, JNothing|JNull) =>
                        FailedG("expected an object with either " + leftLabel + " or " + rightLabel, Nil)

                    case (leftJV, JNothing|JNull) =>
                        leftValueCoder.decode(classLoader, leftJV) map Left.apply
                    case (JNothing|JNull, rightJV) =>
                        rightValueCoder.decode(classLoader, rightJV) map Right.apply

                    case _ =>
                        FailedG("expected an object with either " + leftLabel + " or " + rightLabel + " but got both", Nil)
                }

            case _ =>
                FailedG("not an object", Nil)
        }

    def encode(classLoader: ClassLoader, in: Either[A, B]) =
        in match {
            case Left(leftV) =>
                leftValueCoder.encode(classLoader, leftV) map { jv => JObject(JField(leftLabel, jv) :: Nil) }
            case Right(rightV) =>
                rightValueCoder.encode(classLoader, rightV) map { jv => JObject(JField(rightLabel, jv) :: Nil) }
        }

    // Avro encoding

    val leftSchema = Schema.createRecord("Left__" + AvroUtils.encodeSchemaName(leftValueCoder.avroSchema._1), "", "scala", false)

    leftSchema.setFields(Arrays.asList(AvroUtils.makeField("value", leftValueCoder.avroSchema)))

    val rightSchema = Schema.createRecord("Right__" + AvroUtils.encodeSchemaName(rightValueCoder.avroSchema._1), "", "scala", false)

    rightSchema.setFields(Arrays.asList(AvroUtils.makeField("value", rightValueCoder.avroSchema)))

    lazy val avroSchema = (Schema.createUnion(Arrays.asList(leftSchema, rightSchema)), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            in.readIndex() match {
                case 0     => leftValueCoder.decodeAvro(classLoader, in) map Left.apply
                case 1     => rightValueCoder.decodeAvro(classLoader, in) map Right.apply
                case other => FailedG("read unknown union index " + other + " from Avro for Either", Nil)
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: Either[A, B], out: Encoder) =
        catchingCoderException {
            in match {
                case Left(value) =>
                    out.writeIndex(0)
                    leftValueCoder.encodeAvro(classLoader, value, out)

                case Right(value) =>
                    out.writeIndex(1)
                    rightValueCoder.encodeAvro(classLoader, value, out)
            }
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Either[A, B]) =
        in match {
            case Left(value) =>
                val obj = jsonNodeFactory.objectNode
                leftValueCoder.encodeAvroDefaultJson(classLoader, value)
                    .map(obj.put("value", _))
                    .then(Okay(obj))
            case Right(value) =>
                FailedG("can't encode a default value for Avro that uses Right, since Avro doesn't allow union defaults using anything but the first branch", Nil)
        }

    // MongoDB encoding

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case m: JavaMap[_, _] =>
                val obj = m.asInstanceOf[JavaMap[String, AnyRef]]
                (obj.get(leftLabel), obj.get(rightLabel)) match {
                    case (null, null) =>
                        FailedG("expected an object with either " + leftLabel + " or " + rightLabel, Nil)

                    case (leftV, null) =>
                        leftValueCoder.decodeMongoDB(classLoader, leftV) map Left.apply
                    case (null, rightV) =>
                        rightValueCoder.decodeMongoDB(classLoader, rightV) map Right.apply

                    case _ =>
                        FailedG("expected an object with either " + leftLabel + " or " + rightLabel + " but got both", Nil)
                }
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Either[A, B]) =
        in match {
            case Left(value) =>
                leftValueCoder.encodeMongoDB(classLoader, value) map { mongoValue =>
                    val obj = new BasicDBObject
                    obj.put(leftLabel, mongoValue)
                    obj
                }

            case Right(value) =>
                rightValueCoder.encodeMongoDB(classLoader, value) map { mongoValue =>
                    val obj = new BasicDBObject
                    obj.put(rightLabel, mongoValue)
                    obj
                }
        }
}

/** Make a union coder that "guesses" the right alternative by trying each coder in turn. */
case class AutomaticUnionCoder[T] (
    flatten: Boolean,
    noApplicableAlternative: String,
    alternatives: List[ComposableCoder[_ <: T]]
)(implicit m: Manifest[T]) extends ComposableCoder[T] with FlattenableCoder
{
    import ComposableCoder.{CoderResult, FailedPath, catchingCoderException}

    private val noApplicableAlternativeFG = FailedG(noApplicableAlternative, Nil: FailedPath)

    val mostSpecificClass = m.erasure.asInstanceOf[Class[T]]

    for (alternative <- alternatives if alternative.mostSpecificClass == mostSpecificClass)
        sys.error("Union with most specific class " + mostSpecificClass +
                  " cannot code alternative with most specific class " + alternative.mostSpecificClass)

    def decode(classLoader: ClassLoader, in: JValue) =
        firstOrLastG(noApplicableAlternativeFG, alternatives) { _.decode(classLoader, in) } | noApplicableAlternativeFG

    def encode(classLoader: ClassLoader, in: T) =
        firstOrLastG(noApplicableAlternativeFG, alternatives) {
            coder => catchingCoderException { coder.forceEncode(classLoader, in) }
        } | noApplicableAlternativeFG

    lazy val avroSchema = (Schema.createUnion(alternatives.map(_.avroSchema._1).asJava), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            in.readIndex() match {
                case i if i < 0                    => FailedG("read negative union index " + i + " from Avro", Nil)
                case i if i >= alternatives.length => FailedG("read overflow union index " + i + " from Avro", Nil)
                case i                             => alternatives(i).decodeAvro(classLoader, in).asAG(mostSpecificClass, Nil)
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        catchingCoderException {
            alternatives.zipWithIndex.find {
                _._1.mostSpecificClass.isInstance(in)
            }.toResult orElse noApplicableAlternativeFG flatMap {
                case (coder, index) => {
                    out.writeIndex(index)
                    coder.forceEncodeAvro(classLoader, in, out)
                }
            }
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        alternatives.zipWithIndex.find {
            _._1.mostSpecificClass.isInstance(in)
        }.toResult orElse noApplicableAlternativeFG flatMap {
            case (coder, 0) =>
                coder.forceEncodeAvroDefaultJson(classLoader, in.asInstanceOf[AnyRef])
            case (_, _) =>
                FailedG("can't encode a default value for Avro that uses anything but the first union alternative, since Avro doesn't allow union defaults using anything but the first branch", Nil)
        }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        firstOrLastG(noApplicableAlternativeFG, alternatives)(_.decodeMongoDB(classLoader, in)) | noApplicableAlternativeFG

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        firstOrLastG(noApplicableAlternativeFG, alternatives)(coder => catchingCoderException(coder.forceEncodeMongoDB(classLoader, in))) | noApplicableAlternativeFG
}

/** Case class that contains information on each alternative of an explicit union */
case class ExplicitUnionAlternative[T](determinantValue: String, subclass: Class[T], coder: ComposableCoder[T])

/**
 * Union coder that uses some explicit field of a JObject (the "determinant") to choose the right decoding alternative and the class of the
 * value to choose the right encoding alternative
 */
case class ExplicitUnionCoder[T]
    (flatten: Boolean, determinantField: String, alternatives: List[ExplicitUnionAlternative[_ <: T]])
    (implicit m: Manifest[T])
extends ComposableCoder[T] with FlattenableCoder
{
    import ComposableCoder.{FailedPath, atTerminal, catchingCoderException}

    val mostSpecificClass = m.erasure.asInstanceOf[Class[T]]

    val noApplicableAlternative: FailedG[FailedPath] = FailedG(determinantField + " value not valid (expected one of: " + alternatives.map(_.determinantValue).mkString(", ") + ")", Nil)
    val missingDeterminant: FailedG[FailedPath]      = FailedG("missing " + determinantField + " to determine type of value", Nil)

    def decode(classLoader: ClassLoader, in: JValue) =
        for {
            givenDeterminant <- in \ determinantField match {
                case JString(s)                    => Okay(s)
                case _ if in.isInstanceOf[JObject] => missingDeterminant
                case _                             => FailedG("not an object", Nil)
            }
            applicableAlternative <- alternatives.find { _.determinantValue == givenDeterminant }.toResult orElse noApplicableAlternative
            value <- applicableAlternative.coder.decode(classLoader, in)
        } yield value

    def encode(classLoader: ClassLoader, in: T) =
        alternatives.find { _.subclass.isInstance(in) }.toResult orElse noApplicableAlternative flatMap {
            case alternative: ExplicitUnionAlternative[u] =>
                atTerminal(cast[u](alternative.subclass, in))
                    .flatMap { alternative.coder.encode(classLoader, _) }
                    .asAG(classOf[JObject], Nil)
                    .orElse("encoder for " + alternative.determinantValue + " did not yield an object")
                    .map { case JObject(fields) => JObject(JField(determinantField, JString(alternative.determinantValue)) :: fields) }
        }

    lazy val avroSchema = (Schema.createUnion(alternatives.map(_.coder.avroSchema._1).asJava), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            in.readIndex() match {
                case i if i < 0                    => FailedG("read negative union index " + i + " from Avro", Nil)
                case i if i >= alternatives.length => FailedG("read overflow union index " + i + " from Avro", Nil)
                case i                             => alternatives(i).coder.decodeAvro(classLoader, in).asAG(mostSpecificClass, Nil)
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        catchingCoderException {
            alternatives.zipWithIndex.find { _._1.subclass.isInstance(in) }.toResult orElse noApplicableAlternative flatMap {
                case (alternative, index) =>
                    out.writeIndex(index)
                    alternative.coder.forceEncodeAvro(classLoader, in, out)
            }
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        (alternatives.zipWithIndex.find { _._1.subclass.isInstance(in) }.toResult orElse noApplicableAlternative) flatMap {
            case (alternative, 0) =>
                alternative.coder.forceEncodeAvroDefaultJson(classLoader, in.asInstanceOf[AnyRef])
            case (_, _) =>
                FailedG("can't encode a default value for Avro that uses anything but the first union alternative, since Avro doesn't allow union defaults using anything but the first branch", Nil)
        }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case m: JavaMap[_, _] =>
                for {
                    givenDeterminant <- Result(m.get(determinantField)).asA[String] orElse missingDeterminant
                    applicableAlternative <- alternatives.find { _.determinantValue == givenDeterminant }.toResult orElse noApplicableAlternative
                    value <- applicableAlternative.coder.decodeMongoDB(classLoader, in)
                } yield value
            case _ => FailedG("not an object", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        alternatives.find { _.subclass.isInstance(in) }.toResult orElse noApplicableAlternative flatMap {
            case alternative: ExplicitUnionAlternative[u] =>
                atTerminal(cast[u](alternative.subclass, in))
                    .flatMap { alternative.coder.encodeMongoDB(classLoader, _) }
                    .asAG(classOf[JavaMap[_, _]], Nil)
                    .orElse("encoder for " + alternative.determinantValue + " did not yield an object")
                    .map { obj =>
                        obj.asInstanceOf[JavaMap[String, AnyRef]].put(determinantField, alternative.determinantValue)
                        obj
                    }
        }
}
