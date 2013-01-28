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

import java.util.{Collection => JavaCollection, HashMap => JavaHashMap, Map => JavaMap}
import scala.collection.JavaConverters.{mapAsScalaMapConverter, seqAsJavaListConverter}

import com.mongodb.BasicDBObject
import net.liftweb.json.JsonAST.{JField, JNothing, JNull, JObject, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.bson.types.ObjectId
import org.codehaus.jackson.node.ObjectNode
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.result.{FailedG, Okay, iterableResultOps}

/**
 * Coder for an argument array where the individual arguments are all decoded out of the body, rather than specific fields of the body
 * object by name as in ArgumentArrayCoder
 *
 * For example, if you wanted to handle input documents like:
 *
 *     {"type": "foo", "arg1": "value", "arg2": "value"}
 *     {"type": "bar", "otherstuff": "othervalue"}
 *
 * Without this coder you would have to do something like:
 *
 *     class MyServiceImpl ... {
 *         def myAction(`type`: String, arg1: Option[String], arg2: Option[String], otherstuff: Option[String]) =
 *             (`type`, arg1, arg2, otherstuff) match {
 *                 case ("foo", Some(x), Some(y), None) => ...
 *                 case ("foo", _, _, _) => Failed("invalid arguments")
 *                 case ("bar", None, None, Some(x)) => ...
 *                 case ("bar", _, _, _) => Failed("invalid arguments")
 *                 case (_, _, _, _) => Failed("invalid type")
 *             }
 *
 * With this coder instead you can do:
 *
 *     sealed class MyServiceArguments
 *     final case class FooArguments(arg1: String, arg2: String) extends MyServiceArguments
 *     final case class BarArguments(otherstuff: String) extends MyServiceArguments
 *     object MyServiceArgumentsCoding extends ExplicitUnionCoding[MyServiceArguments] {
 *         override val determinantField = "type"
 *         alternative[FooArguments]("foo")
 *         alternative[BarArguments]("bar")
 *     }
 *
 *     class MyServiceImpl ... {
 *         def myAction(args: MyServiceArguments) = args match {
 *             case FooArguments(x, y) => ...
 *             case BarArguments(x) => ...
 *         }
 *     }
 *
 *     object MyServiceMeta extends StandardServiceMeta {
 *         ...
 *         applyTo("myAction") { _.inputCoder = Coding.forClass[MyServiceArguments].flatMap(FlatArgumentArrayCoder.apply) }
 *         ...
 *     }
 *
 * FlatArgumentCoder supports multiple arguments, each of which is extracted from the same input value.
 *
 * Operates like ArgumentArrayCoder with anonymous arguments for Avro.
 */
case class FlatArgumentArrayCoder(valueCoders: ComposableCoder[_]*) extends ComposableCoder[Array[AnyRef]]
{
    import ComposableCoder.{atProperty, catchingCoderException}

    val mostSpecificClass = classOf[Array[AnyRef]]

    def decode(classLoader: ClassLoader, in: JValue) =
        valueCoders.mapResult {
            _.decode(classLoader, in).asAG(classOf[AnyRef], Nil)
        } map { result => Array[AnyRef](result: _*) }

    def encode(classLoader: ClassLoader, in: Array[AnyRef]) =
        valueCoders.zipWithIndex
        .mapResult { case (coder, i) =>
            coder.forceEncode(classLoader, in(i))
        } map { _ reduceLeft (_ merge _) }

    lazy val avroSchema =
        (Schema.createRecord(valueCoders.zipWithIndex.map {
            case (coder, i) => AvroUtils.makeField("_" + i.toString, coder.avroSchema)
        }.asJava), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            val result = Array.ofDim[AnyRef](valueCoders.length)
            in.readFieldOrder.toSeq.foreachResult { f =>
                val coder = valueCoders(f.pos)
                atProperty(f.pos.toString)(coder.decodeAvro(classLoader, in).map(v => result(f.pos) = v.asInstanceOf[AnyRef]))
            } then Okay(result)
        }

    def encodeAvro(classLoader: ClassLoader, in: Array[AnyRef], out: Encoder) =
        catchingCoderException {
            (in zip valueCoders.zipWithIndex).toSeq.foreachResult { case (v, (coder, i)) =>
                atProperty(i.toString)(coder.forceEncodeAvro(classLoader, v, out))
            }
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Array[AnyRef]) = {
        val obj = jsonNodeFactory.objectNode

        catchingCoderException {
            (in zip valueCoders.zipWithIndex).toSeq.foreachResult { case (v, (coder, i)) =>
                atProperty(i.toString)(coder.forceEncodeAvroDefaultJson(classLoader, v).map(obj.put("_" + i, _)).unit)
            }
        } then Okay(obj)
    }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        valueCoders.mapResult {
            _.decodeMongoDB(classLoader, in).asAG(classOf[AnyRef], Nil)
        } map { result => Array[AnyRef](result: _*) }

    def encodeMongoDB(classLoader: ClassLoader, in: Array[AnyRef]) = {
        def mongoMerge(a: AnyRef, b: AnyRef): AnyRef =
            (a, b) match {
                case (am: JavaMap[_, _], bm: JavaMap[_, _]) => {
                    val result = new JavaHashMap[String, AnyRef]
                    for ((k, v) <- am.asInstanceOf[JavaMap[String, AnyRef]].asScala) result.put(k, v)
                    for ((k, v) <- bm.asInstanceOf[JavaMap[String, AnyRef]].asScala) result.put(k, v)
                    result
                }

                case (ac: JavaCollection[_], bc: JavaCollection[_]) => {
                    val result = new java.util.ArrayList[AnyRef]
                    result.addAll(ac.asInstanceOf[JavaCollection[AnyRef]])
                    result.addAll(bc.asInstanceOf[JavaCollection[AnyRef]])
                    result
                }

                case _ => {
                    val result = new java.util.LinkedList[AnyRef]
                    result.add(a)
                    result.add(b)
                    result
                }
            }

        valueCoders mapResult { _.forceEncodeMongoDB(classLoader, in(0)) } map { _ reduceLeft mongoMerge }
    }

    override def toString = valueCoders.mkString("(", ", ", ")")
}

/** Case class that holds the coder for a single argument array member, erasing the type variable to avoid annoying coercions */
final case class ArgumentCoding(name: String, coder: ComposableCoder[_])

/** Coder for argument arrays, which unlike traditional arrays are heterogeneous and coded in JSON as objects */
case class ArgumentArrayCoder(flatten: Boolean, arguments: List[ArgumentCoding]) extends ComposableCoder[Array[AnyRef]] with FlattenableCoder {
    import ComposableCoder.{CoderResult, catchingCoderException, atProperty}

    val mostSpecificClass = classOf[Array[AnyRef]]

    private def shouldFlatten(in: ComposableCoder[_]): Boolean = in match {
        case flattenable: FlattenableCoder if flattenable.flatten => true
        case _                                                    => false
    }

    def decode(classLoader: ClassLoader, in: JValue) = {
        def decodeObject(in: JObject): CoderResult[Array[AnyRef]] =
            arguments.mapResult { ac =>
                atProperty(ac.name) {
                    val jv = if (shouldFlatten(ac.coder)) in else in \ ac.name
                    ac.coder.decode(classLoader, jv) map { _.asInstanceOf[AnyRef] }
                }
            } map { values => Array[AnyRef](values: _*) }

        in match {
            case jobject: JObject => decodeObject(jobject)
            case JNothing|JNull   => FailedG("required but missing", Nil)
            case _                => FailedG("not an object", Nil)
        }
    }

    def encode(classLoader: ClassLoader, in: Array[AnyRef]) =
        in.toList.zip(arguments).mapResult {
            case (value, ArgumentCoding(name, _)) if value.asInstanceOf[AnyRef] eq null =>
                Okay(Nil)

            case (value, ArgumentCoding(name, coder)) =>
                atProperty(name) {
                    for {
                        encoded <- coder.forceEncode(classLoader, value)
                        fields <- encoded match {
                            case JNothing =>
                                Okay(Nil)

                            case jvalue if shouldFlatten(coder) =>
                                jvalue match {
                                    case JObject(fields) => Okay(fields)
                                    case otherJValue =>
                                        FailedG(coder + " should have yielded a JObject since it is configured for flattening, but instead yielded " + otherJValue, Nil)
                                }

                            case jvalue =>
                                Okay(JField(name, jvalue) :: Nil)
                        }
                    } yield fields
                }
        } map { jfields => JObject(jfields.flatten.toList) }

    lazy val avroSchema =
        (Schema.createRecord(arguments.map(ac => AvroUtils.makeField(ac.name, ac.coder.avroSchema)).asJava), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            val result = Array.ofDim[AnyRef](arguments.length)
            in.readFieldOrder.toSeq.foreachResult { f =>
                val ArgumentCoding(name, coder) = arguments(f.pos)
                atProperty(name)(coder.decodeAvro(classLoader, in).map(v => result(f.pos) = v.asInstanceOf[AnyRef]))
            } then Okay(result)
        }

    def encodeAvro(classLoader: ClassLoader, in: Array[AnyRef], out: Encoder) =
        catchingCoderException {
            (in zip arguments).toSeq.foreachResult {
                case (v, ArgumentCoding(name, coder)) =>
                    atProperty(name)(coder.forceEncodeAvro(classLoader, v, out))
            }
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Array[AnyRef]) = {
        val obj = jsonNodeFactory.objectNode

        in.toList.zip(arguments).foreachResult {
            case (value, ArgumentCoding(name, coder)) =>
                atProperty(name) {
                    coder.forceEncodeAvroDefaultJson(classLoader, value).map(obj.put(name, _)).unit
                }
        } then Okay(obj)
    }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case m: JavaMap[_, _] =>
                val obj = m.asInstanceOf[JavaMap[String, AnyRef]]
                arguments.mapResult { ac =>
                    atProperty(ac.name) {
                        val v = if (shouldFlatten(ac.coder)) in else obj.get(ac.name)
                        ac.coder.decodeMongoDB(classLoader, v) map { _.asInstanceOf[AnyRef] }
                    }
                } map { values => Array[AnyRef](values: _*) }

            case null => FailedG("required but missing", Nil)
            case _    => FailedG("not an object", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Array[AnyRef]) = {
        val obj = new BasicDBObject
        in.toList.zip(arguments).foreachResult {
            case (value, _) if value.asInstanceOf[AnyRef] == null => Okay(())
            case (value, ArgumentCoding(name, coder)) =>
                atProperty(name) {
                    for {
                        encoded <- coder.forceEncodeMongoDB(classLoader, value)
                        fieldsSetOk <- encoded match {
                            case null => Okay(())
                            case m: JavaMap[_, _] if shouldFlatten(coder) =>
                                for ((k, v) <- m.asInstanceOf[JavaMap[String, AnyRef]].asScala)
                                    obj.put(k, v)
                                Okay(())

                            case _ if shouldFlatten(coder) =>
                                FailedG(coder + " should have yielded an object since it is configured for flattening, but instead yielded " + encoded, Nil)

                            case _ =>
                                obj.put(name, encoded)
                                Okay(())
                        }
                    } yield ()
                }
        } then Okay(obj)
    }

    override def toString =
        arguments.map(ac => ac.name + ": " + ac.coder.toString).mkString("(", ", ", ")")
}
