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

import java.lang.reflect.{Constructor, Method}
import java.util.{Map => JavaMap}
import scala.collection.JavaConverters.seqAsJavaListConverter

import com.mongodb.BasicDBObject
import net.liftweb.json.JsonAST.{JField, JNothing, JNull, JObject, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.result.{FailedG, Okay, Result, iterableResultOps, parameter, tryCatch}

/** Associate a field name in JSON with a coder and getter/setter, for coding POJOs and POSOs */
final case class FieldCoding(name: String, coder: ComposableCoder[_], getter: Method, setter: Result[Method]) {
    override def equals(other: Any): Boolean =
        other match {
            case FieldCoding(otherName, otherCoder, otherGetter, otherSetter) =>
                name == otherName && coder == otherCoder && getter == otherGetter && (
                    for (s1 <- setter; s2 <- otherSetter) yield s1 == s2
                ).getOrElse(setter.isDefined == otherSetter.isDefined)
        }
}

/** Map a object (POJO, case class, others) */
case class ObjectCoder[T](
    clazz:                 Class[T],
    constructor:           Constructor[T],
    constructorFieldNames: List[String],
    fieldCodings:          List[FieldCoding],
    flatten:               Boolean
) extends ComposableCoder[T] with FlattenableCoder {
    import ComposableCoder.{CoderResult, atProperty, atTerminal, catchingCoderException}

    val mostSpecificClass = clazz

    private val fieldCodingByName = Map(fieldCodings.map(fc => (fc.name, fc)): _*)
    private lazy val nonConstructorFields = fieldCodings filterNot { constructorFieldNames contains _.name }

    private def shouldFlatten(in: ComposableCoder[_]): Boolean =
        in match {
            case flattenable: FlattenableCoder if flattenable.flatten => true
            case _ => false
        }

    /** Set the value of the field on the object */
    private def setField(fieldCoding: FieldCoding, instance: T, valueResult: => CoderResult[AnyRef]): CoderResult[Unit] =
        fieldCoding.setter match {
            case Okay(setter) =>
                    for {
                        value <- valueResult
                        setOk <- catchingCoderException(Okay(setter.invoke(instance, value))) | ("failed to set property \"" + fieldCoding.name + "\" of " + clazz.getName)
                    } yield ()

            case _ => Okay(())
        }

    /** Get the value of the field from the object instance, converting null to None and non-null to Some */
    private def getField(fieldCoding: FieldCoding, instance: T): CoderResult[Option[AnyRef]] =
        atProperty(fieldCoding.name) {
            tryCatch.value(fieldCoding.getter.invoke(instance)) match {
                case Okay(v: AnyRef) if v ne null => Okay(Some(v))
                case Okay(null)                   => Okay(None)
                case failed: FailedG[_]           => failed | parameter(Nil)
            }
        }


    private def instantiate(classLoader: ClassLoader, in: JObject): CoderResult[T] = {
        def fetchAndDecode(fc: FieldCoding): CoderResult[AnyRef] =
            atProperty(fc.name) {
                fc.coder.decode(classLoader, if (shouldFlatten(fc.coder)) in else in \ fc.name).map(_.asInstanceOf[AnyRef])
            }

        for {
            constructorArgs <- constructorFieldNames.mapResult { name =>
                fieldCodingByName.get(name) match {
                    case Some(fieldCoding) => fetchAndDecode(fieldCoding)
                    case None => FailedG("property \"" + name + "\" specified as a constructor property could not be found in field codings", Nil)
                }
            }

            instance <- catchingCoderException(Okay(constructor.newInstance(constructorArgs: _*)))

            nonConstructorFieldsSetOk <- nonConstructorFields mapResult {
                fieldCoding => setField(fieldCoding, instance, fetchAndDecode(fieldCoding))
            }
        } yield instance
    }

    def decode(classLoader: ClassLoader, in: JValue) = {
        in match {
            case jobject: JObject => instantiate(classLoader, jobject)
            case JNothing|JNull   => FailedG("required but missing", Nil)
            case _                => FailedG("not an object", Nil)
        }
    }

    def encode(classLoader: ClassLoader, in: T) =
        /*
         * Short circuit if we're given something that shouldn't be allowed. even without this check most cases would throw
         * ClassCastException or IllegalArgumentException or similar when the getters are invoked on the wrong type, but this
         * does it earlier and ensures that even objects with no fields fail properly.
         */
        if (!clazz.isInstance(in)) {
            val isA = if (in == null) "null" else in.asInstanceOf[AnyRef].getClass.getName
            FailedG("expected a " + clazz.getName + " not a " + isA, Nil)
        } else {
            fieldCodings mapResult { fc =>
                for {
                    value   <- getField(fc, in)
                    encoded <- fc.coder.forceEncode(classLoader, value.getOrElse(null))
                    fields  <- encoded match {
                        case JNothing => Okay(Nil)
                        case jvalue if shouldFlatten(fc.coder) => jvalue match {
                            case JObject(fields) => Okay(fields)
                            case otherJValue     => FailedG(fc.coder + " should have yielded a JObject since it is configured for flattening, but instead yielded " + otherJValue, Nil)
                        }
                        case jvalue => Okay(JField(fc.name, jvalue) :: Nil)
                    }
                } yield fields
            } map { fields => JObject(fields.flatten.toList) }
        }

    lazy val avroSchema = {
        import AvroUtils.nameAndNamespaceFromClass
        val (namespace, name) = nameAndNamespaceFromClass(clazz)
        val s = Schema.createRecord(name, "", namespace, false)
        s.setFields(fieldCodings.map(fc => AvroUtils.makeField(fc.name, fc.coder.avroSchema)).asJava)
        (s, None)
    }

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            for {
                valuesByName <- in.readFieldOrder.toSeq mapResult { f =>
                    fieldCodingByName(f.name) match {
                        case fc@FieldCoding(name, coder, _, _) =>
                            atProperty(fc.name) {
                                coder.decodeAvro(classLoader, in) map { v => (fc.name, v.asInstanceOf[AnyRef]) }
                            }
                    }
                } map { pairs => Map(pairs: _*) }

                constructorArgs <- constructorFieldNames.mapResult(name => {
                    fieldCodingByName.get(name) match {
                        case Some(fieldCoding) => Okay(valuesByName(name))
                        case None              => FailedG("property \"" + name + "\" specified as a constructor property could not be found in field codings", Nil)
                    }
                })

                instance <- catchingCoderException(Okay(constructor.newInstance(constructorArgs: _*)))

                nonConstructorFieldsSetOk <- nonConstructorFields foreachResult {
                    fieldCoding => setField(fieldCoding, instance, Okay(valuesByName(fieldCoding.name)))
                }
            } yield instance
        }

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        catchingCoderException {
            /*
             * Short circuit if we're given something that shouldn't be allowed. even without this check most cases would throw
             * ClassCastException or IllegalArgumentException or similar when the getters are invoked on the wrong type, but this
             * does it earlier and ensures that even objects with no fields fail properly.
             */
            if (!clazz.isInstance(in)) {
                FailedG("expected a " + clazz.getName + " not a " +
                        (if (in == null) "null" else in.asInstanceOf[AnyRef].getClass.getName), Nil)
            } else {
                fieldCodings foreachResult { fc =>
                    getField(fc, in) flatMap { v =>
                        fc.coder.forceEncodeAvro(classLoader, v.getOrElse(null), out)
                    }
                }
            }
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T) =
        if (!clazz.isInstance(in)) {
            val isA = if (in == null) "null" else in.asInstanceOf[AnyRef].getClass.getName
            FailedG("expected a " + clazz.getName + " not a " + isA, Nil)
        } else {
            val obj = jsonNodeFactory.objectNode
            fieldCodings foreachResult { fc =>
                for {
                    value   <- getField(fc, in)
                    encoded <- fc.coder.forceEncodeAvroDefaultJson(classLoader, value.getOrElse(null))
                } yield
                    obj.put(fc.name, encoded)
            } then Okay(obj)
        }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case m: JavaMap[_, _] => instantiateFromMongoDB(classLoader, m.asInstanceOf[JavaMap[String, AnyRef]])
            case null             => FailedG("required but missing", Nil)
            case _                => FailedG("not an object", Nil)
        }

    private def instantiateFromMongoDB(classLoader: ClassLoader, in: JavaMap[String, AnyRef]): CoderResult[T] = {
        def fetchAndDecode(fc: FieldCoding): CoderResult[AnyRef] =
            atProperty(fc.name) {
                val v = if (shouldFlatten(fc.coder)) in else in.get(fc.name)
                fc.coder.decodeMongoDB(classLoader, v) map { _.asInstanceOf[AnyRef] }
            }

        for {
            constructorArgs <- constructorFieldNames.mapResult { name =>
                fieldCodingByName.get(name) match {
                    case Some(fieldCoding) => fetchAndDecode(fieldCoding)
                    case None              => FailedG("property \"" + name + "\" specified as a constructor property could not be found in field codings", Nil)
                }
            }

            instance <- catchingCoderException(Okay(constructor.newInstance(constructorArgs: _*)))

            nonConstructorFieldsSetOk <- nonConstructorFields mapResult {
                fieldCoding => setField(fieldCoding, instance, fetchAndDecode(fieldCoding))
            }
        } yield instance
    }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        /*
         * Short circuit if we're given something that shouldn't be allowed. even without this check most cases would throw
         * ClassCastException or IllegalArgumentException or similar when the getters are invoked on the wrong type, but this
         * does it earlier and ensures that even objects with no fields fail properly.
         */
        if (!clazz.isInstance(in)) {
            val isA = if (in == null) "null" else in.asInstanceOf[AnyRef].getClass.getName
            FailedG("expected a " + clazz.getName + " not a " + isA, Nil)
        } else {
            val obj = new BasicDBObject
            fieldCodings.foreachResult { fc =>
                for {
                    v       <- getField(fc, in)
                    encoded <- fc.coder.forceEncodeMongoDB(classLoader, v getOrElse null)
                } yield obj.put(fc.name, encoded)
            } then Okay(obj)
        }

    /** Helper method that copies this ObjectCoder, but applies some mapping PF to the field coders */
    def modify(flatten: Boolean, f: PartialFunction[FieldCoding, Option[FieldCoding]]): ObjectCoder[T] =
        this.copy (
            flatten = flatten,
            fieldCodings = fieldCodings.map { in => if (f.isDefinedAt(in)) f(in) else Some(in) }.flatten
        )

    override def toString =
        "ObjectCoder(" + clazz.getName + ", " + constructor + ", " + constructorFieldNames.mkString("[", ", ", "]") + ", " +
            fieldCodings.map { case FieldCoding(name, coder, _, _) => name + ": " + coder }.mkString("{", ", ", "}") + ")"
}
