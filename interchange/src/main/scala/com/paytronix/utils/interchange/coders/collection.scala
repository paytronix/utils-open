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

import java.util.{Arrays, ArrayList => JavaArrayList, Collection => JavaCollection, HashMap => JavaHashMap, List => JavaList, Map => JavaMap}
import scala.annotation.tailrec
import scala.collection.JavaConverters.{collectionAsScalaIterableConverter, mapAsScalaMapConverter}
import scala.collection.generic.{CanBuild, CanBuildFrom}
import scala.collection.mutable

import com.mongodb.{BasicDBObject, DBObject}
import net.liftweb.json.JsonAST.{JArray, JField, JNothing, JNull, JObject, JString, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.bson.BSONObject
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.JsonNodeFactory.{instance => jsonNodeFactory}

import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, iterableResultOps, parameter, tryCatch}

// Arrays in Java are freaking crazy. Shelving this for the moment.

// /** Map an  of some type to a JArray */
// class ArrayCoder[T](component: ComposableCoder[T]) extends ComposableCoder[Array[T]] {
//     def decode(classLoader: ClassLoader, in: JValue) = in match {
//         case JArray(jvalues) => for (values <- jvalues.mapResult[T, Array[T]](component.decode _)) yield Okay(Array(values: _*))
//         case _ => Failed("not a JArray")
//     }
//
//     def encode(classLoader: ClassLoader, in: Array[T]) = for (jvalues <- in mapResult (component.encode [_)) yield Okay(JArray(jvalues toList))
//
//     def toString = "Array[" + component + "]"
// }

abstract class ListLikeCoder[Elem, Coll](valueCoder: ComposableCoder[Elem]) extends ComposableCoder[Coll] {
    import ComposableCoder.{CoderResult, atIndex, catchingCoderException}

    protected def builder(): mutable.Builder[Elem, Coll]
    protected def valueAsIterable(in: Coll): Iterable[Elem]

    protected def makeFromValues(classLoader: ClassLoader, in: Iterable[JValue]): CoderResult[Coll] = {
        val b = builder()
        b.sizeHint(in)
        in.zipWithIndex.foreachResult {
            case (v, i) => atIndex(i)(valueCoder.decode(classLoader, v)).map(b += _)
        } then Okay(b.result())
    }

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JObject(fields) => (
                fields
                .mapResult(field => tryCatch.value(field.name.toInt).map(num => (num, field.value)))
                .orElse(parameter(Nil))
                .flatMap(pairs => makeFromValues(classLoader, pairs.sortBy(_._1).map(_._2)))
            )

            case JArray(elems)  => makeFromValues(classLoader, elems)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not an array", Nil)
        }

    def encode(classLoader: ClassLoader, in: Coll) =
        valueAsIterable(in).view.zipWithIndex.mapResult {
            case (v, i) => atIndex(i)(valueCoder.encode(classLoader, v))
        }.map(jvalues => JArray(jvalues.toList))

    lazy val avroSchema = (Schema.createArray(valueCoder.avroSchema._1), None)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            val b = builder()
            var index = 0

            @tailrec def accumulate(limit: Long): CoderResult[Unit] =
                if (limit == 0) Okay(())
                else {
                    var l: Long = 0
                    while (l < limit) {
                        atIndex(index)(valueCoder.decodeAvro(classLoader, in)) match {
                            case Okay(v) => b += v
                            case failed: FailedG[_] => return failed
                        }
                        index += 1
                        l += 1
                    }
                    accumulate(in.arrayNext())
                }

            val initial = in.readArrayStart()
            b.sizeHint(initial match {
                case l if l > Integer.MAX_VALUE => Integer.MAX_VALUE
                case l                          => l.asInstanceOf[Int]
            })
            accumulate(initial) then Okay(b.result())
        }

    def encodeAvro(classLoader: ClassLoader, in: Coll, out: Encoder) =
        catchingCoderException {
            out.writeArrayStart()
            val it = valueAsIterable(in)
            var index = 0
            out.setItemCount(it.size)
            it.foreachResult { v =>
                out.startItem()
                atIndex(index) {
                    index += 1
                    valueCoder.encodeAvro(classLoader, v, out)
                }
            } then Okay(out.writeArrayEnd())
        }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Coll) = {
        val array = jsonNodeFactory.arrayNode
        valueAsIterable(in).foreachResult { v =>
            valueCoder.encodeAvroDefaultJson(classLoader, v).map(array.add)
        } then Okay(array)
    }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case coll: JavaCollection[_] => {
                val b = builder()
                coll.asScala foreachResult (
                    v => valueCoder.decodeMongoDB(classLoader, v.asInstanceOf[AnyRef]).map(b += _)
                ) then Okay(b.result())
            }
            case null => FailedG("required but missing", Nil)
            case _    => FailedG("not a collection", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Coll) = {
        val iterable = valueAsIterable(in)
        val a = new java.util.ArrayList[AnyRef](iterable.size)
        iterable.foreachResult (
            v => valueCoder.encodeMongoDB(classLoader, v).map(a.add(_))
        ) then Okay(a)
    }
}


/** Coder that can encode and decode collections that conform to the standard scala collection library interfaces */
case class CollectionCoder[Elem, Coll](valueCoder: ComposableCoder[Elem])(implicit cb: CanBuild[Elem, Coll], ev: Coll => Iterable[Elem], m: Manifest[Coll])
extends ListLikeCoder[Elem, Coll](valueCoder) {
    val mostSpecificClass = m.erasure.asInstanceOf[Class[Coll]]

    protected def builder() = cb()
    protected def valueAsIterable(in: Coll) = in
}

/** CollectionCoder preconfigured to build and decompose java.util.Lists. Builds ArrayLists when decoding. */
case class JavaListCoder[T](valueCoder: ComposableCoder[T]) extends ListLikeCoder[T, JavaList[T]](valueCoder)
{
    val mostSpecificClass = classOf[JavaList[T]]

    protected def builder() = new mutable.Builder[T, JavaList[T]] {
        val jl = new JavaArrayList[T]
        def clear() = jl.clear()
        def result() = jl
        def += (v: T) = { jl.add(v); this }
    }

    protected def valueAsIterable(in: JavaList[T]): Iterable[T] = in.asScala
}

object ScalaListCoder {
    /** Make a CollectionCoder preconfigured to build and decompose scala Lists. */
    def apply[T](valueCoder: ComposableCoder[T]): CollectionCoder[T, List[T]] =
        CollectionCoder(valueCoder) (
            implicitly[CanBuild[T, List[T]]],
            implicitly[List[T] => Iterable[T]],
            implicitly[Manifest[List[_]]].asInstanceOf[Manifest[List[T]]]
        )
}

case class ScalaSeqCoder[T](valueCoder: ComposableCoder[T]) extends ListLikeCoder[T, Seq[T]](valueCoder)
{
    val mostSpecificClass = classOf[Seq[T]]

    protected def builder() = {
        val cb = implicitly[CanBuild[T, Vector[T]]]
        cb()
    }
    protected def valueAsIterable(in: Seq[T]) = in
}

object ScalaImmutableSetCoder {
    /** Make a CollectionCoder preconfigured to build and decompose immutable scala Sets. */
    def apply[T](valueCoder: ComposableCoder[T]): CollectionCoder[T, Set[T]] =
        CollectionCoder(valueCoder) (
            implicitly[CanBuild[T, Set[T]]],
            implicitly[Set[T] => Iterable[T]],
            implicitly[Manifest[Set[_]]].asInstanceOf[Manifest[Set[T]]]
        )
}

object ScalaMutableSetCoder {
    /** Make a CollectionCoder preconfigured to build and decompose mutable scala Sets. */
    def apply[T](valueCoder: ComposableCoder[T]): CollectionCoder[T, mutable.Set[T]] =
        CollectionCoder(valueCoder) (
            implicitly[CanBuild[T, mutable.Set[T]]],
            implicitly[mutable.Set[T] => Iterable[T]],
            implicitly[Manifest[mutable.Set[_]]].asInstanceOf[Manifest[mutable.Set[T]]]
        )
}

/**
 * Abstract coder that codes Map-like things, automatically deciding whether to encode as a { key: value } or [ { key: "key", value: "value" } ]
 * by checking whether the key coder implements StringSafeCoder.
 */
abstract class MapLikeCoder[K, V, Coll](keyCoder: ComposableCoder[K], valueCoder: ComposableCoder[V]) extends ComposableCoder[Coll] {
    import ComposableCoder.{CoderResult, atIndex, atProperty, catchingCoderException}

    protected def builder(): mutable.Builder[(K, V), Coll]
    protected def valueAsIterable(in: Coll): Iterable[(K, V)]

    def decode(classLoader: ClassLoader, in: JValue) = {
        val b = builder()

        def decodePairs(in: List[JValue]): CoderResult[Unit] =
            in.zipWithIndex.foreachResult {
                case (jobj: JObject, i) =>
                    (jobj \ "key", jobj \ "value") match {
                        case (JNothing, _) => atIndex(i)(FailedG("Missing \"key\" -- expected { key: ..., value: ... }", Nil))
                        case (_, JNothing) => atIndex(i)(FailedG("Missing \"value\" -- expected { key: ..., value: ... }", Nil))
                        case (keyJValue, valueJValue) =>
                            for {
                                key <- atIndex(i)(atProperty("key")(keyCoder.decode(classLoader, keyJValue)))
                                value <- atIndex(keyJValue)(valueCoder.decode(classLoader, valueJValue))
                            } yield b += ((key, value))
                    }

                case (JArray(keyJValue :: valueJValue :: Nil), i) =>
                    for {
                        key <- atIndex(i)(atIndex(0)(keyCoder.decode(classLoader, keyJValue)))
                        value <- atIndex(keyJValue)(valueCoder.decode(classLoader, valueJValue))
                    } yield b += ((key, value))

                case _ => FailedG("Expected [key, value] or { key: ..., value: ... }", Nil)
            }

        def decodeFields(in: List[JField]): CoderResult[Unit] =
            in.foreachResult(field => {
                val keyJValue = JString(field.name)
                for {
                    key <- atIndex(keyJValue)(atProperty("<key>")(keyCoder.decode(classLoader, JString(field.name))))
                    value <- atIndex(keyJValue)(valueCoder.decode(classLoader, field.value))
                } yield b += ((key, value))
            })

        in match {
            case JArray(jvalues) =>
                b.sizeHint(jvalues.size)
                decodePairs(jvalues) then Okay(b.result())

            case JObject(jfields) =>
                b.sizeHint(jfields.size)
                decodeFields(jfields) then Okay(b.result())

            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not an object or array", Nil)
        }
    }


    def encode(classLoader: ClassLoader, in: Coll) = {
        def encodePairs(in: Iterable[(K, V)]): CoderResult[List[JObject]] =
            in.mapResult {
                case (key, value) =>
                    for {
                        keyJValue <- atIndex(JString(key.toString))(keyCoder.encode(classLoader, key))
                        valueJValue <- atIndex(keyJValue)(valueCoder.encode(classLoader, value))
                    } yield JObject(List(JField("key", keyJValue), JField("value", valueJValue)))
            }

        def encodeFields(in: Iterable[(K, V)]): CoderResult[List[JField]] =
            in.mapResult {
                case (key, value) =>
                    for {
                        keyString <- atIndex(JString(key.toString))(keyCoder.asInstanceOf[StringSafeCoder[K]].encodeString(classLoader, key))
                        valueJValue <- atIndex(JString(keyString))(valueCoder.encode(classLoader, value))
                    } yield JField(keyString, valueJValue)
            }

        keyCoder match {
            case _: StringSafeCoder[_] => encodeFields(valueAsIterable(in)).map(fields => JObject(fields))
            case _                     => encodePairs(valueAsIterable(in)).map(pairs => JArray(pairs))
        }
    }

    lazy val avroSchema = keyCoder match {
        case _: StringSafeCoder[_] => (Schema.createMap(valueCoder.avroSchema._1), None)
        case _ => Schema.createArray {
            import AvroUtils.encodeSchemaName
            val recordName = "kvpair__" + encodeSchemaName(keyCoder.avroSchema._1) + encodeSchemaName(valueCoder.avroSchema._1)
            val pairSchema = Schema.createRecord(recordName, "", "", false)
            pairSchema.setFields(Arrays.asList (
                AvroUtils.makeField("key",   keyCoder.avroSchema),
                AvroUtils.makeField("value", valueCoder.avroSchema)
            ))
            pairSchema
        } -> None
    }

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = {
        val b = builder()

        def decodeAvroMap(keyStringCoder: StringSafeCoder[K]): CoderResult[Unit] = {
            @tailrec def accumulate(limit: Long): CoderResult[Unit] =
                if (limit == 0) Okay(())
                else {
                    var l: Long = 0
                    while (l < limit) {
                        (
                            for {
                                k <- keyStringCoder.decodeString(classLoader, in.readString(null).toString)
                                v <- valueCoder.decodeAvro(classLoader, in)
                            } yield (k, v)
                        ) match {
                            case Okay(p) => b += p
                            case failed: FailedG[_] => return failed
                        }
                        l += 1
                    }
                    accumulate(in.mapNext())
                }

            val initial = in.readMapStart()
            b.sizeHint(initial match {
                case l if l > Integer.MAX_VALUE => Integer.MAX_VALUE
                case l => l.asInstanceOf[Int]
            })

            accumulate(initial)
        }

        def decodeAvroArray(): CoderResult[Unit] = {
            @tailrec def accumulate(limit: Long): CoderResult[Unit] =
                if (limit == 0) Okay(())
                else {
                    var l: Long = 0
                    while (l < limit) {
                        (
                            // FIXME doesn't bother to handle variances in the schema (does not use in.readFieldOrder())
                            for {
                                k <- keyCoder.decodeAvro(classLoader, in)
                                v <- valueCoder.decodeAvro(classLoader, in)
                            } yield (k, v)
                        ) match {
                            case Okay(p) => b += p
                            case failed: FailedG[_] => return failed
                        }
                        l += 1
                    }
                    accumulate(in.arrayNext())
                }

            val initial = in.readArrayStart()
            b.sizeHint(initial match {
                case l if l > Integer.MAX_VALUE => Integer.MAX_VALUE
                case l => l.asInstanceOf[Int]
            })

            accumulate(initial)
        }

        catchingCoderException {
            (keyCoder match {
                case ssc: StringSafeCoder[_] => decodeAvroMap(ssc.asInstanceOf[StringSafeCoder[K]])
                case _ => decodeAvroArray()
            }) then Okay(b.result())
        }
    }


    def encodeAvro(classLoader: ClassLoader, in: Coll, out: Encoder) = {
        def encodeAvroMap(in: Iterable[(K, V)], keyStringCoder: StringSafeCoder[K]): CoderResult[Unit] = {
            out.writeMapStart()
            out.setItemCount(in.size)
            in.foreachResult { case (k, v) =>
                out.startItem()
                for {
                    keyOk <- keyStringCoder.encodeString(classLoader, k).map(out.writeString)
                    valueOk <- valueCoder.encodeAvro(classLoader, v, out)
                } yield ()
            } then Okay(out.writeMapEnd())
        }

        def encodeAvroArray(in: Iterable[(K, V)]): CoderResult[Unit] = {
            out.writeArrayStart()
            out.setItemCount(in.size)
            in.foreachResult { case (k, v) =>
                out.startItem()
                for {
                    keyOk <- keyCoder.encodeAvro(classLoader, k, out)
                    valueOk <- valueCoder.encodeAvro(classLoader, v, out)
                } yield ()
            } then Okay(out.writeArrayEnd())
        }

        catchingCoderException {
            keyCoder match {
                case ssc: StringSafeCoder[_] =>
                    encodeAvroMap(valueAsIterable(in), ssc.asInstanceOf[StringSafeCoder[K]])
                case _ =>
                    encodeAvroArray(valueAsIterable(in))
            }
        }
    }

    def encodeAvroDefaultJson(classLoader: ClassLoader, in: Coll) = {
        def encodeAvroMap(in: Iterable[(K, V)], keyStringCoder: StringSafeCoder[K]): CoderResult[JsonNode] = {
            val obj = jsonNodeFactory.objectNode
            in.foreachResult { case (k, v) =>
                for {
                    key <- keyStringCoder.encodeString(classLoader, k)
                    value <- valueCoder.encodeAvroDefaultJson(classLoader, v)
                } yield obj.put(key, value)
            } then Okay(obj)
        }

        def encodeAvroArray(in: Iterable[(K, V)]): CoderResult[JsonNode] = {
            val array = jsonNodeFactory.arrayNode
            in.foreachResult { case (k, v) =>
                for {
                    key <- keyCoder.encodeAvroDefaultJson(classLoader, k)
                    value <- valueCoder.encodeAvroDefaultJson(classLoader, v)
                } yield {
                    val item = jsonNodeFactory.objectNode
                    item.put("key", key)
                    item.put("value", value)
                    array.add(item)
                }
            } then Okay(array)
        }

        keyCoder match {
            case ssc: StringSafeCoder[_] =>
                encodeAvroMap(valueAsIterable(in), ssc.asInstanceOf[StringSafeCoder[K]])
            case _ =>
                encodeAvroArray(valueAsIterable(in))
        }
    }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        catchingCoderException {
            in match {
                case m: JavaMap[_, _] if keyCoder.isInstanceOf[StringSafeCoder[_]] => {
                    val ssc = keyCoder.asInstanceOf[StringSafeCoder[K]]
                    val b = builder()
                    b.sizeHint(m.size)
                    m.asInstanceOf[JavaMap[String, AnyRef]].asScala foreachResult {
                        case (ke, ve) =>
                            for {
                                kes <- Result(ke).asA[String] | parameter(Nil)
                                kd <- atIndex(JString(ke))(ssc.decodeString(classLoader, kes))
                                vd <- atIndex(JString(ke))(valueCoder.decodeMongoDB(classLoader, ve))
                            } yield b += (kd -> vd)
                    } then Okay(b.result())
                }

                case coll: JavaCollection[_] => {
                    val b = builder()
                    b.sizeHint(coll.size)
                    coll.asScala.zipWithIndex.foreachResult {
                        case (obj: BSONObject, i) =>
                            for {
                                ke <- atIndex(i)(Result(obj.get("key"))   | "Missing \"key\" -- expected { key: ..., value: ... }" | parameter(Nil))
                                ve <- atIndex(i)(Result(obj.get("value")) | "Missing \"value\" -- expected { key: ..., value: ... }" | parameter(Nil))
                                kd <- atIndex(i)(atProperty("key")(keyCoder.decodeMongoDB(classLoader, ke)))
                                vd <- atIndex(i)(atProperty("value")(valueCoder.decodeMongoDB(classLoader, ve)))
                            } yield b += (kd -> vd)

                        case (pair: JavaCollection[_], i) if coll.size == 2 => {
                            val iter = pair.asInstanceOf[JavaCollection[AnyRef]].iterator()
                            val ke = iter.next()
                            val ve = iter.next()
                            for {
                                kd <- atIndex(i)(atIndex(0)(keyCoder.decodeMongoDB(classLoader, ke)))
                                vd <- atIndex(i)(atIndex(1)(valueCoder.decodeMongoDB(classLoader, ve)))
                            } yield b += (kd -> vd)
                        }

                        case _ => FailedG("Expected [key, value] or { \"key\": ..., \"value\": ... }", Nil)
                    } then Okay(b.result())
                }

                case _ => FailedG("Expected either a map or an array of pairs", Nil)
            }
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Coll) =
        /*
            2012-06-02 RMM: never encode as a BasicDBObject even for string keys because Mongo has additional requirements
                            on keys (such as no periods) that can't be statically ensured.

        if (keyCoder.isInstanceOf[StringSafeCoder[_]]) {
            val ssc = keyCoder.asInstanceOf[StringSafeCoder[K]]
            val obj = new BasicDBObject
            valueAsIterable(in).zipWithIndex.foreachResult {
                case ((kd, vd), i) =>
                    for {
                        ke <- atIndex(i)(ssc.encodeString(classLoader, kd))
                        ve <- atIndex(JString(ke))(valueCoder.encodeMongoDB(classLoader, vd))
                    } yield obj.put(ke, ve)
            } then Okay(obj)
        } else {
        */
            valueAsIterable(in).zipWithIndex mapResult {
                case ((kd, vd), i) =>
                    for {
                        ke <- atIndex(i)(keyCoder.encodeMongoDB(classLoader, kd))
                        ve <- atIndex(JString(ke.toString))(valueCoder.encodeMongoDB(classLoader, vd))
                    } yield {
                        val obj = new BasicDBObject
                        obj.put("key", ke)
                        obj.put("value", ve)
                        obj
                    }
            } map { seq =>
                // .asJava doesn't work here because for some reason the Mongo driver inserts it as [ [ contents ] ], not [ contents ]
                val result = new java.util.ArrayList[DBObject]
                seq.foreach(result.add)
                result
            }
        //}
}

/** Coder for mutable JavaMaps. Creates HashMaps on decoding */
case class JavaMapCoder[K, V](keyCoder: ComposableCoder[K], valueCoder: ComposableCoder[V])
    extends MapLikeCoder[K, V, JavaMap[K, V]](keyCoder, valueCoder)
{
    val mostSpecificClass = classOf[JavaMap[K, V]]

    protected def builder() = new mutable.Builder[(K, V), JavaMap[K, V]] {
        val jm = new JavaHashMap[K, V]
        def clear() = jm.clear()
        def result() = jm
        def += (p: (K, V)) = { jm.put(p._1, p._2); this }
    }

    protected def valueAsIterable(in: JavaMap[K, V]): Iterable[(K, V)] = in.asScala
}

/** Coder for scala.collection.Maps */
case class ScalaImmutableMapCoder[K, V](keyCoder: ComposableCoder[K], valueCoder: ComposableCoder[V])
     extends MapLikeCoder[K, V, Map[K, V]](keyCoder, valueCoder)
{
    val mostSpecificClass = classOf[Map[K, V]]

    protected def builder() = implicitly[CanBuildFrom[Nothing, (K, V), Map[K, V]]].apply()
    protected def valueAsIterable(in: Map[K, V]): Iterable[(K, V)] = in
}

/** Coder for scala.collection.mutable.Maps */
case class ScalaMutableMapCoder[K, V](keyCoder: ComposableCoder[K], valueCoder: ComposableCoder[V])
     extends MapLikeCoder[K, V, mutable.Map[K, V]](keyCoder, valueCoder)
{
    val mostSpecificClass = classOf[mutable.Map[K, V]]

    protected def builder() = implicitly[CanBuildFrom[Nothing, (K, V), mutable.Map[K, V]]].apply()
    protected def valueAsIterable(in: mutable.Map[K, V]): Iterable[(K, V)] = in
}
