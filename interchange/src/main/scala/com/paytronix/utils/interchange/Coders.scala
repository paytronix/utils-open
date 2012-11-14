//
// Copyright 2010-2012 Paytronix Systems, Inc.
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

import java.io.{ByteArrayOutputStream, InputStream, IOException, OutputStream}
import java.lang.reflect.{Constructor, Method}
import java.math.BigInteger
import java.nio.ByteBuffer
import java.util.{
    Arrays,
    ArrayList  => JavaArrayList,
    Collection => JavaCollection,
    Date       => JavaDate,
    HashMap    => JavaHashMap,
    List       => JavaList,
    Map        => JavaMap
}
import javax.xml.bind.DatatypeConverter
import scala.annotation.tailrec
import scala.collection.JavaConverters.{iterableAsScalaIterableConverter, mapAsScalaMapConverter, seqAsJavaListConverter}
import scala.collection.generic.{CanBuild, CanBuildFrom}
import scala.collection.immutable.{Map => ImmutableMap, Set => ImmutableSet}
import scala.collection.mutable.{ArrayBuffer, Buffer, Builder, Map => MutableMap, Set => MutableSet}
import scala.reflect.Manifest
import com.mongodb.{BasicDBObject, DBObject}
import net.liftweb.json.Implicits.string2jvalue
import net.liftweb.json.JsonAST.{JArray, JBool, JDouble, JField, JInt, JNothing, JNull, JObject, JString, JValue, render}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.Printer.compact
import org.apache.avro.Schema
import org.apache.avro.io.{DatumReader, DatumWriter, Decoder, DecoderFactory, Encoder, EncoderFactory, ResolvingDecoder}
import org.bson.{BSON, BSONObject}
import org.bson.types.{Binary, ObjectId}
import org.joda.time.{DateTime, Duration, LocalDate, LocalDateTime, LocalTime}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.slf4j.LoggerFactory
import com.paytronix.utils.extendedreflection
import com.paytronix.utils.scala.concurrent.ThreadLocal
import com.paytronix.utils.scala.log.resultLoggerOps
import com.paytronix.utils.scala.reflection.{classByName, paranamer, splitFullyQualifiedName}
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, ResultG, cast, firstOrLastG, iterableResultOps, optionOps, parameter, tryCatch, tryCatching}


/** Object that contains thread-local settings for coding and decoding */
object CoderSettings {
    /**
     * Unless specifically forced by a particular coding, whether to hide Faileds (or Failures, for Box support).
     * Usually failures are hidden for REST calls, and not for interserver calls.
     * Defaults to false.
     */
    object hideFailures extends ThreadLocal[Boolean] {
        val initial = false
    }

    /**
     * Whether or not decoding is happening in an insecure context presently.
     * Used to trigger errors on InsecureCoders.
     * Defaults to false.
     */
    object isInsecureContext extends ThreadLocal[Boolean] {
        val initial = false
    }

    /** Reset the coder settings to default, usually for tests */
    def reset: Unit = {
        hideFailures.reset
        isInsecureContext.reset
    }

    reset
}


/** Frontend object that wraps a ComposableCoder that does the real work but formats error messages on the way out */
final case class Coder[T](contextClassLoader: ClassLoader, implementation: ComposableCoder[T]) {
    import ComposableCoder.formatFailedPath

    /**
    * Unsafe coercion to another type. Use this instead of asInstanceOf[Coder[T]] because it's safer (e.g.
    * okayCoder.asInstanceOf[Coder[U]] will compile but always fail)
    */
    def asCoderFor[U]: Coder[U] = this.asInstanceOf[Coder[U]]

    /** Attempt conversion of a JValue to a value of the mapped type */
    def decode(in: JValue): Result[T] = formatFailedPath(implementation.decode(contextClassLoader, in))

    /** Attempt conversion of the mapped type into a JValue */
    def encode(in: T): Result[JValue] = formatFailedPath(implementation.encode(contextClassLoader, in))

    /** Attempt conversion of some value which does not conform to type T into a JValue. Will fail from either an encoding problem or a type mismatch */
    def forceEncode(in: Any): Result[JValue] = formatFailedPath(implementation.forceEncode(contextClassLoader, in))

    /** Attempt conversion of a byte array to a value of the mapped type */
    def decodeAvro(writerSchema: Schema, in: Array[Byte]): Result[T] = decodeAvro(writerSchema, in, 0, in.length)

    /** Attempt conversion of a byte array to a value of the mapped type */
    def decodeAvro(writerSchema: Schema, in: Array[Byte], offset: Int, length: Int): Result[T] =
        for {
            decoder <- tryCatch.value(DecoderFactory.get.binaryDecoder(in, offset, length, null)) | "Failed to create Avro decoder"
            result  <- decodeAvro(writerSchema, decoder)
        } yield result

    /** Attempt conversion of an input stream to a value of the mapped type  */
    def decodeAvro(writerSchema: Schema, in: InputStream): Result[T] =
        for {
            decoder <- tryCatch.value(DecoderFactory.get.binaryDecoder(in, null)) | "Failed to create Avro decoder"
            result  <- decodeAvro(writerSchema, decoder)
        } yield result

    /** Attempt decoding from some Avro decoder */
    def decodeAvro(writerSchema: Schema, in: Decoder): Result[T] =
        tryCatch.result {
            val resolver = ResolvingDecoderCache(avroSchema, writerSchema)
            resolver.configure(in)
            val result = formatFailedPath(implementation.decodeAvro(contextClassLoader, resolver))
            resolver.drain()
            result
        } | "failed to decode from byte array"

    /** Attempt conversion of a value of the mapped type to a byte array */
    def encodeAvro(in: T): Result[Array[Byte]] = {
        val baos = new ByteArrayOutputStream
        for {
            encoder   <- tryCatch.value(EncoderFactory.get.directBinaryEncoder(baos, null)) | "Failed to create Avro encoder"
            encodedOk <- encodeAvro(in, encoder)
            flushedOk <- tryCatch.value(encoder.flush) | "Failed to flush Avro encoder"
        } yield baos.toByteArray
    }

    /** Attempt conversion of some value that does not conform to type T to a byte array. Will fail from either an encoding problem or a type mismatch */
    def forceEncodeAvro(in: Any): Result[Array[Byte]] =
        cast(implementation.mostSpecificClass, in).flatMap(encodeAvro)

    /** Attempt encoding of a value to a given output stream */
    def encodeAvro(in: T, out: OutputStream): Result[Unit] =
        for {
            encoder   <- tryCatch.value(EncoderFactory.get.binaryEncoder(out, null)) | "Failed to create Avro encoder"
            encodedOk <- encodeAvro(in, encoder)
            flushedOk <- tryCatch.value(encoder.flush) | "Failed to flush Avro encoder"
        } yield { }

    /** Attempt conversion of some value that does not conform to type T to an output stream. Will fail from either an encoding problem or a type mismatch */
    def forceEncodeAvro(in: Any, out: OutputStream): Result[Unit] =
        cast(implementation.mostSpecificClass, in).flatMap(encodeAvro(_, out))

    /** Attempt encoding of a value to a given encoder */
    def encodeAvro(in: T, out: Encoder): Result[Unit] =
        formatFailedPath(implementation.encodeAvro(contextClassLoader, in, out))

    /** Attempt conversion of some value that does not conform to type T to an Avro encoder. Will fail from either an encoding problem or a type mismatch */
    def forceEncodeAvro(in: Any, out: Encoder): Result[Unit] =
        cast(implementation.mostSpecificClass, in).flatMap(encodeAvro(_, out))

    /** Produce an Avro schema for the encoded value */
    def avroSchema: Schema = implementation.avroSchema

    /** Produce a DatumReader that reads whatever this Coder codes from some Avro decoder */
    def avroDatumReader(writerSchema: Schema = null): DatumReader[T] =
        new DatumReader[T] {
            private val _reader: Schema = avroSchema
            private var _writer: Schema = writerSchema

            def setSchema(writer: Schema): Unit = { _writer = writer }

            @throws(classOf[IOException])
            def read(reuse: T, decoder: Decoder): T = {
                val resolver = ResolvingDecoderCache(_reader, _writer)
                resolver.configure(decoder)
                val result = formatFailedPath(implementation.decodeAvro(contextClassLoader, resolver))
                resolver.drain()
                result.orThrow
            }
        }

    /** Produce a DatumWriter that writes whatever this Coder codes to some Avro encoder */
    lazy val avroDatumWriter: DatumWriter[T] =
        new DatumWriter[T] {
            private val _writer = avroSchema

            def setSchema(schema: Schema) =
                if (_writer == avroSchema) ()
                else sys.error("setSchema called with different schema than this Coder encodes")

            @throws(classOf[IOException])
            def write(in: T, out: Encoder): Unit =
                formatFailedPath(implementation.encodeAvro(contextClassLoader, in, out)).orThrow
        }

    /** Decode a MongoDB value, which can be a DBObject (BSON object) or a number of scalar types */
    def decodeMongoDB(in: AnyRef): Result[T] =
        formatFailedPath(implementation.decodeMongoDB(contextClassLoader, in))

    /** Encode a value into a MongoDB value */
    def encodeMongoDB(in: T): Result[AnyRef] =
        formatFailedPath(implementation.encodeMongoDB(contextClassLoader, in))

    /** Encode a value that isn't known to be the of the appropriate type for this coder into a MongoDB value */
    def forceEncodeMongoDB(in: Any): Result[AnyRef] =
        cast(implementation.mostSpecificClass, in).flatMap(encodeMongoDB)
}


object ComposableCoder {
    type CoderResult[A] = ResultG[FailedPath, A]
    type FailedPath = List[Segment]
    sealed abstract class Segment
    final case class PropertySegment(prop: String) extends Segment { override def toString = "/" + prop }
    final case class IntIndexSegment(idx: Int) extends Segment { override def toString = "[" + idx + "]" }
    final case class JValueIndexSegment(idx: JValue) extends Segment { override def toString = "[" + compact(render(idx)) + "]" }

    /** Convert a Result[A] to CoderResult[A] */
    def atTerminal[A](f: Result[A]): CoderResult[A] = f | parameter(Nil)

    /** Catch an exception in a block yielding CoderResult */
    def catchingCoderException[A](f: => CoderResult[A]): CoderResult[A] =
        atTerminal(tryCatch.value(f)).flatten

    /** Equivalent to trackFailedPath with an IntIndexSegment */
    def atIndex[A](idx: Int)(f: CoderResult[A]): CoderResult[A] = trackFailedPath(IntIndexSegment(idx))(f)

    /** Equivalent to trackFailedPath with a JValueIndexSegment */
    def atIndex[A](idx: JValue)(f: CoderResult[A]): CoderResult[A] = trackFailedPath(JValueIndexSegment(idx))(f)

    /** Equivalent to trackFailedPath with a PropertySegment  */
    def atProperty[A](prop: String)(f: CoderResult[A]): CoderResult[A] = trackFailedPath(PropertySegment(prop))(f)

    /** Add a path segment to a ParamFailed that keeps track of a nested path within JSON, for improved error messaging */
    def trackFailedPath[A](segment: Segment)(f: CoderResult[A]): CoderResult[A] =
        f match {
            case Okay(value)                  => Okay(value)
            case FailedG(throwable, segments) => FailedG(throwable, segment :: segments)
        }

    /** Convert a ParamFailed containing a path back to a "normal" Failed with a prefix to the message */
    def formatFailedPath[A](in: CoderResult[A]): Result[A] = in match {
        case Okay(value) => Okay(value)
        case FailedG(throwable, path) => {
            val formattedPath = path match {
                case PropertySegment(_) :: _ => path.mkString("").substring(1)
                case _ => path.mkString("")
            }
            val msg = throwable.getMessage match {
                case null => throwable.toString
                case s => s
            }
            Failed((if (formattedPath.isEmpty) "" else "At " + formattedPath + ": ") + msg, throwable)
        }
    }
}


/** Abstract superclass of objects that convert to and from JSON representation */
abstract class ComposableCoder[T] {
    import ComposableCoder.{CoderResult, atTerminal}

    type ValueType = T

    /**
     * Class that represents the most specific type of all values coded by this coder.
     * For most coders, this is the type being coded, e.g. classOf[java.lang.Boolean] for BooleanCoder.
     * For coders that encode unions, this is the superclass of the union.
     */
    val mostSpecificClass: Class[T]

    /**
     * Unsafe coercion to another type. Use this instead of asInstanceOf[ComposableCoder[T]] because it's safer
     * (e.g. okayCoder.asInstanceOf[ComposableCoder[U]] will compile but always fail)
     */
    def asCoderFor[U]: ComposableCoder[U] = this.asInstanceOf[ComposableCoder[U]]

    /** Attempt conversion of a JValue to a value of the mapped type */
    def decode(classLoader: ClassLoader, in: JValue): CoderResult[T]

    /** Attempt conversion of the mapped type into a JValue */
    def encode(classLoader: ClassLoader, in: T): CoderResult[JValue]

    /**
     * Force encoding some value, even if it doesn't conform to type T  (yet).
     * Simplifies the numerous cases where a value is obtained from reflection where otherwise the caller would have
     * to pattern match on the coder to capture the type variable and coerce using asInstanceOf.
     */
    def forceEncode(classLoader: ClassLoader, in: Any): CoderResult[JValue] =
        atTerminal(cast(mostSpecificClass, in)) flatMap { encode(classLoader, _) }

    /** Yield the Avro Schema for the JSON this coder produces and consumes */
    val avroSchema: Schema

    /** Attempt to decode from an Avro decoder to a value of the mapped type */
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder): CoderResult[T]

    /** Attempt to encode to an Avro encoder a value of the mapped type */
    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder): CoderResult[Unit]

    /**
     * Force encoding some value to an Avro encoder, even if it doesn't conform to type T  (yet).
     * Simplifies the numerous cases where a value is obtained from reflection where otherwise the caller would have
     * to pattern match on the coder to capture the type variable and coerce using asInstanceOf.
     */
    def forceEncodeAvro(classLoader: ClassLoader, in: Any, out: Encoder): CoderResult[Unit] =
        atTerminal(cast(mostSpecificClass, in)) flatMap { encodeAvro(classLoader, _, out) }

    /** Decode a MongoDB value, which can be a DBObject (BSON object) or a number of scalar types */
    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef): CoderResult[T]

    /** Encode a value into a MongoDB value */
    def encodeMongoDB(classLoader: ClassLoader, in: T): CoderResult[AnyRef]

    /** Encode a value that isn't known to be the of the appropriate type for this coder into a MongoDB value */
    def forceEncodeMongoDB(classLoader: ClassLoader, in: Any): CoderResult[AnyRef] =
        atTerminal(cast(mostSpecificClass, in)) flatMap { encodeMongoDB(classLoader, _) }
}

/** Trait of coders that can encode and decode to plain Strings, for keys of maps and so on */
trait StringSafeCoder[T] extends ComposableCoder[T] {
    import ComposableCoder.CoderResult

    /** Attempt conversion of a String to a value of the mapped type */
    def decodeString(classLoader: ClassLoader, in: String): CoderResult[T]

    /** Attempt conversion of the mapped type into a String */
    def encodeString(classLoader: ClassLoader, in: T): CoderResult[String]
}

/**
 * Trait of coders that code something like an option, where it can be some value or null-ish, and can nest (unlike null)
 * Used to control smart nesting of Option in Option or similar, where the JSON coding needs to be different.
 */
trait OptionLikeCoder[T] extends ComposableCoder[T]

/**
 * Trait of coders that can be specified to "flatten" into an enclosing ObjectCoder or ArgumentArrayCoder.
 *
 * When configured to do so (by flatten being true) an ObjectCoder ignores the field name and instead passes the JObject directly to the flattenable coder,
 * and merges the encoded value from the flattenable coder into the result JObject.
 *
 * For example, take the following:
 *
 *     final case class EnclosingObject(first: EnclosedUnion, second: String)
 *     sealed abstract class EnclosedUnion
 *     final case class AlternativeA(a: String) extends EnclosedUnion
 *     final case class AlternativeB(b: String) extends EnclosedUnion
 *     object EnclosedUnionCoding extends AutomaticUnionCoding[EnclosedUnion] {
 *         alternative[AlternativeA]
 *         alternative[AlternativeB]
 *     }
 *
 * Would cause:
 *     EnclosingObject(AlternativeA("foobar"), "bizbaz")
 * to encode as:
 *     { "first": { "a": "foobar" }, "second": "bizbaz" }
 *
 * since flatten is by default false. However if instead the union coding was defined as:
 *
 *     object EnclosedUnionCoding extends AutomaticUnionCoding[EnclosedUnion] {
 *         flatten = true
 *         alternative[AlternativeA]
 *         alternative[AlternativeB]
 *     }
 *
 * Would cause:
 *     EnclosingObject(AlternativeA("foobar"), "bizbaz")
 * to instead encode as:
 *     { "a": "foobar", "second": "bizbaz" }
 */
trait FlattenableCoder {
    self: ComposableCoder[_] =>

    val flatten: Boolean
}

/** Identity coder (just capture/emit some JValue) */
object JValueCoder extends ComposableCoder[JValue] {
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[JValue]

    def decode(classLoader: ClassLoader, in: JValue) = Okay(in)
    def encode(classLoader: ClassLoader, in: JValue) = Okay(in)

    val avroSchema = Schema.create(Schema.Type.STRING)
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException(Okay(parse(in.readString(null).toString)))

    def encodeAvro(classLoader: ClassLoader, in: JValue, out: Encoder) =
        catchingCoderException(Okay(out.writeString(compact(render(in)))))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (in == null) Okay(JNothing)
        else catchingCoderException(Okay(parse(in.toString)))

    def encodeMongoDB(classLoader: ClassLoader, in: JValue) =
        catchingCoderException(Okay(compact(render(in))))

    override def toString = "JValueCoder"
}

/** Coder for MongoDB ObjectId fields */
object ObjectIdCoder extends StringSafeCoder[ObjectId] {
    val mostSpecificClass = classOf[ObjectId]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s) => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _ => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: ObjectId) = encodeString(classLoader, in).map(JString.apply)

    def decodeString(classLoader: ClassLoader, in: String) = if (ObjectId.isValid(in)) Okay(new ObjectId(in)) else FailedG("not a valid object id", Nil)
    def encodeString(classLoader: ClassLoader, in: ObjectId) = Okay(in.toString)

    val avroSchema = Schema.create(Schema.Type.STRING)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = decodeString(classLoader, in.readString(null).toString)
    def encodeAvro(classLoader: ClassLoader, in: ObjectId, out: Encoder) = encodeString(classLoader, in).map(out.writeString)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case oid: ObjectId => Okay(oid)
            case s: String     => decodeString(classLoader, s)
            case null          => FailedG("required but missing", Nil)
            case _             => FailedG("not a string", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: ObjectId) =
        Okay(in)

    override def toString = "ObjectIdCoder"
}

/** Coder for MongoDB literal DBObjects */
object DBObjectCoder extends ComposableCoder[DBObject] {
    val mostSpecificClass = classOf[DBObject]

    def decode(classLoader: ClassLoader, in: JValue) = FailedG("DBObject not codable in JSON", Nil)
    def encode(classLoader: ClassLoader, in: DBObject) = FailedG("DBObject not codable in JSON", Nil)

    val avroSchema = Schema.create(Schema.Type.NULL)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = FailedG("DBObject not codable in Avro", Nil)
    def encodeAvro(classLoader: ClassLoader, in: DBObject, out: Encoder) = FailedG("DBObject not codable in Avro", Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case dbo: DBObject => Okay(dbo)
            case _             => FailedG("not a DBObject", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: DBObject) =
        Okay(in)

    override def toString = "DBObjectCoder"
}

/** Unit coder, which codes the Unit */
object UnitCoder extends ComposableCoder[Unit] {
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[Unit]

    def decode(classLoader: ClassLoader, in: JValue) = Okay(())
    def encode(classLoader: ClassLoader, in: Unit) = Okay(JNothing)

    val avroSchema = Schema.create(Schema.Type.NULL)
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = catchingCoderException(Okay(in.readNull()))
    def encodeAvro(classLoader: ClassLoader, in: Unit, out: Encoder) = catchingCoderException(Okay(out.writeNull()))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) = if (in == null) Okay(()) else FailedG("input is not null", Nil)
    def encodeMongoDB(classLoader: ClassLoader, in: Unit) = Okay(null)

    override def toString = "UnitCoder"
}

/** Singleton coder, which always encodes as an empty object and always decodes a particular value regardless of the input JSON */
case class SingletonCoder[T](inst: T, encodedFields: List[JField] = Nil) extends ComposableCoder[T]
{
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = inst.asInstanceOf[AnyRef].getClass.asInstanceOf[Class[T]]

    def decode(classLoader: ClassLoader, in: JValue) = Okay(inst)
    def encode(classLoader: ClassLoader, in: T) = Okay(JObject(encodedFields))

    val avroSchema = {
        import AvroUtils.nameAndNamespaceFromClass
        val (namespace, name) = nameAndNamespaceFromClass(inst.asInstanceOf[AnyRef].getClass)
        val s = Schema.createRecord(name, "", namespace, false)
        s.setFields(new java.util.ArrayList())
        s
    }

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = Okay(inst)
    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) = Okay(())

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) = Okay(inst)
    def encodeMongoDB(classLoader: ClassLoader, in: T) = catchingCoderException(Okay(MongoUtils.fromJValue(encodedFields)))
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

    val avroSchema = coder.avroSchema
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        if (CoderSettings.isInsecureContext.get) atTerminal(substitute)
        else coder.decodeAvro(classLoader, in)
    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        if (CoderSettings.isInsecureContext.get) Okay(())
        else coder.encodeAvro(classLoader, in, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (CoderSettings.isInsecureContext.get) atTerminal(substitute)
        else coder.decodeMongoDB(classLoader, in)
    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        if (CoderSettings.isInsecureContext.get) Okay(null)
        else coder.encodeMongoDB(classLoader, in)
}

/**
 * Coder which always returns the given Failed when decoding or encoding is attempted. Usually used in places where a Coder is
 * required, but couldn't be built and there is no acceptable place to give back a Result until later (e.g. during initialization of
 * an object.
 */
case class FailCoder[T](failed: Result[T] = Failed("cannot encode/decode"))(implicit m: Manifest[T]) extends ComposableCoder[T]
{
    import ComposableCoder.atTerminal

    val mostSpecificClass = m.erasure.asInstanceOf[Class[T]]

    def decode(classLoader: ClassLoader, in: JValue) = atTerminal(failed)
    def encode(classLoader: ClassLoader, in: T) = atTerminal(failed.asA[JValue])

    val avroSchema = Schema.create(Schema.Type.NULL)
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) = atTerminal(failed)
    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) = atTerminal(failed.asA[Unit])

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) = atTerminal(failed)
    def encodeMongoDB(classLoader: ClassLoader, in: T) = atTerminal(failed.asA[AnyRef])
}

object FailCoder {
    def unless[T](in: Result[Coder[T]])(implicit m: Manifest[T]): Coder[T] =
        in match {
            case Okay(coder) => coder
            case failed => Coder(getClass.getClassLoader, FailCoder[T](failed.asA[T]))
        }
}

/* ******************************************************************************** */


/** Map a Java BigDecimal to a JString */
object JavaBigDecimalCoder extends StringSafeCoder[java.math.BigDecimal] {
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[java.math.BigDecimal]

    def decode(classLoader: ClassLoader, in: JValue) = in match {
        case JString(s)     => decodeString(classLoader, s)
        case JDouble(d)     => decodeString(classLoader, d.toString)
        case JInt(bi)       => decodeString(classLoader, bi.toString)
        case JNothing|JNull => FailedG("required but missing", Nil)
        case _              => FailedG("not a string", Nil)
    }

    def encode(classLoader: ClassLoader, in: java.math.BigDecimal) = Okay(JString(in.toString))

    def decodeString(classLoader: ClassLoader, in: String) =
        catchingCoderException(Okay(new java.math.BigDecimal(in)))

    def encodeString(classLoader: ClassLoader, in: java.math.BigDecimal) =
        Okay(in.toString)

    val avroSchema = Schema.create(Schema.Type.BYTES)
    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            val byteBuffer = in.readBytes(null)
            if (byteBuffer.remaining < 5)
                FailedG("insufficient number of bytes to represent a BigDecimal - got " +
                        byteBuffer.remaining + " but need at least 5", Nil)
            else {
                val scale = byteBuffer.getInt
                val bytes = Array.ofDim[Byte](byteBuffer.remaining)
                byteBuffer.get(bytes)
                Okay(new java.math.BigDecimal(new BigInteger(bytes), scale))
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: java.math.BigDecimal, out: Encoder) =
        catchingCoderException {
            val bytes = in.unscaledValue.toByteArray
            val byteBuffer = ByteBuffer.allocate(bytes.length + 4)
            byteBuffer.putInt(in.scale)
            byteBuffer.put(bytes).rewind
            out.writeBytes(byteBuffer)
            Okay(())
        }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (in == null) FailedG("required but missing", Nil)
        else decodeString(classLoader, in.toString)

    def encodeMongoDB(classLoader: ClassLoader, in: java.math.BigDecimal) =
        encodeString(classLoader, in)

    override def toString = "JavaBigDecimalCoder"
}

/** Map a Scala BigDecimal to a JString */
object ScalaBigDecimalCoder extends StringSafeCoder[scala.math.BigDecimal] {
    val mostSpecificClass = classOf[scala.math.BigDecimal]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JDouble(d)     => decodeString(classLoader, d.toString)
            case JInt(bi)       => decodeString(classLoader, bi.toString)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: scala.math.BigDecimal) =
        tryCatch.value(JString(in.toString)) | parameter(Nil)

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(scala.math.BigDecimal(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: scala.math.BigDecimal) =
        Okay(in.toString)

    val avroSchema = JavaBigDecimalCoder.avroSchema

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        JavaBigDecimalCoder.decodeAvro(classLoader, in).map(new scala.math.BigDecimal(_))

    def encodeAvro(classLoader: ClassLoader, in: scala.math.BigDecimal, out: Encoder) =
        JavaBigDecimalCoder.encodeAvro(classLoader, in.bigDecimal, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (in == null) FailedG("required but missing", Nil) else decodeString(classLoader, in.toString)

    def encodeMongoDB(classLoader: ClassLoader, in: BigDecimal) =
        encodeString(classLoader, in)

    override def toString = "ScalaBigDecimalCoder"
}

/** Map a BigInt to a JInt */
object BigIntCoder extends StringSafeCoder[BigInt] {
    val mostSpecificClass = classOf[BigInt]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JInt(bi)       => Okay(bi)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: BigInt) =
        Okay(JInt(in))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(BigInt(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: BigInt) =
        tryCatch.value(in.toString) | parameter(Nil)

    val avroSchema = Schema.create(Schema.Type.BYTES)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        BigIntegerCoder.decodeAvro(classLoader, in).map(new BigInt(_))

    def encodeAvro(classLoader: ClassLoader, in: BigInt, out: Encoder) =
        BigIntegerCoder.encodeAvro(classLoader, in.bigInteger, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (in == null) FailedG("required but missing", Nil) else decodeString(classLoader, in.toString)

    def encodeMongoDB(classLoader: ClassLoader, in: BigInt) =
        encodeString(classLoader, in)

    override def toString = "BigIntCoder"
}

/** Map a BigInteger to a JInt */
object BigIntegerCoder extends StringSafeCoder[BigInteger] {
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[BigInteger]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JInt(bi)       => Okay(bi.bigInteger)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: BigInteger) =
        Okay(JInt(new BigInt(in)))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(new BigInteger(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: BigInteger) =
        tryCatch.value(in.toString) | parameter(Nil)

    val avroSchema = Schema.create(Schema.Type.BYTES)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            val byteBuffer = in.readBytes(null)
            if (byteBuffer.remaining < 1)
                FailedG("insufficient number of bytes to represent a BigInteger - got " +
                        byteBuffer.remaining + " but need at least 1", Nil)
            else {
                val bytes = Array.ofDim[Byte](byteBuffer.remaining)
                byteBuffer.get(bytes)
                Okay(new BigInteger(bytes))
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: BigInteger, out: Encoder) =
        tryCatch.value {
            val bytes = in.toByteArray
            val byteBuffer = ByteBuffer.allocate(bytes.length)
            byteBuffer.put(bytes).rewind
            out.writeBytes(byteBuffer)
        } | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        if (in == null) FailedG("required but missing", Nil) else decodeString(classLoader, in.toString)

    def encodeMongoDB(classLoader: ClassLoader, in: BigInteger) =
        encodeString(classLoader, in)

    override def toString = "BigIntegerCoder"
}

/** Map a Boolean to a JBool */
object BooleanCoder extends StringSafeCoder[Boolean] {
    val mostSpecificClass = classOf[java.lang.Boolean].asInstanceOf[Class[Boolean]]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JBool(b)       => Okay(b)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a boolean", Nil)
        }

    def encode(classLoader: ClassLoader, in: Boolean) =
        Okay(JBool(in))

    def decodeString(classLoader: ClassLoader, in: String) =
        in.toLowerCase match {
            case "true"  => Okay(true)
            case "false" => Okay(false)
            case _       => FailedG("not \"true\" or \"false\"", Nil)
        }

    def encodeString(classLoader: ClassLoader, in: Boolean) =
        tryCatch.value(in.toString) | parameter(Nil)

    val avroSchema = Schema.create(Schema.Type.BOOLEAN)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readBoolean()) | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Boolean, out: Encoder) =
        tryCatch.value(out.writeBoolean(in)) | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String            => decodeString(classLoader, s)
            case b: java.lang.Boolean => Okay(b.booleanValue)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not a boolean", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Boolean) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "BooleanCoder"
}

/** Map a Byte to a JInt */
object ByteCoder extends StringSafeCoder[Byte] {
    val mostSpecificClass = classOf[java.lang.Byte].asInstanceOf[Class[Byte]]

    private val upperBound = BigInt(java.lang.Byte.MAX_VALUE.toString)
    private val lowerBound = BigInt(java.lang.Byte.MIN_VALUE.toString)

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)                  => decodeString(classLoader, s)
            case JInt(bi) if bi > upperBound => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case JInt(bi) if bi < lowerBound => FailedG("number out of bounds (minimum is " + upperBound + ")", Nil)
            case JInt(bi)                    => Okay(bi.byteValue)
            case JNothing|JNull              => FailedG("required but missing", Nil)
            case _                           => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: Byte) =
        Okay(JInt(BigInt(in)))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Byte.parseByte(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Byte) =
        tryCatch.value(in.toString) | parameter(Nil)

    val avroSchema = Schema.createFixed("byte", "", "", 1)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](1)
            in.readFixed(bytes, 0, 1)
            bytes(0)
        } | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Byte, out: Encoder) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](1)
            bytes(0) = in
            out.writeFixed(bytes, 0, 1)
        } | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case i: java.lang.Integer if i.intValue  > java.lang.Byte.MAX_VALUE => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case i: java.lang.Integer if i.intValue  < java.lang.Byte.MIN_VALUE => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)
            case l: java.lang.Long    if l.longValue > java.lang.Byte.MAX_VALUE => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case l: java.lang.Long    if l.longValue < java.lang.Byte.MIN_VALUE => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)

            case s: String            => decodeString(classLoader, s)
            case i: java.lang.Integer => Okay(i.byteValue)
            case l: java.lang.Long    => Okay(l.byteValue)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not an integer", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Byte) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "ByteCoder"
}

/** Map a Char to a JString */
object CharCoder extends StringSafeCoder[Char] {
    val mostSpecificClass = classOf[java.lang.Character].asInstanceOf[Class[Char]]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: Char) =
        tryCatch.value(JString(in.toString)) | parameter(Nil)

    def decodeString(classLoader: ClassLoader, in: String) =
        if (in.length == 1) {
            Okay(in.charAt(0))
        } else {
            FailedG("expected a string with exactly one character in it", Nil)
        }

    def encodeString(classLoader: ClassLoader, in: Char) =
        tryCatch.value(in.toString) | parameter(Nil)

    val avroSchema = Schema.createFixed("char", "", "", 2)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](2)
            in.readFixed(bytes, 0, 2)
            ((bytes(0) << 8) | bytes(1)).asInstanceOf[Char]
        } | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Char, out: Encoder) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](2)
            bytes(0) = ((in >>> 8) & 0xff).asInstanceOf[Byte]
            bytes(1) = ((in >>> 0) & 0xff).asInstanceOf[Byte]
            out.writeFixed(bytes, 0, 2)
        } | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String => decodeString(classLoader, s)
            case null      => FailedG("required but missing", Nil)
            case _         => FailedG("not a string", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Char) =
        encodeString(classLoader, in)

    override def toString = "CharCoder"
}

/* Map a Double to a JDouble */
object DoubleCoder extends StringSafeCoder[Double] {
    val mostSpecificClass = classOf[java.lang.Double].asInstanceOf[Class[Double]]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JDouble(d)     => Okay(d)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a double", Nil)
        }

    def encode(classLoader: ClassLoader, in: Double) =
        Okay(JDouble(in))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Double.parseDouble(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Double) =
        tryCatch.value(in.toString) | parameter(Nil)

    val avroSchema = Schema.create(Schema.Type.DOUBLE)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readDouble()) | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Double, out: Encoder) =
        tryCatch.value(out.writeDouble(in)) | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String           => decodeString(classLoader, s)
            case d: java.lang.Double => Okay(d)
            case null                => FailedG("required but missing", Nil)
            case _                   => FailedG("not a double", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Double) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "DoubleCoder"
}

/* Map a Float to a JDouble */
object FloatCoder extends StringSafeCoder[Float] {
    val mostSpecificClass = classOf[java.lang.Float].asInstanceOf[Class[Float]]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JDouble(d)     => Okay(d.floatValue)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a double", Nil)
        }

    def encode(classLoader: ClassLoader, in: Float) =
        Okay(JDouble(in))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Float.parseFloat(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Float) =
        tryCatch.value(in.toString) | parameter(Nil)

    val avroSchema = Schema.create(Schema.Type.FLOAT)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readFloat()) | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Float, out: Encoder) =
        tryCatch.value(out.writeFloat(in)) | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String           => decodeString(classLoader, s)
            case d: java.lang.Double => Okay(d.floatValue)
            case null                => FailedG("required but missing", Nil)
            case _                   => FailedG("not a double", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Float) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "FloatCoder"
}

/** Map an Integer to a JInt */
object IntCoder extends StringSafeCoder[Int] {
    val mostSpecificClass = classOf[java.lang.Integer].asInstanceOf[Class[Int]]

    private val upperBound = BigInt(java.lang.Integer.MAX_VALUE.toString)
    private val lowerBound = BigInt(java.lang.Integer.MIN_VALUE.toString)

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)                  => decodeString(classLoader, s)
            case JInt(bi) if bi > upperBound => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case JInt(bi) if bi < lowerBound => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)
            case JInt(bi)                    => Okay(bi.intValue)
            case JNothing|JNull              => FailedG("required but missing", Nil)
            case _                           => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: Int) =
        Okay(JInt(BigInt(in)))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Integer.parseInt(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Int) =
        tryCatch.value(in.toString) | parameter(Nil)

    val avroSchema = Schema.create(Schema.Type.INT)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readInt()) | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Int, out: Encoder) =
        tryCatch.value(out.writeInt(in)) | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case l: java.lang.Long if l.longValue > java.lang.Integer.MAX_VALUE => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case l: java.lang.Long if l.longValue < java.lang.Integer.MIN_VALUE => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)

            case s: String            => decodeString(classLoader, s)
            case i: java.lang.Integer => Okay(i)
            case l: java.lang.Long    => Okay(l.intValue)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not an integer", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: Int) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "IntCoder"
}

/** Map a Long to a JInt */
object LongCoder extends StringSafeCoder[Long] {
    val mostSpecificClass = classOf[java.lang.Long].asInstanceOf[Class[Long]]

    private val upperBound = BigInt(java.lang.Long.MAX_VALUE.toString)
    private val lowerBound = BigInt(java.lang.Long.MIN_VALUE.toString)

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)                  => decodeString(classLoader, s)
            case JInt(bi) if bi > upperBound => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case JInt(bi) if bi < lowerBound => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)
            case JInt(bi)                    => Okay(bi.longValue)
            case JNothing|JNull              => FailedG("required but missing", Nil)
            case _                           => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: Long) =
        Okay(JInt(BigInt(in)))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Long.parseLong(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Long) =
        tryCatch.value(in.toString) | parameter(Nil)

    val avroSchema = Schema.create(Schema.Type.LONG)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readLong()) | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Long, out: Encoder) =
        tryCatch.value(out.writeLong(in)) | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String            => decodeString(classLoader, s)
            case i: java.lang.Integer => Okay(i.longValue)
            case l: java.lang.Long    => Okay(l)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not an integer", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: Long) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "LongCoder"
}

/** Map a Short to a JInt */
object ShortCoder extends StringSafeCoder[Short] {
    val mostSpecificClass = classOf[java.lang.Short].asInstanceOf[Class[Short]]

    private val upperBound = BigInt(java.lang.Short.MAX_VALUE.toString)
    private val lowerBound = BigInt(java.lang.Short.MIN_VALUE.toString)

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)                  => decodeString(classLoader, s)
            case JInt(bi) if bi > upperBound => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case JInt(bi) if bi < lowerBound => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)
            case JInt(bi)                    => Okay(bi.shortValue)
            case JNothing|JNull              => FailedG("required but missing", Nil)
            case _                           => FailedG("not an integer", Nil)
        }

    def encode(classLoader: ClassLoader, in: Short) =
        Okay(JInt(BigInt(in)))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatch.value(java.lang.Short.parseShort(in)) | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: Short) =
        tryCatch.value(in.toString) | parameter(Nil)

    val avroSchema = Schema.createFixed("short", "", "", 2)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](2)
            in.readFixed(bytes, 0, 2)
            (
                ((bytes(0) << 8) & 0xff00) |
                ( bytes(1)       & 0x00ff)
            ).asInstanceOf[Short]
        } | parameter(Nil)

    def encodeAvro(classLoader: ClassLoader, in: Short, out: Encoder) =
        tryCatch.value {
            val bytes = Array.ofDim[Byte](2)
            bytes(0) = ((in >>> 8) & 0xff).asInstanceOf[Byte]
            bytes(1) = ((in >>> 0) & 0xff).asInstanceOf[Byte]
            out.writeFixed(bytes, 0, 2)
        } | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case i: java.lang.Integer if i > java.lang.Short.MAX_VALUE => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case i: java.lang.Integer if i < java.lang.Short.MIN_VALUE => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)
            case l: java.lang.Long if l > java.lang.Short.MAX_VALUE    => FailedG("number out of bounds (maximum is " + upperBound + ")", Nil)
            case l: java.lang.Long if l < java.lang.Short.MIN_VALUE    => FailedG("number out of bounds (minimum is " + lowerBound + ")", Nil)

            case s: String            => decodeString(classLoader, s)
            case i: java.lang.Integer => Okay(i.shortValue)
            case l: java.lang.Long    => Okay(l.shortValue)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not an integer", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: Short) =
        Okay(in.asInstanceOf[AnyRef])

    override def toString = "ShortCoder"
}


/** Map something that encodes as a string to a JString */
abstract class StringLikeCoder[T] extends StringSafeCoder[T] {
    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T) =
        encodeString(classLoader, in).map(s => JString(s))

    val avroSchema = Schema.create(Schema.Type.STRING)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readString(null).toString()).orElse(parameter(Nil)).flatMap(decodeString(classLoader, _))

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        encodeString(classLoader, in).flatMap { s =>
            if (s == null) FailedG("cannot encode null string", Nil)
            else tryCatch.value(out.writeString(s)) | parameter(Nil)
        }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String => decodeString(classLoader, s)
            case null      => FailedG("required but missing", Nil)
            case _         => FailedG("not a string", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        encodeString(classLoader, in)

    override def toString = "StringCoder"
}

object StringCoder extends StringLikeCoder[String] {
    val mostSpecificClass = classOf[String]

    def decodeString(classLoader: ClassLoader, s: String) = Okay(s)
    def encodeString(classLoader: ClassLoader, s: String) = Okay(s)
}

/** Abstract superclass of coders that code JavaDate or some subclass using SimpleDateFormat */
abstract class DateCoder[T <: JavaDate] extends StringSafeCoder[T] {
    implicit private val logger = LoggerFactory.getLogger(getClass)

    import ComposableCoder.CoderResult
    protected val additionalFormats: List[String] = Nil

    val defaultFormatString: String
    lazy val formats: List[String] = defaultFormatString :: additionalFormats

    protected def createFromDate(in: JavaDate): CoderResult[T] = createFromMillisSinceEpoch(in.getTime)
    protected def createFromMillisSinceEpoch(in: Long): CoderResult[T]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T) =
        formatDate(in).map(JString.apply)

    def decodeString(classLoader: ClassLoader, in: String) =
        for {
            parsed <- firstOrLastG (
                FailedG("incorrectly formatted date -- expected format like  " + formatDate(new JavaDate()).getOrElse("SYSTEM ERROR"), Nil),
                formats
            )(parseDate(in, _))
            instance <- createFromDate(parsed)
        } yield instance

    def encodeString(classLoader: ClassLoader, in: T) =
        formatDate(in)

    private def parseDate(in: String, fmtStr: String) =
        tryCatch.value {
            val dateFormat = new java.text.SimpleDateFormat(fmtStr)
            dateFormat.setLenient(false)
            dateFormat.parse(in)
        } | parameter(Nil)

    private def formatDate(in: JavaDate) =
        tryCatch.value(new java.text.SimpleDateFormat(defaultFormatString).format(in)).logError("Failed to format date: " + String.valueOf(in)) | parameter(Nil)

    val avroSchema = Schema.create(Schema.Type.LONG)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        createFromMillisSinceEpoch(in.readLong())

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) = {
        out.writeLong(in.getTime)
        Okay(())
    }

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case d: JavaDate => createFromDate(d)
            case s: String   => decodeString(classLoader, s)
            case null        => FailedG("required but missing", Nil)
            case _           => FailedG("not a date", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        Okay(in)
}

/** Map a Java Date to a JString */
object JavaDateCoder extends DateCoder[JavaDate] {
    val mostSpecificClass = classOf[JavaDate]

    val defaultFormatString = "yyyy-MM-dd HH:mm:ss Z"
    override val additionalFormats = List("E MMM dd HH:mm:ss Z yyyy", "E, dd MMM yy HH:mm:ss Z")

    override protected def createFromDate(in: JavaDate) =
        Okay(in)

    protected def createFromMillisSinceEpoch(in: Long) =
        Okay(new JavaDate(in))

    override def toString = "JavaDateCoder"
}

/** Map a Java SQL Date to a JString */
object JavaSqlDateCoder extends DateCoder[java.sql.Date] {
    val mostSpecificClass = classOf[java.sql.Date]

    val defaultFormatString = "yyyy-MM-dd"
    override val additionalFormats = List("E MMM dd yyyy", "E, dd MMM yy")

    protected def createFromMillisSinceEpoch(in: Long) =
        Okay(new java.sql.Date(in))

    override def toString = "JavaSqlDateCoder"
}

/** Map a Java SQL Timestamp to a JString */
object JavaSqlTimestampCoder extends DateCoder[java.sql.Timestamp] {
    val mostSpecificClass = classOf[java.sql.Timestamp]

    val defaultFormatString = "yyyy-MM-dd hh:mm:ss.SSS"
    override val additionalFormats = List("yyyy-MM-dd hh:mm:ss")

    protected def createFromMillisSinceEpoch(in: Long) =
        Okay(new java.sql.Timestamp(in))

    override def toString = "JavaSqlTimestampCoder"
}

/** Abstract superclass of coders that code ReadableDateTime or some subclass using DateTimeFormatter */
abstract class JodaDateTimeCoder[T] extends StringSafeCoder[T] {
    import ComposableCoder.{CoderResult, FailedPath}

    protected val additionalFormats: List[String] = Nil

    val defaultFormatString: String
    lazy val formats: List[String] = defaultFormatString :: additionalFormats
    lazy val formatters: List[DateTimeFormatter] = formats map DateTimeFormat.forPattern

    protected def fromDateTime(in: DateTime): T
    protected def toDateTime(in: T): DateTime

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T) =
        formatDate(toDateTime(in)).map(JString.apply)

    def decodeString(classLoader: ClassLoader, in: String) =
        firstOrLastG (
            FailedG("incorrectly formatted date -- expected format like  " + formatDate(new DateTime()), Nil: FailedPath),
            formatters
        )(parseDate(in, _)) map fromDateTime

    def encodeString(classLoader: ClassLoader, in: T) =
        formatDate(toDateTime(in))

    private def parseDate(in: String, formatter: DateTimeFormatter): CoderResult[DateTime] =
        tryCatch.valueG(parameter(Nil))(formatter.parseDateTime(in))

    private def formatDate(in: DateTime): CoderResult[String] =
        tryCatch.valueG(parameter(Nil))(formatters.head.print(in))

    val avroSchema = Schema.create(Schema.Type.LONG)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.valueG(parameter(Nil))(fromDateTime(new DateTime(in.readLong())))

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        Okay(out.writeLong(toDateTime(in).getMillis))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case d: java.util.Date => Okay(fromDateTime(new DateTime(d)))
            case s: String         => decodeString(classLoader, s)
            case null              => FailedG("required but missing", Nil)
            case _                 => FailedG("not a date", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        Okay(toDateTime(in).toDate)
}

object DateTimeCoder extends JodaDateTimeCoder[DateTime] {
    val mostSpecificClass = classOf[DateTime]

    val defaultFormatString = "yyyy-MM-dd HH:mm:ss Z"
    override val additionalFormats = List("E MMM dd HH:mm:ss Z yyyy", "E, dd MMM yy HH:mm:ss Z")

    protected def fromDateTime(in: DateTime) = in
    protected def toDateTime(in: DateTime) = in
}

object LocalDateCoder extends JodaDateTimeCoder[LocalDate] {
    val mostSpecificClass = classOf[LocalDate]

    val defaultFormatString = "yyyy-MM-dd"
    override val additionalFormats = List("E MMM dd yyyy", "E, dd MMM yy")

    protected def fromDateTime(in: DateTime) = in.toLocalDate
    protected def toDateTime(in: LocalDate) = in.toDateTimeAtStartOfDay
}

object LocalDateTimeCoder extends JodaDateTimeCoder[LocalDateTime] {
    val mostSpecificClass = classOf[LocalDateTime]

    val defaultFormatString = "yyyy-MM-dd HH:mm:ss"
    override val additionalFormats = List("E MMM dd HH:mm:ss yyyy", "E, dd MMM yy HH:mm:ss")

    protected def fromDateTime(in: DateTime) = in.toLocalDateTime
    protected def toDateTime(in: LocalDateTime) = in.toDateTime
}

object LocalTimeCoder extends JodaDateTimeCoder[LocalTime] {
    val mostSpecificClass = classOf[LocalTime]

    val defaultFormatString = "yyyy-MM-dd hh:mm:ss.SSS"
    override val additionalFormats = List("yyyy-MM-dd hh:mm:ss")

    protected def fromDateTime(in: DateTime) = in.toLocalTime
    protected def toDateTime(in: LocalTime) = in.toDateTimeToday
}

object DurationCoder extends StringSafeCoder[Duration] {
    val mostSpecificClass = classOf[Duration]

    def decode(classLoader: ClassLoader, in: JValue) =
        LongCoder.decode(classLoader, in) map { l => new Duration(l) }

    def encode(classLoader: ClassLoader, in: Duration) =
        LongCoder.encode(classLoader, in.getMillis)

    def decodeString(classLoader: ClassLoader, in: String) =
        LongCoder.decodeString(classLoader, in) map { l => new Duration(l) }

    def encodeString(classLoader: ClassLoader, in: Duration) =
        LongCoder.encodeString(classLoader, in.getMillis)

    val avroSchema =
        LongCoder.avroSchema

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        LongCoder.decodeAvro(classLoader, in) map { l => new Duration(l) }

    def encodeAvro(classLoader: ClassLoader, in: Duration, out: Encoder) =
        LongCoder.encodeAvro(classLoader, in.getMillis, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        LongCoder.decodeMongoDB(classLoader, in) map { l => new Duration(l) }

    def encodeMongoDB(classLoader: ClassLoader, in: Duration) =
        LongCoder.encodeMongoDB(classLoader, in.getMillis)

    override def toString = "DurationCoder"
}


/* ******************************************************************************** */


object ByteArrayCoder extends StringSafeCoder[Array[Byte]] {
    val mostSpecificClass = classOf[Array[Byte]]

    def decode(classLoader: ClassLoader, in: JValue) =
        ByteBufferCoder.decode(classLoader, in) map { _.array }

    def encode(classLoader: ClassLoader, in: Array[Byte]) =
        ByteBufferCoder.encode(classLoader, ByteBuffer.wrap(in))

    def decodeString(classLoader: ClassLoader, in: String) =
        ByteBufferCoder.decodeString(classLoader, in) map { _.array }

    def encodeString(classLoader: ClassLoader, in: Array[Byte]) =
        ByteBufferCoder.encodeString(classLoader, ByteBuffer.wrap(in))

    val avroSchema =
        ByteBufferCoder.avroSchema

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        ByteBufferCoder.decodeAvro(classLoader, in) map { _.array }

    def encodeAvro(classLoader: ClassLoader, in: Array[Byte], out: Encoder) =
        ByteBufferCoder.encodeAvro(classLoader, ByteBuffer.wrap(in), out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        ByteBufferCoder.decodeMongoDB(classLoader, in) map { _.array }

    def encodeMongoDB(classLoader: ClassLoader, in: Array[Byte]) =
        ByteBufferCoder.encodeMongoDB(classLoader, ByteBuffer.wrap(in))

    override def toString = "ByteArrayCoder"
}

object ByteBufferCoder extends StringSafeCoder[ByteBuffer] {
    import ComposableCoder.atTerminal

    val mostSpecificClass = classOf[ByteBuffer]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: ByteBuffer) =
        encodeString(classLoader, in) map JString.apply

    def decodeString(classLoader: ClassLoader, in: String) =
        atTerminal {
            tryCatch.value {
                ByteBuffer.wrap(DatatypeConverter.parseBase64Binary(in))
            }
        }

    def encodeString(classLoader: ClassLoader, in: ByteBuffer) =
        atTerminal {
            tryCatch.value {
                if (in.hasArray && in.capacity == in.limit && in.position == 0)
                    DatatypeConverter.printBase64Binary(in.array)
                else {
                    // FIXME not the most efficient, better to chop it up into blocks divisible by 3
                    val a = Array.ofDim[Byte](in.remaining)
                    in.get(a)
                    DatatypeConverter.printBase64Binary(a)
                }
            }
        }

    val avroSchema = Schema.create(Schema.Type.BYTES)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        atTerminal(tryCatch.value(in.readBytes(null)))

    def encodeAvro(classLoader: ClassLoader, in: ByteBuffer, out: Encoder) =
        atTerminal(tryCatch.value(out.writeBytes(in)))

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case b: Binary            => Okay(ByteBuffer.wrap(b.getData))
            case s: String            => decodeString(classLoader, s)
            case null                 => FailedG("required but missing", Nil)
            case _                    => FailedG("not a string", Nil)
        }
    def encodeMongoDB(classLoader: ClassLoader, in: ByteBuffer) =
        atTerminal {
            tryCatch.value {
                val a = Array.ofDim[Byte](in.remaining)
                in.get(a)
                new Binary(BSON.BINARY, a)
            }
        }

    override def toString = "ByteBufferCoder"
}


/* ******************************************************************************** */


/** Trait that implements Avro coding of tuples by storing as a record with _1, _2, _3, etc. and mongo coding as an array */
trait TupleCoder {
    self: ComposableCoder[_ <: Product] =>

    import ComposableCoder.{CoderResult, catchingCoderException}

    val arity: Int
    protected val coders: Array[ComposableCoder[AnyRef]]
    protected def makeValue(in: Array[AnyRef]): Result[ValueType]

    lazy val avroSchema = {
        val s = Schema.createRecord("Tuple" + arity + "__" + coders.map(c => AvroUtils.encodeSchemaName(c.avroSchema)).mkString(""), "", "scala.lang", false)
        s.setFields((1 to arity).map(n => new Schema.Field("_" + n, coders(n-1).avroSchema, "", null)).asJava)
        s
    }

    def decodeAvroTuple(classLoader: ClassLoader, in: ResolvingDecoder): CoderResult[ValueType] =
        catchingCoderException {
            val a = Array.ofDim[AnyRef](arity)
            in.readFieldOrder.toSeq.foreachResult (
                f => coders(f.pos).decodeAvro(classLoader, in).map(a(f.pos) = _)
            ) then (makeValue(a) | parameter(Nil))
        }

    def encodeAvroTuple(classLoader: ClassLoader, in: ValueType, out: Encoder): CoderResult[Unit] =
        avroSchema.getFields.asScala.foreachResult (
            f => coders(f.pos).encodeAvro(classLoader, in.productElement(f.pos).asInstanceOf[AnyRef], out) then Okay(())
        )

    def decodeMongoDBTuple(classLoader: ClassLoader, in: AnyRef): CoderResult[ValueType] =
        in match {
            case coll: JavaCollection[_] if coll.size == coders.length => {
                val a = Array.ofDim[AnyRef](arity)
                val iter = coll.iterator()
                (1 to arity) foreachResult (
                    (i: Int) => coders(i).decodeMongoDB(classLoader, iter.next().asInstanceOf[AnyRef]).map(a(i) = _)
                ) then (makeValue(a) | parameter(Nil))
            }

            case coll: JavaCollection[_] => FailedG("expected a collection of " + coders.length + " elements but got " + coll.size + " elements", Nil)
            case null                    => FailedG("required but missing", Nil)
            case _                       => FailedG("not a collection", Nil)
        }

    def encodeMongoDBTuple(classLoader: ClassLoader, in: ValueType): CoderResult[AnyRef] = {
        val a = Array.ofDim[AnyRef](arity)
        (1 to arity) foreachResult (
            (i: Int) => coders(i).encodeMongoDB(classLoader, in.productElement(i).asInstanceOf[AnyRef]).map(a(i) = _)
        ) then Okay(a)
    }
}

/** Map a 1-tuple */
case class Tuple1Coder[A] (
    coder: ComposableCoder[A]
) extends ComposableCoder[Tuple1[A]] with TupleCoder
{
    val mostSpecificClass = classOf[Tuple1[A]]

    val arity = 1

    protected val coders = Array(coder.asCoderFor[AnyRef])

    protected def makeValue(in: Array[AnyRef]) =
        in match {
            case Array(a) => Okay(new Tuple1(a.asInstanceOf[A]))
            case _        => FailedG("expected 1 elements from tuple decode, got " + in.length, Nil)
        }

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JArray(jvalue :: Nil) => coder.decode(classLoader, jvalue).map(new Tuple1(_))
            case JArray(_)             => FailedG("expected an array with exactly one element", Nil)
            case _                     => coder.decode(classLoader, in).map(new Tuple1(_))
        }

    def encode(classLoader: ClassLoader, in: Tuple1[A]) =
        for {
            jvalue <- coder.encode(classLoader,in._1)
        } yield JArray(jvalue :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple1[A], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple1[A]) =
        encodeMongoDBTuple(classLoader, in)
}

/** Map a 2-tuple */
case class Tuple2Coder[A,B] (
    coder1: ComposableCoder[A],
    coder2: ComposableCoder[B]
) extends ComposableCoder[(A,B)] with TupleCoder
{
    val mostSpecificClass = classOf[(A,B)]

    val arity = 2

    protected val coders = Array(coder1.asCoderFor[AnyRef], coder2.asCoderFor[AnyRef])
    protected def makeValue(in: Array[AnyRef]) =
        in match {
            case Array(a, b) => Okay((a.asInstanceOf[A], b.asInstanceOf[B]))
            case _ => Failed("expected 2 elements from tuple decode, got " + in.length)
        }

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JArray(jvalue1 :: jvalue2 :: Nil) =>
                for {
                    value1 <- coder1.decode(classLoader, jvalue1)
                    value2 <- coder2.decode(classLoader, jvalue2)
                } yield (value1, value2)

            case JArray(_) => FailedG("expected an array with exactly two elements", Nil)
            case _         => FailedG("not an array", Nil)
        }

    def encode(classLoader: ClassLoader, in: (A,B)) =
        for {
            jvalue1 <- coder1.encode(classLoader, in._1)
            jvalue2 <- coder2.encode(classLoader, in._2)
        } yield JArray(jvalue1 :: jvalue2 :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple2[A, B], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple2[A, B]) =
        encodeMongoDBTuple(classLoader, in)
}

/** Map a 3-tuple */
case class Tuple3Coder[A,B,C] (
    coder1: ComposableCoder[A],
    coder2: ComposableCoder[B],
    coder3: ComposableCoder[C]
) extends ComposableCoder[(A,B,C)] with TupleCoder
{
    val mostSpecificClass = classOf[(A,B,C)]

    val arity = 3

    protected val coders = Array(coder1.asCoderFor[AnyRef], coder2.asCoderFor[AnyRef], coder3.asCoderFor[AnyRef])
    protected def makeValue(in: Array[AnyRef]) = in match {
        case Array(a, b, c) => Okay((a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C]))
        case _ => FailedG("expected 3 elements from tuple decode, got " + in.length, Nil)
    }

    def decode(classLoader: ClassLoader, in: JValue) = in match {
        case JArray(jvalue1 :: jvalue2 :: jvalue3 :: Nil) =>
            for {
                value1 <- coder1.decode(classLoader, jvalue1)
                value2 <- coder2.decode(classLoader, jvalue2)
                value3 <- coder3.decode(classLoader, jvalue3)
            } yield (value1, value2, value3)

        case JArray(_) => FailedG("expected an array with exactly three elements", Nil)
        case _         => FailedG("not an array", Nil)
    }

    def encode(classLoader: ClassLoader, in: (A,B,C)) =
        for {
            jvalue1 <- coder1.encode(classLoader, in._1)
            jvalue2 <- coder2.encode(classLoader, in._2)
            jvalue3 <- coder3.encode(classLoader, in._3)
        } yield JArray(jvalue1 :: jvalue2 :: jvalue3 :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple3[A, B, C], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple3[A, B, C]) =
        encodeMongoDBTuple(classLoader, in)
}

/** Map a 4-tuple */
case class Tuple4Coder[A,B,C,D] (
    coder1: ComposableCoder[A],
    coder2: ComposableCoder[B],
    coder3: ComposableCoder[C],
    coder4: ComposableCoder[D]
) extends ComposableCoder[(A,B,C,D)] with TupleCoder
{
    val mostSpecificClass = classOf[(A,B,C,D)]

    val arity = 4

    protected val coders =
        Array(coder1.asCoderFor[AnyRef], coder2.asCoderFor[AnyRef], coder3.asCoderFor[AnyRef], coder4.asCoderFor[AnyRef])
    protected def makeValue(in: Array[AnyRef]) = in match {
        case Array(a, b, c, d) => Okay((a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C], d.asInstanceOf[D]))
        case _ => FailedG("expected 4 elements from tuple decode, got " + in.length, Nil)
    }

    def decode(classLoader: ClassLoader, in: JValue) = in match {
        case JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: Nil) =>
            for {
                value1 <- coder1.decode(classLoader, jvalue1)
                value2 <- coder2.decode(classLoader, jvalue2)
                value3 <- coder3.decode(classLoader, jvalue3)
                value4 <- coder4.decode(classLoader, jvalue4)
            } yield (value1, value2, value3, value4)

        case JArray(_) => FailedG("expected an array with exactly four elements", Nil)
        case _         => FailedG("not an array", Nil)
    }

    def encode(classLoader: ClassLoader, in: (A,B,C,D)) =
        for {
            jvalue1 <- coder1.encode(classLoader, in._1)
            jvalue2 <- coder2.encode(classLoader, in._2)
            jvalue3 <- coder3.encode(classLoader, in._3)
            jvalue4 <- coder4.encode(classLoader, in._4)
        } yield JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple4[A, B, C, D], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple4[A, B, C, D]) =
        encodeMongoDBTuple(classLoader, in)
}

/** Map a 5-tuple */
case class Tuple5Coder[A,B,C,D,E] (
    coder1: ComposableCoder[A],
    coder2: ComposableCoder[B],
    coder3: ComposableCoder[C],
    coder4: ComposableCoder[D],
    coder5: ComposableCoder[E]
) extends ComposableCoder[(A,B,C,D,E)] with TupleCoder
{
    val mostSpecificClass = classOf[(A,B,C,D,E)]

    val arity = 5

    protected val coders = Array (
        coder1.asCoderFor[AnyRef],
        coder2.asCoderFor[AnyRef],
        coder3.asCoderFor[AnyRef],
        coder4.asCoderFor[AnyRef],
        coder5.asCoderFor[AnyRef]
    )
    protected def makeValue(in: Array[AnyRef]) = in match {
        case Array(a, b, c, d, e) => Okay((a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C],
                                           d.asInstanceOf[D], e.asInstanceOf[E]))
        case _ => FailedG("expected 5 elements from tuple decode, got " + in.length, Nil)
    }

    def decode(classLoader: ClassLoader, in: JValue) = in match {
        case JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: jvalue5 :: Nil) =>
            for {
                value1 <- coder1.decode(classLoader, jvalue1)
                value2 <- coder2.decode(classLoader, jvalue2)
                value3 <- coder3.decode(classLoader, jvalue3)
                value4 <- coder4.decode(classLoader, jvalue4)
                value5 <- coder5.decode(classLoader, jvalue5)
            } yield (value1, value2, value3, value4, value5)

        case JArray(_) => FailedG("expected an array with exactly five elements", Nil)
        case _         => FailedG("not an array", Nil)
    }

    def encode(classLoader: ClassLoader, in: (A,B,C,D,E)) =
        for {
            jvalue1 <- coder1.encode(classLoader, in._1)
            jvalue2 <- coder2.encode(classLoader, in._2)
            jvalue3 <- coder3.encode(classLoader, in._3)
            jvalue4 <- coder4.encode(classLoader, in._4)
            jvalue5 <- coder5.encode(classLoader, in._5)
        } yield JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: jvalue5 :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple5[A, B, C, D, E], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple5[A, B, C, D, E]) =
        encodeMongoDBTuple(classLoader, in)
}

/** Map a 6-tuple */
case class Tuple6Coder[A,B,C,D,E,F] (
    coder1: ComposableCoder[A],
    coder2: ComposableCoder[B],
    coder3: ComposableCoder[C],
    coder4: ComposableCoder[D],
    coder5: ComposableCoder[E],
    coder6: ComposableCoder[F]
) extends ComposableCoder[(A,B,C,D,E,F)] with TupleCoder
{
    val mostSpecificClass = classOf[(A,B,C,D,E,F)]

    val arity = 6

    protected val coders = Array (
        coder1.asCoderFor[AnyRef],
        coder2.asCoderFor[AnyRef],
        coder3.asCoderFor[AnyRef],
        coder4.asCoderFor[AnyRef],
        coder5.asCoderFor[AnyRef],
        coder6.asCoderFor[AnyRef]
    )
    protected def makeValue(in: Array[AnyRef]) = in match {
        case Array(a, b, c, d, e, f) => Okay((a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C],
                                              d.asInstanceOf[D], e.asInstanceOf[E], f.asInstanceOf[F]))
        case _ => FailedG("expected 6 elements from tuple decode, got " + in.length, Nil)
    }

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: jvalue5 :: jvalue6 :: Nil) =>
                for {
                    value1 <- coder1.decode(classLoader, jvalue1)
                    value2 <- coder2.decode(classLoader, jvalue2)
                    value3 <- coder3.decode(classLoader, jvalue3)
                    value4 <- coder4.decode(classLoader, jvalue4)
                    value5 <- coder5.decode(classLoader, jvalue5)
                    value6 <- coder6.decode(classLoader, jvalue6)
                } yield (value1, value2, value3, value4, value5, value6)

            case JArray(_) => FailedG("expected an array with exactly six elements", Nil)
            case _         => FailedG("not an array", Nil)
        }

    def encode(classLoader: ClassLoader, in: (A,B,C,D,E,F)) =
        for {
            jvalue1 <- coder1.encode(classLoader, in._1)
            jvalue2 <- coder2.encode(classLoader, in._2)
            jvalue3 <- coder3.encode(classLoader, in._3)
            jvalue4 <- coder4.encode(classLoader, in._4)
            jvalue5 <- coder5.encode(classLoader, in._5)
            jvalue6 <- coder6.encode(classLoader, in._6)
        } yield JArray(jvalue1 :: jvalue2 :: jvalue3 :: jvalue4 :: jvalue5 :: jvalue6 :: Nil)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        decodeAvroTuple(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: Tuple6[A, B, C, D, E, F], out: Encoder) =
        encodeAvroTuple(classLoader, in, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        decodeMongoDBTuple(classLoader, in)

    def encodeMongoDB(classLoader: ClassLoader, in: Tuple6[A, B, C, D, E, F]) =
        encodeMongoDBTuple(classLoader, in)
}


/* ******************************************************************************** */


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

    protected def builder(): Builder[Elem, Coll]
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

    val avroSchema = Schema.createArray(valueCoder.avroSchema)

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
            it.foreachResult {
                v =>
                out.startItem()
                atIndex(index) {
                    index += 1
                    valueCoder.encodeAvro(classLoader, v, out)
                }
            } then Okay(out.writeArrayEnd())
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

    protected def builder() = new Builder[T, JavaList[T]] {
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
        val cb = implicitly[CanBuild[T, ArrayBuffer[T]]]
        cb()
    }
    protected def valueAsIterable(in: Seq[T]) = in
}

object ScalaImmutableSetCoder {
    /** Make a CollectionCoder preconfigured to build and decompose immutable scala Sets. */
    def apply[T](valueCoder: ComposableCoder[T]): CollectionCoder[T, ImmutableSet[T]] =
        CollectionCoder(valueCoder) (
            implicitly[CanBuild[T, ImmutableSet[T]]],
            implicitly[ImmutableSet[T] => Iterable[T]],
            implicitly[Manifest[ImmutableSet[_]]].asInstanceOf[Manifest[ImmutableSet[T]]]
        )
}

object ScalaMutableSetCoder {
    /** Make a CollectionCoder preconfigured to build and decompose mutable scala Sets. */
    def apply[T](valueCoder: ComposableCoder[T]): CollectionCoder[T, MutableSet[T]] =
        CollectionCoder(valueCoder) (
            implicitly[CanBuild[T, MutableSet[T]]],
            implicitly[MutableSet[T] => Iterable[T]],
            implicitly[Manifest[MutableSet[_]]].asInstanceOf[Manifest[MutableSet[T]]]
        )
}

/**
 * Abstract coder that codes Map-like things, automatically deciding whether to encode as a { key: value } or [ { key: "key", value: "value" } ]
 * by checking whether the key coder implements StringSafeCoder.
 */
abstract class MapLikeCoder[K, V, Coll](keyCoder: ComposableCoder[K], valueCoder: ComposableCoder[V]) extends ComposableCoder[Coll] {
    import ComposableCoder.{CoderResult, atIndex, atProperty, catchingCoderException}

    protected def builder(): Builder[(K, V), Coll]
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
                    } yield ("key" -> keyJValue) ~ ("value" -> valueJValue)
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

    val avroSchema = keyCoder match {
        case _: StringSafeCoder[_] => Schema.createMap(valueCoder.avroSchema)
        case _ => Schema.createArray {
            import AvroUtils.encodeSchemaName
            val recordName = "kvpair__" + encodeSchemaName(keyCoder.avroSchema) + encodeSchemaName(valueCoder.avroSchema)
            val pairSchema = Schema.createRecord(recordName, "", "", false)
            pairSchema.setFields(Arrays.asList (
                new Schema.Field("key",   keyCoder.avroSchema,   "", null),
                new Schema.Field("value", valueCoder.avroSchema, "", null)
            ))
            pairSchema
        }
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
            in.foreachResult(p => {
                out.startItem()
                for {
                    keyOk <- keyStringCoder.encodeString(classLoader, p._1).map(out.writeString)
                    valueOk <- valueCoder.encodeAvro(classLoader, p._2, out)
                } yield ()
            }) then Okay(out.writeMapEnd())
        }

        def encodeAvroArray(in: Iterable[(K, V)]): CoderResult[Unit] = {
            out.writeArrayStart()
            out.setItemCount(in.size)
            in.foreachResult { p =>
                out.startItem()
                for {
                    keyOk <- keyCoder.encodeAvro(classLoader, p._1, out)
                    valueOk <- valueCoder.encodeAvro(classLoader, p._2, out)
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

    protected def builder() = new Builder[(K, V), JavaMap[K, V]] {
        val jm = new JavaHashMap[K, V]
        def clear() = jm.clear()
        def result() = jm
        def += (p: (K, V)) = { jm.put(p._1, p._2); this }
    }

    protected def valueAsIterable(in: JavaMap[K, V]): Iterable[(K, V)] = in.asScala
}

/** Coder for scala.collection.immutable.Maps */
case class ScalaImmutableMapCoder[K, V](keyCoder: ComposableCoder[K], valueCoder: ComposableCoder[V])
     extends MapLikeCoder[K, V, ImmutableMap[K, V]](keyCoder, valueCoder)
{
    val mostSpecificClass = classOf[ImmutableMap[K, V]]

    protected def builder() = implicitly[CanBuildFrom[Nothing, (K, V), ImmutableMap[K, V]]].apply()
    protected def valueAsIterable(in: ImmutableMap[K, V]): Iterable[(K, V)] = in
}

/** Coder for scala.collection.mutable.Maps */
case class ScalaMutableMapCoder[K, V](keyCoder: ComposableCoder[K], valueCoder: ComposableCoder[V])
     extends MapLikeCoder[K, V, MutableMap[K, V]](keyCoder, valueCoder)
{
    val mostSpecificClass = classOf[MutableMap[K, V]]

    protected def builder() = implicitly[CanBuildFrom[Nothing, (K, V), MutableMap[K, V]]].apply()
    protected def valueAsIterable(in: MutableMap[K, V]): Iterable[(K, V)] = in
}


/* ******************************************************************************** */


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

    val avroSchema = AvroUtils.nullable(valueCoder.avroSchema)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            in.readIndex() match {
                case 0 => valueCoder.decodeAvro(classLoader, in)
                case 1 => {
                    in.readNull()
                    Okay(null.asInstanceOf[T])
                }
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        catchingCoderException {
            in match {
                case null => {
                    out.writeIndex(1)
                    out.writeNull()
                    Okay(())
                }

                case _ => {
                    out.writeIndex(0)
                    valueCoder.encodeAvro(classLoader, in, out)
                }
            }
        }

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

    val avroSchema = AvroUtils.nullable(valueCoder.avroSchema match {
        case schema if schema.getType != Schema.Type.UNION => schema
        case otherSchema => {
            val someSchema = Schema.createRecord("Some__" + AvroUtils.encodeSchemaName(valueCoder.avroSchema), "", "scala", false)

            someSchema.setFields(Arrays.asList (
                new Schema.Field("value", otherSchema, "", null)
            ))

            someSchema
        }
    })

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            in.readIndex() match {
                case 0     => valueCoder.decodeAvro(classLoader, in).map(Some.apply)
                case 1     => { in.readNull(); Okay(None) }
                case other => FailedG("read unknown union index " + other + " from Avro for Option", Nil)
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: Option[T], out: Encoder) =
        catchingCoderException {
            in match {
                case None => {
                    out.writeIndex(1)
                    out.writeNull()
                    Okay(())
                }

                case Some(v) => {
                    out.writeIndex(0)
                    valueCoder.encodeAvro(classLoader, v, out)
                }
            }
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


object ResultCoder {
    val throwableSchema = Schema.createRecord("Throwable", "", "com.paytronix.utils.scala.result", false)
    throwableSchema.setFields(Arrays.asList (
        new Schema.Field("isA", Schema.create(Schema.Type.STRING), "", null),
        new Schema.Field("message", Schema.create(Schema.Type.STRING), "", null),
        new Schema.Field("cause", AvroUtils.nullable(throwableSchema), "", null)
    ))
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
                            decodeThrowable(nestedCauseObj) flatMap {
                            nestedCause => instThrowable(classLoader, className, message, Some(nestedCause)) | parameter(Nil)
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
                        JField("isA", throwable.getClass.getName) ::
                        JField("message",
                            Option(throwable.getMessage) map JString getOrElse JString(throwable.toString)) ::
                        Nil
                    ))

                case cause =>
                    encodeThrowable(cause) map { causeObj =>
                        JObject (
                            JField("isA", throwable.getClass.getName) ::
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
                    JObject(JField("result", "success") :: fields)
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

    import ResultCoder.throwableSchema

    val okaySchema = Schema.createRecord("Okay__" + AvroUtils.encodeSchemaName(valueCoder.avroSchema),
                                         "", "com.paytronix.utils.scala.result", false)

    okaySchema.setFields(Arrays.asList (
        new Schema.Field("value", valueCoder.avroSchema, "", null)
    ))

    val failedSchema = Schema.createRecord (
        if (valueCoder == UnitCoder) "Failed"
        else "FailedG__" + AvroUtils.encodeSchemaName(valueCoder.avroSchema),
        "", "com.paytronix.utils.scala.result", false
    )

    failedSchema.setFields(Arrays.asList (
        new Schema.Field("throwable", throwableSchema, "", null),
        new Schema.Field("param", failedParamCoder.avroSchema, "", null)
    ))

    val avroSchema = Schema.createUnion(Arrays.asList (
        okaySchema,
        failedSchema
    ))

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
                            case 0 => decodeThrowable() map Some.apply
                            case 1 => in.readNull(); Okay(None)
                            case other => FailedG("read unknown union index " + other + " from Avro for cause of throwable", Nil)
                        }
                    }

                    instance <- instThrowable(classLoader, isA, message, causeOpt) | parameter(Nil)
                } yield instance
            }

        catchingCoderException {
            in.readIndex() match {
                case 0     => valueCoder.decodeAvro(classLoader, in) map Okay.apply
                case 1     => decodeFailed()
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
                        out.writeIndex(1)
                        out.writeNull()
                        Okay(())

                    case cause =>
                        out.writeIndex(0)
                        encodeThrowable(cause)
                }
            }

        catchingCoderException {
            in match {
                case Okay(value) =>
                    out.writeIndex(0)
                    valueCoder.encodeAvro(classLoader, value, out)

                case failed: FailedG[_] =>
                    out.writeIndex(1)
                    encodeFailed(failed)
            }
        }
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

    val leftSchema = Schema.createRecord("Left__" + AvroUtils.encodeSchemaName(leftValueCoder.avroSchema), "", "scala", false)

    leftSchema.setFields(Arrays.asList(new Schema.Field("value", leftValueCoder.avroSchema, "", null)))

    val rightSchema = Schema.createRecord("Right__" + AvroUtils.encodeSchemaName(rightValueCoder.avroSchema), "", "scala", false)

    rightSchema.setFields(Arrays.asList(new Schema.Field("value", rightValueCoder.avroSchema, "", null)))

    val avroSchema = Schema.createUnion(Arrays.asList (
        leftSchema,
        rightSchema
    ))

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

/* ******************************************************************************** */


/** Coder for Java enumerations */
case class JavaEnumCoder[T <: Enum[T]](enumClass: Class[T]) extends StringSafeCoder[T]
{
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = enumClass

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T) =
        Okay(JString(in.toString))

    def decodeString(classLoader: ClassLoader, in: String) =
        tryCatching[IllegalArgumentException].value(Enum.valueOf(enumClass, in)) | ("\"" + in + "\" is not a valid enumeration value") | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: T) =
        Okay(in.toString)

    private def enumValues =
        enumClass.getMethod("values").invoke(null).asInstanceOf[Array[T]]

    val avroSchema = {
        import AvroUtils.nameAndNamespaceFromClass
        val (namespace, name) = nameAndNamespaceFromClass(enumClass)
        Schema.createEnum(name, "", namespace, enumValues.map(_.toString.replaceAll("[^_a-zA-Z0-9]", "")).toSeq.asJava)
    }

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            val values = enumValues
            in.readEnum() match {
                case i if i < 0              => FailedG("read negative enum index " + i + " from Avro", Nil)
                case i if i >= values.length => FailedG("read overflow enum index " + i + " from Avro", Nil)
                case i                       => Okay(enumValues(i))
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        tryCatch.value {
            out.writeEnum(in.ordinal)
        } | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String => decodeString(classLoader, s)
            case null      => FailedG("required but missing", Nil)
            case _         => FailedG("not a string", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        encodeString(classLoader, in)
}

/** Coder for Scala enumerations */
case class ScalaEnumCoder[T <: Enumeration](enum: T) extends StringSafeCoder[T#Value]
{
    import ComposableCoder.catchingCoderException

    val mostSpecificClass = classOf[T#Value]

    def decode(classLoader: ClassLoader, in: JValue) =
        in match {
            case JString(s)     => decodeString(classLoader, s)
            case JNothing|JNull => FailedG("required but missing", Nil)
            case _              => FailedG("not a string", Nil)
        }

    def encode(classLoader: ClassLoader, in: T#Value) =
        Okay(JString(in.toString))

    def decodeString(classLoader: ClassLoader, in: String) =
        enum.values.find(_.toString == in).toResult | ("\"" + in + "\" is not a valid enumeration value") | parameter(Nil)

    def encodeString(classLoader: ClassLoader, in: T#Value) =
        Okay(in.toString)

    private lazy val enumsByOrdinal: Map[Int, T#Value] = Map(enum.values.zipWithIndex.map(_.swap).toSeq: _*)
    private lazy val ordinalsByEnum: Map[T#Value, Int] = Map(enum.values.zipWithIndex.toSeq            : _*)

    val avroSchema = {
        import AvroUtils.nameAndNamespaceFromClass
        val (namespace, name) = nameAndNamespaceFromClass(enum.getClass)
        Schema.createEnum(name, "", namespace, enum.values.map(_.toString.replaceAll("[^_a-zA-Z0-9]", "")).toSeq.asJava)
    }

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        catchingCoderException {
            in.readEnum() match {
                case i if i < 0                    => FailedG("read negative enum index " + i + " from Avro", Nil)
                case i if i >= enumsByOrdinal.size => FailedG("read overflow enum index " + i + " from Avro", Nil)
                case i                             => Okay(enumsByOrdinal(i))
            }
        }

    def encodeAvro(classLoader: ClassLoader, in: T#Value, out: Encoder) =
        tryCatch.value {
            out.writeEnum(ordinalsByEnum(in))
        } | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case s: String => decodeString(classLoader, s)
            case null      => FailedG("required but missing", Nil)
            case _         => FailedG("not a string", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T#Value) =
        encodeString(classLoader, in)
}


/* ******************************************************************************** */


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

    val avroSchema = Schema.createUnion(alternatives.map(_.avroSchema).asJava)

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
                    .map { _ ~ (determinantField -> alternative.determinantValue) }
        }

    val avroSchema = Schema.createUnion(alternatives.map(_.coder.avroSchema).asJava)

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


/* ******************************************************************************** */


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
 *         applyTo("myAction") { _.inputCoder = Coding.forClass[MyServiceArguments].flatMap(SingleArgumentCoder.apply) }
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
        valueCoders.mapResult {
            _.forceEncode(classLoader, in(0))
        } map { _ reduceLeft (_ merge _) }

    val avroSchema =
        Schema.createRecord(valueCoders.zipWithIndex.map {
            case (coder, i) => new Schema.Field("_" + i.toString, coder.avroSchema, "", null)
        }.asJava)

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

    val avroSchema = Schema.createRecord(arguments.map(ac => new Schema.Field(ac.name, ac.coder.avroSchema, "", null)).asJava)

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


/** Code a single field in a JObject. Typically used for single-field extraction */
case class FieldCoder[T](field: String, coder: ComposableCoder[T]) extends ComposableCoder[T] {
    import ComposableCoder.atProperty

    val mostSpecificClass = coder.mostSpecificClass

    def decode(classLoader: ClassLoader, in: JValue) =
        atProperty(field) {
            coder.decode(classLoader, in \ field)
        }

    def encode(classLoader: ClassLoader, in: T) =
        atProperty(field) {
            coder.encode(classLoader, in).map(field -> _)
        }

    val avroSchema = coder.avroSchema

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        coder.decodeAvro(classLoader, in)

    def encodeAvro(classLoader: ClassLoader, in: T, out: Encoder) =
        coder.encodeAvro(classLoader, in, out)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        in match {
            case m: JavaMap[_, _] =>
                atProperty(field)(coder.decodeMongoDB(classLoader, m.asInstanceOf[JavaMap[String, AnyRef]].get(field)))
            case null => FailedG("required but expected", Nil)
            case _    => FailedG("not an object", Nil)
        }

    def encodeMongoDB(classLoader: ClassLoader, in: T) =
        atProperty(field)(coder.encodeMongoDB(classLoader, in)) map { encoded =>
            val obj = new BasicDBObject
            obj.put(field, encoded)
        }
}


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

    val avroSchema = {
        import AvroUtils.nameAndNamespaceFromClass
        val (namespace, name) = nameAndNamespaceFromClass(clazz)
        val s = Schema.createRecord(name, "", namespace, false)
        s.setFields(fieldCodings.map(fc => new Schema.Field(fc.name, fc.coder.avroSchema, "", null)).asJava)
        s
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
