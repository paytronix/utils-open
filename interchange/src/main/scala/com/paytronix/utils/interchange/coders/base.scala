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

import java.io.{ByteArrayOutputStream, InputStream, IOException, OutputStream}

import net.liftweb.json.JsonAST.{JValue, render}
import net.liftweb.json.Printer.compact
import org.apache.avro.Schema
import org.apache.avro.io.{DatumReader, DatumWriter, Decoder, DecoderFactory, Encoder, EncoderFactory, ResolvingDecoder}
import org.codehaus.jackson.JsonNode

import com.paytronix.utils.scala.concurrent.ThreadLocal
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, ResultG, cast, parameter, tryCatch}

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
            val resolver = ResolvingDecoderCache(avroSchema._1, writerSchema)
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
    def avroSchema: (Schema, Option[JsonNode]) = implementation.avroSchema

    /** Produce a DatumReader that reads whatever this Coder codes from some Avro decoder */
    def avroDatumReader(writerSchema: Schema = null): DatumReader[T] =
        new DatumReader[T] {
            private val _reader: Schema = avroSchema._1
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

    /** Yield the Avro Schema this coder produces and consumes */
    def avroSchema: (Schema, Option[JsonNode])

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

    /**
     * Avro has its own very special way of encoding default values in Json, so each coder that supports Avro must
     * also be able to generate that
     */
    def encodeAvroDefaultJson(classLoader: ClassLoader, in: T): CoderResult[JsonNode]

    def forceEncodeAvroDefaultJson(classLoader: ClassLoader, in: AnyRef): CoderResult[JsonNode] =
        atTerminal(cast(mostSpecificClass, in)) flatMap { encodeAvroDefaultJson(classLoader, _) }

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
