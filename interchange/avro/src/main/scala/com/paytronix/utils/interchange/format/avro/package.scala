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

import java.io.{ByteArrayOutputStream, InputStream, IOException, OutputStream}
import scala.annotation.{StaticAnnotation, implicitNotFound}

import org.apache.avro
import org.codehaus.jackson.JsonNode

import com.paytronix.utils.interchange.base.{
    Coder, CoderFailure, CoderResult, Decoder, Encoder, Format, Receiver, TypeConverter, atTerminal, formatFailedPath, terminal
}
import com.paytronix.utils.scala.result.{FailedG, Okay, Result, tryCatchValue, tryCatchResult}

/**
 * Annotation which allows setting aliases on the generated Avro schema for a type.
 * <strong>Note:</strong> This only works for coders that honor it, which is typically only the
 * macro derived coders.
 */
class aliases(aliases: String*) extends StaticAnnotation

/**
 * Annotation which allows overriding the Avro schema name of the type.
 * <strong>Note:</strong> This only works for coders that honor it, which is typically only the
 * macro derived coders.
 */
class name(name: String) extends StaticAnnotation

/** Format which consumes and produces streams of binary data using Avro */
object AvroFormat extends Format {
    type Source = avro.io.ResolvingDecoder
    type Sink = avro.io.Encoder
}

/** Trait of Avro encoders and decoders which must use a schema to format the binary data */
trait AvroEncoderOrDecoder {
    /** Produce an Avro schema for the encoded value */
    def schema: avro.Schema

    /** Produce the JSON AST for the default value of this field if it is missing from the input */
    def defaultJson: Option[JsonNode]
}

object AvroEncoder extends AvroEncoderLPI {
    /** Materializer for `AvroEncoder`, just finds the `AvroEncoder` in the implicit scope. For example: `AvroEncoder[Int]` */
    def apply[A](implicit encoder: AvroEncoder[A]): AvroEncoder[A] = implicitly
}

trait AvroEncoderLPI {
    implicit def fromCoder[A](implicit coder: AvroCoder[A]): AvroEncoder[A] = coder.encode
}

/** Encoder which encodes binary data incrementally via an Avro `Encoder` */
@implicitNotFound(msg="No AvroEncoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.avro.coders, or need to add some @derive annotations")
trait AvroEncoder[A] extends Encoder[A, AvroFormat.type] with AvroEncoderOrDecoder { outer =>
    /**
     * Encode a value into the JSON format used by Avro in the schema to indicate default values.
     * Note that there are limitations in Avro as to what values can be used as defaults, so this may fail (with `FailedG`)
     * on a range of values that can otherwise be encoded with `run` (`toBytes`, `toOutputStream`, `asDatumWriter`)
     */
    def encodeDefaultJson(a: A): CoderResult[JsonNode]

    /**
     * Produce a new encoder whose schema has the given value as a default.
     * <strong>WARNING:</strong> throws an exception if the value cannot be encoded as default! This method is only inteded to be used
     * at static initialization time.
     */
    def default(a: A): AvroEncoder[A] =
        withDefaultJson(encodeDefaultJson(a).orThrow)

    /** Produce a new encoder whose schema uses the given default JSON, usually encoded via `encodeDefaultJson` */
    def withDefaultJson(json: JsonNode) =
        new AvroEncoder[A] {
            val schema = outer.schema
            val defaultJson = Some(json)
            def encodeDefaultJson(a: A) = outer.encodeDefaultJson(a)
            def run(a: A, sink: avro.io.Encoder) = outer.run(a, sink)
        }

    /** Map a function over this encoder, yielding an encoder which takes in values of type `B` */
    def map[B](f: B => A): AvroEncoder[B] = mapKleisli(b => Okay(f(b)))

    /** Map a kleisli (function which may fail gracefully) over this encoder, yielding an encoder which takes in values of type `B` */
    def mapKleisli[B](k: B => Result[A]): AvroEncoder[B] = new AvroEncoder[B] {
        def run(b: B, sink: avro.io.Encoder) = atTerminal(k(b)) >>= { outer.run(_, sink) }
        def encodeDefaultJson(b: B)       = atTerminal(k(b)) >>= outer.encodeDefaultJson
        def schema                        = outer.schema
        def defaultJson                   = outer.defaultJson
    }

    /** Attempt conversion of a value of the mapped type to a byte array */
    def toBytes(in: A): Result[Array[Byte]] = {
        val baos = new ByteArrayOutputStream()
        toOutputStream(in, baos) map { _ => baos.toByteArray }
    }

    /** Attempt encoding of a value to a given output stream */
    def toOutputStream(in: A, out: OutputStream): Result[Unit] =
        for {
            encoder   <- tryCatchValue(avro.io.EncoderFactory.get.binaryEncoder(out, null)) | "Failed to create Avro encoder"
            encodedOk <- formatFailedPath(run(in, encoder))
            flushedOk <- tryCatchValue(encoder.flush) | "Failed to flush Avro encoder"
        } yield ()

    /** Produce a DatumWriter that writes whatever this Coder codes to some Avro encoder */
    lazy val asDatumWriter: avro.io.DatumWriter[A] =
        new avro.io.DatumWriter[A] {
            def setSchema(schema: avro.Schema) =
                if (schema == outer.schema) ()
                else sys.error("setSchema called with different schema than this Coder encodes")

            @throws(classOf[IOException])
            def write(in: A, encoder: avro.io.Encoder): Unit =
                formatFailedPath(run(in, encoder)).orThrow
        }
}


object AvroDecoder extends AvroDecoderLPI {
    /** Materializer for AvroDecoder, just finds the AvroDecoder in implicit scope. For example: AvroDecoder[Int] */
    def apply[A](implicit decoder: AvroDecoder[A]): AvroDecoder[A] = implicitly
}

trait AvroDecoderLPI {
    implicit def fromCoder[A](implicit coder: AvroCoder[A]): AvroDecoder[A] = coder.decode
}

/** Decoder which consumes binary data in the Avro format via an Avro `ResolvingDecoder` */
@implicitNotFound(msg="No AvroDecoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.avro.coders, or need to add some @derive annotations")
trait AvroDecoder[A] extends Decoder[A, AvroFormat.type] with AvroEncoderOrDecoder { outer =>
    /**
     * Produce a new decoder whose schema has the given value as a default.
     * <strong>WARNING:</strong> throws an exception if the value cannot be encoded as default! This method is only inteded to be used
     * at static initialization time.
     */
    def default(a: A)(implicit encoder: AvroEncoder[A]): AvroDecoder[A] =
        withDefaultJson(encoder.encodeDefaultJson(a).orThrow)

    /** Produce a new decoder whose schema uses the given default JSON, usually encoded via `encodeDefaultJson` */
    def withDefaultJson(json: JsonNode) =
        new AvroDecoder[A] {
            val schema = outer.schema
            val defaultJson = Some(json)
            def run(in: avro.io.ResolvingDecoder, out: Receiver[A]) = outer.run(in, out)
        }

    /** Map a function over this decoder, yielding an decoder which produces values of type `B` by transforming `A`s using the given function */
    def map[B](f: A => B): AvroDecoder[B] = mapKleisli(a => Okay(f(a)))

    /** Map a kleisli (function which may fail gracefully) over this decoder, yielding an decoder which produces values of type `B` */
    def mapKleisli[B](k: A => Result[B]): AvroDecoder[B] = new AvroDecoder[B] {
        def run(source: avro.io.ResolvingDecoder, outB: Receiver[B]) = {
            val outA = new Receiver[A]
            outer.run(source, outA) match {
                case _: Okay[_] =>
                    k(outA.value) match {
                        case Okay(b) => outB(b)
                        case failed: FailedG[_] => failed.mapFailure { _ => CoderFailure.terminal }
                    }
                case failed => failed
            }
        }

        def schema      = outer.schema
        def defaultJson = outer.defaultJson
    }

    /** Attempt conversion of a byte array to a value of the mapped type */
    def fromBytes(writerSchema: avro.Schema)(in: Array[Byte]): Result[A] = fromBytesRange(writerSchema)(in, 0, in.length)

    /** Attempt conversion of a byte array to a value of the mapped type */
    def fromBytesRange(writerSchema: avro.Schema)(in: Array[Byte], offset: Int, length: Int): Result[A] =
        for {
            decoder <- tryCatchValue(avro.io.DecoderFactory.get.binaryDecoder(in, offset, length, null)) | "Failed to create Avro decoder"
            result  <- fromDecoder(writerSchema)(decoder)
        } yield result

    /** Attempt conversion of an input stream to a value of the mapped type  */
    def fromInputStream(writerSchema: avro.Schema)(in: InputStream): Result[A] = {
        for {
            decoder <- tryCatchValue(avro.io.DecoderFactory.get.directBinaryDecoder(in, null)) | "Failed to create Avro decoder"
            result  <- fromDecoder(writerSchema)(decoder)
        } yield result
    }

    /** Attempt decoding from some Avro decoder */
    def fromDecoder(writerSchema: avro.Schema)(in: avro.io.Decoder): Result[A] =
        tryCatchResult {
            val resolver = utils.ResolvingDecoderCache(schema, writerSchema)
            resolver.configure(in)
            val receiver = new Receiver[A]
            val result = run(resolver, receiver)
            resolver.drain()
            formatFailedPath(result).map { _ => receiver.value }
        } | com.paytronix.utils.scala.result.ResultG.stringAsFailedMessage("failed to decode from byte array")

    /** Produce a DatumReader that reads whatever this Coder codes from some Avro decoder */
    def asDatumReader(writerSchema: avro.Schema = null): avro.io.DatumReader[A] =
        new avro.io.DatumReader[A] {
            private val _reader: avro.Schema = schema
            private var _writer: avro.Schema = writerSchema

            def setSchema(writer: avro.Schema): Unit = { _writer = writer }

            @throws(classOf[IOException])
            def read(reuse: A, decoder: avro.io.Decoder): A = {
                val resolver = utils.ResolvingDecoderCache(_reader, _writer)
                resolver.configure(decoder)
                val receiver = new Receiver[A]
                val result = run(resolver, receiver)
                resolver.drain()
                val _ = formatFailedPath(result).orThrow
                receiver.value
            }
        }
}

object AvroCoder  {
    /** Materializer for `AvroCoder` which looks in the implicit scope. Equivalent to `implicitly[AvroCoder[A]]` */
    def apply[A](implicit coder: AvroCoder[A]): AvroCoder[A] = implicitly

    /** Make an `AvroCoder` from an `AvroEncoder` and `AvroDecoder` */
    def make[A](implicit encoder: AvroEncoder[A], decoder: AvroDecoder[A]): AvroCoder[A] =
        new AvroCoder[A] {
            val encode = encoder
            val decode = decoder
        }
}

/** Module of a Avro streaming encoder and decoder pair */
@implicitNotFound(msg="No AvroCoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.avro.coders, or need to add some @derive annotations")
trait AvroCoder[A] extends Coder[AvroEncoder, AvroDecoder, A, AvroFormat.type] {
    /** Encoder for the type `A` */
    val encode: AvroEncoder[A]

    /** Decoder for the type `A` */
    val decode: AvroDecoder[A]

    /** Avro schema for the value. Note that this returns the encoder schema but they should be identical for any given type */
    def schema: avro.Schema = encode.schema

    /**
     * Default value to use when a field of this type is not present in the incoming data,
     * e.g. is being decoded from an older version of data that does not include it
     */
    def defaultJson: Option[JsonNode] = decode.defaultJson

    /**
     * Map a bijection (pair of kleislis `A => Result[B]` and `B => Result[A]`) over this coder pair, yielding a new coder pair for some
     * type `B` which can be bijected to the underlying type `A`.
     *
     * For example, to make a `AvroCoder` that is encoded in Avro as a string but decodes to Scala as a sequence of space separated tokens:
     *
     *    val tokenizedStringAvroCoder = scalar.stringAvroCoder.mapBijection(bijection (
     *        (tokens: Seq[String]) => Okay(tokens.mkString(" ")),
     *        (s: String) => Okay(s.split(' '))
     *    ))
     */
    def mapWithConverter[B](converter: TypeConverter[B, A]): AvroCoder[B] = AvroCoder.make(encode.mapKleisli(converter.to), decode.mapKleisli(converter.from))

    /**
     * Yield a new `AvroCoder` for the same type which defaults to some value when decoding from a schema that doesn't include the field
     * <strong>WARNING:</strong> throws an exception if the default value cannot be encoded as a default to be used in the Avro schema.
     */
    def default(value: A): AvroCoder[A] = {
        val defaultJson = encode.encodeDefaultJson(value).orThrow
        AvroCoder.make(encode.withDefaultJson(defaultJson), decode.withDefaultJson(defaultJson))
    }
}
