//
// Copyright 2014 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange.format.json

import java.io.{
    InputStream, OutputStream, Reader, Writer,
    ByteArrayInputStream, ByteArrayOutputStream, StringReader, StringWriter
}
import scala.annotation.implicitNotFound

import com.fasterxml.jackson.core.{JsonEncoding, JsonFactory, JsonGenerator, JsonLocation, JsonParser, JsonToken}
import scalaz.BijectionT

import com.paytronix.utils.interchange.base.{
    Coder, CoderFailure, CoderResult, Decoder, Encoder, Format, Receiver, atTerminal, formatFailedPath, terminal
}
import com.paytronix.utils.scala.resource.{Closeable, withResource}
import com.paytronix.utils.scala.result.{FailedG, Okay, Result, tryCatch}

object closeables {
    implicit object JsonGeneratorCloseable extends Closeable[JsonGenerator] {
        def close(gen: JsonGenerator) = gen.close()
    }

    implicit object JsonParserCloseable extends Closeable[JsonParser] {
        def close(gen: JsonParser) = gen.close()
    }
}

import closeables.{JsonGeneratorCloseable, JsonParserCloseable}

/**
 * Wrapper around a `JsonGenerator` which includes additional state for writing out objects correctly.
 *
 * In particular, some encoders want the option of writing no value (not `null`, but really no value) and if the enclosing
 * coder has already called `writeFieldName` then it's too late.
 *
 * So, the `InterchangeJsonGenerator` keeps track of the intended field name so the enclosed encoder can trigger
 * writing the field name or not.
 */
final class InterchangeJsonGenerator(val generator: JsonGenerator) {
    private var latchedFieldName: String = null // yeah null!

    def aboutToWriteValue(): Unit =
        if (latchedFieldName != null) { generator.writeFieldName(latchedFieldName); latchedFieldName = null }

    def writeNothing(): CoderResult[Unit] = { latchedFieldName = null; Okay.unit }
    def writeFieldName(name: String): Unit = { latchedFieldName = name }

    def writeBoolean(b: Boolean): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeBoolean(b); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
    def writeNumber(s: Short): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeNumber(s); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
    def writeNumber(i: Int): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeNumber(i); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
    def writeNumber(l: Long): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeNumber(l); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
    def writeNumber(f: Float): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeNumber(f); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
    def writeNumber(d: Double): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeNumber(d); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
    def writeNumber(bi: java.math.BigInteger): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeNumber(bi); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
    def writeNull(): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeNull(); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
    def writeString(s: String): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeString(s); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }

    def writeStartArray(): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeStartArray(); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
    def writeEndArray(): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeEndArray(); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }

    def writeStartObject(): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeStartObject(); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
    def writeEndObject(): CoderResult[Unit] =
        try { aboutToWriteValue(); generator.writeEndObject(); Okay.unit }
        catch { case e: Exception => FailedG(e, CoderFailure.terminal) }
}

/**
 * Wrapper around a `JsonParser` which includes additional state to support missing values.
 *
 * Because the object decoding incrementally walks through field names and so knows which fields are missing by the end
 * but things like `nullableJsonDecoder` and `optionJsonDecoder` are what handles missing values, those decoders need to
 * be run but have it signalled to them that the value is missing.
 */
final class InterchangeJsonParser(val parser: JsonParser) {
    private var _nextValueIsMissing   = false
    private var _didCheckMissingValue = false // make sure to fail fast if anybody doesn't call `hasNextToken`

    def hasNextToken: Boolean = {
        _didCheckMissingValue = true
        !_nextValueIsMissing
    }

    def nextValueIsMissing(): Unit = {
        _didCheckMissingValue = false
        _nextValueIsMissing = true
    }

    def nextToken(): JsonToken =
        if (!_didCheckMissingValue)
            sys.error("decoder should have checked whether a value was present prior to calling nextToken")
        else {
            _didCheckMissingValue = false
            _nextValueIsMissing = false
            parser.nextToken()
        }

    /** Format an error indicating that the current token was unexpected */
    def unexpectedToken(expectedWhat: String): CoderResult[Unit] = {
        val what = parser.getCurrentToken.toString
        FailedG(s"expected $expectedWhat but instead got $what", terminal)
    }

    /** Format an error indicating a value was expected but missing */
    def missingValue: CoderResult[Unit] =
        FailedG("required but missing", terminal)

    /** Compute the path name and source position of the JSON parser for use in error messages */
    def sourceLocation: Option[String] =
        parser.getCurrentLocation match {
            case loc if loc == JsonLocation.NA => None
            case loc                           => Some(s"${loc.getLineNr}:${loc.getColumnNr}")
        }

    /** An `orElse` failure processing function which sets the source location appropriately for the location of the parser */
    def noteSourceLocation: FailedG[CoderFailure] => FailedG[CoderFailure] =
        _.mapFailure(_.copy(sourceLocation=this.sourceLocation))

    /** A terminal `CoderFailure` at the current parsing location */
    def terminal: CoderFailure =
        sourceLocation map CoderFailure.terminalAt getOrElse CoderFailure.terminal
}

/** Format which consumes and produces JSON using the Jackson streaming parser/generator */
object JsonFormat extends Format {
    type Source = InterchangeJsonParser
    type Sink   = InterchangeJsonGenerator

    /**
     * Default `JsonFactory` used by `JsonEncoder` and `JsonDecoder` methods where some other factory has not been
     * given explicitly or implicitly.
     *
     * While there's nothing to prevent it, it's highly recommended to not tweak settings of this factory, as it may cause
     * confusing side effects with other code that is expecting the default settings.
     */
    lazy val defaultFactory: JsonFactory = {
        val fact = new JsonFactory
        fact.disable(JsonParser.Feature.AUTO_CLOSE_SOURCE)
        fact.disable(JsonGenerator.Feature.AUTO_CLOSE_TARGET)
        fact
    }
}

object JsonEncoder extends JsonEncoderLPI {
    /** Materializer for `JsonEncoder`, just finds the `JsonEncoder` in the implicit scope. For example: `JsonEncoder[Int]` */
    def apply[A](implicit encoder: JsonEncoder[A]): JsonEncoder[A] = implicitly
}

trait JsonEncoderLPI {
    implicit def fromCoder[A](implicit coder: JsonCoder[A]): JsonEncoder[A] = coder.encode
}

/** Encoder which encodes JSON incrementally via a Jackson `JsonGenerator` */
@implicitNotFound(msg="No JsonEncoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.json.coders, or need to add some @derive annotations")
trait JsonEncoder[A] extends Encoder[A, JsonFormat.type] { outer =>
    /** Map a function over this encoder, yielding an encoder which takes in values of type `B` */
    def map[B](f: B => A): JsonEncoder[B] = mapKleisli(b => Okay(f(b)))

    /** Map a kleisli (function which may fail gracefully) over this encoder, yielding an encoder which takes in values of type `B` */
    def mapKleisli[B](k: B => Result[A]): JsonEncoder[B] = new JsonEncoder[B] {
        def run(b: B, sink: InterchangeJsonGenerator) = atTerminal(k(b)) >>= { outer.run(_, sink) }
    }

    /** Encode a value to a byte array encoded using UTF-8 */
    def toBytes(in: A, enc: JsonEncoding = JsonEncoding.UTF8, pretty: Boolean = false)
               (implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[Array[Byte]] =
        withResource(new ByteArrayOutputStream()) { baos =>
            toOutputStream(in, baos, pretty=pretty) map { _ => baos.toByteArray }
        }

    /**
     * Encode a value to an `OutputStream` with the specified encoding.
     * When using the default `JsonFactory` the underlying output stream will NOT be closed by this method completing
     */
    def toOutputStream(in: A, out: OutputStream, enc: JsonEncoding = JsonEncoding.UTF8, pretty: Boolean = false)
                      (implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[Unit] =
        tryCatch.result {
            withResource(jsonFactory.createGenerator(out, enc)) { gen =>
                toGenerator(in, gen, pretty=pretty)
            }
        }

    /** Encode a value to a String */
    def toString(in: A, pretty: Boolean = false)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[String] =
        withResource(new StringWriter()) { sw =>
            toWriter(in, sw, pretty=pretty) map { _ => sw.toString }
        }

    /**
     * Encode a value to a `Writer`.
     * When using the default `JsonFactory` the underlying writer will NOT be closed by this method completing
     */
    def toWriter(in: A, out: Writer, pretty: Boolean = false)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[Unit] =
        tryCatch.result {
            withResource(jsonFactory.createGenerator(out)) { gen =>
                toGenerator(in, gen)
            }
        }

    /** Encode a value to a `JsonGenerator`. This differs from `run` in that it returns `Result[Unit]` not `ResultG[FailedPath, Unit]` */
    def toGenerator(in: A, gen: JsonGenerator, pretty: Boolean = false): Result[Unit] = {
        if (pretty) gen.useDefaultPrettyPrinter()
        formatFailedPath(run(in, new InterchangeJsonGenerator(gen)))
    }
}

object JsonDecoder extends JsonDecoderLPI {
    /** Materializer for JsonDecoder, just finds the JsonDecoder in implicit scope. For example: JsonDecoder[Int] */
    def apply[A](implicit decoder: JsonDecoder[A]): JsonDecoder[A] = implicitly
}

trait JsonDecoderLPI {
    implicit def fromCoder[A](implicit coder: JsonCoder[A]): JsonDecoder[A] = coder.decode
}

/** Decoder which consumes binary data in the Avro format via an Avro `ResolvingDecoder` */
@implicitNotFound(msg="No JsonDecoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.json.coders, or need to add some @derive annotations")
trait JsonDecoder[A] extends Decoder[A, JsonFormat.type] { outer =>
    /** Map a function over this decoder, yielding an decoder which produces values of type `B` by transforming `A`s using the given function */
    def map[B](f: A => B): JsonDecoder[B] = mapKleisli(a => Okay(f(a)))

    /** Map a kleisli (function which may fail gracefully) over this decoder, yielding an decoder which produces values of type `B` */
    def mapKleisli[B](k: A => Result[B]): JsonDecoder[B] = new JsonDecoder[B] {
        def run(source: InterchangeJsonParser, outB: Receiver[B]) = {
            val outA = new Receiver[A]
            outer.run(source, outA) match {
                case _: Okay[_] =>
                    k(outA.value) match {
                        case Okay(b) => outB(b)
                        case failed: FailedG[_] => failed.mapFailure { _ => source.terminal }
                    }
                case failed => failed
            }
        }
    }

    /** Attempt conversion of a byte array to a value of the mapped type */
    def fromBytes(in: Array[Byte])(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        fromBytesRange(in, 0, in.length)

    /** Attempt conversion of a byte array to a value of the mapped type */
    def fromBytesRange(in: Array[Byte], offset: Int, length: Int)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        tryCatch.result {
            withResource(jsonFactory.createParser(in, offset, length))(fromParser)
        }

    /** Attempt conversion of a character array to a value of the mapped type */
    def fromChars(in: Array[Char])(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] = fromCharsRange(in, 0, in.length)

    /** Attempt conversion of a character array to a value of the mapped type */
    def fromCharsRange(in: Array[Char], offset: Int, length: Int)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        tryCatch.result {
            withResource(jsonFactory.createParser(in, offset, length))(fromParser)
        }

    /** Attempt conversion of an input stream to a value of the mapped type  */
    def fromInputStream(in: InputStream)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        tryCatch.result {
            withResource(jsonFactory.createParser(in))(fromParser)
        }

    /** Attempt conversion of JSON string to a value of the mapped type  */
    def fromString(in: String)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        withResource(new StringReader(in)) { sr =>
            fromReader(sr)
        }

    /** Attempt conversion of an input reader to a value of the mapped type  */
    def fromReader(in: Reader)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        tryCatch.result {
            withResource(jsonFactory.createParser(in))(fromParser)
        }

    /**
     * Decode a value from a `JsonParser`.
     * This differs from `run` in that it yields `Result[A]` rather than taking a `Receiver[A]` and yielding a `ResultG[FailedPath, Unit]`.
     */
    def fromParser(in: JsonParser): Result[A] = {
        val receiver = new Receiver[A]
        formatFailedPath(run(new InterchangeJsonParser(in), receiver)) map { _ => receiver.value }
    }
}

object JsonCoder  {
    /** Materializer for `JsonCoder` which looks in the implicit scope. Equivalent to `implicitly[JsonCoder[A]]` */
    def apply[A](implicit coder: JsonCoder[A]): JsonCoder[A] = implicitly

    /** Make an `JsonCoder` from an `JsonEncoder` and `JsonDecoder` */
    def make[A](implicit encoder: JsonEncoder[A], decoder: JsonDecoder[A]): JsonCoder[A] =
        new JsonCoder[A] {
            val encode = encoder
            val decode = decoder
        }
}

/** Module of a Avro streaming encoder and decoder pair */
@implicitNotFound(msg="No JsonCoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.json.coders, or need to add some @derive annotations")
trait JsonCoder[A] extends Coder[JsonEncoder, JsonDecoder, A, JsonFormat.type] {
    /** Encoder for the type `A` */
    val encode: JsonEncoder[A]

    /** Decoder for the type `A` */
    val decode: JsonDecoder[A]

    /**
     * Map a bijection (pair of kleislis `A => Result[B]` and `B => Result[A]`) over this coder pair, yielding a new coder pair for some
     * type `B` which can be bijected to the underlying type `A`.
     *
     * For example, to make a `JsonCoder` that is encoded in Avro as a string but decodes to Scala as a sequence of space separated tokens:
     *
     *    val tokenizedStringJsonCoder = scalar.stringJsonCoder.mapBijection(bijection (
     *        (tokens: Seq[String]) => Okay(tokens.mkString(" ")),
     *        (s: String) => Okay(s.split(' '))
     *    ))
     */
    def mapBijection[B](bijection: BijectionT[Result, Result, B, A]) =
        JsonCoder.make(encode.mapKleisli(bijection.to), decode.mapKleisli(bijection.from))
}
