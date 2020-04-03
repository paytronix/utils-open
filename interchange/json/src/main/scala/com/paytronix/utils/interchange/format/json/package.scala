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

package com.paytronix.utils.interchange.format.json

import com.fasterxml.jackson.core.{JsonEncoding, JsonFactory, JsonGenerator, JsonLocation, JsonParser, JsonParseException, JsonToken}
import java.io.{
    InputStream, OutputStream, Reader, Writer,
    ByteArrayInputStream, ByteArrayOutputStream, StringReader, StringWriter
}
import scala.annotation.{StaticAnnotation, implicitNotFound, tailrec}
import scala.collection.mutable.Queue

import com.paytronix.utils.interchange.base.{
    Coder, CoderFailure, CoderResult, Decoder, Encoder, Format, Receiver, TypeConverter, atTerminal, formatFailedPath, terminal
}
import com.paytronix.utils.scala.resource.{Closeable, withResource}
import com.paytronix.utils.scala.result.{FailedG, Okay, Result, tryCatchValue, tryCatchResult, tryCatchResultG}

object closeables {
    implicit object JsonGeneratorCloseable extends Closeable[JsonGenerator] {
        def close(gen: JsonGenerator) = gen.close()
    }

    implicit object JsonParserCloseable extends Closeable[JsonParser] {
        def close(gen: JsonParser) = gen.close()
    }
}

import closeables.{JsonGeneratorCloseable, JsonParserCloseable}

object InterchangeJsonGenerator {
    /**
     * Trait of objects which filter objects as they're written, for example to write additional
     * fields at the beginning or end, or watch what fields are created
     */
    trait ObjectFilter {
        val suppressStartAndEnd: Boolean = false
        def beginning(): CoderResult[Unit] = Okay.unit
        def fieldName(name: String): CoderResult[Unit] = Okay.unit
        def end(): CoderResult[Unit] = Okay.unit
    }

    object ObjectFilter {
        val zero = new ObjectFilter {}
        val flatten = new ObjectFilter {
            override val suppressStartAndEnd = true
        }

        // The operator >> as used in this function is basically a flatmap,
        // and the lines return Okay.unit if both things happen successfully
        def combineFilters(a: ObjectFilter, b: ObjectFilter): ObjectFilter = new ObjectFilter {
            override val suppressStartAndEnd = a.suppressStartAndEnd || b.suppressStartAndEnd
            override def beginning(): CoderResult[Unit] = a.beginning() >> b.beginning()

            override def fieldName(name: String): CoderResult[Unit] = a.fieldName(name) >> b.fieldName(name)

            override def end(): CoderResult[Unit] = a.end() >> b.end()
        }
    }
}

/** Interface of something which can sink JSON events from the encoder hierarchy, usually `InterchangeJacksonJsonGenerator` */
trait InterchangeJsonGenerator {
    import InterchangeJsonGenerator._

    def writeNothingOrNull(): CoderResult[Unit]
    def writeFieldName(name: String): CoderResult[Unit]
    def writeFieldNameNotMissing(name: String): CoderResult[Unit]
    def omitNextMissing(): Unit
    def filterNextObject(filter: ObjectFilter): Unit
    def writeBoolean(b: Boolean): CoderResult[Unit]
    def writeNumber(s: Short): CoderResult[Unit]
    def writeNumber(i: Int): CoderResult[Unit]
    def writeNumber(l: Long): CoderResult[Unit]
    def writeNumber(f: Float): CoderResult[Unit]
    def writeNumber(d: Double): CoderResult[Unit]
    def writeNumber(bi: java.math.BigInteger): CoderResult[Unit]
    def writeNull(): CoderResult[Unit]
    def writeString(s: String): CoderResult[Unit]
    def writeStartArray(): CoderResult[Unit]
    def writeEndArray(): CoderResult[Unit]
    def writeStartObject(): CoderResult[Unit]
    def writeEndObject(): CoderResult[Unit]
}

/**
 * Wrapper around a `JsonGenerator` which includes additional state for writing out objects correctly.
 *
 * In particular, some encoders want the option of writing no value (not `null`, but really no value) and if the enclosing
 * coder has already called `writeFieldName` then it's too late.
 *
 * So, the `InterchangeJsonGenerator` keeps track of the intended field name so the enclosed encoder can trigger
 * writing the field name or not.
 *
 * This class essentially encapsulates all the messiness that results from generating JSON incrementally and is not intended
 * for prettiness.
 */
final class InterchangeJacksonJsonGenerator(generator: JsonGenerator) extends InterchangeJsonGenerator {
    import InterchangeJsonGenerator._

    private var _fieldName: String = null // yeah null!
    private var _omitNextMissing: Boolean = false
    private var _nextObjectFilter: ObjectFilter = null
    private var _objectFilters: List[ObjectFilter] = Nil

    private def clearLatched(): Unit = {
        _fieldName = null
        _omitNextMissing = false
        _nextObjectFilter = null
    }

    private def aboutToWriteToken(): CoderResult[Unit] =
        if (_fieldName != null) {
            writeFieldNameNotMissing(_fieldName)
        } else Okay.unit

    def writeNothingOrNull(): CoderResult[Unit] =
        if (_fieldName == null && !_omitNextMissing) {
            try {
                generator.writeNull()
                clearLatched()
                Okay.unit
            } catch {
                case e: Exception => FailedG(e, CoderFailure.terminal)
            }
        } else {
            clearLatched()
            Okay.unit
        }

    def writeFieldName(name: String): CoderResult[Unit] = { _fieldName = name; Okay.unit }
    def writeFieldNameNotMissing(name: String): CoderResult[Unit] =
        tryCatchResultG(terminal) { clearLatched(); generator.writeFieldName(name); Okay.unit } >>
        (_objectFilters match {
            case filter :: _ => filter.fieldName(name)
            case _           => Okay.unit
        })

    def omitNextMissing(): Unit = { _omitNextMissing = true }
    def filterNextObject(newFilter: ObjectFilter): Unit = {
        _nextObjectFilter = Option(_nextObjectFilter) match {
            case Some(oldFilter) => ObjectFilter.combineFilters(oldFilter, newFilter)
            case None            => newFilter
        }
    }

    def writeBoolean(b: Boolean): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeBoolean(b); Okay.unit }
    def writeNumber(s: Short): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeNumber(s); Okay.unit }
    def writeNumber(i: Int): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeNumber(i); Okay.unit }
    def writeNumber(l: Long): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeNumber(l); Okay.unit }
    def writeNumber(f: Float): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeNumber(f); Okay.unit }
    def writeNumber(d: Double): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeNumber(d); Okay.unit }
    def writeNumber(bi: java.math.BigInteger): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeNumber(bi); Okay.unit }
    def writeNull(): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeNull(); Okay.unit }
    def writeString(s: String): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeString(s); Okay.unit }

    def writeStartArray(): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeStartArray(); Okay.unit }
    def writeEndArray(): CoderResult[Unit] =
        aboutToWriteToken() >> tryCatchResultG(terminal) { generator.writeEndArray(); Okay.unit }

    def writeStartObject(): CoderResult[Unit] =
        tryCatchResultG(terminal) {
            val nextObjectFilter = _nextObjectFilter
            aboutToWriteToken()
            if (nextObjectFilter != null) {
                _nextObjectFilter = null
                _objectFilters ::= nextObjectFilter
                if (!nextObjectFilter.suppressStartAndEnd) generator.writeStartObject()
                nextObjectFilter.beginning()
            } else if (_objectFilters.nonEmpty) {
                // push a null object filter so that the current object filter doesn't get calls until the current object is left
                _objectFilters ::= ObjectFilter.zero
                generator.writeStartObject()
                Okay.unit
            } else {
                generator.writeStartObject()
                Okay.unit
            }
        }
    def writeEndObject(): CoderResult[Unit] =
        tryCatchResultG(terminal) {
            clearLatched()

            _objectFilters match {
                case filter :: tail =>
                    _objectFilters = tail
                    filter.end() >> tryCatchResultG(terminal) {
                        if (!filter.suppressStartAndEnd) generator.writeEndObject()
                        Okay.unit
                    }
                case _ =>
                    generator.writeEndObject()
                    Okay.unit
            }
        }
}


/** Interface of things which can provide tokens from a source of JSON, usually `InterchangeJacksonJsonParser` */
trait InterchangeJsonParser {
    type Mark

    def hasValue: Boolean
    def currentValueIsMissing(): Unit
    def currentToken: JsonToken
    def currentLocation: JsonLocation
    def advanceToken(): CoderResult[Unit]
    def advanceTokenUnguarded(): CoderResult[Unit]
    def mark(): Mark
    def rewind(m: Mark): Unit

    def byteValue: Byte
    def shortValue: Short
    def intValue: Int
    def longValue: Long
    def bigIntegerValue: java.math.BigInteger
    def floatValue: Float
    def doubleValue: Double
    def bigDecimalValue: java.math.BigDecimal
    def stringValue: String
    def fieldName: String

    /** Perform some excursion into future JSON data, rewinding back to the beginning of the excursion when the enclosed function returns */
    def excursion[A](f: => A): A = {
        val m = mark()
        try f finally rewind(m)
    }

    /** Peek at the fields of the current object (expecting `currentToken` to be `START_OBJECT`) extracting the string value of each */
    def peekFields(names: Array[String]): CoderResult[Array[Option[String]]] =
        excursion {
            val out = Array.fill(names.length)(None: Option[String])
            var found = 0
            val skip = FailedG("done peeking fields", CoderFailure.terminal)
            foreachFields { name =>
                names.indexOf(name) match {
                    case -1 => skipToEndOfValue()
                    case n =>
                        if (out(n) == None) found += 1
                        out(n) = Some(stringValue)

                        if (found == names.length) skip
                        else skipToEndOfValue()
                }
            } match {
                case (_: Okay[_])|`skip` =>
                    Okay(out)
                case failed: FailedG[_] =>
                    failed
            }
        }

    /** Skip past the current value, even if it's complicated */
    def skipToEndOfValue(): CoderResult[Unit] = {
        //println(s"skipToEndOfValue: currentToken=$currentToken, currentLocation=$currentLocation")
        tryCatchResultG(terminal) {
            def go(depth: Int): Unit = {
                //println(s"go: depth=$depth, currentToken=$currentToken, currentLocation=$currentLocation")
                currentToken match {
                    case JsonToken.START_OBJECT|JsonToken.START_ARRAY =>
                        advanceTokenUnguarded().orThrow
                        go(depth+1)
                    case JsonToken.END_OBJECT|JsonToken.END_ARRAY if depth == 1 =>
                        ()
                    case JsonToken.END_OBJECT|JsonToken.END_ARRAY =>
                        advanceTokenUnguarded().orThrow
                        go(depth-1)
                    case _ if depth == 0 =>
                        ()
                    case null =>
                        sys.error("EOF reached while skipping")
                    case _ =>
                        advanceTokenUnguarded().orThrow
                        go(depth)
                }
            }
            go(0)
            Okay.unit
        }
    }

    /**
     * While positioned at the start of an object, call the function for each field in the enclosed object.
     * If any invocation of the function fails, abort the parse
     */
    def foreachFields(f: String => CoderResult[Unit]): CoderResult[Unit] =
        require(JsonToken.START_OBJECT) >>
        {
            @tailrec
            def iterate(): CoderResult[Unit] = {
                var atEnd = false
                val fieldResult = advanceTokenUnguarded() >> {
                    currentToken match {
                        case JsonToken.FIELD_NAME =>
                            val n = fieldName
                            advanceTokenUnguarded() >> f(n)
                        case JsonToken.END_OBJECT =>
                            atEnd = true
                            Okay.unit
                        case _ =>
                            unexpectedToken("field name")
                    }
                }

                fieldResult match {
                    case _: Okay[_] if !atEnd =>
                        iterate()
                    case other =>
                        other
                }
            }

            iterate()
        }

    /** Fail if the current token is not the given one */
    def require(token: JsonToken): CoderResult[Unit] =
        if (!hasValue || currentToken != token) unexpectedToken(if (token != null) token.asString else "<eof>")
        else Okay.unit

    /** Fail if the current token is not the given one */
    def requireValue: CoderResult[Unit] =
        if (!hasValue) unexpectedMissingValue
        else Okay.unit

    /** Format an error indicating that the current token was unexpected */
    def unexpectedToken(expectedWhat: String): CoderResult[Unit] = {
        val what =
            currentToken match {
                case null                         => "EOF"
                case JsonToken.FIELD_NAME         => s""" field "$fieldName" """.trim
                case JsonToken.VALUE_FALSE        => s"false"
                case JsonToken.VALUE_TRUE         => s"true"
                case JsonToken.VALUE_NULL         => s"null"
                case JsonToken.VALUE_NUMBER_INT   => s"integer ($bigIntegerValue)"
                case JsonToken.VALUE_NUMBER_FLOAT => s"decimal ($bigDecimalValue)"
                case JsonToken.VALUE_STRING       => s"string ($stringValue)"
                case JsonToken.START_OBJECT       => s"start of object ({)"
                case JsonToken.END_OBJECT         => s"end of object (})"
                case JsonToken.START_ARRAY        => s"start of array ([)"
                case JsonToken.END_ARRAY          => s"end of array (])"
                case other => other.toString
            }

        if (!hasValue)  FailedG(s"required but missing. Expected $expectedWhat, but found nothing.", terminal)
        else            FailedG(s"expected $expectedWhat but instead got $what", terminal)
    }

    /** Format an error indicating a value was expected but missing */
    def unexpectedMissingValue: CoderResult[Unit] =
        FailedG("required but missing", terminal)

    /** Compute the path name and source position of the JSON parser for use in error messages */
    def sourceLocation: Option[String] =
        currentLocation match {
            case loc if loc == JsonLocation.NA => None
            case loc                           => Some(s"${loc.getLineNr}:${loc.getColumnNr}")
        }

    /** An `orElse` failure processing function which sets the source location appropriately for the location of the parser */
    def noteSourceLocation: FailedG[CoderFailure] => FailedG[CoderFailure] =
        { case FailedG(t, p) => FailedG(t, p.copy(sourceLocation=this.sourceLocation)) }

    /** A terminal `CoderFailure` at the current parsing location */
    def terminal: CoderFailure =
        sourceLocation map CoderFailure.terminalAt getOrElse CoderFailure.terminal
}

object InterchangeJacksonJsonParser {
    /**
     * Recorded parse event along with any value.
     *
     * Recording a long stream of events can cause a bunch of allocs and slowdown as they will be stored in a pretty
     * inefficient form (BigInteger / BigDecimal / String) to make sure we have the full fidelity version since we won't
     * know how it will be used later.
     */
    sealed abstract class RecordedParseEvent {
        val token: JsonToken
    }

    final case class RecordedInteger(value: java.math.BigInteger) extends RecordedParseEvent {
        val token = JsonToken.VALUE_NUMBER_INT
    }

    final case class RecordedDecimal(value: java.math.BigDecimal) extends RecordedParseEvent {
        val token = JsonToken.VALUE_NUMBER_FLOAT
    }

    final case class RecordedString(value: String) extends RecordedParseEvent {
        val token = JsonToken.VALUE_STRING
    }

    final case class RecordedFieldName(field: String) extends RecordedParseEvent {
        val token = JsonToken.FIELD_NAME
    }

    case object RecordedStartObject extends RecordedParseEvent { val token = JsonToken.START_OBJECT }
    case object RecordedEndObject extends RecordedParseEvent { val token = JsonToken.END_OBJECT }

    case object RecordedStartArray extends RecordedParseEvent { val token = JsonToken.START_ARRAY }
    case object RecordedEndArray extends RecordedParseEvent { val token = JsonToken.END_ARRAY }

    case object RecordedTrue extends RecordedParseEvent { val token = JsonToken.VALUE_TRUE }
    case object RecordedFalse extends RecordedParseEvent { val token = JsonToken.VALUE_FALSE }
    case object RecordedNull extends RecordedParseEvent { val token = JsonToken.VALUE_NULL }

    val MIN_BYTE = new java.math.BigInteger("-128")
    val MAX_BYTE = new java.math.BigInteger("255")

    val MIN_SHORT = new java.math.BigInteger("-32768")
    val MAX_SHORT = new java.math.BigInteger("65535")

    val MIN_INT = new java.math.BigInteger("-2147483648")
    val MAX_INT = new java.math.BigInteger("4294967295")

    val MIN_LONG = new java.math.BigInteger("-9223372036854775808")
    val MAX_LONG = new java.math.BigInteger("18446744073709551615")

    final class Mark private[json] (val point: RecordBuffer) {
        var consumed: Boolean = false

        override def toString = s"Mark($point, consumed=$consumed)"
    }

    private[json] final class RecordBuffer(val event: RecordedParseEvent, val location: JsonLocation) {
        var next: RecordBuffer = null

        override def toString = {
            var i = 0
            var b = next
            while (b != null) {
                i += 1
                b = b.next
            }
            s"RecordBuffer($event, <location $location>, next=<$i more>)"
        }
    }
}

/**
 * Wrapper around a `JsonParser` which includes additional state to support missing values and record/rewind.
 *
 * Because the object decoding incrementally walks through field names and so knows which fields are missing by the end
 * but things like `nullableJsonDecoder` and `optionJsonDecoder` are what handles missing values, those decoders need to
 * be run but have it signalled to them that the value is missing.
 *
 * Unions are encoded intensionally with an additional field indicating which union alternative was encoded, so for decoding
 * those the union decoder needs to scan ahead into the object of find the discriminant, then rewind and replay the visited
 * tokens back to the alternate decoder.
 */
final class InterchangeJacksonJsonParser(parser: JsonParser) extends InterchangeJsonParser {
    import InterchangeJacksonJsonParser._

    type Mark = InterchangeJacksonJsonParser.Mark

    private var _currentValueIsMissing   = false
    private var _didCheckMissingValue = false // make sure to fail fast if anybody doesn't call `hasValue`
    private var _replaying: RecordBuffer = null
    private var _recording: RecordBuffer = null
    private var _activeMarks: List[Mark] = Nil

    /** Yield `true` if the current value is missing and `currentToken` should not be called */
    def hasValue: Boolean = {
        _didCheckMissingValue = true
        !_currentValueIsMissing
    }

    /** Signal that the current value is missing and that `currentToken` should fail because there is no valid token to give */
    def currentValueIsMissing(): Unit = {
        _didCheckMissingValue = false
        _currentValueIsMissing = true
    }

    /** Yield the current (i.e. most recently read) token */
    def currentToken: JsonToken =
        if (!_didCheckMissingValue) sys.error("decoder should have checked whether a value was present prior to calling currentToken")
        else if (_replaying == null) parser.getCurrentToken
        else _replaying.event.token

    /** Yield the location of the current token in the source material or `JsonLocation.NA` if not available */
    def currentLocation: JsonLocation =
        if (_replaying == null) parser.getCurrentLocation
        else _replaying.location


    /** Move the parser forward to the next token from the input. */
    def advanceToken(): CoderResult[Unit] = {
        _didCheckMissingValue = false
        _currentValueIsMissing = false
        _advance()
    }


    /**
     * Move the parser forward to the next token from the input but don't require a call to `hasValue` to follow.
     * This is the "real" form of `advanceToken` without the missing value protection.
     */
    def advanceTokenUnguarded(): CoderResult[Unit] = {
        _didCheckMissingValue = true
        _currentValueIsMissing = false
        _advance()
    }

    /** Mark the current location in the token stream and allow resuming to that point with `rewind` */
    def mark(): Mark = {
        if (!_didCheckMissingValue) sys.error("decoder should have checked whether a value was present prior to calling mark")

        val point =
            if (_replaying != null) {
                // marking while replaying, so just snap another copy of the replay point
                _replaying
            } else if (_recording != null) {
                // we're already recording, so just mark the current recording point
                _recording
            } else {
                // marking while reading from the parser, so set the recording point and use that
                val buf = _record()
                _recording = buf
                buf
            }

        val m = new Mark(point)
        _activeMarks ::= m
        //println(s"mark(): _recording = ${_recording}, _replaying = ${_replaying}, _activeMarks = ${_activeMarks}, mark = $m")
        m
    }

    /** Rewind to a given marked location */
    def rewind(m: Mark): Unit =
        if (m.consumed) sys.error("attempted to rewind back to consumed mark")
        else {
            _replaying = m.point
            m.consumed = true
            _activeMarks = _activeMarks.filterNot(_ == m)
            _didCheckMissingValue = true // mark blows up if you haven't checked
            _currentValueIsMissing = false
            //println(s"rewind($m): _replaying = ${_replaying}, _activeMarks = ${_activeMarks}, _recording = ${_recording}")

        }

    private def _advance(): CoderResult[Unit] = {
        //println(s"_advance(): _replaying = ${_replaying}, _activeMarks = ${_activeMarks}, _recording = ${_recording}")
        if (_replaying == null || _replaying.next == null) {
            _replaying = null

            // not replaying or there is no more to replay, so read from the underlying parser
            val res = tryCatchResultG(terminal) {
                parser.nextToken()
                Okay.unit
            }

            if (_activeMarks.nonEmpty) {
                // but if there are active marks we should record what we just read and advance the recording point
                val buf = _record()
                _recording.next = buf
                //println(s"_advance: appended new buf = $buf, _recording = ${_recording}")
                _recording = buf
            } else {
                // if no active marks, no reason to record so make sure to clear that
                _recording = null
            }

            res
        } else {
            // _replaying is non-null and has more links, so just advance to the next link and call it a day
            _replaying = _replaying.next
            Okay.unit
        }
    }

    private def _record(): RecordBuffer =
        _replaying match {
            case null =>
                val event = parser.getCurrentToken match {
                    case JsonToken.VALUE_NULL         => RecordedNull
                    case JsonToken.VALUE_TRUE         => RecordedTrue
                    case JsonToken.VALUE_FALSE        => RecordedFalse
                    case JsonToken.VALUE_STRING       => RecordedString(parser.getText)
                    case JsonToken.VALUE_NUMBER_INT   => RecordedInteger(parser.getBigIntegerValue)
                    case JsonToken.VALUE_NUMBER_FLOAT => RecordedDecimal(parser.getDecimalValue)
                    case JsonToken.START_ARRAY        => RecordedStartArray
                    case JsonToken.END_ARRAY          => RecordedEndArray
                    case JsonToken.START_OBJECT       => RecordedStartObject
                    case JsonToken.END_OBJECT         => RecordedEndObject
                    case JsonToken.FIELD_NAME         => RecordedFieldName(parser.getCurrentName)
                    case null                         => null // null means EOF in the parser, so this is acceptable
                    case other => sys.error(s"unexpected JSON token $other that shouldn't show up for normal JSON parser")
                }

                val buf = new RecordBuffer(event, parser.getCurrentLocation)
                //println(s"_record(): new buf = $buf")
                buf

            case buf =>
                sys.error("_record() called when not reading from the parser")
        }

    private def replayingEvent: RecordedParseEvent =
        if (_replaying == null) null
        else _replaying.event

    private def inBound(bi: java.math.BigInteger, min: java.math.BigInteger, max: java.math.BigInteger): Boolean =
        bi.compareTo(min) >= 0 && bi.compareTo(max) <= 0

    /** Yield the current integer value as a `Byte`, throwing `JsonParseException` if the current value is out of bounds or not an integer */
    def byteValue: Byte =
        replayingEvent match {
            case null                                                    => parser.getByteValue
            case RecordedInteger(bi) if !inBound(bi, MIN_BYTE, MAX_BYTE) => throw new JsonParseException("number out of bounds", currentLocation)
            case RecordedInteger(bi)                                     => bi.byteValue
            case _                                                       => throw new JsonParseException("not an integer", currentLocation)
        }

    /** Yield the current integer value as a `Short`, throwing `JsonParseException` if the current value is out of bounds or not an integer */
    def shortValue: Short =
        replayingEvent match {
            case null                                                      => parser.getShortValue
            case RecordedInteger(bi) if !inBound(bi, MIN_SHORT, MAX_SHORT) => throw new JsonParseException("number out of bounds", currentLocation)
            case RecordedInteger(bi)                                       => bi.shortValue
            case _                                                         => throw new JsonParseException("not an integer", currentLocation)
        }

    /** Yield the current integer value as a `Int`, throwing `JsonParseException` if the current value is out of bounds or not an integer */
    def intValue: Int =
        replayingEvent match {
            case null                                                  => parser.getIntValue
            case RecordedInteger(bi) if !inBound(bi, MIN_INT, MAX_INT) => throw new JsonParseException("number out of bounds", currentLocation)
            case RecordedInteger(bi)                                   => bi.intValue
            case _                                                     => throw new JsonParseException("not an integer", currentLocation)
        }

    /** Yield the current integer value as a `Short`, throwing `JsonParseException` if the current value is out of bounds or not an integer */
    def longValue: Long =
        replayingEvent match {
            case null                                                    => parser.getLongValue
            case RecordedInteger(bi) if !inBound(bi, MIN_LONG, MAX_LONG) => throw new JsonParseException("number out of bounds", currentLocation)
            case RecordedInteger(bi)                                     => bi.longValue
            case _                                                       => throw new JsonParseException("not an integer", currentLocation)
        }

    /** Yield the current integer value as a `BigInteger`, throwing `JsonParseException` if the current value is not an integer */
    def bigIntegerValue: java.math.BigInteger =
        replayingEvent match {
            case null                => parser.getBigIntegerValue
            case RecordedInteger(bi) => bi
            case _                   => throw new JsonParseException("not an integer", currentLocation)
        }

    /** Yield the current decimal value as a `Float`, throwing `JsonParseException` if the current value is not a decimal */
    def floatValue: Float =
        replayingEvent match {
            case null                => parser.getFloatValue
            case RecordedDecimal(bd) => bd.floatValue
            case _                   => throw new JsonParseException("not a decimal", currentLocation)
        }

    /** Yield the current decimal value as a `Double`, throwing `JsonParseException` if the current value is not a decimal */
    def doubleValue: Double =
        replayingEvent match {
            case null                => parser.getDoubleValue
            case RecordedDecimal(bd) => bd.doubleValue
            case _                   => throw new JsonParseException("not a decimal", currentLocation)
        }

    /** Yield the current decimal value as a `BigDecimal`, throwing `JsonParseException` if the current value is not a decimal */
    def bigDecimalValue: java.math.BigDecimal =
        replayingEvent match {
            case null                => parser.getDecimalValue
            case RecordedDecimal(bd) => bd
            case _                   => throw new JsonParseException("not a decimal", currentLocation)
        }

    /** Yield the current string value, throwing `JsonParseException` if the current value is not a string */
    def stringValue: String =
        replayingEvent match {
            case null              => parser.getText
            case RecordedString(s) => s
            case _                 => throw new JsonParseException("not a string", currentLocation)
        }

    /** Yield the current field name, throwing `JsonParseException` if the current value is not a field */
    def fieldName: String =
        replayingEvent match {
            case null                 => parser.getCurrentName
            case RecordedFieldName(s) => s
            case _                    => throw new JsonParseException("not a field name", currentLocation)
        }
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

/** Common attributes of both `JsonEncoder` and `JsonDecoder` */
trait JsonEncoderOrDecoder {
    /**
     * Whether this coder might encode to `null` and therefore need a wrapper.
     * The primary use case of this is stacked `Option`s - in the case of `Option[Option[String]]` for example, it's impossible to tell
     * whether `null` means `None` or `Some(None)` so this bit is investigated by the outer `Option` coder and if `true` then the outer coder
     * will wrap a `Some` with a one-element array, so that `Some(Some("foo"))` encodes as `["foo"]`, `Some(None)` encodes as `[]` and `None`
     * encodes as nothing or `null` like `Option[String]` would.
     */
    val mightBeNull: Boolean

    /**
     * Whether this coder will encode/decode from an object.
     * Used so that coders which want to add fields to / look at fields of an object encoded by this coder,
     * such as union coders using an intensional discriminant field ("type" field).
     */
    val codesAsObject: Boolean
}

/** Encoder which encodes JSON incrementally via a Jackson `JsonGenerator` */
@implicitNotFound(msg="No JsonEncoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.json.coders, or need to add some @derive annotations")
trait JsonEncoder[A] extends Encoder[A, JsonFormat.type] with JsonEncoderOrDecoder { outer =>
    /** Map a function over this encoder, yielding an encoder which takes in values of type `B` */
    def map[B](f: B => A): JsonEncoder[B] = mapKleisli(b => Okay(f(b)))

    /** Map a kleisli (function which may fail gracefully) over this encoder, yielding an encoder which takes in values of type `B` */
    def mapKleisli[B](k: B => Result[A]): JsonEncoder[B] = new JsonEncoder[B] {
        val mightBeNull = outer.mightBeNull
        val codesAsObject = outer.codesAsObject

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
        tryCatchResult {
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
        tryCatchResult {
            withResource(jsonFactory.createGenerator(out)) { gen =>
                toGenerator(in, gen, pretty=pretty)
            }
        }

    /** Encode a value to a `JsonGenerator`. This differs from `run` in that it returns `Result[Unit]` not `ResultG[FailedPath, Unit]` */
    def toGenerator(in: A, gen: JsonGenerator, pretty: Boolean = false): Result[Unit] = {
        if (pretty) gen.useDefaultPrettyPrinter()
        formatFailedPath(run(in, new InterchangeJacksonJsonGenerator(gen)))
    }
}

object JsonDecoder extends JsonDecoderLPI {
    /** Materializer for JsonDecoder, just finds the JsonDecoder in implicit scope. For example: JsonDecoder[Int] */
    def apply[A](implicit decoder: JsonDecoder[A]): JsonDecoder[A] = implicitly
}

trait JsonDecoderLPI {
    implicit def fromCoder[A](implicit coder: JsonCoder[A]): JsonDecoder[A] = coder.decode
}

/** Decoder which consumes JSON via a the Jackson `JsonParser`, as wrapped with a `InterchangeJsonParser` which manages Interchange parsing state */
@implicitNotFound(msg="No JsonDecoder for ${A} found in the implicit scope. Perhaps you forgot to import something from com.paytronix.utils.interchange.format.json.coders, or need to add some @derive annotations")
trait JsonDecoder[A] extends Decoder[A, JsonFormat.type] with JsonEncoderOrDecoder { outer =>
    /** Map a function over this decoder, yielding an decoder which produces values of type `B` by transforming `A`s using the given function */
    def map[B](f: A => B): JsonDecoder[B] = mapKleisli(a => Okay(f(a)))

    /** Map a kleisli (function which may fail gracefully) over this decoder, yielding an decoder which produces values of type `B` */
    def mapKleisli[B](k: A => Result[B]): JsonDecoder[B] = new JsonDecoder[B] {
        val mightBeNull = outer.mightBeNull
        val codesAsObject = outer.codesAsObject

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

    /** Wrap this decoder with a `defaultJsonDecoder` to provide a default in the case where the value is missing or null */
    def default(value: A): JsonDecoder[A] =
        container.defaultJsonDecoder(value)(this)

    /** Attempt conversion of a byte array to a value of the mapped type */
    def fromBytes(in: Array[Byte])(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        fromBytesRange(in, 0, in.length)

    /** Attempt conversion of a byte array to a value of the mapped type */
    def fromBytesRange(in: Array[Byte], offset: Int, length: Int)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        tryCatchResult {
            withResource(jsonFactory.createParser(in, offset, length))(fromParser)
        }

    /** Attempt conversion of a character array to a value of the mapped type */
    def fromChars(in: Array[Char])(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] = fromCharsRange(in, 0, in.length)

    /** Attempt conversion of a character array to a value of the mapped type */
    def fromCharsRange(in: Array[Char], offset: Int, length: Int)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        tryCatchResult {
            withResource(jsonFactory.createParser(in, offset, length))(fromParser)
        }

    /** Attempt conversion of an input stream to a value of the mapped type  */
    def fromInputStream(in: InputStream)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        tryCatchResult {
            withResource(jsonFactory.createParser(in))(fromParser)
        }

    /** Attempt conversion of JSON string to a value of the mapped type  */
    def fromString(in: String)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        withResource(new StringReader(in)) { sr =>
            fromReader(sr)
        }

    /** Attempt conversion of an input reader to a value of the mapped type  */
    def fromReader(in: Reader)(implicit jsonFactory: JsonFactory = JsonFormat.defaultFactory): Result[A] =
        tryCatchResult {
            withResource(jsonFactory.createParser(in))(fromParser)
        }

    /**
     * Decode a value from a `JsonParser`.
     * This differs from `run` in that it yields `Result[A]` rather than taking a `Receiver[A]` and yielding a `ResultG[FailedPath, Unit]`.
     */
    def fromParser(in: JsonParser): Result[A] = {
        val receiver = new Receiver[A]
        val ijp = new InterchangeJacksonJsonParser(in)
        tryCatchValue(ijp.advanceToken()) >> formatFailedPath(run(ijp, receiver)).map { _ => receiver.value }
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
     * For example, to make a `JsonCoder` that is encoded in Json as a string but decodes to Scala as a sequence of space separated tokens:
     *
     *    val tokenizedStringJsonCoder = scalar.stringJsonCoder.mapBijection(bijection (
     *        (tokens: Seq[String]) => Okay(tokens.mkString(" ")),
     *        (s: String) => Okay(s.split(' '))
     *    ))
     */
    def mapWithConverter[B](converter: TypeConverter[B, A]): JsonCoder[B] = JsonCoder.make(encode.mapKleisli(converter.to), decode.mapKleisli(converter.from))

    /** Wrap the decoder with a `defaultJsonDecoder` to provide a default in the case where the value is missing or null */
    def default(value: A): JsonCoder[A] =
        JsonCoder.make(encode, decode.default(value))
}

/**
 * Specify that a field's value should "flatten" into the enclosing object rather than being a discrete field.
 *
 * For example, the usual coding of:
 *
 *     @derive.structure.implicitCoder
 *     final case class Foo(a: Int, b: Bar)
 *     @derive.structure.implicitCoder
 *     final case class Bar(c: Int, d: Int)
 *
 * would be:
 *
 *     {
 *         "a": 123,
 *         "b": { "c": 123, "d": 123 }
 *     }
 *
 * however if flattening is turned on for Bar:
 *
 *     @derive.structure.implicitCoder
 *     final case class Foo(a: Int, @flatten b: Bar)
 *     @derive.structure.implicitCoder
 *     final case class Bar(c: Int, d: Int)
 *
 * then that coding becomes;
 *
 *     {
 *         "a": 123,
 *         "c": 123, "d": 123
 *     }
 */
class flatten extends StaticAnnotation


