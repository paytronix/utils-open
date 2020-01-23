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

package com.paytronix.utils.interchange.format

import scala.collection.mutable.Stack

import com.fasterxml.jackson.core.{JsonLocation, JsonParseException, JsonToken}
import net.liftweb.json.JsonAST.{JValue, JArray, JBool, JDouble, JField, JInt, JNothing, JNull, JObject, JString, compactRender}

import com.paytronix.utils.interchange.base.{CoderFailure, CoderResult, Receiver, formatFailedPath}
import com.paytronix.utils.interchange.format.json.{JsonDecoder, JsonEncoder, InterchangeJsonGenerator, InterchangeJsonParser}
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, tryCatchValue}
import string.coders.stringCoder

package object liftjson {
    /** Wrap a `JsonEncoder` with an additional operation to convert to a `JValue` */
    implicit class jsonEncoderOps[A](jsonEncoder: JsonEncoder[A]) {
        /** Encode a value to a `JValue` */
        def toJValue(in: A): Result[JValue] = {
            val gen = new InterchangeLiftJsonGenerator()
            formatFailedPath(jsonEncoder.run(in, gen)) >> gen.jvalue
        }
    }

    /** Wrap a `JsonDecoder` with an additional operation to convert from a `JValue` */
    implicit class jsonDecoderOps[A](jsonDecoder: JsonDecoder[A]) {
        /** Encode a `JValue` to a value */
        def fromJValue(in: JValue): Result[A] = {
            val rec = new Receiver[A]
            val par = new InterchangeLiftJsonParser(in)
            formatFailedPath(jsonDecoder.run(par, rec)) map { _ => rec.value }
        }
    }
}

package liftjson {
    /** Implementation of `InterchangeJsonGenerator` which produces a `JValue` */
    final class InterchangeLiftJsonGenerator extends InterchangeJsonGenerator {
        import InterchangeJsonGenerator._

        private sealed abstract class Frame
        private case object TopFrame extends Frame { val receiver = new Receiver[JValue] }
        private final class ArrayFrame extends Frame { val builder = List.newBuilder[JValue] }
        private final class ObjectFrame extends Frame {
            val builder = List.newBuilder[JField]
            var fieldName: String = null
        }

        private val _stack: Stack[Frame] = Stack(TopFrame)
        private var _omitNextMissing: Boolean = false
        private var _nextObjectFilter: ObjectFilter = null
        private var _objectFilters: List[ObjectFilter] = Nil

        def jvalue: Result[JValue] = tryCatchValue(_stack.top) >>= {
            case TopFrame => Okay(TopFrame.receiver.value)
            case _ => Failed(s"not at top level of stack! stack is: ${_stack}")
        }

        private def clearLatched(fieldName: Boolean = true): Unit = {
            if (fieldName) {
                _stack.top match {
                    case of: ObjectFrame => of.fieldName = null
                    case _ => ()
                }
            }
            _omitNextMissing = false
            _nextObjectFilter = null
        }

        private def emitValue(jv: JValue): CoderResult[Unit] =
            _stack.top match {
                case af: ArrayFrame if jv == JNothing && _omitNextMissing =>
                    clearLatched()
                    Okay.unit

                case af: ArrayFrame =>
                    af.builder += (if (jv == JNothing) JNull else jv)
                    clearLatched()
                    Okay.unit

                case of: ObjectFrame if of.fieldName == null =>
                    FailedG("tried to emit a value when no field name given", CoderFailure.terminal)

                case of: ObjectFrame if jv == JNothing && _omitNextMissing =>
                    clearLatched()
                    Okay.unit

                case of: ObjectFrame =>
                    val filterResult = _objectFilters match {
                        case filter :: _ => filter.fieldName(of.fieldName)
                        case _           => Okay.unit
                    }

                    filterResult >> {
                        of.builder += JField(of.fieldName, if (jv == JNothing) JNull else jv)
                        clearLatched()
                        Okay.unit
                    }

                case TopFrame =>
                    TopFrame.receiver(if (jv == JNothing) JNull else jv)
            }

        def writeNothingOrNull(): CoderResult[Unit] = emitValue(JNothing)

        def writeFieldName(name: String): CoderResult[Unit] =
            _stack.top match {
                case of: ObjectFrame =>
                    of.fieldName = name
                    _omitNextMissing = true
                    Okay.unit
                case _ =>
                    FailedG("not in an object, can't emit field names", CoderFailure.terminal)
            }

        def writeFieldNameNotMissing(name: String): CoderResult[Unit] =
            _stack.top match {
                case of: ObjectFrame =>
                    of.fieldName = name
                    _omitNextMissing = false
                    Okay.unit
                case _ =>
                    FailedG("not in an object, can't emit field names", CoderFailure.terminal)
            }

        def omitNextMissing(): Unit = { _omitNextMissing = true }
        def filterNextObject(newFilter: ObjectFilter): Unit = {
            _nextObjectFilter = Option(_nextObjectFilter) match {
                case Some(oldFilter) => ObjectFilter.combineFilters(oldFilter, newFilter)
                case None            => newFilter
            }
        }

        def writeBoolean(b: Boolean): CoderResult[Unit] = emitValue(JBool(b))
        def writeNumber(s: Short): CoderResult[Unit] = emitValue(JInt(BigInt(s)))
        def writeNumber(i: Int): CoderResult[Unit] = emitValue(JInt(BigInt(i)))
        def writeNumber(l: Long): CoderResult[Unit] = emitValue(JInt(BigInt(l)))
        def writeNumber(f: Float): CoderResult[Unit] = emitValue(JDouble(f))
        def writeNumber(d: Double): CoderResult[Unit] = emitValue(JDouble(d))
        def writeNumber(bi: java.math.BigInteger): CoderResult[Unit] = emitValue(JInt(bi))
        def writeNull(): CoderResult[Unit] = emitValue(JNull)
        def writeString(s: String): CoderResult[Unit] = emitValue(JString(s))

        def writeStartArray(): CoderResult[Unit] = {
            clearLatched(false)
            _stack.push(new ArrayFrame())
            Okay.unit
        }

        def writeEndArray(): CoderResult[Unit] = {
            clearLatched()
            _stack.pop match {
                case af: ArrayFrame => emitValue(JArray(af.builder.result()))
                case other => FailedG(s"mismatched call to writeEndArray when current stack frame is $other", CoderFailure.terminal)
            }
        }

        def writeStartObject(): CoderResult[Unit] = {
            val filter = _nextObjectFilter
            clearLatched(false)
            if (filter != null) {
                _objectFilters ::= filter
                if (!filter.suppressStartAndEnd) _stack.push(new ObjectFrame())
                filter.beginning()
            } else if (_objectFilters.nonEmpty) {
                _objectFilters ::= ObjectFilter.zero
                _stack.push(new ObjectFrame())
                Okay.unit
            } else {
                _stack.push(new ObjectFrame())
                Okay.unit
            }
        }

        def writeEndObject(): CoderResult[Unit] = {
            clearLatched()

            def pop() =
                _stack.pop match {
                    case of: ObjectFrame => emitValue(JObject(of.builder.result()))
                    case other => FailedG(s"mismatched call to writeEndObject when current stack frame is $other", CoderFailure.terminal)
                }


            _objectFilters match {
                case filter :: tail =>
                    _objectFilters = tail
                    filter.end() >> {
                        if (!filter.suppressStartAndEnd) pop()
                        Okay.unit
                    }
                case _ =>
                    pop()
                    Okay.unit
            }
        }
    }

    object InterchangeLiftJsonParser {
        val MIN_BYTE = BigInt("-128")
        val MAX_BYTE = BigInt("255")

        val MIN_SHORT = BigInt("-32768")
        val MAX_SHORT = BigInt("65535")

        val MIN_INT = BigInt("-2147483648")
        val MAX_INT = BigInt("4294967295")

        val MIN_LONG = BigInt("-9223372036854775808")
        val MAX_LONG = BigInt("18446744073709551615")
    }

    /** Implementation of `InterchangeJsonParser` which consumes a `JValue` */
    final class InterchangeLiftJsonParser(root: JValue) extends InterchangeJsonParser {
        import InterchangeLiftJsonParser._

        sealed abstract class Frame { def dup: Frame }
        final case class TopFrame(var visited: Boolean) extends Frame {
            def dup = copy()
        }
        final case class ArrayFrame(var rest: List[JValue]) extends Frame {
            def dup = copy()
            override def toString = s"ArrayFrame(${rest.size} els)"
        }
        final case class ObjectFrame(var lookingAtFieldName: Boolean, var rest: List[JField]) extends Frame {
            def dup = copy()
            override def toString = s"ObjectFrame($lookingAtFieldName, ${rest.map { case JField(n, _) => n }.mkString(", ")})"
        }

        type Mark = List[Frame]

        private var _stack: Stack[Frame] = Stack(TopFrame(false))
        private var _currentValueIsMissing = false
        private var _didCheckMissingValue = false

        def hasValue: Boolean = {
            _didCheckMissingValue = true
            !_currentValueIsMissing
        }

        def currentValueIsMissing(): Unit = {
            _didCheckMissingValue = false
            _currentValueIsMissing = true
        }

        def currentToken: JsonToken =
            if (!_didCheckMissingValue) sys.error("decoder should have checked whether a value was present prior to calling currentToken")
            else {
                def jvToTok(jv: JValue): JsonToken =
                    jv match {
                        case JArray(_)      => JsonToken.START_ARRAY
                        case JBool(true)    => JsonToken.VALUE_TRUE
                        case JBool(false)   => JsonToken.VALUE_FALSE
                        case JDouble(_)     => JsonToken.VALUE_NUMBER_FLOAT
                        case JInt(_)        => JsonToken.VALUE_NUMBER_INT
                        case JNull|JNothing => JsonToken.VALUE_NULL
                        case JObject(_)     => JsonToken.START_OBJECT
                        case JString(_)     => JsonToken.VALUE_STRING
                    }

                val tok = _stack.top match {
                    case TopFrame(false)                        => jvToTok(root)
                    case TopFrame(true)                         => sys.error("past end of input")
                    case ArrayFrame(Nil)                        => JsonToken.END_ARRAY
                    case ArrayFrame(jv :: _)                    => jvToTok(jv)
                    case ObjectFrame(_, Nil)                    => JsonToken.END_OBJECT
                    case ObjectFrame(true, _)                   => JsonToken.FIELD_NAME
                    case ObjectFrame(false, JField(_, jv) :: _) => jvToTok(jv)
                }

                //println(s"${Thread.currentThread}: currentToken = $tok | stack = ${_stack}")

                tok
            }

        def currentLocation: JsonLocation = JsonLocation.NA

        def advanceToken(): CoderResult[Unit] = {
            _didCheckMissingValue = false
            _currentValueIsMissing = false
            _advance()
        }

        def advanceTokenUnguarded(): CoderResult[Unit] = {
            _didCheckMissingValue = true
            _currentValueIsMissing = false
            _advance()
        }

        private def _advance(): CoderResult[Unit] = {
            //println(s"${Thread.currentThread}: _advance()")
            _stack.top match {
                case TopFrame(true) =>
                    FailedG("at end of input", CoderFailure.terminal)

                case f@TopFrame(_) =>
                    root match {
                        case JArray(els) =>
                            _stack.push(ArrayFrame(els))
                            f.visited = true
                            Okay.unit

                        case JObject(flds) =>
                            _stack.push(ObjectFrame(true, flds))
                            f.visited = true
                            Okay.unit

                        case _ =>
                            FailedG("at end of input", CoderFailure.terminal)
                    }

                case ArrayFrame(Nil) =>
                    _stack.pop
                    Okay.unit

                case f@ArrayFrame(JArray(els) :: tail) =>
                    _stack.push(ArrayFrame(els))
                    f.rest = tail
                    Okay.unit

                case f@ArrayFrame(JObject(flds) :: tail) =>
                    _stack.push(ObjectFrame(true, flds))
                    f.rest = tail
                    Okay.unit

                case f@ArrayFrame(_ :: tail) =>
                    f.rest = tail
                    Okay.unit

                case ObjectFrame(_, Nil) =>
                    _stack.pop
                    Okay.unit

                case f@ObjectFrame(true, _) =>
                    f.lookingAtFieldName = false
                    Okay.unit

                case f@ObjectFrame(false, JField(_, JArray(els)) :: tail) =>
                    _stack.push(ArrayFrame(els))
                    f.lookingAtFieldName = true
                    f.rest = tail
                    Okay.unit

                case f@ObjectFrame(false, JField(_, JObject(flds)) :: tail) =>
                    _stack.push(ObjectFrame(true, flds))
                    f.lookingAtFieldName = true
                    f.rest = tail
                    Okay.unit

                case f@ObjectFrame(false, _ :: tail) =>
                    f.lookingAtFieldName = true
                    f.rest = tail
                    Okay.unit
            }
        }

        def mark(): Mark = {
            if (!_didCheckMissingValue) sys.error("decoder should have checked whether a value was present prior to calling mark")
            _stack.toList.map(_.dup)
        }

        def rewind(m: Mark): Unit = {
            _stack = Stack(m: _*)
            _didCheckMissingValue = true
            _currentValueIsMissing = false
        }

        private def inBound(bi: BigInt, min: BigInt, max: BigInt): Boolean =
            bi >= min && bi <= max

        private def currentJValue: JValue =
            _stack.top match {
                case TopFrame(false)                        => root
                case ArrayFrame(jv :: _)                    => jv
                case ObjectFrame(false, JField(_, jv) :: _) => jv
                case _                                      => throw new JsonParseException("not at a value token", JsonLocation.NA)
            }

        def byteValue: Byte =
            currentJValue match {
                case JInt(bi) if inBound(bi, MIN_BYTE, MAX_BYTE) => bi.byteValue
                case JInt(_)                                     => throw new JsonParseException("number out of bounds", JsonLocation.NA)
                case _                                           => throw new JsonParseException("not an integer", JsonLocation.NA)
            }

        def shortValue: Short =
            currentJValue match {
                case JInt(bi) if inBound(bi, MIN_SHORT, MAX_SHORT) => bi.shortValue
                case JInt(_)                                       => throw new JsonParseException("number out of bounds", JsonLocation.NA)
                case _                                             => throw new JsonParseException("not an integer", JsonLocation.NA)
            }

        def intValue: Int =
            currentJValue match {
                case JInt(bi) if inBound(bi, MIN_INT, MAX_INT) => bi.intValue
                case JInt(_)                                   => throw new JsonParseException("number out of bounds", JsonLocation.NA)
                case _                                         => throw new JsonParseException("not an integer", JsonLocation.NA)
            }

        def longValue: Long =
            currentJValue match {
                case JInt(bi) if inBound(bi, MIN_LONG, MAX_LONG) => bi.longValue
                case JInt(_)                                     => throw new JsonParseException("number out of bounds", JsonLocation.NA)
                case _                                           => throw new JsonParseException("not an integer", JsonLocation.NA)
            }

        def bigIntegerValue: java.math.BigInteger =
            currentJValue match {
                case JInt(bi) => bi.bigInteger
                case _        => throw new JsonParseException("not an integer", JsonLocation.NA)
            }

        def floatValue: Float =
            currentJValue match {
                case JDouble(d) => d.asInstanceOf[Float]
                case JInt(bi)   => bi.floatValue
                case _          => throw new JsonParseException("not a number", JsonLocation.NA)
            }

        def doubleValue: Double =
            currentJValue match {
                case JDouble(d) => d
                case JInt(bi)   => bi.doubleValue
                case _          => throw new JsonParseException("not a number", JsonLocation.NA)
            }

        def bigDecimalValue: java.math.BigDecimal =
            currentJValue match {
                case JDouble(d) => java.math.BigDecimal.valueOf(d)
                case JInt(bi)   => new java.math.BigDecimal(bi.bigInteger)
                case _          => throw new JsonParseException("not a number", JsonLocation.NA)
            }

        def stringValue: String =
            currentJValue match {
                case JString(s)   => s
                case JInt(bi)     => bi.toString
                case JDouble(d)   => d.toString
                case JBool(b)     => b.toString
                case a@JArray(_)  => compactRender(a)
                case o@JObject(_) => compactRender(o)
                case JNull        => "null"
                case _            => throw new JsonParseException("not a string", JsonLocation.NA)
            }

        def fieldName: String =
            _stack.top match {
                case ObjectFrame(true, JField(name, _) :: _) => name
                case _ => throw new JsonParseException("not at a field name", JsonLocation.NA)
            }
    }
}
