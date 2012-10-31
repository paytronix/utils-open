//
// Copyright 2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

import scala.collection.mutable.Builder

import net.liftweb.json.JsonAST.{JArray, JBool, JDouble, JField, JInt, JNothing, JNull, JObject, JString, JValue}
import net.liftweb.json.JsonParser.{Parser, BoolVal, DoubleVal, IntVal, NullVal, StringVal, End, OpenArr, CloseArr, OpenObj, FieldStart, CloseObj, Token}

object json {
    /** Consume one entire value (such as string, number, object, array) from the pull parser, transforming it into a JValue */
    def pullOneAST(p: Parser): Either[Token, JValue] = {
        sealed abstract class Frame
        final case class ArrayFrame(builder: Builder[JValue, List[JValue]] = List.newBuilder) extends Frame
        final case class ObjectFrame(builder: Builder[JField, List[JField]] = List.newBuilder, var curField: String = null) extends Frame

        var stack: List[Frame] = Nil
        var done = false
        var result: Either[Token, JValue] = null

        while (!done) {
            def completed(in: JValue): Unit =
                stack match {
                    case Nil =>
                        result = Right(in)
                        done = true
                    case ArrayFrame(builder) :: _ =>
                        builder += in
                    case (f@ObjectFrame(builder, null)) :: _ =>
                        sys.error("unexpected value " + in + "; expected field name")
                    case (f@ObjectFrame(builder, curField)) :: _ =>
                        builder += JField(f.curField, in)
                        f.curField = null

                }

            p.nextToken match {
                case End =>
                    stack match {
                        case Nil =>
                            result = Left(End)
                            done = true
                        case (_: ArrayFrame) :: _ =>
                            sys.error("unexpected end of input in array")
                        case (_: ObjectFrame) :: _ =>
                            sys.error("unexpected end of input in object")
                    }

                case FieldStart(field) =>
                    stack match {
                        case Nil =>
                            sys.error("unexpected field name at top")
                        case (f@ObjectFrame(_, null)) :: _ =>
                            f.curField = field
                        case ObjectFrame(_, field) :: _ =>
                            sys.error("expected value for field " + field)
                        case (_: ArrayFrame) :: _ =>
                            sys.error("unexpected field name in array")
                    }

                case OpenObj =>
                    stack ::= ObjectFrame()

                case OpenArr =>
                    stack ::= ArrayFrame()

                case CloseObj =>
                    stack match {
                        case Nil =>
                            result = Left(CloseObj)
                            done = true
                        case (f@ObjectFrame(builder, null)) :: rest =>
                            stack = rest
                            completed(JObject(builder.result()))
                        case (f@ObjectFrame(builder, field)) :: _ =>
                            sys.error("unexpected end of object immediately following field " + field)
                        case (_: ArrayFrame) :: _ =>
                            sys.error("unexpected } in array")
                    }

                case CloseArr =>
                    stack match {
                        case Nil =>
                            result = Left(CloseArr)
                            done = true
                        case ArrayFrame(builder) :: rest =>
                            stack = rest
                            completed(JArray(builder.result()))
                        case (_: ObjectFrame) :: _ =>
                            sys.error("unexpected ] in object")
                    }

                case StringVal(v) => completed(JString(v))
                case IntVal(v)    => completed(JInt(v))
                case DoubleVal(v) => completed(JDouble(v))
                case BoolVal(v)   => completed(JBool(v))
                case NullVal      => completed(JNull)
            }
        }

        result match {
            case null =>
                sys.error("expected result to be populated")
            case other =>
                other
        }
    }
}
