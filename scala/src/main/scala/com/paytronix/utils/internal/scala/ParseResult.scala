//
// Copyright 2009-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

import net.liftweb.common.{Box, Empty, Failure, Full}

/** Return type for functions that have parse failures that are typically (but not always) annotated with line and column information */
sealed abstract class ParseResult[+A] {
    def map[B](f: A => B): ParseResult[B]
    def flatMap[B](f: A => ParseResult[B]): ParseResult[B]
}

/** Case class indicating successful parsing, carrying along the typed result */
case class ParseSucceeded[+A](val value: A) extends ParseResult[A] {
    def map[B](f: A => B): ParseResult[B] = ParseSucceeded(f(value))
    def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = f(value)
}

/** Case class indicating failure to parse, carrying the list of ParseErrors */
case class ParseFailed(val errors: Seq[ParseError]) extends ParseResult[Nothing] {
    def map[B](f: Nothing => B): ParseResult[B] = this
    def flatMap[B](f: Nothing => ParseResult[B]): ParseResult[B] = this
    def asType[B]: ParseResult[B] = this
}

object ParseResult {
    /** Format a sequence of error messages from a ParseResult into a list of strings with "At line x, column y: message" */
    def formatParseErrors(errors: Seq[ParseError]): Seq[String] = errors.map(_.format)

    /** Convert an Either type as returned by stringToXhtml, stringToXhtmlFragment, etc into a Box with a decently formatted error message */
    implicit def parseResultToBox[A](e: ParseResult[A]): Box[A] =
        e match {
            case ParseFailed(errors) => Failure(formatParseErrors(errors).mkString("\n"), Empty, Empty)
            case ParseSucceeded(value) => Full(value)
        }
}

/** Abstract base class of parse errors, the usual two subclasses being for located (with line and column) and unlocated errors */
sealed abstract class ParseError {
    def message: String
    def format: String
}

object ParseError {
    case class LineAndColumn(line: Int, column: Int, message: String) extends ParseError {
        def format = "At line " + line + ", column " + column + ": " + message
    }

    case class Line(line: Int, message: String) extends ParseError {
        def format = "At line " + line + ": " + message
    }

    case class Unlocated(message: String) extends ParseError {
        def format = message
    }
}
