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

import java.io.IOException

import net.liftweb.json.JsonParser
import org.apache.avro.Schema
import org.apache.avro.io.BinaryDecoder

import com.paytronix.utils.scala.result.{Failed, FailedG, FailedParameter, Okay, Result, ResultG, tryCatch}

import JsonParser.{CloseArr, End}
import json.pullOneAST

/**
 * Functions for reading potentially giant streams of encoded objects from JSON and Avro inputs using an `Iterator`.
 * See `stream` for `Stream` versions, which are potentially more dangerous to use
 */
object iterator {
    /** Result parameter for pull iterators */
    type IteratorError = stream.StreamError
    /** Iteration failed due to some underlying I/O error */
    val IteratorIOError = stream.StreamIOError
    /** Iteration failed due to some formatting error in the container format (e.g. closing a JSON array with }, or premature end of stream */
    val IteratorFormatError = stream.StreamFormatError
    /** Iteration failed due to an error while decoding using a `Coder` */
    val IteratorDecodeError = stream.StreamDecodeError

    /**
     * Stream which transforms JSON nodes from a `JsonParser` until an end of array signal.
     */
    def jsonArray[A] (
        coder: Coder[A], parser: JsonParser.Parser,
        end: => Stream[ResultG[IteratorError, A]] = Stream.empty
    ): Iterator[ResultG[IteratorError, A]] =
        stream.jsonArray(coder, parser, end).iterator


    /**
     * Stream which reads items out of a stream of Avro data until there is no more data left from the decoder
     * See `avroArray` for one appropriate for reading an array of items
     */
    def avroItems[A] (
        coder: Coder[A], writerSchema: Schema, decoder: BinaryDecoder,
        end: => Stream[ResultG[IteratorError, A]] = Stream.empty
    ): Iterator[ResultG[IteratorError, A]] =
        stream.avroItems(coder, writerSchema, decoder, end).iterator

    /**
     * Stream which reads items out of an Avro array until the array is exhausted.
     * See `avroItems` for one appropriate for reading a stream of items bounded only by available input
     */
    def avroArray[A] (
        coder: Coder[A], writerSchema: Schema, decoder: BinaryDecoder,
        end: => Stream[ResultG[IteratorError, A]] = Stream.empty
    ): Iterator[ResultG[IteratorError, A]] =
        stream.avroArray(coder, writerSchema, decoder, end).iterator
}

/**
 * Functions for reading potentially giant streams of encoded objects from JSON and Avro inputs using a `Stream`.
 * See `iterator` for `Iterator` versions, which are potentially more dangerous to use as it's impossible to accidentally accumulate the whole sequence of objects.
 */
object stream {
    /** Result parameter for pull iterators */
    sealed abstract class StreamError extends FailedParameter
    /** Iteration failed due to some underlying I/O error */
    final case object StreamIOError extends StreamError
    /** Iteration failed due to some formatting error in the container format (e.g. closing a JSON array with }, or premature end of stream */
    final case object StreamFormatError extends StreamError
    /** Iteration failed due to an error while decoding using a `Coder` */
    final case object StreamDecodeError extends StreamError

    /**
     * Stream which transforms JSON nodes from a `JsonParser` until an end of array signal.
     * <strong>BIG OLD CAUTION</strong> using Stream for large sequences can be dangerous as you can easily hold onto the head of the stream by accident and accumulate
     * the whole sequence. If you can, use the `iterator.jsonArray` version instead.
     */
    def jsonArray[A] (
        coder: Coder[A], parser: JsonParser.Parser,
        end: => Stream[ResultG[StreamError, A]] = Stream.empty
    ): Stream[ResultG[StreamError, A]] =
        pullOneAST(parser) match {
            case Okay(Left(End)) =>
                Stream.cons(FailedG("unexpected end of JSON stream, was expecting end of array", StreamFormatError), end)

            case Okay(Left(CloseArr)) =>
                end

            case Okay(Left(other)) =>
                Stream.cons(FailedG("unexpected " + other + ", was expecting end of array", StreamFormatError), end)

            case Okay(Right(jvalue)) =>
                Stream.cons(coder.decode(jvalue) | StreamDecodeError, jsonArray(coder, parser))

            case failure@FailedG(_, _) =>
                Stream.cons(failure | "failed to parse JSON midway through stream" | StreamIOError, end)
        }

    /**
     * Stream which reads items out of a stream of Avro data until there is no more data left from the decoder
     * See `avroArray` for one appropriate for reading an array of items
     * <strong>BIG OLD CAUTION</strong> using Stream for large sequences can be dangerous as you can easily hold onto the head of the stream by accident and accumulate
     * the whole sequence. If you can, use the `iterator.avroItems` version instead.
     */
    def avroItems[A] (
        coder: Coder[A], writerSchema: Schema, decoder: BinaryDecoder,
        end: => Stream[ResultG[StreamError, A]] = Stream.empty
    ): Stream[ResultG[StreamError, A]] =
        if (decoder.isEnd)
            end
        else
            (coder.decodeAvro(writerSchema, decoder) | StreamDecodeError) match {
                case o@Okay(_)                    => Stream.cons(o, avroItems(coder, writerSchema, decoder))
                case f@FailedG(_: IOException, _) => Stream.cons(f | StreamIOError, end)
                case f@FailedG(_, _)              => Stream.cons(f | StreamFormatError, end)
            }

    /**
     * Stream which reads items out of an Avro array until the array is exhausted.
     * See `avroItems` for one appropriate for reading a stream of items bounded only by available input
     * <strong>BIG OLD CAUTION</strong> using Stream for large sequences can be dangerous as you can easily hold onto the head of the stream by accident and accumulate
     * the whole sequence. If you can, use the `iterator.avroArray` version instead.
     */
    def avroArray[A] (
        coder: Coder[A], writerSchema: Schema, decoder: BinaryDecoder,
        end: => Stream[ResultG[StreamError, A]] = Stream.empty
    ): Stream[ResultG[StreamError, A]] = {
        def consume(remaining: Long, limit: Long): Stream[ResultG[StreamError, A]] =
            if (limit == 0) end
            else if (remaining > 0)
                (coder.decodeAvro(writerSchema, decoder) | StreamDecodeError) match {
                    case o@Okay(_)                    => Stream.cons(o, consume(remaining-1, limit))
                    case f@FailedG(_: IOException, _) => Stream.cons(f | StreamIOError, end)
                    case f@FailedG(_, _)              => Stream.cons(f | StreamFormatError, end)
                }
            else
                tryCatch.value(decoder.arrayNext()) match {
                    case Okay(newLimit)               => consume(newLimit, newLimit)
                    case f@FailedG(_: IOException, _) => Stream.cons(f | StreamIOError, end)
                    case f@FailedG(_, _)              => Stream.cons(f | StreamFormatError, end)
                }

        tryCatch.value(decoder.readArrayStart()) match {
            case Okay(limit)                  => consume(limit, limit)
            case f@FailedG(_: IOException, _) => Stream.cons(f | StreamIOError, end)
            case f@FailedG(_, _)              => Stream.cons(f | StreamFormatError, end)
        }
    }
}

