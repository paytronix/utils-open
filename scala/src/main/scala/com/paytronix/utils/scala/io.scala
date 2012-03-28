//
// Copyright 2009-2012 Paytronix Systems, Inc.
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

package com.paytronix.utils.scala

import java.io.{ByteArrayOutputStream, InputStream, InputStreamReader, Reader}
import java.nio.charset.Charset

import resource.withResource

/**
 * Helpers for I/O
 */
object io
{
    /** Apply f(buf, offs, len) to blocks of bytes until inputStream is exhausted */
    def readBytes(inputStream: InputStream, f: (Array[Byte], Int, Int) => Unit): Unit = {
        val buf = new Array[Byte](3072)
        var bytesRead = 0
        while ({ bytesRead = inputStream.read(buf); bytesRead } >= 0) {
            f(buf, 0, bytesRead)
        }
    }

    /** Read all the bytes in the given InputStream into a byte array */
    def readBytes(inputStream: InputStream): Array[Byte] =
        withResource(new ByteArrayOutputStream()) {
            os =>
            readBytes(inputStream, os.write(_, _, _))
            os.toByteArray
        }

    /** Apply f(buf, offs, len) to blocks of characters until reader is exhausted */
    def readChars(reader: Reader, f: (Array[Char], Int, Int) => Unit): Unit = {
        val buf = new Array[Char](1536)
        var charsRead = 0
        while ({ charsRead = reader.read(buf); charsRead } >= 0) {
            f(buf, 0, charsRead)
        }
    }

    /** Read all the characters in the given Reader into a char array */
    def readChars(reader: Reader): Array[Char] = {
        val sb = new StringBuilder
        readChars(reader, sb.appendAll(_, _, _))
        sb.toArray
    }

    /** Read all the characters in the given reader into a string */
    def readString(reader: Reader): String = {
        val sb = new StringBuilder
        readChars(reader, sb.appendAll(_, _, _))
        sb.toString
    }

    /** Read an entire input stream as UTF-8 into a string */
    def readUtf8String(inputStream: InputStream): String =
        readString(new InputStreamReader(inputStream, Charset.forName("UTF-8")))
 }
