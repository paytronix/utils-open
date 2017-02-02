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
package test
package Streams

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import net.liftweb.json.JsonParser
import org.apache.avro.io.{DecoderFactory, EncoderFactory}
import org.specs2.{SpecificationFeatures, SpecificationWithJUnit}
import org.specs2.matcher.MatchResult

import com.paytronix.utils.scala.result.{Okay, ResultG}

import fixtures.{CaseClass, Coders}

object Helper {
    val classLoader = getClass.getClassLoader
}

import Helper._

trait Helper {
    self: SpecificationFeatures =>

    def check(stream: Stream[ResultG[Any, CaseClass]]): MatchResult[Any] =
        stream.toList must_== List (
            Okay(CaseClass(1, null, None)),
            Okay(CaseClass(2, null, None)),
            Okay(CaseClass(3, null, None))
        )
}


class JSONArrayStreamSpecTest extends SpecificationWithJUnit with Helper {
    val json = """
    [
        { "foo": 1 },
        { "foo": 2 },
        { "foo": 3 }
    ]
    """

    def is =
    "stream.jsonArray" ^
    "basically function" ! {
        JsonParser.parse(json, { (parser: JsonParser.Parser) =>
            { parser.nextToken must_== JsonParser.OpenArr } and
            check(stream.jsonArray(Coder(classLoader, Coders.caseClassCoder), parser))
        })
    }
}

class AvroStreamStreamSpecTest extends SpecificationWithJUnit with Helper {
    val bytes =
        Coder(classLoader, Coders.caseClassCoder).encodeAvro(CaseClass(1, null, None)).orThrow ++
        Coder(classLoader, Coders.caseClassCoder).encodeAvro(CaseClass(2, null, None)).orThrow ++
        Coder(classLoader, Coders.caseClassCoder).encodeAvro(CaseClass(3, null, None)).orThrow

    def is =
    "AvroStreamStream" ^
    "basically function" ! {
        val decoder = DecoderFactory.get.binaryDecoder(new ByteArrayInputStream(bytes), null)
        check(stream.avroItems(Coder(classLoader, Coders.caseClassCoder), Coders.caseClassCoder.avroSchema._1, decoder))
    }
}

class AvroArrayStreamSpecTest extends SpecificationWithJUnit with Helper {
    val bytes = {
        val baos = new ByteArrayOutputStream()
        val encoder = EncoderFactory.get.directBinaryEncoder(baos, null)
        encoder.writeArrayStart()
        encoder.setItemCount(1)
        encoder.startItem()
        Coders.caseClassCoder.encodeAvro(classLoader, CaseClass(1, null, None), encoder)
        encoder.setItemCount(2)
        encoder.startItem(); Coders.caseClassCoder.encodeAvro(classLoader, CaseClass(2, null, None), encoder)
        encoder.startItem(); Coders.caseClassCoder.encodeAvro(classLoader, CaseClass(3, null, None), encoder)
        encoder.writeArrayEnd()
        encoder.writeString("foobar")
        encoder.flush()
        baos.toByteArray
    }

    def is =
    "AvroArrayStream" ^
    "basically function" ! {
        val decoder = DecoderFactory.get.binaryDecoder(new ByteArrayInputStream(bytes), null)
        check(stream.avroArray(Coder(classLoader, Coders.caseClassCoder), Coders.caseClassCoder.avroSchema._1, decoder)) and {
            decoder.readString() must_== "foobar"
        }
    }
}
