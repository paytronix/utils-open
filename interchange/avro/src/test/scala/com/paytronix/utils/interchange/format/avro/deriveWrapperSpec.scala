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

package com.paytronix.utils.interchange.format.avro

import org.apache.avro.Schema
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.interchange.test.fixtures.{Wrapper, WrapperVal}
import com.paytronix.utils.scala.result.Okay

import scalar.stringAvroCoder

class deriveWrapperCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        Deriving wrapper coders for simple classes
            must have the correct (underlying) schema $eschema
            must encode to the underlying value $eencode
            must decode from the underlying value $edecode
    """

    lazy val coder = derive.wrapper.coder[Wrapper]

    def eschema = coder.schema.getType ==== Schema.Type.STRING
    def eencode = prop { (s: String) =>
        coder.encode.toBytes(Wrapper(s)) must beLike { case Okay(a) => a must beAvroString(s) }
    }
    def edecode = prop { (s: String) =>
        coder.decode.fromBytes(coder.schema)(makeAvroString(s)) ==== Okay(Wrapper(s))
    }
}

class deriveWrapperValCoderTest extends SpecificationWithJUnit with ScalaCheck with AvroMatchers {
    def is = s2"""
        Deriving wrapper coders for value classes
            must have the correct (underlying) schema $eschema
            must encode to the underlying value $eencode
            must decode from the underlying value $edecode
    """

    lazy val coder = derive.wrapper.coder[WrapperVal]

    def eschema = coder.schema.getType ==== Schema.Type.STRING
    def eencode = prop { (s: String) =>
        coder.encode.toBytes(WrapperVal(s)) must beLike { case Okay(a) => a must beAvroString(s) }
    }
    def edecode = prop { (s: String) =>
        coder.decode.fromBytes(coder.schema)(makeAvroString(s)) ==== Okay(WrapperVal(s))
    }
}

// FIXME derive implicit coder
// FIXME derive encoder
// FIXME derive decoder
