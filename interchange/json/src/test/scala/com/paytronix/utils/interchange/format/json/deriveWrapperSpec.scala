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

import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.interchange.test.fixtures.{Wrapper, WrapperVal}
import com.paytronix.utils.scala.result.Okay

import scalar.stringJsonCoder

class deriveWrapperCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        Deriving wrapper coders for simple classes
            must encode to the underlying value $encodeCase
            must decode from the underlying value $decodeCase
    """

    lazy val coder = derive.wrapper.coder[Wrapper]

    def encodeCase = prop { (s: String) => coder.encode.toString(Wrapper(s)) ==== stringJsonCoder.encode.toString(s) }
    def decodeCase = prop { (s: String) => (stringJsonCoder.encode.toString(s) >>= decode(coder.decode)) ==== Okay(Wrapper(s)) }
}

class deriveWrapperValCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        Deriving wrapper coders for value classes
            must encode to the underlying value $encodeCase
            must decode from the underlying value $decodeCase
    """

    lazy val coder = derive.wrapper.coder[WrapperVal]

    def encodeCase = prop { (s: String) => coder.encode.toString(WrapperVal(s)) ==== stringJsonCoder.encode.toString(s) }
    def decodeCase = prop { (s: String) => (stringJsonCoder.encode.toString(s) >>= decode(coder.decode)) ==== Okay(WrapperVal(s)) }
}

// FIXME derive implicit coder
// FIXME derive encoder
// FIXME derive decoder
