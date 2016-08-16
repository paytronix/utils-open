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

import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import com.paytronix.utils.scala.result.Okay

class tuple1JsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `tuple1JsonCoder`
            must encode correctly $encodeCase
            must decode correctly $decodeCase
            must be the implicit coder for Tuple1 $implicitCase
    """

    val coder = tuple.tuple1JsonCoder(scalar.intJsonCoder)

    def encodeCase = prop { (i: Int) => coder.encode.toString(Tuple1(i)) ==== Okay(s"[$i]") }
    def decodeCase = prop { (i: Int) => decode(coder.decode)(s"[$i]") ==== Okay(Tuple1(i)) }
    def implicitCase = { import coders._; JsonCoder[Tuple1[Int]].encode.getClass must_== coder.encode.getClass }
}

class tuple2JsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `tuple2JsonCoder`
            must encode correctly $encodeCase
            must decode correctly $decodeCase
            must be the implicit coder for Tuple2 $implicitCase
    """

    val coder = tuple.tuple2JsonCoder(scalar.intJsonCoder, scalar.intJsonCoder)

    def encodeCase = prop { (t: (Int, Int)) => coder.encode.toString(t) ==== Okay(s"[${t._1},${t._2}]") }
    def decodeCase = prop { (t: (Int, Int)) => decode(coder.decode)(s"[${t._1},${t._2}]") ==== Okay(t) }
    def implicitCase = { import coders._; JsonCoder[(Int, Int)].encode.getClass must_== coder.encode.getClass }
}

class tuple3JsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `tuple3JsonCoder`
            must encode correctly $encodeCase
            must decode correctly $decodeCase
            must be the implicit coder for Tuple3 $implicitCase
    """

    val coder = tuple.tuple3JsonCoder(scalar.intJsonCoder, scalar.intJsonCoder, scalar.intJsonCoder)

    def encodeCase = prop { (t: (Int, Int, Int)) => coder.encode.toString(t) ==== Okay(s"[${t._1},${t._2},${t._3}]") }
    def decodeCase = prop { (t: (Int, Int, Int)) => decode(coder.decode)(s"[${t._1},${t._2},${t._3}]") ==== Okay(t) }
    def implicitCase = { import coders._; JsonCoder[(Int, Int, Int)].encode.getClass must_== coder.encode.getClass }
}

class tuple4JsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `tuple4JsonCoder`
            must encode correctly $encodeCase
            must decode correctly $decodeCase
            must be the implicit coder for Tuple4 $implicitCase
    """

    val coder = tuple.tuple4JsonCoder(scalar.intJsonCoder, scalar.intJsonCoder, scalar.intJsonCoder, scalar.intJsonCoder)

    def encodeCase = prop { (t: (Int, Int, Int, Int)) => coder.encode.toString(t) ==== Okay(s"[${t._1},${t._2},${t._3},${t._4}]") }
    def decodeCase = prop { (t: (Int, Int, Int, Int)) => decode(coder.decode)(s"[${t._1},${t._2},${t._3},${t._4}]") ==== Okay(t) }
    def implicitCase = { import coders._; JsonCoder[(Int, Int, Int, Int)].encode.getClass must_== coder.encode.getClass }
}

class tuple5JsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `tuple5JsonCoder`
            must encode correctly $encodeCase
            must decode correctly $decodeCase
            must be the implicit coder for Tuple5 $implicitCase
    """

    val coder = tuple.tuple5JsonCoder(scalar.intJsonCoder, scalar.intJsonCoder, scalar.intJsonCoder, scalar.intJsonCoder, scalar.intJsonCoder)

    def encodeCase = prop { (t: (Int, Int, Int, Int, Int)) => coder.encode.toString(t) ==== Okay(s"[${t._1},${t._2},${t._3},${t._4},${t._5}]") }
    def decodeCase = prop { (t: (Int, Int, Int, Int, Int)) => decode(coder.decode)(s"[${t._1},${t._2},${t._3},${t._4},${t._5}]") ==== Okay(t) }
    def implicitCase = { import coders._; JsonCoder[(Int, Int, Int, Int, Int)].encode.getClass must_== coder.encode.getClass }
}

class tuple6JsonCoderTest extends SpecificationWithJUnit with ScalaCheck with JsonMatchers {
    def is = s2"""
        `tuple6JsonCoder`
            must encode correctly $encodeCase
            must decode correctly $decodeCase
            must be the implicit coder for Tuple6 $implicitCase
    """

    val coder = tuple.tuple6JsonCoder(scalar.intJsonCoder, scalar.intJsonCoder, scalar.intJsonCoder, scalar.intJsonCoder, scalar.intJsonCoder, scalar.intJsonCoder)

    def encodeCase = prop { (t: (Int, Int, Int, Int, Int, Int)) => coder.encode.toString(t) ==== Okay(s"[${t._1},${t._2},${t._3},${t._4},${t._5},${t._6}]") }
    def decodeCase = prop { (t: (Int, Int, Int, Int, Int, Int)) => decode(coder.decode)(s"[${t._1},${t._2},${t._3},${t._4},${t._5},${t._6}]") ==== Okay(t) }
    def implicitCase = { import coders._; JsonCoder[(Int, Int, Int, Int, Int, Int)].encode.getClass must_== coder.encode.getClass }
}
