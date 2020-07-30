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

package com.paytronix.utils.validation

import org.specs2.SpecificationWithJUnit

import column.{columns, required, optional, requiredColumnError, unknownColumnError}

class columnTest extends SpecificationWithJUnit {
    def is = s2"""Column validation
        should work with a `required` column that's provided                                $e1
        should fail with a `required` column that's missing                                 $e2
        should work with an `optional` column that's provided                               $e3
        should work with an `optional` column that's missing                                $e4
        should work with combinations of `required` and `optional` columns                  $e5
        should work with combinations of `required` and `optional` columns in another order $e6
        should work with combinations of `required` and `optional` columns with optional missing $e7
        should fail with combinations of `required` and `optional` columns with required missing $e8
        should fail on extraneous columns                                                   $e9
    """

    val foo = required[Vector[Int], Vector[Int]]("foo", _ :+ _)
    val bar = optional[Vector[Int]]("bar", _ :+ _)

    def e1 = columns(Vector.empty[Int])(foo)(List("foo"))              ==== base.success(Vector(0))
    def e2 = columns(Vector.empty[Int])(foo)(Nil)                      ==== base.failure(requiredColumnError("foo"))
    def e3 = columns(Vector.empty[Int])(bar)(List("bar"))              ==== base.success(Vector(0))
    def e4 = columns(Vector.empty[Int])(bar)(Nil)                      ==== base.success(Vector())
    def e5 = columns(Vector.empty[Int])(foo, bar)(List("foo", "bar"))  ==== base.success(Vector(0, 1))
    def e6 = columns(Vector.empty[Int])(foo, bar)(List("bar", "foo"))  ==== base.success(Vector(1, 0))
    def e7 = columns(Vector.empty[Int])(foo, bar)(List("foo"))         ==== base.success(Vector(0))
    def e8 = columns(Vector.empty[Int])(foo, bar)(List("bar"))         ==== base.failure(requiredColumnError("foo"))
    def e9 = columns(Vector.empty[Int])(foo)(List("foo", "bar"))       ==== base.failure(unknownColumnError("bar"))
}
