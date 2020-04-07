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

import org.scalacheck.Prop
import org.scalacheck.Arbitrary.arbitrary
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import base.ValidationError

class booleanTest extends SpecificationWithJUnit with ScalaCheck {
    def is = s2"""`boolean`
        should accept the empty string   $e1
        should accept false              $e2
        should accept no                 $e3
        should accept off                $e4
        should accept true               $e5
        should accept yes                $e6
        should accept on                 $e7
        should accept integers           $e8
        should not accept other things   $e9
        should use the given error       $e10
    """

    val validStrs = Set("", "false", "no", "off", "true", "yes", "on")
    val invalidStrs = arbitrary[String].filter { s =>
        !(validStrs.contains(s) || (try { Integer.parseInt(s); true } catch { case _: Exception => false }))
    }

    def e1 = boolean.boolean("") ==== base.success(false)
    def e2 = boolean.boolean("false") ==== base.success(false)
    def e3 = boolean.boolean("no") ==== base.success(false)
    def e4 = boolean.boolean("off") ==== base.success(false)
    def e5 = boolean.boolean("true") ==== base.success(true)
    def e6 = boolean.boolean("yes") ==== base.success(true)
    def e7 = boolean.boolean("on") ==== base.success(true)
    def e8 = prop { (i: Int) => boolean.boolean(i.toString) ==== base.success(i != 0) }
    def e9 = Prop.forAll(invalidStrs) { s => boolean.boolean(s) ==== base.failure(boolean.invalidBooleanError) }
    def e10 = boolean.booleanE(ValidationError("nope"))("noooope") ==== base.failure(ValidationError("nope"))
}
