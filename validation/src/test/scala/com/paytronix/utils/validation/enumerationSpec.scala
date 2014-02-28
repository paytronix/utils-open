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

package com.paytronix.utils.validation

import org.specs2.SpecificationWithJUnit
import scalaz.{Failure, NonEmptyList}

import base.{ValidationError, valueOps}
import enumeration._

class enumerationDefinedInTest extends SpecificationWithJUnit {
    def is = s2"""
        `definedIn`
            should work for PFs where the value is defined      $e1
            should fail for PFs where the value is not defined  $e2
            should give an appropriate error                    $e3
            should give a custom error                          $e4
    """

    val pf: PartialFunction[Int, String] = {
        case 1 => "one"
    }

    val customVE = ValidationError("foo")

    def e1 = (1 is definedIn(pf)) ==== base.success("one")
    def e2 = (2 is definedIn(pf)) must beLike { case Failure(_) => ok }
    def e3 = (3 is definedIn(pf)) must beLike { case Failure(NonEmptyList(ve)) => (ve.code ==== "invalid_enumeration") and (ve.text ==== """value "3" is not an allowed option for this field""") }
    def e4 = (4 is definedInE[Int, String](_ => customVE)(pf)) must beLike { case Failure(NonEmptyList(ve)) => ve ==== customVE }
}

object TestScalaEnum extends Enumeration {
    val one = Value("1")
}

class enumerationValueOfTest extends SpecificationWithJUnit {
    def is = s2"""
        `valueOf`
            should work for valid enumeration values        $e1
            should not work for invalid enumeration values  $e2
            should give an appropriate error                $e3
            should give a custom error                      $e4
    """

    val customVE = ValidationError("foo")

    def e1 = ("1" is valueOf(TestScalaEnum)) ==== base.success(TestScalaEnum.one)
    def e2 = ("2" is valueOf(TestScalaEnum)) must beLike { case Failure(_) => ok }
    def e3 = ("3" is valueOf(TestScalaEnum)) must beLike { case Failure(NonEmptyList(ve)) => (ve.code ==== "invalid_enumeration") and (ve.text ==== """value "3" is not an allowed option for this field""") }
    def e4 = ("4" is valueOfE(_ => customVE)(TestScalaEnum)) must beLike { case Failure(NonEmptyList(ve)) => ve ==== customVE }
}

class enumerationValueOfJavaEnumTest extends SpecificationWithJUnit {
    def is = s2"""
        `valueOfJavaEnum`
            should work for valid enumeration values        $e1
            should not work for invalid enumeration values  $e2
            should give an appropriate error                $e3
            should give a custom error                      $e4
    """

    val customVE = ValidationError("foo")

    def e1 = ("ONE"   is valueOfJavaEnum(classOf[TestJavaEnum])) ==== base.success(TestJavaEnum.ONE)
    def e2 = ("TWO"   is valueOfJavaEnum(classOf[TestJavaEnum])) must beLike { case Failure(_) => ok }
    def e3 = ("THREE" is valueOfJavaEnum(classOf[TestJavaEnum])) must beLike { case Failure(NonEmptyList(ve)) => (ve.code ==== "invalid_enumeration") and (ve.text ==== """value "THREE" is not an allowed option for this field""") }
    def e4 = ("FOUR"  is valueOfJavaEnumE(_ => customVE)(classOf[TestJavaEnum])) must beLike { case Failure(NonEmptyList(ve)) => ve ==== customVE }
}
