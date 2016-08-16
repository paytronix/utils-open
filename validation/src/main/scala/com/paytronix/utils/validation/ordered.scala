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

import scalaz.Order

import base.{Validated, ValidationError, predicateE}

object ordered {
    def tooSmallErrorExclusive[A](a: A) = ValidationError("underflow", "must greater than $a")
    def tooSmallError[A](a: A)          = ValidationError("underflow", "must greater than or equal to $a")
    def tooLargeErrorExclusive[A](a: A) = ValidationError("overflow", "must less than $a")
    def tooLargeError[A](a: A)          = ValidationError("overflow", "must less than or equal to $a")

    /** Assert that some value is ordered greater than some value (> x) */
    def greaterThan[A: Order](minimum: A): A => Validated[A] =
        greaterThanE[A](tooSmallErrorExclusive)(minimum)

    /** Assert that some value is ordered greater than some value (> x) */
    def greaterThanE[A: Order](error: A => ValidationError)(minimum: A): A => Validated[A] =
        predicateE(error(minimum))(Order[A].greaterThan(_, minimum))

    /** Assert that some value is not ordered less than some value (>= x) */
    def noLessThan[A: Order](minimum: A): A => Validated[A] =
        noLessThanE[A](tooSmallError)(minimum)

    /** Assert that some value is not ordered less than some value (>= x) */
    def noLessThanE[A: Order](error: A => ValidationError)(minimum: A): A => Validated[A] =
        predicateE(error(minimum))(Order[A].greaterThanOrEqual(_, minimum))

    /** Assert that some value is ordered lesser than some value (< x) */
    def lessThan[A: Order](maximum: A): A => Validated[A] =
        lessThanE[A](tooLargeErrorExclusive)(maximum)

    /** Assert that some value is ordered lesser than some value (< x) */
    def lessThanE[A: Order](error: A => ValidationError)(maximum: A): A => Validated[A] =
        predicateE(error(maximum))(Order[A].lessThan(_, maximum))

    /** Assert that some value is not ordered lesser than some value (<= x) */
    def noGreaterThan[A: Order](maximum: A): A => Validated[A] =
        noGreaterThanE[A](tooLargeError)(maximum)

    /** Assert that some value is not ordered lesser than some value (<= x) */
    def noGreaterThanE[A: Order](error: A => ValidationError)(maximum: A): A => Validated[A] =
        predicateE(error(maximum))(Order[A].lessThanOrEqual(_, maximum))
}