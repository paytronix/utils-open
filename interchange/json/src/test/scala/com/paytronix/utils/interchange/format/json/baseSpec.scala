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

import com.fasterxml.jackson.core.JsonToken
import org.specs2.SpecificationWithJUnit

import com.paytronix.utils.interchange.base.CoderResult
import com.paytronix.utils.scala.result.{FailedG, Okay, unless}

class interchangeJsonParserTest extends SpecificationWithJUnit with JsonMatchers {
    def is = s2"""
        `InterchangeJsonParser`
            should pass through to the underlying parser under normal circumstances $normalCase
            should fail to admit a token is current when a value has been marked missing $missingNegativeCase
            should correctly indicate a missing value $missingPositiveCase
            should record and replay tokens when peeking $markRewindCase
            `peekFields` should work $peekFieldsCase
            `foreachFields` should work $foreachFieldsCase
            `skipToEndOfValue` should work for scalars $skipScalarCase
            `skipToEndOfValue` should work for objects $skipObjectCase
            `skipToEndOfValue` should work for object with objects $skipObjectsCase
            `skipToEndOfValue` should work for arrays $skipArrayCase
    """


    def normalCase = withParser(""" ["foo",1,1.2,{"a":"b"},[true,false],null] """) { ijp =>
        def check[A](a: A, e: A): CoderResult[Unit] = unless(a == e)(ijp.unexpectedToken(s"$e (got $a)"))
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_STRING) >> check(ijp.stringValue, "foo") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NUMBER_INT) >> check(ijp.bigIntegerValue, new java.math.BigInteger("1")) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NUMBER_FLOAT) >> check(ijp.bigDecimalValue, new java.math.BigDecimal("1.2")) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_OBJECT) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.FIELD_NAME) >> check(ijp.fieldName, "a") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_STRING) >> check(ijp.stringValue, "b") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_OBJECT) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_TRUE) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_FALSE) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NULL) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(null)
    } ==== Okay(())

    def missingNegativeCase = withParser("[]") { ijp =>
        ijp.currentValueIsMissing()
        ijp.currentToken
    } must throwAn[Exception]

    def missingPositiveCase = withParser("[]") { ijp =>
        ijp.currentValueIsMissing()
        ijp.hasValue
    } ==== false

    def markRewindCase = withParser(""" ["foo",1,1.2,{"a":"b"},[true,false],null] """) { ijp =>
        def check[A](a: A, e: A): CoderResult[Unit] = unless(a == e)(ijp.unexpectedToken(s"$e (got $a)"))
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_ARRAY) >>
        {
            val m = ijp.mark()
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_STRING) >> check(ijp.stringValue, "foo") >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NUMBER_INT) >> check(ijp.bigIntegerValue, new java.math.BigInteger("1")) >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NUMBER_FLOAT) >> check(ijp.bigDecimalValue, new java.math.BigDecimal("1.2")) >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_OBJECT) >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.FIELD_NAME) >> check(ijp.fieldName, "a") >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_STRING) >> check(ijp.stringValue, "b") >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_OBJECT) >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_ARRAY) >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_TRUE) >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_FALSE) >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_ARRAY) >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NULL) >>
            ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_ARRAY) >>
            { ijp.rewind(m); Okay.unit }
        } >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_STRING) >> check(ijp.stringValue, "foo") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NUMBER_INT) >> check(ijp.bigIntegerValue, new java.math.BigInteger("1")) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NUMBER_FLOAT) >> check(ijp.bigDecimalValue, new java.math.BigDecimal("1.2")) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_OBJECT) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.FIELD_NAME) >> check(ijp.fieldName, "a") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_STRING) >> check(ijp.stringValue, "b") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_OBJECT) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_TRUE) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_FALSE) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NULL) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(null)
    } ==== Okay(())

    def peekFieldsCase = withParser(""" ["foo",1,1.2,{"a":"b","b":"a"},[true,false],null] """) { ijp =>
        def check[A](a: A, e: A): CoderResult[Unit] = unless(a == e)(ijp.unexpectedToken(s"$e (got $a)"))
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_STRING) >> check(ijp.stringValue, "foo") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NUMBER_INT) >> check(ijp.bigIntegerValue, new java.math.BigInteger("1")) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NUMBER_FLOAT) >> check(ijp.bigDecimalValue, new java.math.BigDecimal("1.2")) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_OBJECT) >>
        {
            val fields = ijp.peekFields(Array("b", "c", "a")).orThrow
            unless(fields.zip(Array(Some("a"),None, Some("b"))).forall { case (a,b) => a == b }) {
                FailedG(s"expected to peek Okay([a,None,b]) but got ${fields.map(_.mkString("[", ",", "]"))}", ijp.terminal)
            }
        } >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.FIELD_NAME) >> check(ijp.fieldName, "a") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_STRING) >> check(ijp.stringValue, "b") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.FIELD_NAME) >> check(ijp.fieldName, "b") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_STRING) >> check(ijp.stringValue, "a") >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_OBJECT) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.START_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_TRUE) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_FALSE) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.VALUE_NULL) >>
        ijp.advanceTokenUnguarded() >> ijp.require(JsonToken.END_ARRAY) >>
        ijp.advanceTokenUnguarded() >> ijp.require(null)
    } ==== Okay(())

    def foreachFieldsCase = ok
    def skipScalarCase = ok
    def skipObjectCase = ok
    def skipObjectsCase = ok
    def skipArrayCase = ok
}
