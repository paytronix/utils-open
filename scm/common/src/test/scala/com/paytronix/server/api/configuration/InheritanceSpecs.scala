//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.common

import scala.collection.immutable.TreeSet

import net.liftweb.json.JsonAST.{JArray, JBool, JDouble, JField, JInt, JObject, JString, JValue, render}
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.Printer.compact
import org.specs2.{SpecificationFeatures, SpecificationWithJUnit}
import org.specs2.matcher.{Matcher, MatchResult}

import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, ResultG}

object InheritanceTestFixtures extends SpecificationFeatures {
    val beFailed: Matcher[ResultG[_, _]] =
        beLike {
            case FailedG(_, _) => ok
        }
}

import InheritanceTestFixtures._

class ExcludeActionSpecTest extends SpecificationWithJUnit { def is =
    "ExcludeAction" ^
    "works" ! { ExcludeAction.combine(JString("foo"), JString("bar")) must_== Okay(JString("foo")) }
}

class ReplaceActionSpecTest extends SpecificationWithJUnit { def is =
    "ReplaceAction" ^
    "works" ! { ReplaceAction.combine(JString("foo"), JString("bar")) must_== Okay(JString("bar")) }
}

class ArrayAppendActionSpecTest extends SpecificationWithJUnit { def is =
    "ArrayAppendAction" ^
    "fails on LHS non-array" ! { ArrayAppendAction.combine(JString("foo"), JArray(Nil)) must beFailed } ^
    "fails on RHS non-array" ! { ArrayAppendAction.combine(JArray(Nil), JString("foo")) must beFailed } ^
    "works" ! (
        ArrayAppendAction.combine(JArray(List(JString("foo"), JString("baz"))), JArray(List(JString("bar")))) must_==
        Okay(JArray(List(JString("foo"), JString("baz"), JString("bar"))))
    )
}

class ArrayAppendDistinctActionSpecTest extends SpecificationWithJUnit { def is =
    "ArrayAppendDistinctAction" ^
    "fails on LHS non-array" ! { ArrayAppendDistinctAction.combine(JString("foo"), JArray(Nil)) must beFailed } ^
    "fails on RHS non-array" ! { ArrayAppendDistinctAction.combine(JArray(Nil), JString("foo")) must beFailed } ^
    "works" ! (
        ArrayAppendDistinctAction.combine(JArray(List(JString("foo"), JString("baz"))), JArray(List(JString("bar"), JString("baz")))) must_==
        Okay(JArray(List(JString("foo"), JString("baz"), JString("bar"))))
    )
}

class ArrayPrependActionSpecTest extends SpecificationWithJUnit { def is =
    "ArrayPrependAction" ^
    "fails on LHS non-array" ! { ArrayPrependAction.combine(JString("foo"), JArray(Nil)) must beFailed } ^
    "fails on RHS non-array" ! { ArrayPrependAction.combine(JArray(Nil), JString("foo")) must beFailed } ^
    "works" ! (
        ArrayPrependAction.combine(JArray(List(JString("foo"), JString("baz"))), JArray(List(JString("bar")))) must_==
        Okay(JArray(List(JString("bar"), JString("foo"), JString("baz"))))
    )
}

class ArrayPrependDistinctActionSpecTest extends SpecificationWithJUnit { def is =
    "ArrayPrependDistinctAction" ^
    "fails on LHS non-array" ! { ArrayPrependDistinctAction.combine(JString("foo"), JArray(Nil)) must beFailed } ^
    "fails on RHS non-array" ! { ArrayPrependDistinctAction.combine(JArray(Nil), JString("foo")) must beFailed } ^
    "works" ! (
        ArrayPrependDistinctAction.combine(JArray(List(JString("foo"), JString("baz"))), JArray(List(JString("bar"), JString("baz")))) must_==
        Okay(JArray(List(JString("bar"), JString("baz"), JString("foo"))))
    )
}

class ObjectMergeActionSpecTest extends SpecificationWithJUnit { def is =
    "ObjectMergeAction" ^
    "fails on LHS non-object" ! { ObjectMergeAction.combine(JString("foo"), JObject(Nil)) must beFailed } ^
    "fails on RHS non-object" ! { ObjectMergeAction.combine(JObject(Nil), JString("foo")) must beFailed } ^
    "works" ! (
        ObjectMergeAction.combine (
            JObject(List(JField("foo", JString("bar")), JField("baz", JString("qux")))),
            JObject(List(JField("baz", JString("zippy")), JField("noodle", JString("blang"))))
        ) must_== Okay (
            JObject(List(JField("baz", JString("zippy")), JField("foo", JString("bar")), JField("noodle", JString("blang"))))
        )
    )
}

class InheritanceRulesDSLSpecTest extends SpecificationWithJUnit with InheritanceRulesDSL {
    import InheritanceRule.ordering

    val a: KeyPath = "foo" / "bar" / "baz"
    val b          = exclude
    val c: KeyPath = "boo" / "bop"
    val d          = replace
    val e: KeyPath = "zippy" / "doodah"
    val f          = arrayAppend
    val g: KeyPath = "woo"
    val h          = arrayPrepend

    def haveRules(expected: InheritanceRule*): Matcher[InheritanceRules] =
        beLike { case InheritanceRules(actual) =>
            if (actual == (TreeSet.empty ++ expected)) ok
            else ko("Expected:\n" + expected.mkString("  ", "\n  ", "\n") + "\nbut got:\n" + actual.mkString("  ", "\n  ", "\n"))
        }

    val rule = InheritanceRule.apply _

    def is =
        "InheritanceRulesDSL" ^
        "string ==> action" ! { ("foo/bar / baz" ==> replace) must_== rule(a, ReplaceAction) } ^
        "(string ==> action): InheritanceRules" ! { (("foo/bar / baz" ==> replace): InheritanceRules) must haveRules(rule(a, ReplaceAction)) } ^
        "keypath ==> action" ! { (a ==> replace) must_== rule(a, ReplaceAction) } ^
        "a ==> b & c ==> d" ! { (a ==> b & c ==> d) must haveRules(rule(a, b), rule(c, d)) } ^
        "(a ==> b & c ==> d) & e ==> f" ! { ((a ==> b & c ==> d) & e ==> f) must haveRules(rule(a, b), rule(c, d), rule(e, f)) } ^
        "a ==> b & (c ==> d & e ==> f)" ! { (a ==> b & (c ==> d & e ==> f)) must haveRules(rule(a, b), rule(c, d), rule(e, f)) } ^
        "(a ==> b & c ==> d) & (e ==> f & g ==> h)" ! {
            ((a ==> b & c ==> d) & (e ==> f & g ==> h)) must haveRules(rule(a, b), rule(c, d), rule(e, f), rule(g, h))
        } ^
        "a ==> b & c ==> d & e ==> f & g ==> h" ! {
            (a ==> b & c ==> d & e ==> f & g ==> h) must haveRules(rule(a, b), rule(c, d), rule(e, f), rule(g, h))
        }
}

class InheritanceRulesSpecTest extends SpecificationWithJUnit {
    def stableRender(jv: JValue): String =
        compact(render(jv.transform { case JObject(fields) => JObject(fields.sortBy(_.name)) }))

    def testInheritance(chain: Seq[String], rules: InheritanceRules, rawExpected: Result[Option[String]]): MatchResult[Any] = {
        val parsedChain = chain.map(parse).map(_.asInstanceOf[JObject])
        val actual = rules(parsedChain).map(_.map(stableRender))
        lazy val unparsedChain = parsedChain.map(stableRender)
        val expected = rawExpected.map(_.map(s => stableRender(parse(s))))

        lazy val situation = (
            "Processing:\n" + unparsedChain.mkString("  ", "\n  ", "") +
            "\nwith:\n" + rules.rules.mkString("  ", "\n  ", "") +
            "\nyielded:\n  " + actual +
            "\nbut was expecting:\n  " + expected
        )

        if ((expected.isDefined && expected == actual) || (!expected.isDefined && !actual.isDefined))
            ok
        else
            ko(situation)

    }

    val testSystem = """{
        "system": "sys",
    }"""
    val testGlobal = """{
        "arrayValue": [1,2]
        "global": "glo",
        "objectValue": {"a": 1, "b": 2, "x": ["foo", "bar"]},
    }"""
    val testFoo = """{
        "arrayValue": [2,3]
        "foo": "yo",
        "objectValue": {"b": "x", "c": "y", "x": ["bar", "baz"]},
    }"""
    val testBar = """{
        "bar": "woo"
    }"""

    val testChain = List (
        testSystem,
        testGlobal,
        testFoo,
        testBar
    )

    val result = """{
        "arrayValue": [2,3],
        "bar": "woo",
        "foo": "yo",
        "global": "glo",
        "objectValue": {"b": "x", "c": "y", "x": ["bar", "baz"]},
        "system": "sys"
    }"""

    val resultWithArrayAppendDistinct = """{
        "arrayValue": [1,2,3],
        "bar": "woo",
        "foo": "yo",
        "global": "glo",
        "objectValue": {"b": "x", "c": "y", "x": ["bar", "baz"]},
        "system": "sys"
    }"""

    val resultWithObjectMerge = """{
        "arrayValue": [2,3],
        "bar": "woo",
        "foo": "yo",
        "global": "glo",
        "objectValue": {"a": 1, "b": "x", "c": "y", "x": ["bar", "baz"]},
        "system": "sys"
    }"""

    val resultWithObjectMergeAndSubArrayAppendDistinct = """{
        "arrayValue": [2,3],
        "bar": "woo",
        "foo": "yo",
        "global": "glo",
        "objectValue": {"a": 1, "b": "x", "c": "y", "x": ["foo", "bar", "baz"]},
        "system": "sys"
    }"""

    val resultWithSubArrayAppendDistinct = """{
        "arrayValue": [2,3],
        "bar": "woo",
        "foo": "yo",
        "global": "glo",
        "objectValue": {"b": "x", "c": "y", "x": ["foo", "bar", "baz"]},
        "system": "sys"
    }"""

    def is =
        "InheritanceRules" ^
        "empty" ! testInheritance (
            testChain,
            InheritanceRules(Nil),
            Okay(Some(result))
        ) ^
        "explicit root action" ! testInheritance (
            testChain,
            InheritanceRules(List (
                InheritanceRule(List(), ReplaceAction)
            )),
            Okay(Some(testBar))
        ) ^
        "append distinct array" ! testInheritance (
            testChain,
            InheritanceRules(List (
                InheritanceRule(List("arrayValue"), ArrayAppendDistinctAction)
            )),
            Okay(Some(resultWithArrayAppendDistinct))
        ) ^
        "explicit root and array append distinct" ! testInheritance (
            testChain,
            InheritanceRules(List (
                InheritanceRule(List("arrayValue"), ArrayAppendDistinctAction),
                InheritanceRule(List(), ObjectMergeAction)
            )),
            Okay(Some(resultWithArrayAppendDistinct))
        ) ^
        "object merge" ! testInheritance (
            testChain,
            InheritanceRules(List (
                InheritanceRule(List("objectValue"), ObjectMergeAction)
            )),
            Okay(Some(resultWithObjectMerge))
        ) ^
        "sub array append distinct" ! testInheritance (
            testChain,
            InheritanceRules(List (
                InheritanceRule(List("objectValue", "x"), ArrayAppendDistinctAction)
            )),
            Okay(Some(resultWithSubArrayAppendDistinct))
        ) ^
        "object merge and sub array append distinct" ! testInheritance (
            testChain,
            InheritanceRules(List (
                InheritanceRule(List("objectValue"), ObjectMergeAction),
                InheritanceRule(List("objectValue", "x"), ArrayAppendDistinctAction)
            )),
            Okay(Some(resultWithObjectMergeAndSubArrayAppendDistinct))
        ) ^
        "invalid root rule" ! testInheritance (
            testChain,
            InheritanceRules(List (
                InheritanceRule(List(), ArrayAppendDistinctAction)
            )),
            Failed("")
        ) ^
        "invalid other rule" ! testInheritance (
            testChain,
            InheritanceRules(List (
                InheritanceRule(List("objectValue"), ArrayAppendDistinctAction)
            )),
            Failed("")
        )
}
