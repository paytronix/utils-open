//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.common

import org.specs2.{SpecificationFeatures, SpecificationWithJUnit}
import org.specs2.matcher.{DataTables, MatchResult}

import context.{Path, id, root}
import context.Segment.stringOps

trait FilterTestFixtures {
    self: SpecificationFeatures =>

    def nodeTest(filter: Filter): (Node, Boolean) => MatchResult[Any] =
        (node, expectation) => filter(node) must_== expectation

    def aspectTest(filter: Filter): (AspectName, Boolean) => MatchResult[Any] =
        (node, expectation) => filter(node) must_== expectation
}

class ContextFilterSpecTest extends SpecificationWithJUnit with FilterTestFixtures with DataTables { def is =
    "ContextFilter" ^
    "default" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz")

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! true
            | Path.fromString("foo").aspect("zippy")             !! true
            | Path.fromString("foo/bar baz").aspect("zap")       !! true
            | Path.fromString("foo/bar baz").aspect("zippy")     !! true
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! false
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! false
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! true
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    } ^
    "excluded" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz").exclude

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! true
            | Path.fromString("bar").aspect("zippy")             !! true
            | Path.fromString("foo").aspect("zap")               !! false
            | Path.fromString("foo").aspect("zippy")             !! false
            | Path.fromString("foo/bar baz").aspect("zap")       !! false
            | Path.fromString("foo/bar baz").aspect("zippy")     !! false
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! true
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! true
            | Path.fromString("foo/bar qux").aspect("zap")       !! true
            | Path.fromString("foo/bar qux").aspect("zippy")     !! true
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! true
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    } ^
    "withDescendants" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz").withDescendants

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! true
            | Path.fromString("foo").aspect("zippy")             !! true
            | Path.fromString("foo/bar baz").aspect("zap")       !! true
            | Path.fromString("foo/bar baz").aspect("zippy")     !! true
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! true
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! true
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! true
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    } ^
    "withoutAncestors" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz").withoutAncestors

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! false
            | Path.fromString("foo").aspect("zippy")             !! false
            | Path.fromString("foo/bar baz").aspect("zap")       !! true
            | Path.fromString("foo/bar baz").aspect("zippy")     !! true
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! false
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! false
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! true
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    } ^
    "withDescendants.descendantsOnly" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz").descendantsOnly

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! false
            | Path.fromString("foo").aspect("zippy")             !! false
            | Path.fromString("foo/bar baz").aspect("zap")       !! false
            | Path.fromString("foo/bar baz").aspect("zippy")     !! false
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! true
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! true
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! true
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    } ^
    "withDescendants.withoutSelf" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz").withoutSelf.withDescendants

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! true
            | Path.fromString("foo").aspect("zippy")             !! true
            | Path.fromString("foo/bar baz").aspect("zap")       !! false
            | Path.fromString("foo/bar baz").aspect("zippy")     !! false
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! true
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! true
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! true
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    }
}

class NodeFilterSpecTest extends SpecificationWithJUnit with FilterTestFixtures with DataTables { def is =
    "NodeFilter" ^
    "default" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz").aspect("zippy")

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! false
            | Path.fromString("foo").aspect("zippy")             !! true
            | Path.fromString("foo/bar baz").aspect("zap")       !! false
            | Path.fromString("foo/bar baz").aspect("zippy")     !! true
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! false
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! false
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! false
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    } ^
    "excluded" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz").aspect("zippy").exclude

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! true
            | Path.fromString("bar").aspect("zippy")             !! true
            | Path.fromString("foo").aspect("zap")               !! true
            | Path.fromString("foo").aspect("zippy")             !! false
            | Path.fromString("foo/bar baz").aspect("zap")       !! true
            | Path.fromString("foo/bar baz").aspect("zippy")     !! false
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! true
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! true
            | Path.fromString("foo/bar qux").aspect("zap")       !! true
            | Path.fromString("foo/bar qux").aspect("zippy")     !! true
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! true
            | "zippy"  !! false
            |> aspectTest(filter)
        )
    } ^
    "withDescendants" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz").aspect("zippy").withDescendants

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! false
            | Path.fromString("foo").aspect("zippy")             !! true
            | Path.fromString("foo/bar baz").aspect("zap")       !! false
            | Path.fromString("foo/bar baz").aspect("zippy")     !! true
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! false
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! true
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! false
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    } ^
    "withoutAncestors" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz").aspect("zippy").withoutAncestors

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! false
            | Path.fromString("foo").aspect("zippy")             !! false
            | Path.fromString("foo/bar baz").aspect("zap")       !! false
            | Path.fromString("foo/bar baz").aspect("zippy")     !! true
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! false
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! false
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! false
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    } ^
    "withDescendants.withoutAncestors" ! {
        val filter = Filter.filter(root / id("foo") / "bar" ~ "baz").aspect("zippy").withDescendants.withoutAncestors

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! false
            | Path.fromString("foo").aspect("zippy")             !! false
            | Path.fromString("foo/bar baz").aspect("zap")       !! false
            | Path.fromString("foo/bar baz").aspect("zippy")     !! true
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! false
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! true
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! false
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    }
}

class AspectFilterSpecTest extends SpecificationWithJUnit with FilterTestFixtures with DataTables { def is =
    "AspectFilter" ^
    "default" ! {
        val filter = Filter.filter("zippy")

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! true
            | Path.fromString("foo").aspect("zap")               !! false
            | Path.fromString("foo").aspect("zippy")             !! true
            | Path.fromString("foo/bar baz").aspect("zap")       !! false
            | Path.fromString("foo/bar baz").aspect("zippy")     !! true
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! false
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! true
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! true
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! false
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    } ^
    "excluded" ! {
        val filter = Filter.filter("zippy").exclude

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! true
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! true
            | Path.fromString("foo").aspect("zippy")             !! false
            | Path.fromString("foo/bar baz").aspect("zap")       !! true
            | Path.fromString("foo/bar baz").aspect("zippy")     !! false
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! true
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! false
            | Path.fromString("foo/bar qux").aspect("zap")       !! true
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! true
            | "zippy"  !! false
            |> aspectTest(filter)
        )
    }
}

class UnfilteredSpecTest extends SpecificationWithJUnit with FilterTestFixtures with DataTables { def is =
    "Unfiltered" ^
    "default" ! {
        val filter = Unfiltered()

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! true
            | Path.fromString("bar").aspect("zippy")             !! true
            | Path.fromString("foo").aspect("zap")               !! true
            | Path.fromString("foo").aspect("zippy")             !! true
            | Path.fromString("foo/bar baz").aspect("zap")       !! true
            | Path.fromString("foo/bar baz").aspect("zippy")     !! true
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! true
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! true
            | Path.fromString("foo/bar qux").aspect("zap")       !! true
            | Path.fromString("foo/bar qux").aspect("zippy")     !! true
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! true
            | "zippy"  !! true
            |> aspectTest(filter)
        )
    } ^
    "excluded" ! {
        val filter = Unfiltered().exclude

        (
              "node"                                             || "expected"
            | Path.fromString("bar").aspect("zap")               !! false
            | Path.fromString("bar").aspect("zippy")             !! false
            | Path.fromString("foo").aspect("zap")               !! false
            | Path.fromString("foo").aspect("zippy")             !! false
            | Path.fromString("foo/bar baz").aspect("zap")       !! false
            | Path.fromString("foo/bar baz").aspect("zippy")     !! false
            | Path.fromString("foo/bar baz/qux").aspect("zap")   !! false
            | Path.fromString("foo/bar baz/qux").aspect("zippy") !! false
            | Path.fromString("foo/bar qux").aspect("zap")       !! false
            | Path.fromString("foo/bar qux").aspect("zippy")     !! false
            |> nodeTest(filter)
        ) and (
              "aspect" || "expected"
            | "zap"    !! false
            | "zippy"  !! false
            |> aspectTest(filter)
        )
    }
}
