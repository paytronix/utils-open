//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.common.context

import org.specs2.SpecificationWithJUnit
import org.specs2.matcher.{DataTables, Matcher, MatchResult}

import com.paytronix.utils.internal.scm.common.{aspect, Node}
import Segment.stringOps

object PathTestFixtures {
    def cp(segs: Segment*) = Path(segs: _*)
    def cp(s: String) = Path.fromString(s)
}

import PathTestFixtures._

class PathDSLSpecTest extends SpecificationWithJUnit { def is =
    "Path DSL" ^
    "compose paths using /" ! {
        (root / id("foo") / id("bar") / "baz" ~ "qux") must_== Path (
            UnqualifiedSegment("foo"),
            UnqualifiedSegment("bar"),
            QualifiedSegment("baz", "qux")
        )
    } ^
    "decompose paths using /" ! {
        Path (
            UnqualifiedSegment("foo"),
            UnqualifiedSegment("bar"),
            QualifiedSegment("baz", "qux")
        ) must beLike {
            case root / id("foo") / id("bar") / "baz" ~ "qux" => ok
            case _ => ko
        }
    }^
    "decompose paths using Path.unapplySeq" ! {
        Path (
            UnqualifiedSegment("foo"),
            UnqualifiedSegment("bar"),
            QualifiedSegment("baz", "qux")
        ) must beLike {
            case Path(id("foo"), id("bar"), "baz" ~ "qux") => ok
            case _ => ko
        }
    } ^
    "compose nodes using aspect" ! {
        (root / id("foo") / id("bar") aspect "baz") must_== Node (
            Path(UnqualifiedSegment("foo"), UnqualifiedSegment("bar")),
            "baz"
        )
    } ^
    "decompose nodes using aspect" ! {
        Node (
            Path(UnqualifiedSegment("foo"), UnqualifiedSegment("bar")),
            "baz"
        ) must beLike {
            case root / id("foo") / id("bar") aspect "baz" => ok
            case _ => ko
        }
    }
}

class PathParseSpecTest extends SpecificationWithJUnit { def is =
    "Path parsing" ^
    "empty path"               ! { Path.fromString("") must_== root } ^
    "unqualified identifier"   ! { Path.fromString("foo") must_== Path(id("foo")) } ^
    "qualified identifier"     ! { Path.fromString("foo bar") must_== Path("foo" ~ "bar") } ^
    "multiple components"      ! { Path.fromString("foo bar/baz/zippy") must_== Path("foo" ~ "bar", id("baz"), id("zippy")) } ^
    "literal system"           ! { Path.fromString("system") must_== system } ^
    "literal global"           ! { Path.fromString("global") must_== global } ^
    "lone slash"               ! { Path.fromString("/") must_== root } ^
    "extraneous leading slash" ! { Path.fromString("/foo/bar") must_== Path(id("foo"), id("bar")) } ^
    "extraneous mid slash"     ! { Path.fromString("foo//bar") must_== Path(id("foo"), id("bar")) } ^
    ".."                       ! { Path.fromString("foo/../bar") must_== Path(id("bar")) } ^
    "."                        ! { Path.fromString("foo/./bar") must_== Path(id("foo"), id("bar")) } ^
    "extraneous whitespace"    ! { Path.fromString("   foo  / bizbaz zipzap / woo") must_== Path(id("foo"), "bizbaz" ~ "zipzap", id("woo")) }
}

class PathConstructionTest extends SpecificationWithJUnit { def is =
    "Path construction" ^
    "empty path"           ! { Path() must_== root } ^
    "extra whitespaces id" ! { Path(id("  foo  ")) must_== Path(id("foo")) } ^
    "extra whitespaces ~"  ! { Path("   foo " ~ "   baz  ") must_== Path("foo" ~ "baz") } ^
    "system"               ! { (Path(id("system")) must_== system) and (system must_== Path(id("system"))) } ^
    "global"               ! { (Path(id("global")) must_== global) and (global must_== Path(id("global"))) } ^
    "root"                 ! { (Path() must_== root) and (root must_== Path()) } ^
    ".."                   ! { Path(id("foo"), `..`, id("bar")) must_== Path(id("bar")) } ^
    "."                    ! { Path(id("foo"), `.`, id("bar")) must_== Path(id("foo"), id("bar")) }
}

class PathRelationSpecTest extends SpecificationWithJUnit with DataTables {
    val checkRelation: (Path, (Path, Path) => Boolean, Path, Boolean) => MatchResult[Any] =
        (lhs, relation, rhs, expectation) => relation(lhs, rhs) must_== expectation

    object isDescendantOf          extends ((Path, Path) => Boolean) { def apply(a: Path, b: Path) = a isDescendantOf          b; override def toString = "isDescendantOf" }
    object isEqualToOrDescendantOf extends ((Path, Path) => Boolean) { def apply(a: Path, b: Path) = a isEqualToOrDescendantOf b; override def toString = "isEqualToOrDescendantOf" }
    object isAncestorOf            extends ((Path, Path) => Boolean) { def apply(a: Path, b: Path) = a isAncestorOf            b; override def toString = "isAncestorOf" }
    object isEqualToOrAncestorOf   extends ((Path, Path) => Boolean) { def apply(a: Path, b: Path) = a isEqualToOrAncestorOf   b; override def toString = "isEqualToOrAncestorOf" }

    def proveIsDescendant(d: Path, a: Path) = (
          "LHS" || "relation"              || "RHS" || "expectation"
        | d     !! isDescendantOf          !! a     !! true
        | d     !! isDescendantOf          !! d     !! false
        | a     !! isDescendantOf          !! d     !! false
        | d     !! isEqualToOrDescendantOf !! a     !! true
        | d     !! isEqualToOrDescendantOf !! d     !! true
        | a     !! isEqualToOrDescendantOf !! d     !! false
        | d     !! isAncestorOf            !! a     !! false
        | d     !! isAncestorOf            !! d     !! false
        | a     !! isAncestorOf            !! d     !! true
        | d     !! isEqualToOrAncestorOf   !! a     !! false
        | d     !! isEqualToOrAncestorOf   !! d     !! true
        | a     !! isEqualToOrAncestorOf   !! d     !! true
        |> checkRelation
    )


    def is =
        proveIsDescendant(cp("foo/bar"), cp("foo")) and
        proveIsDescendant(cp("foo/bar"), system) and
        proveIsDescendant(cp("foo/bar"), global) and
        proveIsDescendant(global, system) and (
              "LHS"         || "relation"              || "RHS"         || "expectation"
            | cp("bar/baz") !! isDescendantOf          !! cp("foo")     !! false
            | cp("foo")     !! isDescendantOf          !! cp("bar/baz") !! false
            | cp("bar/baz") !! isEqualToOrDescendantOf !! cp("foo")     !! false
            | cp("foo")     !! isEqualToOrDescendantOf !! cp("bar/baz") !! false
            | cp("bar/baz") !! isAncestorOf            !! cp("foo")     !! false
            | cp("foo")     !! isAncestorOf            !! cp("bar/baz") !! false
            | cp("bar/baz") !! isEqualToOrAncestorOf   !! cp("foo")     !! false
            | cp("foo")     !! isEqualToOrAncestorOf   !! cp("bar/baz") !! false
            |> checkRelation
        )

}

class PathAncestorSpecTest extends SpecificationWithJUnit { def is =
    "Path ancestor computation" ^
    "(foo/bar baz/zippy).ancestors" ! {
        cp("foo/bar baz/zippy").ancestors.toList must_== List(system, global, cp("foo"), cp("foo/bar baz"))
    } ^
    "(foo/bar baz/zippy).andAncestors" ! {
        cp("foo/bar baz/zippy").andAncestors.toList must_== List(system, global, cp("foo"), cp("foo/bar baz"), cp("foo/bar baz/zippy"))
    } ^
    "system.ancestors" ! { system.ancestors must be empty } ^
    "system.andAncestors" ! { system.andAncestors must_== List(system: Path) } ^
    "global.ancestors" ! { global.ancestors must_== List(system: Path) } ^
    "global.andAncestors" ! { global.andAncestors must_== List(system: Path, global: Path) }
}




