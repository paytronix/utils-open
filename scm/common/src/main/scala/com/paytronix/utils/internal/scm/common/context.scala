//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.common
package context

import com.paytronix.utils.interchange.{OverrideCoding, StringCoder}
import com.paytronix.utils.scala.result.Okay

import PartialFunction.condOpt

/** Pair of domain (such as merchant or cardTemplate) and identifier (such as 84 or 3) */
sealed abstract class Segment {
    val domainOption: Option[String]
    val identifier: String

    def / (cps: Segment): Path = Path(this, cps)
    def / (cp: Path): Path = Path(this +: cp.segments)
}

object Segment {
    implicit val ordering = Ordering[String].on[Segment](_.toString)
    implicit def stringOps(in: String): StringOps = StringOps(in)

    final case class StringOps(s: String) {
        def id: UnqualifiedSegment = UnqualifiedSegment(s.trim)
        def ~ (identifier: String): QualifiedSegment = QualifiedSegment(s.trim, identifier.trim)
    }

    def fromString(in: String): Segment =
        in.trim match {
            case "."   => `.`
            case ".."  => `..`
            case other =>
                other.span(ch => !Character.isWhitespace(ch)) match {
                    case (identifier, "") => UnqualifiedSegment(identifier)
                    case (domain, rest)   => QualifiedSegment(domain, rest.dropWhile(Character.isWhitespace))
                }
        }

    implicit def toPath(in: Segment): Path =
        Path(Vector(in))
}

final class UnqualifiedSegment private[common] (val identifier: String) extends Segment {
    lazy val domainOption = None

    override def equals(other: Any): Boolean =
        other match {
            case UnqualifiedSegment(`identifier`) => true
            case _ => false
        }

    override def hashCode() =
        identifier.hashCode

    override def toString = identifier
}

object UnqualifiedSegment {
    def apply(identifier: String): UnqualifiedSegment = new UnqualifiedSegment(identifier.trim)
    def unapply(in: UnqualifiedSegment): Option[String] = Some(in.identifier)
}

final class QualifiedSegment private[common] (val domain: String, val identifier: String) extends Segment {
    lazy val domainOption = Some(domain)

    override def equals(other: Any): Boolean =
        other match {
            case QualifiedSegment(`domain`, `identifier`) => true
            case _ => false
        }

    override def hashCode() =
        domain.hashCode << 3 ^ identifier.hashCode

    override def toString =
        domain + " " + identifier
}

object QualifiedSegment {
    def apply(domain: String, identifier: String): QualifiedSegment =
        new QualifiedSegment(domain.trim, identifier.trim)
    def unapply(in: QualifiedSegment): Option[(String, String)] =
        Some((in.domain, in.identifier))
}

final case object `.` extends Segment {
    lazy val domainOption = None

    val identifier = "."

    override def toString = "."
}

final case object `..` extends Segment {
    lazy val domainOption = None

    val identifier = ".."

    override def toString = ".."
}

object ~ {
    def unapply(in: Segment): Option[(String, String)] =
        in match {
            case QualifiedSegment(d, i) => Some((d, i))
            case _ => None
        }
}

object id {
    def apply(ident: String): Segment = UnqualifiedSegment(ident.trim)
    def unapply(in: Segment): Option[String] =
        in match {
            case UnqualifiedSegment(i) => Some(i)
            case _ => None
        }
}

/** Type of paths to configuration contexts, which are a sequence of path segments */
sealed abstract class Path {
    val segments: Vector[Segment]

    def isDescendantOf(other: Path): Boolean

    def isEqualToOrDescendantOf(other: Path): Boolean = this == other || isDescendantOf(other)
    def isAncestorOf(other: Path): Boolean = other isDescendantOf this
    def isEqualToOrAncestorOf(other: Path): Boolean = other isEqualToOrDescendantOf this

    def / (other: Path): Path = Path(segments ++ other.segments)
    def / (other: Segment): Path = Path(segments :+ other)

    def aspect(aspect: AspectName): Node =
        Node(this, aspect)

    def ancestors: Iterable[Path]

    lazy val andAncestors: Iterable[Path] =
        ancestors ++ List(this)

    override def toString =
        segments.mkString("/")
}

object PathCoding extends OverrideCoding (
    StringCoder.transform
        (s => Okay(Path.fromString(s)))
        (p => Okay(p.toString))
)

object Path {
    implicit val ordering = Ordering[Iterable[Segment]].on[Path](_.segments)

    val commonAncestors = List(system, global)

    def resolve(in: Vector[Segment]): Vector[Segment] =
        if (in == Vector(id(""))) Vector.empty
        else if (!(in.contains(`..`) || in.contains(`.`))) in.filterNot(_ == id(""))
        else in.foldLeft(Vector.empty[Segment]) { (v, seg) =>
            seg match {
                case id("")|`.` => v
                case `..`       => v.dropRight(1)
                case other      => v :+ other
            }
        }

    def apply(in: Segment*): Path =
        apply(Vector(in: _*))

    def apply(in: Vector[Segment]): Path = {
        val resolved = resolve(in)

             if (resolved == system.segments) system
        else if (resolved == global.segments) global
        else if (resolved == root.segments)   root
        else                            UserPath(resolved)
    }

    def unapplySeq(in: Path): Option[Vector[Segment]] =
        Some(in.segments)

    def fromString(in: String): Path =
        apply(Vector.empty ++ in.split('/').map(Segment.fromString))
}

final case class UserPath private[common] (segments: Vector[Segment]) extends Path {
    def isDescendantOf(other: Path): Boolean = other == system || other == global || (segments.startsWith(other.segments) && segments.size > other.segments.size)

    lazy val ancestors: Iterable[Path] =
        Path.commonAncestors ++ (1 to (segments.size-1)).map { n => Path(segments.take(n)) }
}

final case object system extends Path {
    val segments = Vector(id("system"))

    def isDescendantOf(other: Path): Boolean = false

    def ancestors = Nil

    def unapply(in: Path): Option[system.type] =
        if (in.segments == segments) Some(this)
        else None
}

final case object global extends Path {
    val segments = Vector(id("global"))

    def isDescendantOf(other: Path): Boolean = other == system

    def ancestors = List(system)

    def unapply(in: Path): Option[global.type] =
        if (in.segments == segments) Some(this)
        else None
}

final case object root extends Path {
    val segments = Vector()

    def isDescendantOf(other: Path): Boolean = other == system || other == global

    override def / (cps: Segment): Path = Path(Vector(cps))

    def ancestors = List(system, global)

    def unapply(in: Path): Option[root.type] =
        if (in.segments == segments) Some(this)
        else None
}

object / {
    def unapply(in: Path): Option[(Path, Segment)] =
        condOpt(in.segments.lastOption) {
            case Some(last) => (Path(in.segments.dropRight(1)), last)
        }
}
