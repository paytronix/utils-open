//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.common

import com.paytronix.utils.interchange.ExplicitUnionCoding

import context.Path

/** Implicit conversions from nodes and aspect names to filters for those */
object Filter {
    implicit def contextPathToFilter(in: Path): ContextFilter = ContextFilter(in)
    implicit def aspectNameToFilter(in: AspectName): AspectFilter = AspectFilter(in)
    implicit def nodeToFilter(in: Node): NodeFilter = NodeFilter(in.context, in.aspect)

    def filter[A, B <: Filter](in: A)(implicit f: A => B): B = in
}

/** Trait of objects which filter nodes to control which node updates cause a watch to trigger */
sealed trait Filter extends (Node => Boolean) {
    val inclusive: Boolean

    /** Include the context path (and descendants, if configured) (the default) */
    def include: Filter

    /** Exclude the context path (and descendants, if configured) instead of include (the default) */
    def exclude: Filter

    def isCovered(in: Node): Boolean
    def isCovered(in: AspectName): Boolean

    def apply(in: Node): Boolean =
        if (inclusive) isCovered(in) else !isCovered(in)
    def apply(in: AspectName): Boolean =
        if (inclusive) isCovered(in) else !isCovered(in)
}

object FilterCoding extends ExplicitUnionCoding[Filter] {
    override val determinantField = "type"
    alternative[ContextFilter]("context")
    alternative[NodeFilter]("node")
    alternative[AspectFilter]("aspect")
    alternative[Unfiltered]("unfiltered")
}

/** Trait of Filters which apply context path filtering */
sealed trait ContextPathFilter {
    _: Filter =>

    val context: Path
    val ancestors: Boolean
    val descendants: Boolean
    val self: Boolean

    /** Match descendants of this node in addition to the node itself */
    def withDescendants: ContextPathFilter

    /** Do not match descendants of this node in addition to the node itself */
    def withoutDescendants: ContextPathFilter

    /** Apply this filter to ancestors of the node (the default) */
    def withAncestors: ContextPathFilter

    /** Do not apply this filter to ancestors of the node */
    def withoutAncestors: ContextPathFilter

    /** Apply this filter to the named context (the default) */
    def withSelf: ContextPathFilter

    /** Do not apply this filter to the named context (only ancestors or descendants, depending on the other settings */
    def withoutSelf: ContextPathFilter

    /** Only match descendants of the context, not the context itself nor ancestors */
    def descendantsOnly: ContextPathFilter

    def isCoveredContext(in: Node): Boolean =
        (if (self)        in.context == context             else false) ||
        (if (descendants) in.context isDescendantOf context else false) ||
        (if (ancestors)   in.context isAncestorOf   context else false)
}

/** Include or exclude a single context and possibly its descendants */
final case class ContextFilter (
    context: Path,
    inclusive: Boolean = true,
    ancestors: Boolean = true,
    descendants: Boolean = false,
    self: Boolean = true
) extends Filter with ContextPathFilter {
    def isCovered(in: Node) = isCoveredContext(in)
    def isCovered(in: AspectName) = true
    override def apply(in: AspectName) = true

    def include            = copy(inclusive=true)
    def exclude            = copy(inclusive=false)
    def withDescendants    = copy(descendants=true)
    def withoutDescendants = copy(descendants=false)
    def withAncestors      = copy(ancestors=true)
    def withoutAncestors   = copy(ancestors=false)
    def withSelf           = copy(self=true)
    def withoutSelf        = copy(self=false)

    def descendantsOnly = copy(self=false, descendants=true, ancestors=false)

    /** Combine this filter with an aspect filter */
    def aspect(aspect: AspectName): NodeFilter =
        NodeFilter(context, aspect, inclusive, ancestors, descendants)

    override def toString = "ContextFilter(" + context + ", inclusive=" + inclusive + ", ancestors=" + ancestors + ", descendants=" + descendants + ", self=" + self + ")"
}

/** Include or exclude a single context and aspect */
final case class NodeFilter (
    context: Path,
    aspect: AspectName,
    inclusive: Boolean = true,
    ancestors: Boolean = true,
    descendants: Boolean = false,
    self: Boolean = true
) extends Filter with ContextPathFilter {
    def isCovered(in: Node) = isCoveredContext(in) && isCovered(in.aspect)
    def isCovered(in: AspectName) = aspect == in

    def include            = copy(inclusive=true)
    def exclude            = copy(inclusive=false)
    def withDescendants    = copy(descendants=true)
    def withoutDescendants = copy(descendants=false)
    def withAncestors      = copy(ancestors=true)
    def withoutAncestors   = copy(ancestors=false)
    def withSelf           = copy(self=true)
    def withoutSelf        = copy(self=false)

    def descendantsOnly = copy(self=false, descendants=true, ancestors=false)

    override def toString = "NodeFilter(" + context + "#" + aspect + ", inclusive=" + inclusive + ", ancestors=" + ancestors + ", descendants=" + descendants + ", self=" + self + ")"
}

/** Include or exclude a single aspect */
final case class AspectFilter(aspect: AspectName, inclusive: Boolean = true) extends Filter {
    def isCovered(in: Node) = in.aspect == aspect
    def isCovered(in: AspectName) = in == aspect

    def include = copy(inclusive=true)
    def exclude = copy(inclusive=false)

    override def toString = "AspectFilter(" + aspect + ", inclusive=" + inclusive + ")"
}

/** Include or exclude all nodes */
final case class Unfiltered(inclusive: Boolean = true) extends Filter {
    def isCovered(in: Node) = true
    def isCovered(in: AspectName) = true

    def include = copy(inclusive=false)
    def exclude = copy(inclusive=false)

    override def toString = "Unfiltered(inclusive=" + inclusive + ")"
}
