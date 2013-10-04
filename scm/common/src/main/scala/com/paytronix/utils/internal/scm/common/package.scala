//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm

import net.liftweb.json.JsonAST.JObject

import com.paytronix.utils.scala.result.{Failed, Okay, Result}

package object common {
    /** Type of aspect identifiers */
    type AspectName = String

    /** Type of contents of a config node */
    type NodeContents = JObject

    /** Key paths through a node's contents */
    type KeyPath = List[String]
}

package common {
    import context.Path

    /** Path and aspect, identifying a configuration node */
    final case class Node(context: Path, aspect: AspectName) {
        def andAncestors = context.andAncestors map { Node(_, aspect) }
        def ancestors = context.ancestors map { Node (_, aspect) }

        def isDescendantOf         (other: Node): Boolean = (this.context isDescendantOf          other.context) && this.aspect == other.aspect
        def isEqualToOrDescendantOf(other: Node): Boolean = (this.context isEqualToOrDescendantOf other.context) && this.aspect == other.aspect
        def isAncestorOf           (other: Node): Boolean = (this.context isAncestorOf            other.context) && this.aspect == other.aspect
        def isEqualToOrAncestorOf  (other: Node): Boolean = (this.context isEqualToOrAncestorOf   other.context) && this.aspect == other.aspect

        override def toString =
            context + "#" + aspect
    }

    object aspect {
        def unapply(in: Node): Option[(Path, AspectName)] =
            Some((in.context, in.aspect))
    }

    object KeyPath {
        def fromString(in: String): KeyPath =
            in.split('/').toList
    }

    object Node {
        def fromString(in: String): Result[Node] =
            in.indexOf('#') match {
                case -1 => Failed("no # found in node path to separate context from aspect")
                case pos => Okay(Node(Path.fromString(in.substring(0, pos)), in.substring(pos+1)))
            }
    }

    trait WatchIdentifier
}
