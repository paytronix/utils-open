//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.engine

import net.liftweb.json.JsonAST.JValue

import com.paytronix.utils.internal.scm.common.{Filter, KeyPath, Node, NodeContents}
import com.paytronix.utils.scala.result.Result

/**
 * Trait of services which provide configuration storage backend support to ConfigurationEngine.
 */
trait ConfigurationStorage {
    /** Enumerate all node names under a given root context, optionally filtered by some aspect */
    def enumerateNodesUsingFilter(filter: Filter): Result[Seq[Node]]

    /**
     * Fetch a single configuration node.
     * @return Okay(None) for nonexistent node
     * @return Okay(Some(...)) for success
     */
    def fetchNode(node: Node): Result[Option[NodeContents]]

    /**
     * Fetch a set of configuration nodes if they exist.
     */
    def fetchNodes(nodes: Iterable[Node]): Result[Seq[Option[NodeContents]]]

    /** Make updates to some node's contents, upsert (replace or update) some values, and then possibly deleting some keys */
    def updateNode(node: Node, upserts: Seq[(KeyPath, JValue)], deletes: Seq[KeyPath], auditInfo: NodeContents): Result[NodeContents]

    /** Replace the content of a single node. */
    def replaceNode(node: Node, newContents: NodeContents, auditInfo: NodeContents): Result[Unit]

    /** Delete a single node. */
    def deleteNode(node: Node, auditInfo: NodeContents): Result[Unit]
}
