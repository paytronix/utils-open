//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.common

import net.liftweb.json.JsonAST.JValue

import com.paytronix.utils.scala.result.Result

import context.Path

/** Interface for a configuration engine, which provides coordination of updates, cache management, and applying the data model appropriately */
trait ConfigurationEngine {
    // Data interface

    /** Enumerate all node names under a given root context, optionally filtered by some aspect */
    def enumerateNodesUsingFilter(filter: Filter): Result[Seq[Node]]

    /** Fetch a single node with inheritance rules applied */
    def fetchNode(node: Node): Result[Option[NodeContents]]

    /** Fetch multiple nodes with inheritance applied */
    def fetchNodes(nodes: Iterable[Node]): Result[Seq[Option[NodeContents]]]

    /** Fetch all nodes using a filter */
    def fetchNodesUsingFilter(filter: Filter): Result[Seq[(Node, NodeContents)]]

    /** Fetch a single node without inheritance rules applied */
    def fetchRawNode(node: Node): Result[Option[NodeContents]]

    /** Fetch multiple nodes without inheritance applied */
    def fetchRawNodes(nodes: Iterable[Node]): Result[Seq[Option[NodeContents]]]

    /** Fetch all nodes using a filter */
    def fetchRawNodesUsingFilter(filter: Filter): Result[Seq[(Node, NodeContents)]]

    /** Update some key paths of an existing node. Upserts are the keys to insert or update (upsert), and deletes are the paths to remove. */
    def updateNode(node: Node, upserts: Seq[(KeyPath, JValue)], deletes: Seq[KeyPath], auditInformation: NodeContents): Result[NodeContents]

    /** Replace the contents of a node wholesale. */
    def replaceNode(node: Node, newContents: NodeContents, auditInformation: NodeContents): Result[NodeContents]

    /** Delete a single node. */
    def deleteNode(node: Node, auditInformation: NodeContents): Result[Unit]

    // Inheritance rules interface

    /**
     * Update the inheritance rules for an aspect.
     * If any rules are already registered for the aspect, any of them that share a keyPath with the new rules will be overwritten.
     */
    def updateInheritanceRulesForAspect(aspect: AspectName, rules: InheritanceRules): Result[Unit]

    /** Fetch the inheritance rules. */
    def fetchInheritanceRules(): Result[Map[AspectName, InheritanceRules]]

    /** Fetch the inheritance rules for an aspect. */
    def fetchInheritanceRulesForAspect(aspect: AspectName): Result[InheritanceRules]

    /** Reload all inheritance rules */
    def reloadInheritanceRules(): Result[Unit]

    /** Reload inheritance rules for a particular aspect */
    def reloadInheritanceRulesForAspect(aspect: AspectName): Result[Unit]

    // Watches

    /**
     * Set a watch on any nodes matching the given filters (union of all filters) that triggers the given function
     * when the contents of the cooked node may have changed.
     *
     * Note that this is called when the filtered node(s) *may* have changed, not definitely. It's upon the watching entity
     * to check if material changes have indeed occurred and react appropriately.
     *
     * Since this is watching the "cooked" version that implies that if any ascendant changes the watch will trigger.
     *
     * Note also that the watcher is held with a weak reference by the `ConfigurationService`, so make sure to hold a hard reference
     * to it somewhere, otherwise the watch will stop working when it's garbage collected.
     */
    def watch(watcher: Watcher, filter: Filter): WatchIdentifier

    /** Cancel a watch */
    def unwatch(identifier: WatchIdentifier): Unit

    // Semi-internal functions

    /** Callback from a ConfigurationStore indicating some nodes changed */
    def invalidate(nodes: Iterable[Node]): Unit

    /**
     * Callback from a ConfigurationStore indicating the underlying storage service cannot guarantee
     * that writes didn't go unnoticed, and so the cache needs to be cleared and all nodes invalidated
     */
    def invalidateAll(): Unit
}

trait CachingConfigurationEngine extends ConfigurationEngine {
    // Cache maintenance functions

    /** Clear the raw cache */
    def clearRawCache(): Unit

    /** Clear the cooked cache */
    def clearCookedCache(): Unit

    /** Show the number of entries in the raw cache */
    def getRawCacheSize(): Int

    /** Show the number of entries in the cooked cache */
    def getCookedCacheSize(): Int

    /** Dump the contents of the raw cache */
    def dumpRawCache(): Seq[CacheEntry]

    /** Dump the contents of the cooked cache as JSON encodings in a HTML table */
    def dumpCookedCache(): Seq[CacheEntry]
}

final case class CacheEntry(node: Node, contents: NodeContents, accessCount: Long, lastAccess: Long)

trait Watcher {
    def watchTriggered(event: WatchEvent): Unit
}

sealed abstract class WatchEvent
final case class NodeInvalidated(node: Node) extends WatchEvent
final case class AspectInvalidated(aspect: AspectName) extends WatchEvent
final case object AllInvalidated extends WatchEvent
