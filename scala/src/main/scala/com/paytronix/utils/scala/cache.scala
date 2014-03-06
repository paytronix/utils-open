//
// Copyright 2012 Paytronix Systems, Inc.
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

package com.paytronix.utils.scala

import java.security.AccessController
import java.util.NoSuchElementException
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.ref.WeakReference
import sun.misc.Unsafe // I've always wanted to use this!

import concurrent.atomicUpdate

object cache {
    /** Helper for `Cache` which calls `waitForAndProcessWrites` continuously until the cache is GCed */
    final class CacheWriter[C <: Cache](cacheReference: WeakReference[C]) extends Runnable {
        var continue = true
        @tailrec
        def run() =
            if (!continue) ()
            else cacheReference.get match {
                case Some(cache) =>
                    cache.waitForAndProcessWrites()
                    run()
                case None =>
                    ()
            }
    }

    /**
     * Trait of lock-free caches with synchronous semantics but asynchronous writing under the hood.
     *
     * Caches always have a primary keying which allows for replacement and lookup, plus zero or more additional indexes, and support a configurable eviction/sizing
     * policy by mixin.
     *
     * Caches are lock-free for reads by way of using `AtomicReference` to hold references to immutable maps, which are replaced by a background writer thread. Writes
     * are processed by appending to a write queue which is consumed by the background thread. Synchronous semantics are maintained for reads by having each read scan
     * through the (hopefully small) write queue for any applicable changes waiting for the writer thread to process.
     */
    trait Cache {
        /** Type of primary key */
        protected type Key
        /** Type of values */
        protected type Value
        /** Type of cache entries, refined by particular cache types to track whatever is appropriate */
        protected type Entry <: EntryInterface

        /** Minimum interface for cache entries */
        protected trait EntryInterface {
            /** Indicate that this cache entry is being accessed by a read */
            def touch: Unit

            /** Whether this cache entry is newer/fresher and should be kept in preference to some other entry when storing */
            def supersedes(other: Entry): Boolean

            /** Do any combining required when this cache entry is superseding an existing one (e.g. summing access counts) */
            def superseding(other: Entry): Entry

            /** The primary key of the value */
            val key: Key

            /** The value being cached  */
            val value: Value
        }

        /** Factory for Entry instances */
        protected def makeEntry(key: Key, value: Value): Entry

        /** Determine which (if any) cache entries to evict before adding `aboutToAdd` entries to the cache */
        protected def evictions(aboutToAdd: Int, store: Iterable[Entry]): Iterable[Entry]

        /** Enqueue some new write action and notify the writer */
        private def write(write: Write): Unit = {
            atomicUpdate(writeQueue)(_ enqueue write)
            unsafe.unpark(writerThread)
        }

        /** Store a new value in the cache along with its primary key */
        def store(key: Key, value: Value): Unit =
            write(Store(makeEntry(key, value)))

        /** Store a sequence of key-value pairs in the cache */
        def store(pairs: Iterable[(Key, Value)]): Unit =
            write(Stores(pairs.map(Function.tupled(makeEntry))))

        /** Remove a value from the cache by primary key */
        def remove(key: Key): Unit =
            byKey.getEntry(key).foreach { e =>
                write(Remove(e))
            }

        /** Remove some values frmo the cache by their primary key */
        def remove(keys: Iterable[Key]): Unit =
            write(Removes(keys.flatMap(byKey.getEntry)))

        /** Clear the entire contents of the cache */
        def clear(): Unit =
            write(Clear)

        /** Estimate the size of the cache. Note: NOT totally accurate, as this does not consider pending writes */
        def size: Int =
            byKey.store.size

        /** Report the size of the write queue */
        def writeQueueSize: Int =
            writeQueue.get.size

        /**
         * A single keyed index into the cache using some alternate key of type `K` that can be projected
         * from a cache entry (usually by way of the primary key or value) using `projection`
         */
        class Index[K](val projection: Entry => K) extends PartialFunction[K, Value] {
            @volatile private[cache] var store: Map[K, Entry] = Map.empty

            def isDefinedAt(k: K): Boolean = store.isDefinedAt(k)
            def apply(k: K): Value = get(k).getOrElse(throw new NoSuchElementException("key " + k + " not cached"))

            /** Look up a cache entry based on the alternate key, yielding `Some(entry)` iff the entry is cached. */
            def getEntry(k: K): Option[Entry] =
                // fold the current state of the index through the write queue to catch pending stores and removals that affect this entry
                writeQueue.get.foldLeft(store.get(k)) { (prev, write) =>
                    write match {
                        case Store(entry) if projection(entry) == k =>
                            // store of entry which matches the target key, so use it
                            Some(entry)
                        case Store(entry) if prev.map(p => p.key == entry.key && projection(p) == k) getOrElse false =>
                            // store of entry which is the same entry we were going to use, but the key for this index has changed so discard it
                            None
                        case Stores(entries) =>
                            (prev, entries.find(entry => projection(entry) == k)) match {
                                case (_, s@Some(entry)) =>
                                    // store of entry which matches the target key, so use it
                                    s
                                case (Some(prev), None) if projection(prev) == k && entries.exists(entry => entry.key == prev.key) =>
                                    // store of entry which is the same entry we were going to use, but the key for this index has changed so discard it
                                    None
                                case _ =>
                                    prev
                            }
                        case Remove(entry) if projection(entry) == k =>
                            None
                        case Removes(entries) if entries.exists(entry => projection(entry) == k) =>
                            None
                        case Clear =>
                            None
                        case _ =>
                            prev
                    }
                } match {
                    case s@Some(entry) =>
                        entry.touch
                        s
                    case None =>
                        None
                }

            /** Look up a cached value based on its alternate key, yielding `Some(value)` iff the entry is cached. */
            def get(k: K): Option[Value] =
                getEntry(k).map(_.value)

            /** Look up a cached value yielding it or some default value if it's not cached. */
            def getOrElse(k: K, default: => Value): Value =
                get(k).getOrElse(default)

            /** Look up a cached value yielding it or storing some value and yielding that value if it's not cached. */
            def getOrElseAdd(k: K, add: => (Key, Value)): Value =
                get(k).getOrElse {
                    val (key, value) = add
                    Cache.this.store(key, value)
                    value
                }

            /** Remove a value from the cache identified by its alternate key */
            def remove(k: K): Unit =
                getEntry(k) match {
                    case Some(entry) => Cache.this.remove(entry.key)
                    case None => ()
                }

            /** Return the current cache entries indexed by the alternate key */
            def entryMap: Map[K, Entry] =
                writeQueue.get.foldLeft(store) { (m, write) =>
                    write match {
                        case Store(entry) =>
                            m + (projection(entry) -> entry)
                        case Stores(entries) =>
                            m ++ entries.map(entry => (projection(entry) -> entry))
                        case Remove(entry) =>
                            m - projection(entry)
                        case Removes(entries) =>
                            m -- entries.map(projection)
                        case Clear =>
                            Map.empty
                    }
                }

            /** Return the current cache values indexed by the alternate key */
            def valueMap: Map[K, Value] = entryMap.mapValues(_.value)

            override def toString = "Index(" + projection + ")"
        }

        /** Main index by primary key. This is the primary storage of the cache. */
        object byKey extends Index[Key](_.key)

        // ADT for representing queued write operations
        sealed private[cache] abstract class Write
        private final case class Store(entry: Entry) extends Write
        private final case class Stores(entry: Iterable[Entry]) extends Write
        private final case class Remove(entry: Entry) extends Write
        private final case class Removes(entries: Iterable[Entry]) extends Write
        private final case object Clear extends Write

        /** Instance of Unsafe so park and unpark can be used */
        private val unsafe = {
            val f = classOf[Unsafe].getDeclaredFields.find(_.getType == classOf[Unsafe]) getOrElse sys.error("couldn't find Unsafe instance field")
            f.setAccessible(true)
            Option(f.get(null).asInstanceOf[Unsafe]) getOrElse sys.error("Unsafe instance was null")
        }

        /** Queue containing writes pending processing by the writer thread */
        private val writeQueue: AtomicReference[Queue[Write]] = new AtomicReference(Queue.empty)

        /** Reference to each index maintained by the cache */
        private lazy val indexes: Seq[Index[_]] = {
            // ugly, but the only convenient way due to lazy initialization of objects
            getClass.getMethods.filter { m =>
                m.getParameterTypes.isEmpty &&
                classOf[Index[_]].isAssignableFrom(m.getReturnType)
            }.map(_.invoke(this).asInstanceOf[Index[_]])
        }

        /** Reference to the `CacheWriter` running asynchronously, kept around so it can be manually stopped during unit tests */
        private val writer = new CacheWriter(new WeakReference(this))

        /** Reference to the thread running the `CacheWriter`, kept around for `unpark`ing */
        private val writerThread = new Thread(writer, this.toString + " Writer")
        writerThread.setDaemon(true)
        writerThread.start()

        /** Process a `Store` request while in the writer thread */
        private def internalStore(e: Entry): Unit =
            byKey.store.get(e.key) match {
                case Some(existingEntry) if existingEntry supersedes e =>
                    // ignore the store request
                    ()
                case Some(existingEntry) =>
                    val newEntry = e superseding existingEntry
                    val value = newEntry.value
                    indexes.foreach { case index => // `case` important here to capture existential
                        index.store = index.store - index.projection(existingEntry) + (index.projection(newEntry) -> newEntry)
                    }
                case None =>
                    val evicted = evictions(1, byKey.store.values)
                    val value = e.value
                    indexes.foreach { case index => // `case` important here to capture existential
                        index.store = index.store -- evicted.map(index.projection) + (index.projection(e) -> e)
                    }
            }

        /** Process a `Stores` request while in the writer thread */
        private def internalStore(es: Iterable[Entry]): Unit = {
            var toRemove: List[Entry] = Nil
            var toUpdate: List[Entry] = Nil
            var toAdd: List[Entry] = Nil

            es.foreach { e =>
                byKey.store.get(e.key) match {
                    case Some(existingEntry) if existingEntry supersedes e => ()
                    case Some(existingEntry) =>
                        toRemove ::= existingEntry
                        toUpdate ::= e superseding existingEntry
                    case None =>
                        toAdd ::= e
                }
            }

            val evicted = evictions(toAdd.size, byKey.store.values)
            indexes.foreach { case index => // `case` important here to capture existential
                index.store = (
                       index.store
                    -- evicted.map(index.projection)
                    -- toRemove.map(index.projection)
                    ++ toAdd.map(e => (index.projection(e) -> e))
                    ++ toUpdate.map(e => (index.projection(e) -> e))
                )
            }
        }

        /** Process a `Remove` request while in the writer thread */
        private def internalRemove(e: Entry): Unit =
            indexes.foreach { case index => // `case` important here to capture existential
                index.store -= index.projection(e)
            }

        /** Process a `Removes` request while in the writer thread */
        private def internalRemove(es: Iterable[Entry]): Unit =
            indexes.foreach { case index => // `case` important here to capture existential
                index.store --= es.map(index.projection)
            }

        /** Process a `Clear` request while in the writer thread */
        private def internalClear(): Unit =
            indexes.foreach { case index => // `case` important here to capture existential
                index.store = Map.empty
            }

        /** Process all the entries in the write queue at entry using `internalStore` and `internalRemove` and then remove those processed entries from the queue */
        private[cache] def processWrites(): Unit = {
            val queue = writeQueue.get
            queue.foreach {
                case Store(entry)    => internalStore(entry)
                case Stores(entries) => internalStore(entries)
                case Remove(key)     => internalRemove(key)
                case Removes(keys)   => internalRemove(keys)
                case Clear           => internalClear()
            }
            atomicUpdate(writeQueue)(_ drop queue.size)
        }

        /** Wait until the write queue has entries by continually `park`ing the thread to be woken by `unpark` or when 100ms elapses, then process the write queue */
        private[cache] def waitForAndProcessWrites(): Unit = {
            while (writeQueue.get.isEmpty)
                unsafe.park(false, 100000000)
            processWrites()
        }

        /** Unit test hook: kill the writer thread to test behavior while entries are still pending in the queue */
        private[scala] def killWriter(): Unit = {
            writer.continue = false
            unsafe.unpark(writerThread)
        }

        /** Unit test hook: restore the writer thread after `killWriter` */
        private[scala] def restoreWriter(): Unit = {
            writer.continue = true
            val newWriter = new Thread(writer, "writer thread")
            newWriter.setDaemon(true)
            newWriter.start()
        }

        /** Unit test hook: wait for any pending write queue entries to be processed to test behavior after entries have been folded back into the main maps */
        private[scala] def waitForWriteQueueToEmpty(): Unit = {
            val start = System.currentTimeMillis
            while (!writeQueue.get.isEmpty) {
                if (System.currentTimeMillis - start > 1000) sys.error("waited a second for write queue to drain but it never did. it contains: " + writeQueue.get)
                Thread.sleep(10)
            }
        }
    }

    /**
     * Mixin to `Cache` that evicts entries when the cache grows past a certain maximum size, evicting entries from lowest priority to highest priority with priority
     * determined by some refinement. Examples of priority might be number of accesses (LFU cache), or cache entry age (LRU cache).
     */
    trait PriorityEvictionPolicy extends Cache {
        /** Type that represents the priority of a particular cache entry */
        protected type Priority

        /** Entries in a cache with `PriorityEvictionPolicy` must implement `PriorityEntryInterface` to calculate priorities during eviction processing */
        type Entry <: PriorityEntryInterface

        /** Refinement of `EntryInterface` that adds a method to calculate entry priority */
        trait PriorityEntryInterface extends EntryInterface {
            def priority: Priority
        }

        /** How to order priorities. Entries with lesser ordering will be evicted before those with higher. */
        protected implicit def priorityOrdering: Ordering[Priority]

        /** Size threshold which will trigger eviction. A cache with `PriorityEvictionPolicy` will evict when another entry and the cache already stores `maxSize` entries */
        def maxSize: Int

        /** What fraction of the `maxSize` to evict when performing an eviction. That is, after eviction occurs the cache will have `maxSize` - `maxSize` * `evictFraction` entires */
        def evictFraction: Double

        protected def evictions(aboutToAdd: Int, store: Iterable[Entry]): Iterable[Entry] = {
            val curSize = store.size
            val newSize = curSize + aboutToAdd

            if (newSize > maxSize) store.toSeq.sortBy(_.priority).take(((newSize - maxSize) + (maxSize * evictFraction).asInstanceOf[Int]) min curSize)
            else Nil
        }
    }

    /** Least-frequently-used cache, which evicts entries that have been used the least. See `PriorityEvictionPolicy` and `Cache` for more. */
    class LFUCache[A, B](var maxSize: Int, var evictFraction: Double) extends Cache with PriorityEvictionPolicy {
        type Key = A
        type Value = B
        type Priority = Long

        protected val priorityOrdering: Ordering[Priority] = implicitly

        final class Entry(val key: A, val value: B, val creation: Long) extends EntryInterface with PriorityEntryInterface {
            @volatile private var _accessCount = 0L
            @volatile private var _lastAccess = System.nanoTime

            def supersedes(existing: Entry): Boolean =
                creation > existing.creation

            def superseding(existing: Entry): Entry = {
                _accessCount = existing.accessCount + _accessCount
                _lastAccess = existing.lastAccess.max(_lastAccess)
                this
            }

            def priority: Long = _accessCount

            /** The access count for this entry. Possibly undercounted under very heavy read load. */
            def accessCount: Long = _accessCount

            /** Last access time for this entry as reported by `System.nanoTime`. */
            def lastAccess: Long = _lastAccess

            def touch = {
                _accessCount += 1
                _lastAccess = System.nanoTime
            }

            override def toString =
                "LFUCache.Entry(" + key + ", " + value + ", creation = " + creation + ", accessCount = " + accessCount + ", lastAccess = " + lastAccess + ")"
        }

        protected def makeEntry(key: Key, value: Value): Entry = new Entry(key, value, creationOf(key, value))

        /** Provide a creation/freshness date for a key/value pair to control supersession. Default implementation is the current high precision timer (`System.nanoTime`) */
        def creationOf(key: Key, value: Value): Long =
            System.nanoTime
    }
}
