//
// Copyright 2012-2014 Paytronix Systems, Inc.
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

import java.util.concurrent.atomic.AtomicLong
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import org.specs2.{SpecificationFeatures, SpecificationWithJUnit}
import org.specs2.matcher.MatchResult

import cache._
import concurrent.atomicUpdate

object LFUCacheTestFixtures {
    def newLFU = new LFUCache[TestKey, TestValue](20, 0.25)
    def newIndexedLFU = new IndexedLFU

    class IndexedLFU extends LFUCache[TestKey, TestValue](20, 0.25) {
        object byB extends Index(_.value.b)
    }

    final case class TestKey(a: String)
    final case class TestValue(b: String, c: String)

    val tkey = TestKey("foo")
    val tvalue = TestValue("bar", "baz")
}

trait LFUCacheHelpers { self: SpecificationFeatures =>
    def writerTests(s: String)(f: (LFUCache[_, _] => Unit, LFUCache[_, _] => Unit) => MatchResult[Any]) =
        (s + " (neutral)") ! f(_ => (), _ => ()).toResult ^
        (s + " (pre-flush)") ! f(_.killWriter(), _ => ()).toResult ^
        (s + " (post-flush)") ! f(_ => (), _.waitForWriteQueueToEmpty()).toResult
}

import LFUCacheTestFixtures._

class LFUCacheBasicSpecTest extends SpecificationWithJUnit with LFUCacheHelpers { def is =
    "LFU Cache basics" ^
    "starts empty" ! { newLFU.byKey.valueMap ==== Map.empty } ^
    writerTests("store(single)") { (pre, post) =>
        val lfu = newLFU

        pre(lfu)
        lfu.store(tkey, tvalue)
        post(lfu)

        lfu.byKey.get(tkey) ==== Some(tvalue)
    } ^
    writerTests("store(iterable)") { (pre, post) =>
        val lfu = newLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(List(tkey -> tvalue, tkey2 -> tvalue2))
        post(lfu)

        { lfu.byKey.get(tkey) ==== Some(tvalue) } and { lfu.byKey.get(tkey2) ==== Some(tvalue2) }
    } ^
    writerTests("remove(single)") { (pre, post) =>
        val lfu = newLFU
        lfu.store(tkey, tvalue)

        { lfu.byKey.get(tkey) ==== Some(tvalue) } and
        {
            pre(lfu)
            lfu.remove(tkey)
            post(lfu)
            lfu.byKey.get(tkey) ==== None
        }
    } ^
    writerTests("remove(iterable)") { (pre, post) =>
        val lfu = newLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)

        { lfu.byKey.get(tkey) ==== Some(tvalue) } and
        { lfu.byKey.get(tkey2) ==== Some(tvalue2) } and
        {
            pre(lfu)
            lfu.remove(List(tkey, tkey2))
            post(lfu)
            ok
        } and
        { lfu.byKey.get(tkey) ==== None } and
        { lfu.byKey.get(tkey2) ==== None }
    } ^
    writerTests("store then remove") { (pre, post) =>
        val lfu = newLFU

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.remove(tkey)
        post(lfu)

        lfu.byKey.get(tkey) ==== None
    } ^
    writerTests("remove then store") { (pre, post) =>
        val lfu = newLFU

        pre(lfu)
        lfu.remove(tkey)
        lfu.store(tkey, tvalue)
        post(lfu)

        lfu.byKey.get(tkey) ==== Some(tvalue)
    } ^
    writerTests("two separate stores") { (pre, post) =>
        val lfu = newLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)
        post(lfu)

        { lfu.byKey.get(tkey) ==== Some(tvalue) } and { lfu.byKey.get(tkey2) ==== Some(tvalue2) }
    } ^
    writerTests("two replacing stores") { (pre, post) =>
        val lfu = newLFU
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        val res =
            { lfu.store(tkey, tvalue ); lfu.byKey.get(tkey) ==== Some(tvalue ) } and
            { lfu.store(tkey, tvalue2); lfu.byKey.get(tkey) ==== Some(tvalue2) }
        post(lfu)
        res
    } ^
    writerTests("two replacing stores with a remove in between") { (pre, post) =>
        val lfu = newLFU
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        val res =
            { lfu.store(tkey, tvalue);  lfu.byKey.get(tkey) ==== Some(tvalue)  } and
            { lfu.remove(tkey);         lfu.byKey.get(tkey) ==== None          } and
            { lfu.store(tkey, tvalue2); lfu.byKey.get(tkey) ==== Some(tvalue2) }
        post(lfu)
        res
    } ^
    writerTests("entryMap") { (pre, post) =>
        val lfu = newLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)
        post(lfu)

        val m = lfu.byKey.entryMap

        { m.get(tkey).map(_.value) ==== Some(tvalue) } and { m.get(tkey2).map(_.value) ==== Some(tvalue2) } and { m.size ==== 2 }
    } ^
    writerTests("valueMap") { (pre, post) =>
        val lfu = newLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)
        post(lfu)

        val m = lfu.byKey.valueMap

        { m.get(tkey) ==== Some(tvalue) } and { m.get(tkey2) ==== Some(tvalue2) } and { m.size ==== 2 }
    }
}

class LFUCacheAlternateIndexSpecTest extends SpecificationWithJUnit with LFUCacheHelpers { def is =
    "LFU Cache alternate indexes" ^
    "starts empty" ! { newIndexedLFU.byB.valueMap ==== Map.empty } ^
    writerTests("store(single)") { (pre, post) =>
        val lfu = newIndexedLFU

        pre(lfu)
        lfu.store(tkey, tvalue)
        post(lfu)

        lfu.byB.get(tvalue.b) ==== Some(tvalue)
    } ^
    writerTests("store(iterable)") { (pre, post) =>
        val lfu = newIndexedLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(List(tkey -> tvalue, tkey2 -> tvalue2))
        post(lfu)

        { lfu.byB.get(tvalue.b) ==== Some(tvalue) } and { lfu.byB.get(tvalue2.b) ==== Some(tvalue2) }
    } ^
    writerTests("remove(single)") { (pre, post) =>
        val lfu = newIndexedLFU
        lfu.store(tkey, tvalue)

        { lfu.byB.get(tvalue.b) ==== Some(tvalue) } and
        {
            pre(lfu)
            lfu.remove(tkey)
            post(lfu)
            lfu.byB.get(tvalue.b) ==== None
        }
    } ^
    writerTests("remove(iterable)") { (pre, post) =>
        val lfu = newIndexedLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)

        { lfu.byB.get(tvalue.b) ==== Some(tvalue) } and
        { lfu.byB.get(tvalue2.b) ==== Some(tvalue2) } and
        {
            pre(lfu)
            lfu.remove(List(tkey, tkey2))
            post(lfu)
            ok
        } and
        { lfu.byB.get(tvalue.b) ==== None } and
        { lfu.byB.get(tvalue2.b) ==== None }
    } ^
    writerTests("store then remove") { (pre, post) =>
        val lfu = newIndexedLFU

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.remove(tkey)
        post(lfu)

        lfu.byB.get(tvalue.b) ==== None
    } ^
    writerTests("remove then store") { (pre, post) =>
        val lfu = newIndexedLFU

        pre(lfu)
        lfu.remove(tkey)
        lfu.store(tkey, tvalue)
        post(lfu)

        lfu.byB.get(tvalue.b) ==== Some(tvalue)
    } ^
    writerTests("two separate stores") { (pre, post) =>
        val lfu = newIndexedLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)
        post(lfu)

        { lfu.byB.get(tvalue.b) ==== Some(tvalue) } and { lfu.byB.get(tvalue2.b) ==== Some(tvalue2) }
    } ^
    writerTests("two replacing stores") { (pre, post) =>
        val lfu = newIndexedLFU
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        val res =
            { lfu.store(tkey, tvalue ); lfu.byB.get(tvalue.b)  ==== Some(tvalue ) } and
            {                           lfu.byB.get(tvalue2.b) ==== None          } and
            { lfu.store(tkey, tvalue2); lfu.byB.get(tvalue.b)  ==== None          } and
            {                           lfu.byB.get(tvalue2.b) ==== Some(tvalue2) }
        post(lfu)
        res
    } ^
    writerTests("two replacing stores with a remove in between") { (pre, post) =>
        val lfu = newIndexedLFU
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        val res =
            { lfu.store(tkey, tvalue);  lfu.byB.get(tvalue.b)  ==== Some(tvalue)  } and
            { lfu.remove(tkey);         lfu.byB.get(tvalue.b)  ==== None          } and
            { lfu.store(tkey, tvalue2); lfu.byB.get(tvalue2.b) ==== Some(tvalue2) }
        post(lfu)
        res
    } ^
    writerTests("entryMap") { (pre, post) =>
        val lfu = newIndexedLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)
        post(lfu)

        val m = lfu.byB.entryMap

        { m.get(tvalue.b).map(_.value) ==== Some(tvalue) } and { m.get(tvalue2.b).map(_.value) ==== Some(tvalue2) } and { m.size ==== 2 }
    } ^
    writerTests("valueMap") { (pre, post) =>
        val lfu = newIndexedLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)
        post(lfu)

        val m = lfu.byB.valueMap

        { m.get(tvalue.b) ==== Some(tvalue) } and { m.get(tvalue2.b) ==== Some(tvalue2) } and { m.size ==== 2 }
    }
}

// 2014-05-12 RMM: I think this test is a bit broken - it completes but when running in the (4,1) configuration it
// completes a low number of reads but it completes a reasonably balanced number in the (5,5) configuration
class LFUCacheLoadSpecTestDisabled extends SpecificationWithJUnit with LFUCacheHelpers {
    def loadTest(readers: Int, writers: Int) = {
        val runtime = 10 /* s */
        val keys = 50

        val lfu = new LFUCache[Int, Long](keys*2 /* ensure we have plenty of keyspace */, 1.0)
        var seed = System.currentTimeMillis
        val failures = ArrayBuffer.empty[(Int, Long, Option[Long])]
        @volatile var reads = 0L
        @volatile var writes = 0L
        val barrier = new AtomicLong(0L)
        val writeValue = new AtomicLong(0L)

        var endAt = 0L

        final class Reader extends Runnable {
            val rand = new Random(seed)
            seed += 1

            @tailrec
            def run() = {
                val savedBarrier = barrier.get
                val now = System.nanoTime
                if (now < endAt) {
                    reads += 1
                    val slot = rand.nextInt(keys)
                    lfu.byKey.get(slot) match {
                        case Some(l) if l >= savedBarrier => ()
                        case other =>
                        failures.synchronized { failures += ((slot, savedBarrier, other)) }
                    }
                    Thread.`yield`()
                    run()
                }
            }
        }

        final class Writer extends Runnable {
            val rand = new Random(seed)
            seed += 1
            val writeOrder = rand.shuffle(ArrayBuffer((0 to (keys-1)).toSeq: _*))
            var offs = 0
            var batchBarrier = writeValue.get

            @tailrec
            def run() = {
                val now = System.nanoTime
                val value = writeValue.incrementAndGet()
                if (now < endAt) {
                    writes += 1
                    val slot = writeOrder(offs)
                    lfu.store(slot, value)
                    offs += 1
                    if (offs >= writeOrder.size) {
                        atomicUpdate(barrier)(_ min batchBarrier)
                        batchBarrier = value
                        offs = 0
                    }
                    Thread.`yield`()
                    run()
                }
            }
        }

        val testId = "Load test (" + readers + "," + writers + ")"
        val writerThreads = (0 to (writers-1)).map(i => new Thread(new Writer(), testId + " writer " + i))
        val readerThreads = (0 to (readers-1)).map(i => new Thread(new Reader(), testId + " reader " + i))

        for (slot <- 0 to (keys-1)) lfu.store(slot, writeValue.get)

        endAt = System.nanoTime + runtime * 1000000000L

        println(testId + ": starting test")
        writerThreads.foreach(_.start)
        readerThreads.foreach(_.start)

        Thread.sleep(runtime * 1000L)

        println(testId + ": waiting for threads to end")
        (writerThreads ++ readerThreads).foreach { th =>
            th.join(5000L)
            if (th.isAlive) println(testId + ": waited 5s for " + th + " to end, but it didn't")
        }

        println(
            testId + ": completed " + (if (failures.isEmpty) "successfully" else "in error") +
            " after " + reads + " reads (~" + (reads / 10.0) + " per sec) and " + writes + " writes (~" + (writes / 10.0) + " per sec)"
        )

        { (writerThreads ++ readerThreads).filter(_.isAlive).toList ==== Nil } and
        { failures.toList ==== Nil }
    }

    def is =
        sequential ^
        "LFU Cache load" ^
        "many readers few writers" ! loadTest(4, 1) ^
        "even readers and writers" ! loadTest(5, 5) ^
        "many writers few readers" ! loadTest(1, 4)
}

class LFUCacheEvictionSpecTest extends SpecificationWithJUnit with LFUCacheHelpers { def is =
    "LFU cache eviction" ^
    "should not evict prematurely" ! {
        val lfu = new LFUCache[Int, Int](20, 0.25)
        (1 to 20).foreach(i => lfu.store(i, i))
        lfu.waitForWriteQueueToEmpty()
        lfu.byKey.valueMap ==== Map((1 to 20).map(i => i -> i): _*)
    } ^
    "should honor the evict fraction" ! {
        val lfu = new LFUCache[Int, Int](20, 0.25)
        (1 to 21).foreach(i => lfu.store(i, i))
        lfu.waitForWriteQueueToEmpty()
        lfu.byKey.valueMap.size ==== 15 // 20 * 0.25 == 15 after eviction
    } ^
    "should evict the least used entries in order (ascending accesses)" ! {
        val lfu = new LFUCache[Int, Int](20, 0.25)
        (1 to 20).foreach(i => lfu.store(i, i))
        (1 to 20).foreach(i => (1 to i).foreach(_ => lfu.byKey.get(i)))

        lfu.waitForWriteQueueToEmpty()

        { lfu.byKey.valueMap ==== Map((1 to 20).map(i => i -> i): _*) } and {
            lfu.store(99, 99) // trigger eviction
            lfu.waitForWriteQueueToEmpty()
            lfu.byKey.valueMap ==== Map((99 :: (7 to 20).toList).map(i => i -> i): _*)
        }
    } ^
    "should evict the least used entries in order (descending accesses)" ! {
        val lfu = new LFUCache[Int, Int](20, 0.25)
        (1 to 20).foreach(i => lfu.store(i, i))
        (1 to 20).foreach(i => (1 to (21-i)).foreach(_ => lfu.byKey.get(i)))

        lfu.waitForWriteQueueToEmpty()

        { lfu.byKey.valueMap ==== Map((1 to 20).map(i => i -> i): _*) } and {
            lfu.store(99, 99) // trigger eviction
            lfu.waitForWriteQueueToEmpty()
            lfu.byKey.valueMap ==== Map((99 :: (1 to 14).toList).map(i => i -> i): _*)
        }
    }
}
