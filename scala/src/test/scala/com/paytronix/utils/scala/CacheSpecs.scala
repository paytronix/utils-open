//
// Copyright 2012 Paytronix Systems, Inc.
// All Rights Reserved
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

object LFUCacheTestFixtures extends SpecificationFeatures {
    def newLFU = new LFUCache[TestKey, TestValue](20, 0.25)
    def newIndexedLFU = new LFUCache[TestKey, TestValue](20, 0.25) {
        object byB extends Index(_.value.b)
    }

    final case class TestKey(a: String)
    final case class TestValue(b: String, c: String)

    val tkey = TestKey("foo")
    val tvalue = TestValue("bar", "baz")

    def writerTests(s: String)(f: (LFUCache[_, _] => Unit, LFUCache[_, _] => Unit) => MatchResult[Any]) =
        (s + " (neutral)") ! f(_ => (), _ => ()) ^
        (s + " (pre-flush)") ! f(_.killWriter(), _ => ()) ^
        (s + " (post-flush)") ! f(_ => (), _.waitForWriteQueueToEmpty())
}

import LFUCacheTestFixtures._

class LFUCacheBasicSpecTest extends SpecificationWithJUnit { def is =
    "LFU Cache basics" ^
    "starts empty" ! { newLFU.byKey.valueMap must_== Map.empty } ^
    writerTests("store(single)") { (pre, post) =>
        val lfu = newLFU

        pre(lfu)
        lfu.store(tkey, tvalue)
        post(lfu)

        lfu.byKey.get(tkey) must_== Some(tvalue)
    } ^
    writerTests("store(iterable)") { (pre, post) =>
        val lfu = newLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(List(tkey -> tvalue, tkey2 -> tvalue2))
        post(lfu)

        { lfu.byKey.get(tkey) must_== Some(tvalue) } and { lfu.byKey.get(tkey2) must_== Some(tvalue2) }
    } ^
    writerTests("remove(single)") { (pre, post) =>
        val lfu = newLFU
        lfu.store(tkey, tvalue)

        { lfu.byKey.get(tkey) must_== Some(tvalue) } and
        {
            pre(lfu)
            lfu.remove(tkey)
            post(lfu)
            lfu.byKey.get(tkey) must_== None
        }
    } ^
    writerTests("remove(iterable)") { (pre, post) =>
        val lfu = newLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)

        { lfu.byKey.get(tkey) must_== Some(tvalue) } and
        { lfu.byKey.get(tkey2) must_== Some(tvalue2) } and
        {
            pre(lfu)
            lfu.remove(List(tkey, tkey2))
            post(lfu)
            ok
        } and
        { lfu.byKey.get(tkey) must_== None } and
        { lfu.byKey.get(tkey2) must_== None }
    } ^
    writerTests("store then remove") { (pre, post) =>
        val lfu = newLFU

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.remove(tkey)
        post(lfu)

        lfu.byKey.get(tkey) must_== None
    } ^
    writerTests("remove then store") { (pre, post) =>
        val lfu = newLFU

        pre(lfu)
        lfu.remove(tkey)
        lfu.store(tkey, tvalue)
        post(lfu)

        lfu.byKey.get(tkey) must_== Some(tvalue)
    } ^
    writerTests("two separate stores") { (pre, post) =>
        val lfu = newLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)
        post(lfu)

        { lfu.byKey.get(tkey) must_== Some(tvalue) } and { lfu.byKey.get(tkey2) must_== Some(tvalue2) }
    } ^
    writerTests("two replacing stores") { (pre, post) =>
        val lfu = newLFU
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        val res =
            { lfu.store(tkey, tvalue ); lfu.byKey.get(tkey) must_== Some(tvalue ) } and
            { lfu.store(tkey, tvalue2); lfu.byKey.get(tkey) must_== Some(tvalue2) }
        post(lfu)
        res
    } ^
    writerTests("two replacing stores with a remove in between") { (pre, post) =>
        val lfu = newLFU
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        val res =
            { lfu.store(tkey, tvalue);  lfu.byKey.get(tkey) must_== Some(tvalue)  } and
            { lfu.remove(tkey);         lfu.byKey.get(tkey) must_== None          } and
            { lfu.store(tkey, tvalue2); lfu.byKey.get(tkey) must_== Some(tvalue2) }
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

        { m.get(tkey).map(_.value) must_== Some(tvalue) } and { m.get(tkey2).map(_.value) must_== Some(tvalue2) } and { m.size must_== 2 }
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

        { m.get(tkey) must_== Some(tvalue) } and { m.get(tkey2) must_== Some(tvalue2) } and { m.size must_== 2 }
    }
}

class LFUCacheAlternateIndexSpecTest extends SpecificationWithJUnit { def is =
    "LFU Cache alternate indexes" ^
    "starts empty" ! { newIndexedLFU.byB.valueMap must_== Map.empty } ^
    writerTests("store(single)") { (pre, post) =>
        val lfu = newIndexedLFU

        pre(lfu)
        lfu.store(tkey, tvalue)
        post(lfu)

        lfu.byB.get(tvalue.b) must_== Some(tvalue)
    } ^
    writerTests("store(iterable)") { (pre, post) =>
        val lfu = newIndexedLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(List(tkey -> tvalue, tkey2 -> tvalue2))
        post(lfu)

        { lfu.byB.get(tvalue.b) must_== Some(tvalue) } and { lfu.byB.get(tvalue2.b) must_== Some(tvalue2) }
    } ^
    writerTests("remove(single)") { (pre, post) =>
        val lfu = newIndexedLFU
        lfu.store(tkey, tvalue)

        { lfu.byB.get(tvalue.b) must_== Some(tvalue) } and
        {
            pre(lfu)
            lfu.remove(tkey)
            post(lfu)
            lfu.byB.get(tvalue.b) must_== None
        }
    } ^
    writerTests("remove(iterable)") { (pre, post) =>
        val lfu = newIndexedLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)

        { lfu.byB.get(tvalue.b) must_== Some(tvalue) } and
        { lfu.byB.get(tvalue2.b) must_== Some(tvalue2) } and
        {
            pre(lfu)
            lfu.remove(List(tkey, tkey2))
            post(lfu)
            ok
        } and
        { lfu.byB.get(tvalue.b) must_== None } and
        { lfu.byB.get(tvalue2.b) must_== None }
    } ^
    writerTests("store then remove") { (pre, post) =>
        val lfu = newIndexedLFU

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.remove(tkey)
        post(lfu)

        lfu.byB.get(tvalue.b) must_== None
    } ^
    writerTests("remove then store") { (pre, post) =>
        val lfu = newIndexedLFU

        pre(lfu)
        lfu.remove(tkey)
        lfu.store(tkey, tvalue)
        post(lfu)

        lfu.byB.get(tvalue.b) must_== Some(tvalue)
    } ^
    writerTests("two separate stores") { (pre, post) =>
        val lfu = newIndexedLFU
        val tkey2 = TestKey("bar")
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        lfu.store(tkey, tvalue)
        lfu.store(tkey2, tvalue2)
        post(lfu)

        { lfu.byB.get(tvalue.b) must_== Some(tvalue) } and { lfu.byB.get(tvalue2.b) must_== Some(tvalue2) }
    } ^
    writerTests("two replacing stores") { (pre, post) =>
        val lfu = newIndexedLFU
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        val res =
            { lfu.store(tkey, tvalue ); lfu.byB.get(tvalue.b)  must_== Some(tvalue ) } and
            {                           lfu.byB.get(tvalue2.b) must_== None          } and
            { lfu.store(tkey, tvalue2); lfu.byB.get(tvalue.b)  must_== None          } and
            {                           lfu.byB.get(tvalue2.b) must_== Some(tvalue2) }
        post(lfu)
        res
    } ^
    writerTests("two replacing stores with a remove in between") { (pre, post) =>
        val lfu = newIndexedLFU
        val tvalue2 = TestValue("biz", "bop")

        pre(lfu)
        val res =
            { lfu.store(tkey, tvalue);  lfu.byB.get(tvalue.b)  must_== Some(tvalue)  } and
            { lfu.remove(tkey);         lfu.byB.get(tvalue.b)  must_== None          } and
            { lfu.store(tkey, tvalue2); lfu.byB.get(tvalue2.b) must_== Some(tvalue2) }
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

        { m.get(tvalue.b).map(_.value) must_== Some(tvalue) } and { m.get(tvalue2.b).map(_.value) must_== Some(tvalue2) } and { m.size must_== 2 }
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

        { m.get(tvalue.b) must_== Some(tvalue) } and { m.get(tvalue2.b) must_== Some(tvalue2) } and { m.size must_== 2 }
    }
}

class LFUCacheLoadSpecTest extends SpecificationWithJUnit {
    def loadTest(readers: Int, writers: Int) = {
        val runtime = 10 /* s */
        val keys = 50
        val leeway = 10 /* ms */

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
            th.join(500L)
            if (th.isAlive) println(testId + ": waited 500ms for " + th + " to end, but it didn't")
        }

        println(
            testId + ": completed " + (if (failures.isEmpty) "successfully" else "in error") +
            " after " + reads + " reads (~" + (reads / 10.0) + " per sec) and " + writes + " writes (~" + (writes / 10.0) + " per sec)"
        )

        { (writerThreads ++ readerThreads).filter(_.isAlive).toList must_== Nil } and
        { failures.toList must_== Nil }
    }

    def is =
        "LFU Cache load" ^
        "many readers few writers" ! loadTest(4, 1) ^
        "even readers and writers" ! loadTest(5, 5) ^
        "many writers few readers" ! loadTest(1, 4)
}

class LFUCacheEvictionSpecTest extends SpecificationWithJUnit { def is =
    "LFU cache eviction" ^
    "should not evict prematurely" ! {
        val lfu = new LFUCache[Int, Int](20, 0.25)
        (1 to 20).foreach(i => lfu.store(i, i))
        lfu.waitForWriteQueueToEmpty()
        lfu.byKey.valueMap must_== Map((1 to 20).map(i => i -> i): _*)
    } ^
    "should honor the evict fraction" ! {
        val lfu = new LFUCache[Int, Int](20, 0.25)
        (1 to 21).foreach(i => lfu.store(i, i))
        lfu.waitForWriteQueueToEmpty()
        lfu.byKey.valueMap.size must_== 15 // 20 * 0.25 == 15 after eviction
    } ^
    "should evict the least used entries in order (ascending accesses)" ! {
        val lfu = new LFUCache[Int, Int](20, 0.25)
        (1 to 20).foreach(i => lfu.store(i, i))
        (1 to 20).foreach(i => (1 to i).foreach(_ => lfu.byKey.get(i)))

        lfu.waitForWriteQueueToEmpty()

        { lfu.byKey.valueMap must_== Map((1 to 20).map(i => i -> i): _*) } and {
            lfu.store(99, 99) // trigger eviction
            lfu.waitForWriteQueueToEmpty()
            lfu.byKey.valueMap must_== Map((99 :: (7 to 20).toList).map(i => i -> i): _*)
        }
    } ^
    "should evict the least used entries in order (descending accesses)" ! {
        val lfu = new LFUCache[Int, Int](20, 0.25)
        (1 to 20).foreach(i => lfu.store(i, i))
        (1 to 20).foreach(i => (1 to (21-i)).foreach(_ => lfu.byKey.get(i)))

        lfu.waitForWriteQueueToEmpty()

        { lfu.byKey.valueMap must_== Map((1 to 20).map(i => i -> i): _*) } and {
            lfu.store(99, 99) // trigger eviction
            lfu.waitForWriteQueueToEmpty()
            lfu.byKey.valueMap must_== Map((99 :: (1 to 14).toList).map(i => i -> i): _*)
        }
    }
}
