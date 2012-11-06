//
// Copyright 2009-2012 Paytronix Systems, Inc.
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

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.{Condition, Lock => JUCLock, ReadWriteLock => JUCReadWriteLock, ReentrantLock, ReentrantReadWriteLock}

/**
 * Helpers for concurrent programming
 */
object concurrent
{
    /** Update a [[java.util.concurrent.atomic.AtomicReference]] by applying an update to the value contained therein, and trying as many times as necessary */
    def atomicUpdate[A](r: AtomicReference[A])(f: A => A): Unit = {
        def tryUpdate: Unit = {
            val expectedVal = r.get
            val newVal = f(expectedVal)
            if (!r.compareAndSet(expectedVal, newVal)) {
                tryUpdate
            }
        }

        tryUpdate
    }

    sealed abstract class AtomicTransactionResult[+A, +B]
    final case class Update[+A, +B](newValue: A, result: B) extends AtomicTransactionResult[A, B]
    final case class NoUpdate[+B](result: B) extends AtomicTransactionResult[Nothing, B]

    /**
     * Possibly update a [[java.util.concurrent.atomic.AtomicReference]] by applying an update to the value contained therein.
     * If an update is attempted and the reference has since moved, the entire transaction will be rewound and applied again with the
     * new value of the reference along with the result of the previous (aborted) transaction.
     */
    def atomicTransaction[A, B](r: AtomicReference[A])(f: (A, Option[B]) => AtomicTransactionResult[A, B]): B = {
        def tryUpdate(previous: Option[B]): B = {
            val expectedVal = r.get
            f(expectedVal, previous) match {
                case Update(newVal, result) =>
                    if (!r.compareAndSet(expectedVal, newVal))
                        tryUpdate(Some(result))
                    else
                        result
                case NoUpdate(result) =>
                    result
            }
        }

        tryUpdate(None)
    }

    /** Wrapper around [[java.lang.ThreadLocal]] that provides a scoped set method */
    trait ThreadLocal[A] {
        /** Initial value of the thread local */
        protected val initial: A

        private lazy val threadLocal = new java.lang.ThreadLocal[A] {
            override protected def initialValue = initial
        }

        /** Get the current value of the thread local */
        def get: A = threadLocal.get

        /** Set the current value of the thread local */
        def set(a: A): Unit = threadLocal.set(a)

        /** Reset the current value of the thread local to the initial one */
        def reset(): Unit = set(initial)

        /** Set the thread local to the given value for the scope of the function, setting it back when the function leaves */
        def doWith[B](a: A)(f: => B): B = {
            val oldValue = get
            try {
                set(a)
                f
            } finally {
                set(oldValue)
            }
        }
    }

    /** Wrapper around a [[java.util.concurrent.locks.Lock]] to make it nicer to use from scala */
    class LockWrapper[A <: JUCLock](val underlying: A) {
        def apply[B](f: => B): B = {
            underlying.lock()
            try {
                f
            } finally {
                underlying.unlock()
            }
        }

        def interruptibly[B](f: => B): B = {
            underlying.lockInterruptibly()
            try {
                f
            } finally {
                underlying.unlock()
            }
        }

        def attempt[B](f: => B): Option[B] =
            if (underlying.tryLock()) {
                try {
                    Some(f)
                } finally {
                    underlying.unlock()
                }
            } else None

        def attemptFor[B](time: Long, unit: TimeUnit)(f: => B): Option[B] =
            if (underlying.tryLock(time, unit)) {
                try {
                    Some(f)
                } finally {
                    underlying.unlock()
                }
            } else None

        def newCondition(): Condition =
            underlying.newCondition()
    }

    /** Type of a ReentrantLock wrapped by a LockWrapper */
    type Lock = LockWrapper[ReentrantLock]

    /** Create a ReentrantLock wrapped by LockWrapper */
    def lock(fair: Boolean = false): Lock = new LockWrapper(new ReentrantLock(fair))

    /** Wrapper around a [[java.util.concurrent.locks.ReadWriteLock]] to make it nicer to use from scala */
    class ReadWriteLockWrapper[A <: JUCReadWriteLock](val underlying: A) {
        lazy val forRead = new LockWrapper(underlying.readLock)
        lazy val read = forRead
        lazy val forWrite = new LockWrapper(underlying.writeLock)
        lazy val write = forWrite
    }

    /** Type of a `ReentrantReadWriteLock` wrapped by a `LockWrapper` */
    type ReadWriteLock = ReadWriteLockWrapper[ReentrantReadWriteLock]

    /** Create a `ReentrantReadWriteLock` wrapped by a `ReadWriteLockWrapper` */
    def readWriteLock(fair: Boolean = false): ReadWriteLock = new ReadWriteLockWrapper(new ReentrantReadWriteLock(fair))

    def readWriteLocked[A](initial: A): ReadWriteLocked[A] =
        new ReadWriteLocked(initial)

    /** Mutable reference which protects mutation with a read write lock */
    final class ReadWriteLocked[A](private var current: A) {
        val lock = readWriteLock()

        def read[B](f: A => B): B =
            lock.read {
                f(current)
            }

        def attemptRead[B](time: Long, unit: TimeUnit)(f: A => B): Option[B] =
            lock.read.attemptFor(time, unit) {
                f(current)
            }

        private def performWrite[B](f: (A, A => Unit) => B): B = {
            var enabled = true
            val set: A => Unit = { updated =>
                if (enabled) current = updated
                else sys.error("set retained outside of lock perimeter and used")
            }
            f(current, set)
        }

        def write[B](f: (A, A => Unit) => B): B =
            lock.write {
                performWrite(f)
            }

        def attemptWrite[B](time: Long, unit: TimeUnit)(f: (A, A => Unit) => B): Option[B] =
            lock.write.attemptFor(time, unit) {
                performWrite(f)
            }
    }
}
