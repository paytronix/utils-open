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
import java.util.concurrent.locks.{Condition, Lock, ReadWriteLock}

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

    /** Wrapper around a [[java.util.concurrent.Lock]] to make it nicer to use from scala */
    class LockWrapper[A <: Lock](val underlying: A) {
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

    /** Wrapper around a java ReadWriteLock to make it nicer to use from scala */
    class ReadWriteLockWrapper[A <: ReadWriteLock](val underlying: A) {
        lazy val forRead = new LockWrapper(underlying.readLock)
        lazy val read = forRead
        lazy val forWrite = new LockWrapper(underlying.writeLock)
        lazy val write = forWrite
    }
}

