//
// Copyright 2010-2012 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange

import java.lang.reflect.{Constructor, Method, Type}
import java.util.concurrent.locks.ReentrantReadWriteLock
import scala.collection.mutable.WeakHashMap
import scala.ref.WeakReference
import com.paytronix.utils.scala.concurrent.ReadWriteLockWrapper

object Cache {
    private val lock = new ReadWriteLockWrapper(new ReentrantReadWriteLock())

    private val cachedMethodCoders:      WeakHashMap[Method,         WeakReference[(ComposableCoder[Array[AnyRef]], ComposableCoder[_])]] = WeakHashMap.empty
    private val cachedConstructorCoders: WeakHashMap[Constructor[_], WeakReference[ComposableCoder[Array[AnyRef]]]]                       = WeakHashMap.empty
    private val cachedValueTypeCoders:   WeakHashMap[Type,           WeakReference[ComposableCoder[_]]]                                   = WeakHashMap.empty

    /** Reset the coding cache for tests */
    def reset =
        lock.forWrite {
            cachedMethodCoders.clear
            cachedConstructorCoders.clear
            cachedValueTypeCoders.clear
        }

    /** Cache a coder for later use */
    def store(ty: Type, coder: ComposableCoder[_]): Unit =
        lock.forWrite { cachedValueTypeCoders += ty -> new WeakReference(coder) }

    /** Cache a coder for later use */
    def store(meth: Method, coders: (ComposableCoder[Array[AnyRef]], ComposableCoder[_])): Unit =
        lock.forWrite { cachedMethodCoders += meth -> new WeakReference(coders) }

    /** Cache a coder for later use */
    def store(ctor: Constructor[_], coder: ComposableCoder[Array[AnyRef]]): Unit =
        lock.forWrite { cachedConstructorCoders += ctor -> new WeakReference(coder) }

    /** Find a coder either explicitly preregistered or cached for the given type */
    def find(ty: Type): Option[ComposableCoder[_]] =
        lock.forRead { cachedValueTypeCoders.get(ty).flatMap(_.get) }

    /** Find a coder cached for the given method */
    def find(meth: Method): Option[(ComposableCoder[Array[AnyRef]], ComposableCoder[_])] =
        lock.forRead { cachedMethodCoders.get(meth).flatMap(_.get) }

    /** Find a coder cached for the given constructor */
    def find(meth: Constructor[_]): Option[ComposableCoder[Array[AnyRef]]] =
        lock.forRead { cachedConstructorCoders.get(meth).flatMap(_.get) }
}
