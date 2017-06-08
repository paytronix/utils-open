//
// Copyright 2014 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange.base.container

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import com.paytronix.utils.interchange.base.{InsecureContext, InterchangeClassLoader}
import com.paytronix.utils.scala.reflection.classByName
import com.paytronix.utils.scala.result.{Okay, Result, tryCatchValue, tryCatchingValue}

import scala.collection.mutable.ListBuffer

object result {
    def instantiateThrowable (
        className: String, message: String, causeOpt: Option[Throwable]
    )(implicit interchangeClassLoader: InterchangeClassLoader): Result[Throwable] =
        for {
            clazz <- if (InsecureContext.get) Okay(classOf[RuntimeException])
                     else classByName[Throwable](interchangeClassLoader.classLoader, className)
            inst <- causeOpt match {
                case Some(cause) =>
                    (tryCatchingValue(classOf[NoSuchMethodException])(clazz.getConstructor(classOf[String], classOf[Throwable]))
                        | ("throwable class " + className + " does not have (String, Throwable) constructor")
                        >>= { ctor => tryCatchValue(ctor.newInstance(message, cause)) })

                case None =>
                    ( (tryCatchingValue(classOf[NoSuchMethodException])(clazz.getConstructor(classOf[String]))
                        | ("throwable class " + className + " does not have a (String) constructor")
                        >>= { ctor => tryCatchValue(ctor.newInstance(message)) })
                    | (tryCatchingValue(classOf[NoSuchMethodException])(clazz.getConstructor(classOf[String], classOf[Throwable]))
                        | ("throwable class " + className + " does not have a (String, Throwable) constructor")
                        >>= { ctor => tryCatchValue(ctor.newInstance(message, null)) })
                    )
            }
        } yield inst
}

object javaCollections {
    /** `CanBuildFrom` for `java.util.List` */
    def canBuildJavaList[E] = new CanBuildFrom[Nothing, E, java.util.List[E]] {
        def apply() = new mutable.Builder[E, java.util.List[E]] {
            val jl = new java.util.ArrayList[E]
            def clear() = jl.clear()
            def result() = jl
            def += (e: E) = { jl.add(e); this }
        }

        def apply(from: Nothing) = apply()
    }

    /** `CanBuildFrom` for `java.util.Set` */
     def canBuildJavaSet[E] = new CanBuildFrom[Nothing, E, java.util.Set[E]] {
        def apply() = new mutable.Builder[E, java.util.Set[E]] {
            val js = new java.util.HashSet[E]
            def clear() = js.clear()
            def result() = js
            def += (e: E) = { js.add(e); this }
        }

        def apply(from: Nothing) = apply()
    }

    /** `CanBuildFrom` for `java.util.Map` */
    def canBuildJavaMap[K, V] = new CanBuildFrom[Nothing, (K, V), java.util.Map[K, V]] {
        def apply() = new mutable.Builder[(K, V), java.util.Map[K, V]] {
            val jm = new java.util.HashMap[K, V]
            def clear() = jm.clear()
            def result() = jm
            def += (p: (K, V)) = { jm.put(p._1, p._2); this }
        }

        def apply(from: Nothing) = apply()
    }

    /** `CanBuildFrom` for `java.util.SortedMap` */
    def canBuildJavaSortedMap[K <: Comparable[K], V] = new CanBuildFrom[Nothing, (K, V), java.util.SortedMap[K, V]] {
        def apply() = new mutable.Builder[(K, V), java.util.SortedMap[K, V]] {
            val jm = new java.util.TreeMap[K, V]
            def clear() = jm.clear()
            def result() = jm
            def += (p: (K, V)) = { jm.put(p._1, p._2); this }
        }

        def apply(from: Nothing) = apply()
    }
}

object scalaIterable {
    /** `CanBuildFrom` for `Scala List` */
    def canBuildScalaList[E] = new CanBuildFrom[Nothing, E, List[E]] {
        def apply() = new mutable.Builder[E, List[E]] {
            val sl = new ListBuffer[E]
            def clear() = sl.clear()
            def result() = sl.result()
            def += (e: E) = { sl += e; this }
        }

        def apply(from: Nothing) = apply()
    }
}
