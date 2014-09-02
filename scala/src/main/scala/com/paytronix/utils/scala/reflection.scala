//
// Copyright 2010-2014 Paytronix Systems, Inc.
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

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.{universe => runtimeUniverse}

import result.{Failed, Okay, Result}

package object reflection {
    /** Split a fully qualified class name into (package name, class name) */
    def splitFullyQualifiedName(in: String): (String, String) =
        in.indexOf('.') match {
            case -1 => ("", in)
            case pos => (in.substring(0, pos), in.substring(pos+1))
        }

    /** Given the encoded name of an object (including the trailing $), reflectively obtain an instance to it */
    def findObjectInstance(loader: ClassLoader, name: String): Result[AnyRef] =
        try {
            val runtimeMirror = runtimeUniverse.runtimeMirror(loader)
            Okay(runtimeMirror.reflectModule(runtimeMirror.staticModule(name)).instance.asInstanceOf[AnyRef])
        } catch { case e: Exception => Failed(e) }

    /** Look up a class by name, failing if the class is not found or does not conform at least to the given type */
    def classByName[A: ClassTag](classLoader: ClassLoader, name: String): Result[Class[A]] =
        try {
            Okay(Class.forName(name, true, classLoader).asSubclass(classTag[A].runtimeClass).asInstanceOf[Class[A]])
        } catch { case e: Exception => Failed(e) }

    /** Instantiate a Class with its no-arg constructor, capturing exceptions */
    def instantiate[A](clazz: Class[A]): Result[A] =
        try Okay(clazz.newInstance())
        catch { case e: Exception => Failed(e) }

    /** Instantiate a class by name with its no-arg constructor, capturing exceptions */
    def instantiate(classLoader: ClassLoader, name: String): Result[AnyRef] =
        classByName[AnyRef](classLoader, name).flatMap(instantiate(_))

    /**
     * Given the name of an object or class, try to find it as a scala object instance or failing that instantiate it.
     * Note that this is probably going to be confusing if used on enclosed objects, because it expects the encoded name of the object
     * (e.g. `foo.bar.Baz$$Zip`) but without the trailing $, since it will use the name without the $ for the instantiate case.
     */
    def findObjectInstanceOrInstantiate(loader: ClassLoader, name: String): Result[AnyRef] =
        findObjectInstance(loader, name + "$") orElse (_ match {
            case Failed(_: ClassNotFoundException|_: InstantiationException) => instantiate(loader, name)
            case f@Failed(_)                                                 => f
        })

    /**
     * Given the name of an object or class, try to instantiate it first or failing that locate it as an object instance.
     * The inverse order of operations from findObjectInstanceOrInstantiate, for when a class is preferred over an object of the same name.
     */
    def instantiateOrFindObjectInstance(loader: ClassLoader, name: String): Result[AnyRef] =
        instantiate(loader, name) orElse (_ match {
            // If there's an object with this name, we'll get an InstantiationException, not a ClassNotFoundException
            case Failed(_: ClassNotFoundException|_: InstantiationException) => findObjectInstance(loader, name + "$")
            case f@Failed(_)                                                 => f
        })
}
