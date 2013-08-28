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

package com.paytronix.utils.scala

import java.lang.reflect.{Constructor, Field, Method, Modifier, ParameterizedType, Type, TypeVariable, WildcardType}
import scala.collection.mutable.ListBuffer
import scala.reflect.NameTransformer
import com.thoughtworks.paranamer.{AdaptiveParanamer, AnnotationParanamer, BytecodeReadingParanamer, CachingParanamer}

import result.{Failed, Okay, Result, cast, iterableResultOps, tryCatch, tryCatching}

object reflection {
    val paranamer = new CachingParanamer(new AdaptiveParanamer(new AnnotationParanamer, new BytecodeReadingParanamer))

    /** Split a fully qualified class name into (package name, class name) */
    def splitFullyQualifiedName(in: String): (String, String) =
        in.indexOf('.') match {
            case -1 => ("", in)
            case pos => (in.substring(0, pos), in.substring(pos+1))
        }

    /** Given the encoded name of an object (including the trailing $), reflectively obtain an instance to it */
    def findObjectInstance(loader: ClassLoader, name: String): Result[AnyRef] = {
        for {
            clazz <- classByName(loader, name)
            field <- tryCatch.value(clazz.getField("MODULE$")) | (name + " is not a Scala object singleton (no MODULE$ field)")
            _ <- if (Modifier.isStatic(field.getModifiers)) Okay(()) else Failed(name + " is not a Scala object singleton (MODULE$ is not static)")
            inst <- tryCatch.value(field.get(null))
        } yield inst
    }

    /** Look up a class by name, failing if the class is not found or does not conform at least to the given type */
    def classByName[A](classLoader: ClassLoader, name: String)(implicit m: Manifest[A]): Result[Class[A]] =
        tryCatch.value(Class.forName(name, true, classLoader).asSubclass(m.erasure).asInstanceOf[Class[A]])

    /** Instantiate a Class with its no-arg constructor, capturing exceptions */
    def instantiate[A](clazz: Class[A]): Result[A] =
        tryCatch.value(clazz.newInstance())

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

    /**
     * Reduce a type towards a concrete Class.
     *
     * For inputs that are:
     *  - a Class, just reduce to that
     *  - a ParameterizedType, just reduce to that
     *  - a type variable, try and reduce to a single upper bound (recursing if necessary). Fails with multiple bounds.
     *  - a wildcard type, reduce to the upper bound
     *  - a generic array, fail (don't know what to do about this yet)
     */
    def reduceType(ty: Type): Result[Type] = ty match {
        case c: Class[_]                                       => Okay(c)
        case pt: ParameterizedType                             => Okay(pt)
        case tv: TypeVariable[_] if tv.getBounds.length == 1   => reduceType(tv.getBounds()(0))
        case tv: TypeVariable[_]                               => Failed("Cannot reduce " + tv + " -- confusing bounds")
        case wt: WildcardType if wt.getUpperBounds.length == 1 => reduceType(wt.getUpperBounds()(0))
        case wt: WildcardType                                  => Failed("Cannot reduce " + wt + " -- confusing upper bound")

        case _ => Failed("Can't reduce " + ty + " (sorry, too dumb)")
    }

    /**
     * Given a generic parameterized type, return the upper bounds or concrete types of each argument.
     *
     * That is, for some type Ty[A <: Foo, Bar] return Foo::AnyRef::Nil.
     *
     * Note that for specific variables the compiler will refine the upper bound, so for example even though the type of
     * List is List[A <: Any], a particular list val l: List[String] will be compiled as List[A <: String] and therefore this
     * function will result in String::Nil for the type of that field.
     */
    def getTypeArguments(ty: Type): Result[Seq[Type]] =
        for {
            pt <- cast[ParameterizedType](ty) | Failed(ty + " not a ParameterizedType")
            args <- wrapRefArray(pt.getActualTypeArguments).mapResult(reduceType)
        } yield args

    /** Return a List of all ancestor classes of the given class, including Object (Any/AnyRef) */
    def ancestors(clazz: Class[_]): List[Class[_]] = {
        val buffer = new ListBuffer[Class[_]]
        var cur = clazz.getSuperclass
        while (cur != null) {
            buffer += cur
            cur = cur.getSuperclass
        }
        buffer.toList
    }

    /** Return a List containing the given class and all its ancestors */
    def classAndAncestors(clazz: Class[_]): List[Class[_]] =
        clazz :: ancestors(clazz)

    val AnyRefType  = classOf[AnyRef]
    val UnitType    = classOf[Unit]
    val BooleanType = classOf[Boolean]
    val ByteType    = classOf[Byte]
    val ShortType   = classOf[Short]
    val CharType    = classOf[Char]
    val IntType     = classOf[Int]
    val LongType    = classOf[Long]
    val FloatType   = classOf[Float]
    val DoubleType  = classOf[Double]

    /**
     * Return true iff the first type is assignable from the second type as clazz1.isAssignableFrom(clazz2) but also consider
     * primitive types to be <: AnyRef (java.lang.Object) since from a reflection standpoint they are.
     */
    def isAssignable(a: Class[_], b: Class[_]): Boolean =
        a.isAssignableFrom(b) || ((a, b) match {
            case (AnyRefType, UnitType|BooleanType|ByteType|CharType|ShortType|IntType|LongType|FloatType|DoubleType) => true
            case _ => false
        })

    /** Return true iff the second given method could override the first */
    def canOverride(a: Method, b: Method): Boolean =
        (a.getName == b.getName && a.getParameterTypes.length == b.getParameterTypes.length &&
         isAssignable(a.getReturnType, b.getReturnType) &&
         (a.getParameterTypes zip b.getParameterTypes).foldLeft(true)((prev, cur) => prev && isAssignable(cur._2, cur._1)))

    /** Return true iff the second given constructor could override the first */
    def canOverride(a: Constructor[_], b: Constructor[_]): Boolean =
        (a.getParameterTypes.length == b.getParameterTypes.length &&
         (a.getParameterTypes zip b.getParameterTypes).foldLeft(true)((prev, cur) => prev && isAssignable(cur._2, cur._1)))

    /** Return true iff all the given methods are compatible; that is, there exists some ordering of the methods where each overrides the other in turn */
    def areMethodsCompatible(in: Iterable[Method]): Boolean =
        in.size <= 1 || !in.exists(a => !in.exists(b => a != b && (canOverride(a,b) || canOverride(b,a))))

    /** Return true iff all the given constructors are compatible; that is, there exists some ordering of the constructors where each overrides the other in turn */
    def areConstructorsCompatible(in: Iterable[Constructor[_]]): Boolean =
        in.size <= 1 || !in.exists(a => !in.exists(b => a != b && (canOverride(a,b) || canOverride(b,a))))

    /** Return the most specific override of a series of compatible methods. If the methods are not compatible, the result is undefined. */
    def mostSpecificMethod(in: Seq[Method]): Option[Method] =
        in.foldLeft[Option[Method]](None)((prev, cur) => if (prev.map(canOverride(_, cur)) getOrElse true) Some(cur) else prev)

    /** Return the most specific override of a series of compatible constructors. If the constructors are not compatible, the result is undefined. */
    def mostSpecificConstructor(in: Seq[Constructor[_]]): Option[Constructor[_]] =
        in.foldLeft[Option[Constructor[_]]](None)((prev, cur) => if (prev.map(canOverride(_, cur)) getOrElse true) Some(cur) else prev)
}
