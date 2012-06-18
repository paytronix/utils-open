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

import java.lang.reflect.{Constructor, Method}
import java.util.concurrent.atomic.AtomicReference
import scala.reflect.Manifest
import scala.tools.scalap.scalax.rules.scalasig.{SingleType, TypeRefType}
import com.mongodb.DBObject
import net.liftweb.json.JsonAST.{JField, JObject, JValue, render}
import net.liftweb.json.Printer.{compact, pretty}
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.bson.types.ObjectId
import org.joda.time.{DateTime, Duration, LocalDate, LocalDateTime, LocalTime}
import org.slf4j.{Logger, LoggerFactory}
import com.paytronix.utils.extendedreflection.{Builder, ClassTypeR, ClassTypeRFor, NameR, ParameterizedTypeRWith, TypeR}
import com.paytronix.utils.scala.concurrent.atomicUpdate
import com.paytronix.utils.scala.reflection.{classAndAncestors, findObjectInstance, findObjectInstanceOrInstantiate, getTypeArguments}
import com.paytronix.utils.scala.result.{Failed, Okay, Result, ResultG, iterableResultOps, optionOps}

import ResultG.resultAsReplacement

/** Front end to coding, with Java friendly signatures */
object JSONCoding {
    val logger = LoggerFactory.getLogger(getClass)

    def forClass[T](clazz: Class[T]): Coder[T] = {
        implicit val context: Builder = new Builder(clazz.getClassLoader);

        {
            for {
                typeR <- context.typeRFor(clazz)
                coder <- Coding.forType(clazz.getClassLoader, typeR)
            } yield coder.asCoderFor[T]
        }.orThrow
    }

    def decode[T](clazz: Class[T], in: JValue): T = {
        implicit val context: Builder = new Builder(clazz.getClassLoader);

        {
            for {
                typeR  <- context.typeRFor(clazz)
                coder  <- Coding.forType(clazz.getClassLoader, typeR)
                result <- coder.asCoderFor[T].decode(in)
            } yield result
        }.orThrow
    }

    def encode[T](clazz: Class[T], in: T): JValue = {
        implicit val context: Builder = new Builder(clazz.getClassLoader);

        {
            for {
                typeR <- context.typeRFor(clazz)
                coder <- Coding.forType(clazz.getClassLoader, typeR)
                result <- coder.asCoderFor[T].encode(in)
            } yield result
        }.orThrow
    }
}

/** Front end to the coding discovery process, automatically using and filling the cache with coders discovered using the registry or reflection */
object Coding {
    private val registeredCoders: AtomicReference[Map[Class[_], GenericRegistration]] = new AtomicReference(Map.empty)

    trait GenericRegistration {
        protected val description: String
        protected def makeCoder(coders: List[ComposableCoder[_]]): Result[ComposableCoder[_]]

        def apply(contextClassLoader: ClassLoader, in: List[TypeR])(implicit builder: Builder): Result[ComposableCoder[_]] =
            in.zipWithIndex mapResult {
                case (argTyR, i) =>
                    Coding.forTypeComposable(contextClassLoader, argTyR) | (description + ": failed to determine coder for type parameter " + i + " - " + argTyR)
            } map { _.toList } flatMap makeCoder
    }

    final case class registration(description: String, f: () => ComposableCoder[_]) extends GenericRegistration {
        def makeCoder(in: List[ComposableCoder[_]]): Result[ComposableCoder[_]] = in match {
            case Nil => Okay(f())
            case _ => Failed(description + ": expected to have no type parameters (instead got: " + in.mkString(", ") + ")")
        }
    }

    final case class oneArgRegistration(description: String, f: ComposableCoder[_] => ComposableCoder[_]) extends GenericRegistration {
        def makeCoder(in: List[ComposableCoder[_]]) = in match {
            case c1 :: Nil => Okay(f(c1))
            case _ => Failed(description + ": expected to have exactly one type parameter (instead got: " + in.mkString(", ") + ")")
        }
    }

    final case class twoArgRegistration(description: String, f: (ComposableCoder[_], ComposableCoder[_]) => ComposableCoder[_]) extends GenericRegistration {
        def makeCoder(in: List[ComposableCoder[_]]) = in match {
            case c1 :: c2 :: Nil => Okay(f(c1, c2))
            case _ => Failed(description + ": expected to have exactly two type parameters (instead got: " + in.mkString(", ") + ")")
        }
    }

    final case class threeArgRegistration(description: String, f: (ComposableCoder[_], ComposableCoder[_], ComposableCoder[_]) => ComposableCoder[_]) extends GenericRegistration {
        def makeCoder(in: List[ComposableCoder[_]]) = in match {
            case c1 :: c2 :: c3 :: Nil => Okay(f(c1, c2, c3))
            case _ => Failed(description + ": expected to have exactly three type parameters (instead got: " + in.mkString(", ") + ")")
        }
    }

    final case class fourArgRegistration(description: String, f: (ComposableCoder[_], ComposableCoder[_], ComposableCoder[_], ComposableCoder[_]) => ComposableCoder[_]) extends GenericRegistration {
        def makeCoder(in: List[ComposableCoder[_]]) = in match {
            case c1 :: c2 :: c3 :: c4 :: Nil => Okay(f(c1, c2, c3, c4))
            case _ => Failed(description + ": expected to have exactly four type parameters (instead got: " + in.mkString(", ") + ")")
        }
    }

    final case class fiveArgRegistration(description: String, f: (ComposableCoder[_], ComposableCoder[_], ComposableCoder[_], ComposableCoder[_], ComposableCoder[_]) => ComposableCoder[_]) extends GenericRegistration {
        def makeCoder(in: List[ComposableCoder[_]]) = in match {
            case c1 :: c2 :: c3 :: c4 :: c5 :: Nil => Okay(f(c1, c2, c3, c4, c5))
            case _ => Failed(description + ": expected to have exactly five type parameters (instead got: " + in.mkString(", ") + ")")
        }
    }

    final case class sixArgRegistration(description: String, f: (ComposableCoder[_], ComposableCoder[_], ComposableCoder[_], ComposableCoder[_], ComposableCoder[_], ComposableCoder[_]) => ComposableCoder[_]) extends GenericRegistration {
        def makeCoder(in: List[ComposableCoder[_]]) = in match {
            case c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: Nil => Okay(f(c1, c2, c3, c4, c5, c6))
            case _ => Failed(description + ": expected to have exactly six type parameters (instead got: " + in.mkString(", ") + ")")
        }
    }

    /** Reset the coding for tests. Clears both cache and explicit registry */
    def reset = {
        Cache.reset
        registeredCoders.set(Map(
            (classOf[Unit],                    registration("Unit",                        () => UnitCoder)),
            (classOf[scala.runtime.BoxedUnit], registration("Unit",                        () => UnitCoder)),
            (classOf[java.math.BigDecimal],    registration("java.math.BigDecimal",        () => JavaBigDecimalCoder)),
            (classOf[scala.math.BigDecimal],   registration("scala.math.BigDecimal",       () => ScalaBigDecimalCoder)),
            (classOf[BigInt],                  registration("scala.math.BigInt",           () => BigIntCoder)),
            (classOf[java.math.BigInteger],    registration("java.math.BigInteger",        () => BigIntegerCoder)),
            (classOf[java.lang.Boolean],       registration("java.lang.Boolean",           () => BooleanCoder)),
            (classOf[Boolean],                 registration("Boolean",                     () => BooleanCoder)),
            (classOf[java.lang.Byte],          registration("java.lang.Byte",              () => ByteCoder)),
            (classOf[Byte],                    registration("Byte",                        () => ByteCoder)),
            (classOf[java.lang.Character],     registration("java.lang.Character",         () => CharCoder)),
            (classOf[Char],                    registration("Char",                        () => CharCoder)),
            (classOf[java.lang.Double],        registration("java.lang.Double",            () => DoubleCoder)),
            (classOf[Double],                  registration("Double",                      () => DoubleCoder)),
            (classOf[java.lang.Float],         registration("java.lang.Float",             () => FloatCoder)),
            (classOf[Float],                   registration("Float",                       () => FloatCoder)),
            (classOf[java.lang.Integer],       registration("java.lang.Integer",           () => IntCoder)),
            (classOf[Int],                     registration("Int",                         () => IntCoder)),
            (classOf[JValue],                  registration("JValue",                      () => JValueCoder)),
            (classOf[java.lang.Long],          registration("java.lang.Long",              () => LongCoder)),
            (classOf[Long],                    registration("Long",                        () => LongCoder)),
            (classOf[java.lang.Short],         registration("java.lang.Short",             () => ShortCoder)),
            (classOf[Short],                   registration("Short",                       () => ShortCoder)),
            (classOf[String],                  registration("String",                      () => StringCoder)),
            (classOf[java.sql.Date],           registration("java.sql.Date",               () => JavaSqlDateCoder)),
            (classOf[java.sql.Timestamp],      registration("java.sql.Timestamp",          () => JavaSqlTimestampCoder)),
            (classOf[java.util.Date],          registration("java.util.Date",              () => JavaDateCoder)),
            (classOf[DateTime],                registration("org.joda.time.DateTime",      () => DateTimeCoder)),
            (classOf[LocalDate],               registration("org.joda.time.LocalDate",     () => LocalDateCoder)),
            (classOf[LocalTime],               registration("org.joda.time.LocalTime",     () => LocalTimeCoder)),
            (classOf[LocalDateTime],           registration("org.joda.time.LocalDateTime", () => LocalDateTimeCoder)),
            (classOf[Duration],                registration("org.joda.time.Duration",      () => DurationCoder)),

            (classOf[JValue],   registration("net.liftweb.json.JsonAST.JValue", () => JValueCoder)),
            (classOf[ObjectId], registration("org.bson.types.ObjectId",         () => ObjectIdCoder)),
            (classOf[DBObject], registration("com.mongodb.DBObject",            () => DBObjectCoder)),

            (classOf[java.util.List[_]], oneArgRegistration("java.util.List", JavaListCoder(_))),
            (classOf[Option[_]],         oneArgRegistration("Option",         OptionCoder(_))),
            (classOf[List[_]],           oneArgRegistration("scala.List",     ScalaListCoder(_))),

            (classOf[Either[_, _]],  twoArgRegistration("Either", EitherCoder(_, _))),
            (classOf[ResultG[_, _]], twoArgRegistration("ResultG", ResultCoder(_, _))),

            (classOf[java.util.Map[_, _]],                  twoArgRegistration("java.util.Map", JavaMapCoder(_,_))),
            (classOf[scala.collection.immutable.Map[_, _]], twoArgRegistration("scala.collection.immutable.Map", ScalaImmutableMapCoder(_,_))),
            (classOf[scala.collection.mutable.Map[_, _]],   twoArgRegistration("scala.collection.mutable.Map", ScalaMutableMapCoder(_,_))),

            (classOf[scala.collection.immutable.Set[_]], oneArgRegistration("scala.collection.immutable.Set", ScalaImmutableSetCoder(_))),
            (classOf[scala.collection.mutable.Set[_]],   oneArgRegistration("scala.collection.mutable.Set", ScalaMutableSetCoder(_))),

            // fallback Seq coder which codes as ArrayBuffer
            (classOf[Seq[_]], oneArgRegistration("scala.Seq", ScalaSeqCoder(_))),

            (classOf[Tuple1[_]],           oneArgRegistration("Tuple1", Tuple1Coder(_))),
            (classOf[Tuple2[_,_]],         twoArgRegistration("Tuple2", Tuple2Coder(_,_))),
            (classOf[Tuple3[_,_,_]],       threeArgRegistration("Tuple3", Tuple3Coder(_,_,_))),
            (classOf[Tuple4[_,_,_,_]],     fourArgRegistration("Tuple4", Tuple4Coder(_,_,_,_))),
            (classOf[Tuple5[_,_,_,_,_]],   fiveArgRegistration("Tuple5", Tuple5Coder(_,_,_,_,_))),
            (classOf[Tuple6[_,_,_,_,_,_]], sixArgRegistration("Tuple6", Tuple6Coder(_,_,_,_,_,_)))
        ))
    }

    reset

    /** Register a function that generates a coder for the given type */
    def register(clazz: Class[_], reg: GenericRegistration): Unit =
        atomicUpdate(registeredCoders)(_ + (clazz -> reg))

    /** Look up coders for a given method, first looking in the cache and failing back to reflection if needed */
    def forMethodComposable(classLoader: ClassLoader, meth: Method)(implicit builder: Builder): Result[(ComposableCoder[Array[AnyRef]], ComposableCoder[_])] =
        Cache.find(meth).toResult orElse {
            Reflection.reflect(classLoader, meth) pass { _ foreach { Cache.store(meth, _) } }
        }

    /** Look up coders for a given method, first looking in the cache and failing back to reflection if needed */
    def forMethod(classLoader: ClassLoader, meth: Method)(implicit builder: Builder): Result[(Coder[Array[AnyRef]], Coder[_])] =
        forMethodComposable(classLoader, meth).map {
            case (argCoder, resultCoder) => (Coder(classLoader, argCoder), Coder(classLoader, resultCoder))
        }

    /** Look up coders for a given constructor, first looking in the cache and failing back to reflection if needed */
    def forConstructorComposable(classLoader: ClassLoader, ctor: Constructor[_])(implicit builder: Builder): Result[ComposableCoder[Array[AnyRef]]] =
        Cache.find(ctor).toResult orElse {
            Reflection.reflect(classLoader, ctor) pass { _ foreach { Cache.store(ctor, _) } }
        }

    /** Look up coders for a given constructor, first looking in the cache and failing back to reflection if needed */
    def forConstructor(classLoader: ClassLoader, ctor: Constructor[_])(implicit builder: Builder): Result[Coder[Array[AnyRef]]] =
        forConstructorComposable(classLoader, ctor).map(Coder(classLoader, _))

    private val EnumClass = classOf[Enum[_]]
    private val EnumerationValueClass = classOf[Enumeration#Value]

    /** Generate some kind of coder by introspecting a value type */
    def forTypeComposable(classLoader: ClassLoader, typeR: TypeR)(implicit builder: Builder): Result[ComposableCoder[_]] = {
        /** Extractor that goes to find a sidecar for a ClassTypeR and yields Some(ResultOfSidecar) if it finds it */
        object ClassTypeWithSidecar {
            def unapply(in: TypeR): Option[Result[Coding]] =
                in match {
                    case ClassTypeRFor(c) =>
                        findObjectInstanceOrInstantiate(classLoader, c.getName + "Coding") match {
                            case ok: Okay[_] => Some(ok.withFailedType[Unit].asA[Coding] | ("sidecar found for " + c.getName + " but it was not a subtype of Coding"))
                            case _ => None
                        }

                    case _ => None
                }
        }

        /** Extractor that finds ClassTypeRs in the cache */
        object ClassTypeAlreadyCached {
            def unapply(in: TypeR): Option[ComposableCoder[_]] =
                in match {
                    case ClassTypeRFor(c) => Cache.find(c)
                    case _ => None
                }
        }

        def coderForSingleton(c: Class[_]): Result[ComposableCoder[_]] =
            for {
                classR <- builder.classRFor(c)
                csym <- classR.signatureModel if csym.isModule
                inst <- findObjectInstance(classLoader, classR.name.qualified + "$")
                coder <- Reflection.reflectSingletonCoder(classLoader, inst)
            } yield coder

        lazy val coderUsingRegistryOrReflection = typeR match {
            case ClassTypeRFor(c) => registeredCoders.get.get(c) match {
                case Some(f) => f(classLoader, Nil)
                case None => coderForSingleton(c) | resultAsReplacement(Reflection.reflectObjectCoder(classLoader, typeR.asInstanceOf[ClassTypeR]))
            }

            case ParameterizedTypeRWith(ClassTypeRFor(c), params) =>
                registeredCoders.get.get(c) match {
                    case Some(f) => f(classLoader, params)
                    case None => Failed(typeR + " is not a ClassTypeR so cannot be reflected, and there was no registration for it")
                }

            case _ => Failed(typeR + " is neither a ClassTypeR nor a ParameterizedTypeR, and so no coder can be obtained for it")
        }

        typeR match {
            case ClassTypeAlreadyCached(coder) =>
                Okay(coder)

            case ClassTypeWithSidecar(sidecarResult) =>
                sidecarResult.flatMap(_.makeCoder(coderUsingRegistryOrReflection, typeR.asInstanceOf[ClassTypeR]))

            // Special magic support for enums, due to their special magic

            case ClassTypeRFor(c) if EnumClass.isAssignableFrom(c) =>
                val coder = JavaEnumCoder(c.asInstanceOf[Class[FictitiousEnum]])
                Cache.store(c, coder)
                Okay(coder)

            case ClassTypeRFor(EnumerationValueClass) =>
                typeR.signatureModel match {
                    case Okay(TypeRefType(SingleType(_, enumSym), _, _)) =>
                        for (enumInst <- findObjectInstance(classLoader, NameR(enumSym).copy(isCompanion=true).encoded))
                        yield ScalaEnumCoder(enumInst.asInstanceOf[Enumeration])

                    case Okay(other) =>
                        Failed("have signature type " + other + " for enumeration value, but it is not a TypeRefType(SingleType(...), ...) as expected")

                    case _ =>
                        Failed("cannot reflect Scala enumeration because the scala signature information is not present")
                }

            case _ =>
                coderUsingRegistryOrReflection.pass(_.foreach(Cache.store(typeR.reflectionModel, _)))
        }
    }

    /** Generate some kind of coder by introspecting a value type */
    def forType(classLoader: ClassLoader, typeR: TypeR): Result[Coder[_]] = {
        implicit val builder = new Builder(classLoader)
        forTypeComposable(classLoader, typeR).map(Coder(classLoader, _))
    }

    /** Same as forTypeComposable, but downcasts the resulting coder for you */
    def forClassComposable[T](implicit m: Manifest[T], builder: Builder): Result[ComposableCoder[T]] =
        builder.typeRFor(m.erasure).flatMap(forTypeComposable(m.erasure.getClassLoader, _)).map(_.asCoderFor[T])

    /** Same as forType, but downcasts the resulting coder for you */
    def forClass[T](implicit m: Manifest[T]): Result[Coder[T]] = {
        implicit val builder = new Builder(m.erasure.getClassLoader)
        forClassComposable[T].map(c => Coder(m.erasure.getClassLoader, c))
    }

    /** Find a coder (using forType) and decode using it */
    def decode[T](in: JValue)(implicit m: Manifest[T]): Result[T] =
        if (m.erasure == classOf[Nothing] || m.erasure == classOf[AnyRef]) {
            Failed("Cannot decode Nothing or AnyRef. You probably need to give a type parameter argument to decode, because the inferencer did not " +
                    "correctly fill it out")
        } else {
            forClass[T].flatMap(_.decode(in))
        }

    /** Find a coder (using forType) and encode using it */
    def encode[T](in: T)(implicit m: Manifest[T]): Result[JValue] = {
        forClass[T].flatMap(_.encode(in))
    }

    /** Helper to implement a toString method by making a JSON string out of an object */
    def toString[T](in: T, verbose: Boolean = false)(implicit m: Manifest[T]): String =
        Coding.encode(in).map(json => if (verbose) pretty(render(json)) else compact(render(json))) match {
            case Okay(json) => json
            case failed => "<failed to convert to string: " + failed.toString + ">"
        }

    /** Extend a JValue with methods to extract single field values from a JObject easily */
    implicit def jvalueFieldOps(in: JValue): JValueFieldOps = JValueFieldOps(in)
}

/** Extension of JValue with methods to extract single field values easily */
final case class JValueFieldOps(jvalue: JValue) {
    /** Fetch a field value, using a manifest to determine what type to extract */
    def get[T](field: String)(implicit m: Manifest[T]): Result[T] =
        if (m.erasure == classOf[Nothing] || m.erasure == classOf[AnyRef]) {
            Failed("Cannot extract field of type Nothing or AnyRef. You probably need to give a type parameter argument to decode, because the inferencer did not " +
                    "correctly fill it out")
        } else {
            implicit val builder = new Builder(m.erasure.getClassLoader)
            Coding.forClassComposable[T] flatMap { get(field, m.erasure.getClassLoader, _) }
        }

    /** Fetch a field value using an explicit coder */
    def get[T](field: String, classLoader: ClassLoader, composableCoder: ComposableCoder[T]): Result[T] =
        ComposableCoder.formatFailedPath(FieldCoder(field, composableCoder).decode(classLoader, jvalue))
}

/** Trait of objects that provide explicit coders for types */
trait Coding {
    def makeCoder(default: => Result[ComposableCoder[_]], classTypeR: ClassTypeR)(implicit builder: Builder): Result[ComposableCoder[_]]
}

/**
 * Refinement of Coding that makes modifying ObjectCoders easier.
 *
 * Usage:
 *
 *     object BlahCoding extends ModifyObjectCoding {
 *         modify {
 *             case FieldCoding("fieldToHide", _, _, _) => None
 *         }
 *     }
 */
trait ModifyObjectCoding extends Coding {
    private var modification: Option[PartialFunction[FieldCoding, Option[FieldCoding]]] = None

    protected var flatten: Boolean = false

    protected def modify(pf: PartialFunction[FieldCoding, Option[FieldCoding]]): Unit = modification = Some(pf)

    def makeCoder(default: => Result[ComposableCoder[_]], classTypeR: ClassTypeR)(implicit builder: Builder) =
        for {
            coder <- Reflection.reflectObjectCoder(classTypeR.reflectionModel.getClassLoader, classTypeR): Result[ObjectCoder[_]]
            modifier <- modification.toResult
        } yield coder.modify(flatten, modifier)
}

/**
 * Coding that generates automatic UnionCoders automatically.
 *
 * Usage:
 *
 *     sealed abstract class Base
 *     case class First(a: Int) extends Base
 *     case class Second(b: String) extends Base
 *     object BaseCoding extends AutomaticUnionCoding[Base] {
 *         alternative[First]
 *         alternative[Second]
 *     }
 */
trait AutomaticUnionCoding[T] extends Coding {
    private var alternatives: List[Class[_ <: T]] = Nil
    private var topType: Option[Manifest[T]] = None

    protected var flatten: Boolean = false
    protected val noApplicableAlternative: String

    protected def alternative[U <: T](implicit m: Manifest[U], n: Manifest[T]) = {
        alternatives ::= m.erasure.asInstanceOf[Class[U]] // presume that the erasure is type compatible with the type parameter
        if (!topType.isDefined) topType = Some(n)
    }

    def makeCoder(default: => Result[ComposableCoder[_]], classTypeR: ClassTypeR)(implicit builder: Builder) =
        alternatives.mapResult { case clazz: Class[u] =>
            builder.typeRFor(clazz)
                .flatMap { Coding.forTypeComposable(clazz.getClassLoader, _) }
                .map { _.asCoderFor[u] }
                : Result[ComposableCoder[_ <: T]]
        } map { coders => AutomaticUnionCoder.apply[T](flatten, noApplicableAlternative, coders.toList)(topType.get) }
}

/**
 * Coding that generates explicit UnionCoders automatically.
 *
 * Usage:
 *
 *     sealed abstract class Base
 *     case class First(a: Int) extends Base
 *     case class Second(b: String) extends Base
 *     object BaseCoding extends ExplicitUnionCoding[Base] {
 *         alternative[First]("first")
 *         alternative[Second]("second")
 *     }
 */
trait ExplicitUnionCoding[T] extends Coding {
    private var alternatives: List[(String, Class[_ <: T])] = Nil
    private var topType: Option[Manifest[T]] = None

    protected var flatten: Boolean = false
    protected val determinantField: String = "_type"

    protected def alternative[U <: T](determinantValue: String)(implicit m: Manifest[U], n: Manifest[T]) = {
        alternatives ::= ((determinantValue, m.erasure.asInstanceOf[Class[U]]))
        if (!topType.isDefined) topType = Some(n)
    }

    def makeCoder(default: => Result[ComposableCoder[_]], classTypeR: ClassTypeR)(implicit builder: Builder) =
        alternatives.mapResult { case (determinantValue, clazz: Class[u]) =>
            builder.typeRFor(clazz)
                .flatMap { Coding.forTypeComposable(clazz.getClassLoader, _) }
                .map { coder => ExplicitUnionAlternative(determinantValue, clazz, coder.asCoderFor[u]) }
                : Result[ExplicitUnionAlternative[_ <: T]]
        } map { members => ExplicitUnionCoder[T](flatten, determinantField, members.toList)(topType.get) }
}

/** Coding that generates an InsecureCoder around a reflected coder */
trait InsecureCoding extends Coding {
    val substitute: Result[Any]

    def makeCoder(defaultCoder: => Result[ComposableCoder[_]], classTypeR: ClassTypeR)(implicit builder: Builder) =
        defaultCoder.map {
            case (coder: ComposableCoder[u]) => InsecureCoder(coder, substitute.asInstanceOf[Result[u]])
        }
}

/** Coding that always returns some coder */
class OverrideCoding(coder: ComposableCoder[_]) extends Coding {
    def makeCoder(default: => Result[ComposableCoder[_]], classTypeR: ClassTypeR)(implicit builder: Builder) = Okay(coder)
}

/**
 * Coding for a wrapper with a single field
 *
 * Usage:
 *
 *     case class MyWrapper(field: Map[String, Int])
 *     object MyWrapperCoding extends WrapperCoding
 *
 * In this case the coding of MyWrapper(Map("one" -> 1, "two" -> 2))
 *   would be { "one": 1, "two": 2 }
 *   instead of { "field": { "one": 1, "two": 2 }
 */
class WrapperCoding extends Coding {
    def makeCoder(default: => Result[ComposableCoder[_]], classTypeR: ClassTypeR)(implicit builder: Builder) =
        default.flatMap(WrapperCoding.make)
}

object WrapperCoding {
    def make(in: ComposableCoder[_]): Result[ComposableCoder[_]] =
        in match {
            case coder: ObjectCoder[t] if coder.fieldCodings.size == 1 =>
                Okay(new ComposableCoder[t] {
                    val mostSpecificClass = coder.mostSpecificClass.asInstanceOf[Class[t]]

                    def decode(classLoader: ClassLoader, in: JValue) =
                        coder.decode(classLoader, JObject(JField(coder.fieldCodings.head.name, in) :: Nil)) // bit of a cheap trick
                    def encode(classLoader: ClassLoader, in: t) =
                        coder.encode(classLoader, in) match {
                            case Okay(JObject(JField(_, jv) :: Nil)) => Okay(jv)
                            case other => other
                        }

                    val avroSchema = coder.avroSchema

                    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
                        coder.decodeAvro(classLoader, in)

                    def encodeAvro(classLoader: ClassLoader, in: t, out: Encoder) =
                        coder.encodeAvro(classLoader, in, out)

                    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
                        coder.decodeMongoDB(classLoader, in)

                    def encodeMongoDB(classLoader: ClassLoader, in: t) =
                        coder.encodeMongoDB(classLoader, in)

                })

            case other =>
                Failed("cannot create a (un)wrapper coding - expected a single field object coder but got " + other)
        }
}

/** Coding that always fails */
class NoCoding(failed: Failed = Failed("type cannot be coded")) extends Coding {
    def makeCoder(default: => Result[ComposableCoder[_]], classTypeR: ClassTypeR)(implicit builder: Builder) = failed
}
