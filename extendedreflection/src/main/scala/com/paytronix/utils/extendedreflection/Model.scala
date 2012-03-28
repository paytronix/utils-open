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

package com.paytronix.utils.extendedreflection

import java.lang.annotation.{Annotation => JavaAnnotation}
import java.lang.reflect
import scala.tools.scalap.scalax.rules.{scalasig => sig}
import org.slf4j.Logger

import com.paytronix.utils.scala.result.Result


object Model {
    def annotationsToPrefixString(in: List[JavaAnnotation]): String = in match {
        case Nil => ""
        case _ => in.mkString("", " ", " ")
    }
}

/**
 * A decomposed name, consisting of the actual package, a potentially empty series of "non-package scopes" (enclosing classes or objects),
 * the actual terminal name segment, and whether or not the name is for a companion object.
 */
final case class NameR(pkg: String, nonPkgs: List[String], local: String, isCompanion: Boolean) {
    val qualified =
        ((if (pkg != "") pkg :: nonPkgs else nonPkgs) :+ local).mkString(".")

    val encoded =
        (if (pkg != "") pkg + "." else "") + (if (nonPkgs != Nil) nonPkgs.mkString("", "$", "$") else "") + local + (if (isCompanion) "$" else "")

    val classFilePath =
        (if (pkg != "") pkg.replace('.', '/') + "/" else "") + (if (nonPkgs != Nil) nonPkgs.mkString("", "$", "$") else "") + local + (if (isCompanion) "$" else "") + ".class"

    def permutations: Seq[NameR] = {
        val splitPkg = pkg.split('.') ++ nonPkgs
        (1 to splitPkg.size).foldLeft(Nil: List[NameR]) { (rest, i) =>
            NameR(splitPkg.take(i).mkString("."), splitPkg.drop(i).toList, local, false) :: rest
        }
    }

    def enclosure: NameR =
        nonPkgs match {
            case Nil =>
                pkg.split('.') match {
                    case toks if !toks.isEmpty =>
                        copy(pkg = toks.dropRight(1).mkString("."), local=toks.last)
                    case _ =>
                        this
                }

            case _ =>
                copy(nonPkgs=nonPkgs.dropRight(1), local=nonPkgs.last)
        }

    override def toString = qualified
}

object NameR {
    def apply(in: String): NameR = {
        val isCompanion = in.endsWith("$")
        val (pkg, rest) = in.stripSuffix("$").lastIndexOf('.') match {
            case -1 => ("", in)
            case pos => (in.substring(0, pos), in.substring(pos+1))
        }

        val parts = rest.split("[$]")
        NameR(pkg, parts.take(parts.length-1).toList, parts.last, isCompanion)
    }

    def apply(sym: sig.Symbol): NameR = {
        def traverse(in: sig.Symbol, rest: List[String]): (String, List[String]) =
            in.parent match {
                case Some(parent) => parent match {
                    case es: sig.ExternalSymbol if es.entry.entryType == 10 => (es.path, rest)
                    case _ => traverse(parent, parent.name :: rest)
                }

                case None => /* really? */ ("", rest)
            }

        val (pkg, nonPkgs) = traverse(sym, Nil)
        NameR(pkg, nonPkgs, sym.name, sym.isModule)
    }
}

// Some extractor objects if you want to match names based on certain components
object QualifiedName { def unapply(in: NameR): Option[String] = Some(in.qualified) }
object LocalName     { def unapply(in: NameR): Option[String] = Some(in.local    ) }
object EncodedName   { def unapply(in: NameR): Option[String] = Some(in.encoded  ) }

/** Abstract superclass of type reflections */
sealed abstract class TypeR {
    val reflectionModel: reflect.Type
    val signatureModel: Result[sig.Type]
}

/** Non-parameterized types */
final case class ClassTypeR(name: NameR, reflectionModel: Class[_], signatureModel: Result[sig.Type]) extends TypeR {
    override def toString = name.qualified
}

/** Extract just the name from a ClassTypeR */
object ClassTypeRWithName {
    def unapply(in: TypeR): Option[NameR] = in match {
        case ClassTypeR(n, _, _) => Some(n)
        case _ => None
    }
}

/** Extract just a Class token from a ClassTypeR */
object ClassTypeRFor {
    def unapply(in: TypeR): Option[Class[_]] = in match {
        case ClassTypeR(_, c, _) => Some(c)
        case _ => None
    }
}

/** Parameterized types */
final case class ParameterizedTypeR(
    head: TypeR, parameters: List[TypeR], reflectionModel: reflect.ParameterizedType, signatureModel: Result[sig.Type]
) extends TypeR {
    override def toString = head.toString + parameters.mkString("[", ", ", "]")
}

/** Extract just the head of a ParameterizedTypeR */
object ParameterizedTypeRWithHead {
    def unapply(in: TypeR): Option[TypeR] = in match {
        case ParameterizedTypeR(typeR, _, _, _) => Some(typeR)
        case _ => None
    }
}

/** Extract the head and parameters of a ParameterizedTypeR */
object ParameterizedTypeRWith {
    def unapply(in: TypeR): Option[(TypeR, List[TypeR])] = in match {
        case ParameterizedTypeR(typeR, params, _, _) => Some((typeR, params))
        case _                                       => None
    }
}

/** Wildcard type (_ in Scala, ? in Java) */
final case class WildcardTypeR(reflectionModel: reflect.WildcardType, signatureModel: Result[sig.Type]) extends TypeR {
    override def toString = "_"
}

/** Other non-simple-to-explain types */
final case class OtherTypeR(reflectionModel: reflect.Type, signatureModel: Result[sig.Type]) extends TypeR {
    override def toString = "<unknown (" + reflectionModel + ", " + signatureModel + ")>"
}

/** Model of a class */
final case class ClassR(
    name:            NameR,
    superClass:      Option[ClassR],
    interfaces:      List[ClassR],
    members:         List[ClassMemberR],
    reflectionModel: Class[_],
    signatureModel:  Result[sig.ClassSymbol]
) {
    lazy val constructors: List[ConstructorR] = members.collect { case cr: ConstructorR if cr.isPublic => cr }
    lazy val methods:      List[MethodR]      = members.collect { case mr: MethodR      if mr.isPublic => mr }
    lazy val properties:   List[PropertyR]    = members.collect { case pr: PropertyR    if pr.isPublic => pr }
    lazy val allMethods: List[MethodR] = methods ::: properties.map(_.getter) ::: properties.map(_.setter).flatten

    lazy val nonPublicConstructors: List[ConstructorR] = members.collect { case cr: ConstructorR if !cr.isPublic => cr }
    lazy val nonPublicMethods:      List[MethodR]      = members.collect { case mr: MethodR      if !mr.isPublic => mr }
    lazy val nonPublicProperties:   List[PropertyR]    = members.collect { case pr: PropertyR    if !pr.isPublic => pr }

    lazy val allNonPublicMethods: List[MethodR] = nonPublicMethods ::: nonPublicProperties.map(_.getter) ::: nonPublicProperties.map(_.setter).flatten

    def debugTo(logger: Logger, prefix: String): Unit = {
        val extendsLine = List(superClass).flatten ::: interfaces match {
            case Nil => ""
            case other => other.map(_.name.qualified).mkString(" extends ", " with ", "")
        }
        logger.debug(prefix + (if (reflectionModel.isInterface) "trait " else "class ") + name.qualified + extendsLine + " {")
        members.foreach(_.debugTo(logger, prefix + "  "))
        logger.debug(prefix + "}")
    }
}

/** Model of a parameter to a method or constructor */
final case class ParameterR(name: Option[String], typeR: TypeR, annotations: List[JavaAnnotation]) {
    override def toString = Model.annotationsToPrefixString(annotations) + name.getOrElse("_") + ": " + typeR
}

/** Abstract superclass of class member models -- constructors, properties, and methods */
sealed abstract class ClassMemberR {
    val ownerName:   NameR
    val name:        String
    val isPublic:    Boolean
    val annotations: List[JavaAnnotation]

    def canOverride(other: ClassMemberR): Boolean

    def debugTo(logger: Logger, prefix: String): Unit
}

/** Trait for things that are close to methods in that they take parameters. That is, constructors and methods */
trait MethodLikeR extends ClassMemberR {
    val parameters: List[ParameterR]

    def matches(meth: reflect.Method): Boolean = meth.getName == name && meth.getParameterTypes.length == parameters.length // FIXME overloads
    def matches(ctor: reflect.Constructor[_]): Boolean = ctor.getParameterTypes.length == parameters.length // FIXME overloads
}

/** Model of a method */
final case class MethodR(
    ownerName:        NameR,
    name:             String,
    isPublic:         Boolean,
    annotations:      List[JavaAnnotation],
    parameters:       List[ParameterR],
    result:           TypeR,
    reflectionModel:  reflect.Method,
    signatureModel:   Result[sig.MethodSymbol]
) extends ClassMemberR with MethodLikeR {
    def canOverride(other: ClassMemberR): Boolean =
        other match {
            case mr: MethodR if name == mr.name && parameters.length == mr.parameters.length => true /* FIXME overloads */
            case _ => false
        }

    def debugTo(logger: Logger, prefix: String): Unit =
        logger.debug(prefix + Model.annotationsToPrefixString(annotations) + (if (isPublic) "" else "nonpublic ") +
                     "def " + name + parameters.mkString("(", ", ", ")") + ": " + result)
}

/** Model of a constructor */
final case class ConstructorR(
    ownerName:       NameR,
    isPublic:        Boolean,
    annotations:     List[JavaAnnotation],
    parameters:      List[ParameterR],
    reflectionModel: reflect.Constructor[_],
    signatureModel:  Result[sig.MethodSymbol]
) extends ClassMemberR with MethodLikeR {
    val name = "<init>"

    def canOverride(other: ClassMemberR): Boolean =
        other match {
            case ctorR: ConstructorR if parameters.length == ctorR.parameters.length => true /* FIXME overloads */
            case _ => false
        }

    def debugTo(logger: Logger, prefix: String): Unit =
        logger.debug(prefix + Model.annotationsToPrefixString(annotations) + (if (isPublic) "" else "nonpublic ") +
                     "def this" + parameters.mkString("(", ", ", ")"))
}

/** Model of a property, whose access is mediated by accessors */
final case class PropertyR(
    ownerName:   NameR,
    name:        String,
    isPublic:    Boolean,
    typeR:       TypeR,
    annotations: List[JavaAnnotation],
    getter:      MethodR,
    setter:      Result[MethodR],
    others:      List[MethodR]
) extends ClassMemberR {
    def canOverride(other: ClassMemberR): Boolean =
        other match {
            case pr: PropertyR if name == pr.name => true
            case _ => false
        }

    def debugTo(logger: Logger, prefix: String): Unit = {
        logger.debug(prefix + Model.annotationsToPrefixString(annotations) + (if (isPublic) "" else "nonpublic ") +
                     (if (setter.isDefined) "var " else "val ") + name + ": " + typeR)
        getter.debugTo(logger, prefix + "  // getter: ")
        setter.foreach(_.debugTo(logger, prefix + "  // setter: "))
        others.foreach(_.debugTo(logger, prefix + "  // other: "))
    }
}
