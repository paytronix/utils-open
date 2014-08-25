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

package com.paytronix.utils.interchange.base.derive

import scala.reflect.macros.Universe
import scala.reflect.macros.whitebox.Context

import scalaz.syntax.std.list.ToListOpsFromList /* groupWhen */

import com.paytronix.utils.interchange.base.{name, coded, notCoded}

object utils {
    /** Distilled information about a single property of a structure */
    final case class Property[U <: Universe] (
        externalName:          String,
        internalName:          String,
        decoderName:           U#TermName,
        encoderName:           U#TermName,
        coderName:             U#TermName,
        tpe:                   U#Type,
        annotations:           List[U#Annotation],
        read:                  U#Tree => U#Tree,
        write:                 Option[(U#Tree, U#Tree) => U#Tree],
        constructorAssignment: Option[U#Tree => U#Tree]
    ) extends Ordered[Property[U]] {
        def compare(other: Property[U]) =
            this.externalName compare other.externalName

        override def toString = {
            val flags = List (
                Some("readable"),
                write.map(_ => "writable"),
                constructorAssignment.map(_ => "constructor")
            ).flatten.mkString(" ")

            val annots = if (annotations.isEmpty) "" else annotations.map("@" + _.toString).mkString("", " ", " ")
            s"$annots$externalName($internalName): $tpe ($flags)"
        }
    }

    /** Distilled information about a structure, either object or class */
    final case class Structure[U <: Universe] (
        tpe:                    U#Type,
        properties:             List[Property[U]],
        constructorProperties:  List[Property[U]],
        mutableProperties:      List[Property[U]],
        encodeOnlyProperties:   List[Property[U]],
        constructor:            U#MethodSymbol,
        constructAndAssign:     (Property[U] => U#Tree) => U#Tree
    ) {
        override def toString = s"""
Type: $tpe
  Chosen constructor: ${constructor.typeSignature}
  Constructor properties:
    ${constructorProperties.mkString("", "\n    ", "\n")}
  Mutable properties:
    ${mutableProperties.mkString("", "\n    ", "\n")}
  Encode-only properties:
    ${encodeOnlyProperties.mkString("", "\n    ", "\n")}
        """.trim
    }

    /** Simplified information about the application of a derive style annotation */
    final case class SimplifiedDeriveAnnotation[U <: Universe] (
        targetType:           U#Type,
        annotationParameters: List[List[U#Tree]],
        implName:             U#Name,
        implDef:              U#ImplDef
    )

    /**
     * Match a `Type` which is the result type of some `MethodType` but only if it's the end of explicit parameters.
     * That is, match either an implicit parameter section or not a parameter section
     */
    def matchEndOfExplicitParameters(c: Context)(in: c.universe.Type): Option[c.universe.Type] = {
        import c.universe.{MethodType, MethodTypeTag}
        in match {
            case MethodType(params, resultTpe) if params.forall(_.isImplicit) && params.nonEmpty
                                  => Some(resultTpe)
            case MethodType(_, _) => None
            case _                => Some(in)
        }
    }

    /** Filter children of the given type to only those which are public constructors */
    def publicConstructorsOf(c: Context)(tpe: c.universe.Type): List[c.universe.MethodSymbol] =
            tpe.members.filter { sym =>
                sym.isMethod &&
                sym.isPublic &&
                sym.asMethod.isConstructor
            }.map(_.asMethod).toList

    /** Find the primary public constructor of a type */
    def publicPrimaryConstructorOf(c: Context)(tpe: c.universe.Type): Option[c.universe.MethodSymbol] =
        publicConstructorsOf(c)(tpe).find(_.isPrimaryConstructor)

    /** Find a constructor in the type which has no explicit parameters */
    def publicNullaryConstructorOf(c: Context)(tpe: c.universe.Type): Option[c.universe.MethodSymbol] = {
        import c.universe.{MethodType, MethodTypeTag, Type}

        object EndOfExplicitParameters {
            def unapply(in: Type): Option[Type] = matchEndOfExplicitParameters(c)(in)
        }

        publicConstructorsOf(c)(tpe).find { ctor =>
            ctor.typeSignature match {
                case EndOfExplicitParameters(_) => true
                case _ => false
            }
        }
    }

    /** Find and model all properties of a type including Java and Scala style accessors, read only or mutable */
    def propertiesOf(c: Context)(tpe: c.universe.Type): List[Property[c.universe.type]] = {
        // really sorry. the compiler API is heinous.
        import c.universe._

        val unitTpe = typeOf[Unit]
        val nameTpe = typeOf[name]

        object EndOfExplicitParameters {
            def unapply(in: Type): Option[Type] = matchEndOfExplicitParameters(c)(in)
        }

        /** Match a method symbol's type and yield `Some(type)` iff it is "getter shaped" - yields some value which is not `Unit` and requires no explicit parameters */
        def getterShape(in: MethodSymbol, allowZeroArg: Boolean): Option[Type] =
            in.typeSignature match {
                case NullaryMethodType(resultTpe) =>
                    if (resultTpe != unitTpe) Some(resultTpe) else None
                case EndOfExplicitParameters(resultTpe) =>
                    if (resultTpe != unitTpe) Some(resultTpe) else None
                case MethodType(Nil, EndOfExplicitParameters(resultTpe)) =>
                    if (allowZeroArg && resultTpe != unitTpe) Some(resultTpe) else None
                case _ =>
                    None
            }

        /** Match a method symbol's type and yield `Some(type)` iff it is "setter shaped" - takes a single explicit parameter */
        def setterShape(in: MethodSymbol): Option[Type] =
            in.typeSignature match {
                case MethodType(param :: Nil, EndOfExplicitParameters(_)) =>
                    Some(param.typeSignature)
                case _ =>
                    None
            }

        /** Extractor for JavaBean style accessor methods, either getters spelled `getX` or `isX` or setters spelled `setX` */
        object JavaBeanAccessor {
            def unapply(in: Symbol): Option[(Boolean, String, MethodSymbol, Type)] = {
                val methodName = in.name.decodedName.toString
                def lowercaseInitial(in: String): String =
                    if (in.size > 0) Character.toLowerCase(in.charAt(0)) + in.substring(1) else ""
                for {
                    (isSetter, propertyName) <- () match {
                        case _ if methodName.size <= 2 => None
                        case _ if methodName.startsWith("is") && Character.isUpperCase(methodName.charAt(2)) =>
                            Some((false, lowercaseInitial(methodName.substring(2))))

                        case _ if methodName.size <= 3 || !Character.isUpperCase(methodName.charAt(3)) => None
                        case _ if methodName.startsWith("get") => Some((false, lowercaseInitial(methodName.substring(3))))
                        case _ if methodName.startsWith("set") => Some((true,  lowercaseInitial(methodName.substring(3))))

                        case _ => None
                    }
                    method <- Some(in).filter(_.isMethod).map(_.asMethod)
                    tpe <- if (isSetter) setterShape(method) else getterShape(method, true)
                } yield (isSetter, propertyName, method, tpe)
            }
        }

        /** Extractor for Scala style getters */
        object ScalaGetter {
            def unapply(in: Symbol): Option[(String, MethodSymbol, Type)] =
                if (in.isMethod && !in.name.decodedName.toString.endsWith("_="))
                    getterShape(in.asMethod, false).map { tpe => (in.name.decodedName.toString, in.asMethod, tpe) }
                else
                    None
        }

        /** Extractor for Scala style setters */
        object ScalaSetter {
            def unapply(in: Symbol): Option[(String, MethodSymbol, Type)] = {
                val methodName = in.name.decodedName.toString
                if (in.isMethod && methodName.endsWith("_="))
                    setterShape(in.asMethod).map { tpe => (methodName.dropRight(2), in.asMethod, tpe) }
                else
                    None
            }
        }

        sealed abstract class Accessor { def name: String; def tpe: Type }
        final case class Getter(name: String, java: Boolean, method: MethodSymbol, tpe: Type) extends Accessor
        final case class Setter(name: String, java: Boolean, method: MethodSymbol, tpe: Type) extends Accessor

        // don't pick up accessors for any of the essential types such as `AnyRef`, `Product`, and so on
        val ignoreOwners = List (
            typeOf[AnyRef].typeSymbol,
            typeOf[java.lang.Object].typeSymbol,
            typeOf[Product].typeSymbol,
            typeOf[Serializable].typeSymbol,
            typeOf[AnyRef].typeSymbol,
            typeOf[Any].typeSymbol
        )

        val primaryConstructorOpt = publicPrimaryConstructorOf(c)(tpe)

        // map of all constructor arguments for annotation discovery
        val constructorArgumentTerms: Map[String, TermSymbol] =
            primaryConstructorOpt match {
                case Some(primaryConstructor) =>
                    Map.empty ++ primaryConstructor.paramLists.flatMap {
                        case params if params.exists(_.isImplicit) => Nil
                        case params => params.collect { case sym if sym.isTerm => sym.name.decodedName.toString -> sym.asTerm }
                    }
                case None =>
                    Map.empty
            }

        // map of all backing fields for annotation discovery
        val fieldTerms: Map[String, TermSymbol] =
            Map.empty ++ tpe.members.collect { case s if s.isTerm && s.name.decodedName.toString.endsWith(" ") => s.name.decodedName.toString.trim -> s.asTerm }

        tpe.members.collect { case sym
            if !ignoreOwners.contains(sym.owner)
            && sym.isPublic
            && !sym.isImplementationArtifact
            && !sym.isSynthetic
            && sym.isMethod
            && !sym.asMethod.isConstructor
            => sym
        }
        .toList.collect {
            case JavaBeanAccessor(isSetter, name, method, tpe) => if (!isSetter) Getter(name, true, method, tpe)
                                                                     else        Setter(name, true, method, tpe)
            case ScalaGetter(name, method, tpe)                =>                Getter(name, false, method, tpe)
            case ScalaSetter(name, method, tpe)                =>                Setter(name, false, method, tpe)
        }
        .groupWhen { (a, b) => a.name == b.name && a.tpe =:= b.tpe }
        .filter(_.exists(_.isInstanceOf[Getter]))
        .map {
            // super awesome hax. this one catches when we'd consider isFoo a property named foo in the JavaBean style that doesn't have
            // an associated JavaBean setter and rewrites it back to the scala style named "isFoo"
            case Getter(name, true, method, tpe) :: Nil if
                method.name.decodedName.toString.length > 2 &&
                method.name.decodedName.toString.startsWith("is") &&
                Character.isUpperCase(method.name.decodedName.toString.charAt(2)) =>
                val (newName, newMethod, newTpe) = ScalaGetter.unapply(method).getOrElse {
                    sys.error("trying to patch up lone isX method to a Scala getter instead of JavaBean getter but can't apply Scala getter shape")
                }
                Getter(newName, false, newMethod, newTpe) :: Nil

            case other => other
        }
        .map { psyms =>
            val name           = psyms.head.name
            val tpe            = psyms.head.tpe
            val getters        = psyms.collect { case g: Getter => g }
            val setters        = psyms.collect { case s: Setter => s }
            val ctorArgTermOpt = constructorArgumentTerms.get(name).filter(_.typeSignature =:= tpe)
            val fieldTermOpt   = fieldTerms.get(name)

            val getterOpt      = if (getters.nonEmpty) Some(getters.minBy(_.java).method) else None
            val setterOpt      = if (setters.nonEmpty) Some(setters.minBy(_.java).method) else None

            val getter = getterOpt.getOrElse {
                sys.error("property " + name + " has neither getters nor setters, so there's a programming error in propertiesOf[A]")
            }

            val annotations = List (
                getterOpt.map(_.annotations).getOrElse(Nil),
                setterOpt.map(_.annotations).getOrElse(Nil),
                ctorArgTermOpt.map(_.annotations).getOrElse(Nil),
                fieldTermOpt.map(_.annotations).getOrElse(Nil)
            ).flatten.distinct

            object Named {
                def unapply(in: Annotation): Option[String] =
                    if (in.tree.tpe =:= nameTpe)
                        in.tree.children.tail.collectFirst {
                            case AssignOrNamedArg(Ident(TermName(name)), rhs) =>
                                c.eval(c.Expr[String](c.untypecheck(rhs.duplicate)))
                        }
                    else None
            }

            val externalName = annotations.collectFirst { case Named(externalName) => externalName } getOrElse name

            Property[c.universe.type] (
                externalName  = externalName,
                internalName  = name,
                decoderName   = TermName(name + "Decoder"),
                encoderName   = TermName(name + "Encoder"),
                coderName     = TermName(name + "Coder"),
                tpe           = tpe,
                read          = (x: Tree) => q"$x.$getter",
                write         = setterOpt.map { setter => (x: Tree, a: Tree) => q"$x.$setter($a)" },

                annotations = annotations,

                constructorAssignment = ctorArgTermOpt.map { ctorArgTerm => (a: Tree) => q"$ctorArgTerm = $a" }
            )
        }.toList.sorted
    }

    /**
     * Model a structure and its properties, picking an appropriate constructor and
     * pre-building some of the trees and names used for coding
     */
    def structureOf(c: Context)(A: c.Type): Structure[c.universe.type] = {
        import c.universe.{
            EmptyTree, Flag, Ident, Modifiers, NoPrefix, Quasiquote, Tree,
            MethodSymbol, MethodType, MethodTypeTag, Type, typeOf,
            TermName, TypeName
        }

        def bail[A](msg: String): A = {
            c.error (
                pos = c.enclosingPosition,
                msg = msg
            )
            sys.error(msg)
        }

        val codedTpe    = typeOf[coded].typeSymbol
        val notCodedTpe = typeOf[notCoded].typeSymbol

        val availableProps  = propertiesOf(c)(A).filterNot(_.annotations.exists(_.tree.tpe.typeSymbol == notCodedTpe))
        val unwritableProps = availableProps.filter { p => !(p.write.isDefined || p.constructorAssignment.isDefined) }

        val allConstructorProps = availableProps.filter { p => p.constructorAssignment.isDefined }

        object EndOfExplicitParameters {
            def unapply(in: Type): Option[Type] =
                in match {
                    case MethodType(params, resultTpe) if params.forall(_.isImplicit) && params.nonEmpty =>
                        Some(resultTpe)
                    case MethodType(_, _) => None
                    case _ => Some(in)
                }
        }

        def missingArguments(ctor: MethodSymbol): Set[(String, Type)] = {
            val availableParams = Set.empty ++ allConstructorProps.map(p => (p.internalName, p.tpe))
            def accumulate(missing: Set[(String, Type)], ty: Type): Set[(String, Type)] =
                ty match {
                    case EndOfExplicitParameters(_) => missing
                    case MethodType(params, resultTpe) if params.nonEmpty && !params.exists(_.isImplicit) =>
                        val requiredParams = Set.empty ++
                            params.map(_.asTerm)
                            .filterNot(_.isParamWithDefault)
                            .map(sym => (sym.name.decodedName.toString, sym.typeSignature))

                        accumulate (
                            missing ++ (requiredParams diff availableParams),
                            resultTpe
                        )
                    case _ => missing
                }
            accumulate(Set.empty, ctor.typeSignature)
        }


        val (chosenConstructor, constructorProps) =
            publicPrimaryConstructorOf(c)(A) match {
                case Some(primaryConstructor) =>
                    val missing = missingArguments(primaryConstructor)
                    if (missing.nonEmpty) {
                        publicNullaryConstructorOf(c)(A).map { ctor => (ctor, Nil) }.getOrElse {
                            val missingDescription = missing.map { case (name, tpe) => s"$name: $tpe" }.mkString(", ")
                            bail (
                                s"cannot construct ${A} with primary constructor " +
                                s"${primaryConstructor.typeSignature} because it requires extra arguments: " +
                                s"$missingDescription and there is no public nullary constructor present"
                            )
                        }
                    } else {
                        (primaryConstructor, allConstructorProps)
                    }

                case None =>
                    publicNullaryConstructorOf(c)(A).map { case ctor => (ctor, Nil) }.getOrElse {
                        bail (
                            s"cannot construct ${A} with primary constructor " +
                            s"because it isn't public " +
                            s"and there is no public nullary constructor present"
                        )
                    }
            }

        val mutableProps    = (availableProps diff constructorProps).filter { p => p.write.isDefined && !p.constructorAssignment.isDefined }
        val encodeOnlyProps = unwritableProps.filter(_.annotations.exists(_.tree.tpe.typeSymbol == codedTpe))
        val allProps        = (constructorProps ++ mutableProps ++ encodeOnlyProps).sortBy(_.externalName)

        def construct(valueOfProperty: Property[c.universe.type] => Tree): Tree = {
            val assignments = constructorProps.map { prop =>
                prop.constructorAssignment.getOrElse(sys.error(prop + " should have been constructable")).apply(valueOfProperty(prop))
            }
            q"new $A(..$assignments)"
        }

        def mutablePropertyAssignments(assignee: Tree, valueOfProperty: Property[c.universe.type] => Tree): List[Tree] =
            mutableProps.map { prop =>
                prop.write.getOrElse(sys.error(prop + " should have been mutable")).apply(assignee, valueOfProperty(prop))
            }

        val constructAndAssign: (Property[c.universe.type] => Tree) => Tree =
            valueOfProperty => {
                val instance = Ident(TermName(c.freshName()))

                q"""
                    {
                        val $instance = ${construct(valueOfProperty)}
                        ..${mutablePropertyAssignments(instance, valueOfProperty)}
                        $instance
                    }
                """
            }

        Structure[c.universe.type] (
            tpe                   = A,
            constructor           = chosenConstructor,
            properties            = allProps,
            constructorProperties = constructorProps,
            mutableProperties     = mutableProps,
            encodeOnlyProperties  = encodeOnlyProps,
            constructAndAssign    = constructAndAssign
        )
    }

    /**
     * Break down an annotation with a type parameter on an object or class into it's component parts for easy
     * consumption, as a helper for deriveCoder[A], deriveDecoder[A], and derivceEncoder[A] style annotations.
     */
    def simplifyDeriveAnnotation(c: Context)(annottees: c.Expr[Any]*): SimplifiedDeriveAnnotation[c.universe.type] = {
        import c.universe.{ClassDef, ClassDefTag, ModuleDef, ModuleDefTag, Quasiquote, TreeTag}
        val q"new $annot[$a](...$annotParams).macroTransform(...$_)" = c.macroApplication
        val targetType = c.typecheck(a, mode = c.TYPEmode, silent = false).tpe

        annottees.map(_.tree).toList match {
            case (moduleDef@ModuleDef(_, name, _)) :: Nil =>
                SimplifiedDeriveAnnotation (
                    targetType           = targetType,
                    annotationParameters = annotParams,
                    implName             = name,
                    implDef              = moduleDef
                )

            case (classDef@ClassDef(_, name, _, _)) :: rest =>
                SimplifiedDeriveAnnotation (
                    targetType           = targetType,
                    annotationParameters = annotParams,
                    implName             = name,
                    implDef              = classDef
                )

            case Nil =>
                sys.error("got no annottees??!?")

            case annottees =>
                sys.error(s"expected $annot to annotate an object or class which should derive from an automatically generated coder")
        }
    }

    /**
     * Sequence (as in monadic sequencing) a series of `Result` binders.
     *
     * From List(a,b,c,…) generates code like:
     *    $action(a) match {
     *        case Okay(a') =>
     *            $accept(a')
     *            $action(b) match {
     *                case Okay(b') =>
     *                    $accept(b')
     *                    $action(c) match {
     *                        case Okay(c') =>
     *                            $accept(c')
     *                            …
     *                            $body(a',b',c',…)
     *                        case failed => failed
     *                    }
     *                case failed => failed
     *            }
     *        case failed => failed
     *    }
     *
     * which is roughly equivalent to the desugaring of the for comprehension:
     *
     *    for {
     *        a' <- $action(a)
     *        $accept(a')
     *        b' <- $action(b)
     *        $accept(b')
     *        c' <- $action(c)
     *        $accept(c')
     *    } yield $body(a',b',c')
     *
     * but without the `flatMap`s.
     */
    def sequenceResultBindings[A](c: Context, input: Seq[A]) (
        action: A => c.universe.Tree,
        accept: (A, c.universe.Tree) => c.universe.Tree,
        body: Seq[c.universe.Tree] => c.universe.Tree
    ): c.Tree = {
        import c.universe.{Bind, Block, Ident, Quasiquote, TermName, termNames}

        val names = input.map(_ => TermName(c.freshName()))

        val failed = TermName(c.freshName())

        (input zip names).foldRight(body(names.map(name => Ident(name)))) { (pair, inside) =>
            val (a, name) = pair
            q"""
                ${Block(Nil, action(a))} match {
                    case com.paytronix.utils.scala.result.Okay(${Bind(name, Ident(termNames.WILDCARD))}) =>
                        ${accept(a, Ident(name))}
                        $inside
                    case $failed => $failed
                }
            """
        }
    }

    /** Add some definitions to the companion object of an annotated class, creating that companion if it doesn't already exist */
    def addToCompanion(c: Context)(annottees: Seq[c.Expr[Any]]) (
        f: (c.universe.TypeName, List[c.universe.Tree]) => List[c.universe.Tree]
    ): c.Expr[Any] = {
        import c.universe.{ClassDef, ClassDefTag, Modifiers, ModifiersTag, ModuleDef, ModuleDefTag, Quasiquote, Template, TreeTag}

        annottees.map(_.tree).toList match {
            case (moduleDef@ModuleDef(_, name, _)) :: Nil =>
                sys.error("expected this annotation to only be used on a class")

            case (classDef@ClassDef(classMods, targetName, _, _)) :: (moduleDef@ModuleDef(objMods, objName, objImpl)) :: Nil =>
                val Modifiers(_, _, annotations) = classMods

                val Template(parents, self, body) = objImpl
                val q"{ ..$updatedBody }" = q"""
                    ..${f(targetName, annotations)}
                    ..$body
                """

                val updatedModuleDef = ModuleDef(objMods, objName, Template(parents, self, updatedBody))

                c.Expr[Any](q"""
                    {
                        $classDef
                        $updatedModuleDef
                    }
                """)

            case (classDef@ClassDef(mods, targetName, _, _)) :: Nil =>
                val Modifiers(_, _, annotations) = mods

                val newModuleDef = q"""
                    object ${targetName.toTermName} {
                        ..${f(targetName, annotations)}
                    }
                """

                c.Expr[Any](q"""
                    {
                        $classDef
                        $newModuleDef
                    }
                """)

            case Nil =>
                sys.error("got no annottees??!?")

            case annottees =>
                sys.error(s"expected implicitly to annotate a class to which a companion coder pair should be attached")
        }
    }

    /** Augment some `ImplDef` with additional statements and parents */
    def augmentImpl(c: Context)(implDef: c.universe.ImplDef, newParents: List[c.universe.Tree], stats: List[c.universe.Tree]): c.universe.ImplDef = {
        import c.universe.{ClassDef, ClassDefTag,  Ident, ModuleDef, ModuleDefTag, Quasiquote, Template}

        val Template(parents, self, body) = implDef.impl
        val newImpl = Template(parents ++ newParents, self, body ++ stats)

        implDef match {
            case ClassDef(m, n, tp, _) => ClassDef(m, n, tp, newImpl)
            case ModuleDef(m, n, _)    => ModuleDef(m, n, newImpl)
        }
    }

}
