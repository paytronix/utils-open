//
// Copyright 2014-2020 Paytronix Systems, Inc.
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

import scala.reflect.macros.whitebox.Context

import cats.data.NonEmptyList

import com.paytronix.utils.interchange.base.{name, coded, notCoded}

trait DeriveUtils {
    val c: Context

    import c.universe.{
        Annotation, Bind, Block, ClassDef, ClassDefTag, Constant, EmptyTree, Flag, Ident, IdentTag, Literal,
        MethodSymbol, MethodType, MethodTypeTag, Modifiers, ModuleDef, ModuleDefTag, NoPrefix, NullaryMethodType, NullaryMethodTypeTag,
        Quasiquote, SingleType, SingleTypeTag, Symbol, Template, TermName, TermNameTag, TermSymbol, Tree, TreeTag, Type,
        typeOf
    }

    /** Distilled information about a single property of a structure */
    final case class Property(
        externalName:          String,
        internalName:          String,
        decoderName:           TermName,
        encoderName:           TermName,
        coderName:             TermName,
        tpe:                   Type,
        annotations:           List[Annotation],
        read:                  Tree => Tree,
        write:                 Option[(Tree, Tree) => Tree],
        constructorAssignment: Option[Tree => Tree]
    ) extends Ordered[Property] {
        def compare(other: Property) =
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
    final case class Structure(
        tpe:                    Type,
        annotations:            List[Annotation],
        properties:             List[Property],
        constructorProperties:  List[Property],
        mutableProperties:      List[Property],
        encodeOnlyProperties:   List[Property],
        constructor:            MethodSymbol,
        constructAndAssign:     (Property => Tree) => Tree
    ) {
        override def toString = s"""
Type: $tpe
  Annotations: $annotations
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
    final case class SimplifiedDeriveAnnotation (
        targetType:           Type,
        annotationParameters: List[List[Tree]],
        implName:             c.universe.Name,
        implDef:              c.universe.ImplDef
    )

    /**
     * Match a `Type` which is the result type of some `MethodType` but only if it's the end of explicit parameters.
     * That is, match either an implicit parameter section or not a parameter section
     */
    def matchEndOfExplicitParameters(in: Type): Option[Type] = {
        in match {
            case MethodType(params, resultTpe) if params.forall(_.isImplicit) && params.nonEmpty
                                  => Some(resultTpe)
            case MethodType(_, _) => None
            case _                => Some(in)
        }
    }

    /** Filter children of the given type to only those which are constructors */
    def constructorsOf(tpe: Type): List[MethodSymbol] =
            tpe.members.filter { sym =>
                sym.isMethod &&
                sym.asMethod.isConstructor
            }.map(_.asMethod).toList

    /** Find the primary constructor of a type */
    def primaryConstructorOf(tpe: Type): Option[MethodSymbol] =
        constructorsOf(tpe).find(_.isPrimaryConstructor)

    /** Find a constructor in the type which has no explicit parameters */
    def nullaryConstructorOf(tpe: Type): Option[MethodSymbol] = {
        object EndOfExplicitParameters {
            def unapply(in: Type): Option[Type] = matchEndOfExplicitParameters(in)
        }

        constructorsOf(tpe).find { ctor =>
            ctor.typeSignature match {
                case EndOfExplicitParameters(_) => true
                case _ => false
            }
        }
    }

    /** Find and model all properties of a type including Java and Scala style accessors, read only or mutable */
    def propertiesOf(tpe: Type): List[Property] = {
        val unitTpe = typeOf[Unit]
        val nameTpe = typeOf[name]

        object EndOfExplicitParameters {
            def unapply(in: Type): Option[Type] = matchEndOfExplicitParameters(in)
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

        val primaryConstructorOpt = primaryConstructorOf(tpe)

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

        val collectedMembers: List[Accessor] = tpe.members.collect { case sym
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

        //System.out.println("collectedMembers: " + collectedMembers)

        //Put access/mutators for each attribute side by side so grouping can actually work
        val sortedCollectedMembers: List[Accessor] = collectedMembers.sortWith{ (a: Accessor, b: Accessor) => a.name.compareTo(b.name) < 0 }

        //System.out.println("sortedCollectedMembers: " + sortedCollectedMembers)

        val groupedCollectedMembers: List[NonEmptyList[Accessor]] = sortedCollectedMembers.foldLeft(List.empty[NonEmptyList[Accessor]]) {
            case ((h@NonEmptyList(first, _)) :: tail, b: Accessor) if first.name == b.name && first.tpe =:= b.tpe => (b :: h) :: tail
            case (nels, b: Accessor) => NonEmptyList.one(b) :: nels
        }

        //System.out.println("groupedCollectedMembers: " + groupedCollectedMembers)

        val filteredMembers = groupedCollectedMembers.filter(_.exists(_.isInstanceOf[Getter]))

        //System.out.println("filteredMembers: " + filteredMembers)

        val listPsyms = filteredMembers.map {
            // super awesome hax. this one catches when we'd consider isFoo a property named foo in the JavaBean style that doesn't have
            // an associated JavaBean setter and rewrites it back to the scala style named "isFoo"
            case NonEmptyList(Getter(name@_, true, method, tpe@_), Nil) if
                method.name.decodedName.toString.length > 2 &&
                method.name.decodedName.toString.startsWith("is") &&
                Character.isUpperCase(method.name.decodedName.toString.charAt(2)) =>
                val (newName, newMethod, newTpe) = {
                    if (method.isMethod && !method.name.decodedName.toString.endsWith("_="))
                        getterShape(method.asMethod, true).map { tpe => (method.name.decodedName.toString, method.asMethod, tpe) }
                    else
                        None
                }.getOrElse {
                    sys.error(s"trying to patch up lone ${method.name.decodedName.toString} method to a Scala getter instead of JavaBean getter " +
                              s"but can't apply Scala getter shape to ${method.asMethod.toString} with type " +
                              s"${method.asMethod.typeSignature.toString} (of class ${method.asMethod.typeSignature.getClass.toString})")
                }

                NonEmptyList.one(Getter(newName, false, newMethod, newTpe))

            case other => other
        }

        //System.out.println("listPsyms: " + listPsyms)

        val toReturn = listPsyms.map { psyms =>
            val name           = psyms.head.name
            val tpe            = psyms.head.tpe
            val getters        = psyms.collect { case g: Getter => g }
            val setters        = psyms.collect { case s: Setter => s }
            // Just as a note, constructor argument terms will never be found for Java classes,
            // as the constructor argument names are not preserved and are given names like
            // x$1, x$2, x$3, etc. -- I don't know why this is.
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
                def unapply(in: Annotation): Option[String] = {
                    if (in.tree.tpe =:= nameTpe) {
                        in.tree.children.tail.collectFirst {
                            case Literal(Constant(name: String)) => name
                        }
                    } else {
                        None
                    }
                }
            }

            val externalName = annotations.collectFirst { case Named(externalName) => externalName } getOrElse name

            Property(
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

        //System.out.println("toReturn: " + toReturn)

        toReturn
    }

    /**
     * Model a structure and its properties, picking an appropriate constructor and
     * pre-building some of the trees and names used for coding
     */
    def structureOf(A: c.Type): Structure = {
        def bail[A](msg: String): A = {
            c.error (
                pos = c.enclosingPosition,
                msg = msg
            )
            sys.error(msg)
        }

        val codedTpe    = typeOf[coded].typeSymbol
        val notCodedTpe = typeOf[notCoded].typeSymbol

        val availableProps  = propertiesOf(A).filterNot(_.annotations.exists(_.tree.tpe.typeSymbol == notCodedTpe))
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
            primaryConstructorOf(A) match {
                case Some(primaryConstructor) =>
                    val missing = missingArguments(primaryConstructor)
                    if (missing.nonEmpty) {
                        nullaryConstructorOf(A).map { ctor => (ctor, Nil) }.getOrElse {
                            val missingDescription = missing.map { case (name, tpe) => s"$name: $tpe" }.mkString(", ")
                            bail (
                                s"cannot construct ${A} with primary constructor " +
                                s"${primaryConstructor.typeSignature} because it requires extra arguments: " +
                                s"$missingDescription and there is no nullary constructor present"
                            )
                        }
                    } else {
                        (primaryConstructor, allConstructorProps)
                    }

                case None =>
                    nullaryConstructorOf(A).map { case ctor => (ctor, Nil) }.getOrElse {
                        bail (
                            s"cannot construct ${A} with primary constructor and there is no nullary constructor present"
                        )
                    }
            }

        val mutableProps    = (availableProps diff constructorProps).filter { p => p.write.isDefined && !p.constructorAssignment.isDefined }
        val encodeOnlyProps = unwritableProps.filter(_.annotations.exists(_.tree.tpe.typeSymbol == codedTpe))
        val allProps        = (constructorProps ++ mutableProps ++ encodeOnlyProps).sortBy(_.externalName)

        def construct(valueOfProperty: Property => Tree): Tree = {
            val assignments = constructorProps.map { prop =>
                prop.constructorAssignment.getOrElse(sys.error(prop + " should have been constructable")).apply(valueOfProperty(prop))
            }
            q"new $A(..$assignments)"
        }

        def mutablePropertyAssignments(assignee: Tree, valueOfProperty: Property => Tree): List[Tree] =
            mutableProps.map { prop =>
                prop.write.getOrElse(sys.error(prop + " should have been mutable")).apply(assignee, valueOfProperty(prop))
            }

        val constructAndAssign: (Property => Tree) => Tree =
            A match {
                case SingleType(_, obj) =>
                    _ => q"$obj"

                case _ => // presume it's a class
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
            }

        Structure (
            tpe                   = A,
            annotations           = A.typeSymbol.annotations,
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
    def simplifyDeriveAnnotation(annottees: c.Expr[Any]*): SimplifiedDeriveAnnotation = {
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

            case (classDef@ClassDef(_, name, _, _)) :: _ =>
                SimplifiedDeriveAnnotation (
                    targetType           = targetType,
                    annotationParameters = annotParams,
                    implName             = name,
                    implDef              = classDef
                )

            case Nil =>
                sys.error("got no annottees??!?")

            case annottees@_ =>
                sys.error(s"expected $annot to annotate an object or class which should derive from an automatically generated coder")
        }
    }

    /**
     * Sequence (as in monadic sequencing) a series of `Result` binders.
     *
     * From List(a,b,c,…) generates code like:
     *    val a2 = $action(a)
     *    if (a2.isOkay]) {
     *        $accept(a2.orThrow)
     *        val b2 = $action(b)
     *        if (b2.isOkay]) {
     *            $accept(b2.orThrow)
     *            val c2 = $action(c)
     *            if (c2.isOkay]) {
     *                $accept(c2.orThrow)
     *                …
     *                $body(a2.orThrow, b2.orThrow, c2.orThrow)
     *            } else {
     *                c2.asFailed
     *            }
     *        } else {
     *            b2.asFailed
     *        }
     *    } else {
     *        a2.asFailed
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
    def sequenceResultBindings[A](input: Seq[A]) (
        action: A => Tree,
        accept: (A, Tree) => Tree,
        body: Seq[Tree] => Tree
    ): c.Tree = {
        val names = input.map(_ => TermName(c.freshName()))

        (input zip names).foldRight(body(names.map(name => Ident(name)))) { (pair, inside) =>
            val (a, name) = pair
            q"""
                val $name = ${Block(Nil, action(a))}
                if ($name.isOkay) {
                    ${accept(a, q"$name.orThrow")}
                    $inside
                } else {
                    $name.asFailed
                }
            """
        }
    }

    /** Add some definitions to the companion object of an annotated class, creating that companion if it doesn't already exist */
    /* 2014-08-27 RMM: having multiple annotation macros which addToCompanion causes the compiler to not emit the object class (Blah$) even though
                       it doesn't error at runtime.
    def addToCompanion(annottees: Seq[c.Expr[Any]]) (
        f: (TypeName, List[Tree]) => List[Tree]
    ): c.Expr[Any] = {
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
    */

    /** Augment some `ImplDef` with additional statements and parents */
    def augmentImpl(implDef: c.universe.ImplDef, newParents: List[Tree], stats: List[Tree]): c.universe.ImplDef = {
        val Template(parents, self, body) = implDef.impl
        val newImpl = Template(parents ++ newParents, self, body ++ stats)

        implDef match {
            case ClassDef(m, n, tp, _) => ClassDef(m, n, tp, newImpl)
            case ModuleDef(m, n, _)    => ModuleDef(m, n, newImpl)
        }
    }

}
