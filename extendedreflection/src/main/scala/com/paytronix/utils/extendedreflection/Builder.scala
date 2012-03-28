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
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ScalaSignature
import scala.reflect.generic.ByteCodecs
import scala.tools.scalap.scalax.rules.{scalasig => sig}
import org.slf4j.{Logger, LoggerFactory}
import com.paytronix.utils.scala.collection.{iterableEitherOps, zip}
import com.paytronix.utils.scala.io.readBytes
import com.paytronix.utils.scala.log.{loggerResultOps, resultLoggerOps}
import com.paytronix.utils.scala.reflection.{
    classByName,
    areConstructorsCompatible, mostSpecificConstructor, areMethodsCompatible, mostSpecificMethod,
    paranamer
}
import com.paytronix.utils.scala.resource.withResource
import com.paytronix.utils.scala.result.{Okay, Failed, FailedG, Result, catchingException, firstOrLast, iterableResultOps, optionOps}


/** Central object which builds reflection models and caches various heavy information */
class Builder(val classLoader: ClassLoader) {
    private implicit val logger = LoggerFactory.getLogger(getClass)

    private var sigCache: Map[Class[_], Result[sig.ScalaSig]] = Map.empty
    private var classCache: Map[Class[_], Result[ClassR]] = Map.empty

    implicit val builder: Builder = this

    /** Make a TypeR by looking up a name */
    def typeRFor(s: String): Result[ClassTypeR] =
        classByName[AnyRef](classLoader, s) flatMap typeRFor

    /** Make a TypeR from a Class token */
    def typeRFor(clazz: Class[_]): Result[ClassTypeR] = {
        val sigType =
            for {
                sig <- sigFor(clazz)
                csym <- classSymbolFor(clazz, sig)
                ctype <- catchingException(csym.infoType)
            } yield ctype

        typeRFor(clazz, sigType.asA[sig.ClassInfoType]) match {
            case Okay(ctr: ClassTypeR) => Okay(ctr)
            case Okay(other)           => Failed("typeRFor a Class resulted in a non-ClassTypeR: " + other)
            case other                 => other.asA[ClassTypeR]
        }
    }

    private val ObjectClass = classOf[Object]
    private object PrimitiveSigType {
        val primitiveTypes = Set("Boolean", "Byte", "Char", "Double", "Float", "Int", "Long", "Short", "Unit")
        def unapply(in: sig.Type): Option[String] = in match {
            case sig.TypeRefType(sig.ThisType(prefixSymbol), symbol, Nil)
                if prefixSymbol.name == "scala" && primitiveTypes.contains(symbol.name) => Some(symbol.name)
            case _ => None
        }
    }

    def pathOf(sym: sig.Symbol): String =
        sym match {
            // an erroneous override in AliasSymbol makes the path = name, even though it shouldn't be,
            // so make up the path ourselves
            case asym: sig.AliasSymbol => asym.parent.map(_.path + ".").getOrElse("") + asym.name
            case other => other.path
        }

    def dumpSymbol(depth: Int, stop: PartialFunction[Either[sig.Symbol, sig.Type], Boolean], in: sig.Symbol): String = {
        val doStop = stop.lift(Left(in)) getOrElse false

        def maybeDumpSymbol(sym: sig.Symbol): String =
            if (doStop) ("    " * depth) + "    <dump stopped>\n"
            else dumpSymbol(depth+1, stop, sym)

        def maybeDumpType(sym: sig.Type): String =
            if (doStop) ("    " * depth) + "    <dump stopped>\n"
            else dumpType(depth+1, stop, sym)

        ("    " * depth) + in.getClass.getName.split('.').last + " \"" + in.name + "\" (child of " + in.parent.map('"' + _.toString + '"').getOrElse("none") + ")" + (
            in match {
                case sig.NoSymbol => ""
                case esym: sig.ExternalSymbol =>
                    " (type " + esym.entry.entryType + ")" + (
                        resolveSymbol(esym) match {
                            case Okay(sym: sig.ExternalSymbol) if sym.path == esym.path => " (is a package or java class)\n"
                            case Okay(sym) => "\n" + maybeDumpSymbol(sym)
                            case _ => ("    " * depth) + " (can't be resolved)\n"
                        }
                    )
                case sisym: sig.SymbolInfoSymbol =>
                    " (type " + sisym.entry.entryType + ") " + "\n" + maybeDumpType(sisym.infoType)
                case sssym: sig.ScalaSigSymbol =>
                    " (type " + sssym.entry.entryType + ") " + "\n"
            }
        )
    }

    def dumpType(depth: Int, stop: PartialFunction[Either[sig.Symbol, sig.Type], Boolean], in: sig.Type): String = {
        val doStop = stop.lift(Right(in)) getOrElse false

        def maybeDumpSymbol(sym: sig.Symbol): String =
            if (doStop) ("    " * depth) + "    <dump stopped>\n"
            else dumpSymbol(depth+1, stop, sym)

        def maybeDumpType(sym: sig.Type): String =
            if (doStop) ("    " * depth) + "    <dump stopped>\n"
            else dumpType(depth+1, stop, sym)

        ("    " * depth) + (
            in match {
                case sig.NoType => "None\n"
                case sig.NoPrefixType => "No prefix\n"
                case sig.ThisType(sym) =>
                    "This\n" + maybeDumpSymbol(sym)
                case sig.SingleType(ref, sym) =>
                    "Single\n" + maybeDumpType(ref) + maybeDumpSymbol(sym)
                case sig.ConstantType(what) =>
                    "Constant(" + what + ")\n"
                case sig.TypeRefType(prefix, sym, args) =>
                    "Ref\n" + maybeDumpType(prefix) + maybeDumpSymbol(sym) + args.map(maybeDumpType(_)).mkString("")
                case sig.TypeBoundsType(lower, upper) =>
                    "Bounded\n" + maybeDumpType(lower) + maybeDumpType(upper)
                case sig.RefinedType(sym, refs) =>
                    "Refined\n" + maybeDumpSymbol(sym) + refs.map(maybeDumpType(_)).mkString("")
                case sig.ClassInfoType(sym, refs) =>
                    "ClassInfo(" + sym + ")\n" + refs.map(maybeDumpType(_)).mkString("")
                case sig.ClassInfoTypeWithCons(sym, refs, cons) =>
                    "ClassInfoWithCons \"" + cons + "\"\n" + maybeDumpSymbol(sym) + refs.map(maybeDumpType(_)).mkString("")
                case sig.MethodType(result, paramSyms) =>
                    "Method\n" + maybeDumpType(result) + paramSyms.map(maybeDumpSymbol(_)).mkString("")
                case sig.NullaryMethodType(result) =>
                    "NullaryMethod\n" + maybeDumpType(result)
                case sig.PolyType(ref, syms) =>
                    "Poly[" + syms.map(_.name).mkString(", ") + "]\n" + maybeDumpType(ref)
                case sig.PolyTypeWithCons(ref, syms, cons) =>
                    "PolyWithCons \"" + cons + "\" [" + syms.mkString(", ") + "]\n" + maybeDumpType(ref)
                case sig.AnnotatedType(ref, treeRefs) =>
                    "Annotated (" + treeRefs.mkString(", ") + ")\n" + maybeDumpType(ref)
                case sig.AnnotatedWithSelfType(ref, sym, treeRefs) =>
                    "AnnotatedWithSelf (" + treeRefs.mkString(", ") + ")\n" + maybeDumpSymbol(sym) + maybeDumpType(ref)
                case sig.DeBruijnIndexType(level, index) =>
                    "DeBruijn(" + level + ", " + index + ")"
                case sig.ExistentialType(ref, syms) =>
                    "Existential[" + syms.map(_.name).mkString(", ") + "]\n" + maybeDumpType(ref)
            }
        )
    }

    def resolveSymbol(in: sig.Symbol): Result[sig.Symbol] =
        in match {
            case esym: sig.ExternalSymbol =>
                val name = NameR(esym)
                // this bit is an awful hack. I don't know from the ExternalSymbol scalap gives me what bits are enclosing
                // objects/classes/types and which are legitimate packages.
                // so, just guess a lot and hope it all turns out well!

                val toTry = name.permutations flatMap { n =>
                    n.copy(isCompanion=true) :: n :: n.enclosure.copy(isCompanion=true) :: n.enclosure :: Nil
                }

                firstOrLast(toTry)(tryName => classByName[AnyRef](classLoader, tryName.encoded)).flatMap { clazz =>
                    sigFor(clazz).flatMap { sig =>
                        sig.symbols.find { sym => pathOf(sym) == name.qualified }.toResult.whenFailed("did not find target " + esym)
                    } whenFailed ("failed to locate external symbol " + name + " in " + clazz)
                } orElse Okay(esym) // will fail if a package or java class, so just yield the original symbol if external resolution fails

            case other =>
                Okay(other)
        }

    def resolveType(in: sig.Type): Result[sig.Type] = {
        def resolveTypeRef(ref: sig.TypeRefType, bindings: Map[String, sig.Type]): Result[sig.TypeRefType] = {
            /*
            println("========== resolving type ref with " + bindings)
            println(dumpType(0, { case Right(_: sig.ClassInfoType) => true }, ref))
            */

            val sig.TypeRefType(prefix, unresolvedSymbol, args) = ref
            args.mapResult {
                case sig.TypeRefType(_, tsym: sig.TypeSymbol, _) =>
                    bindings.get(tsym.name).toResult whenFailed ("unknown type symbol binding " + tsym.name)
                case other => Okay(other)
            } flatMap { boundArgs =>
                resolveSymbol(unresolvedSymbol) flatMap {
                    case sisym: sig.SymbolInfoSymbol =>
                        sisym.infoType match {
                            case sig.TypeRefType(innerPrefix, innerSym, innerArgs) =>
                                Okay(sig.TypeRefType(innerPrefix, innerSym, innerArgs ++ boundArgs /* FIXME this curries, is that sensible? */))

                            case clazz: sig.ClassInfoType =>
                                Okay(sig.TypeRefType(prefix, sisym, boundArgs))

                            case poly@sig.PolyType(innerRef: sig.TypeRefType, symbols) =>
                                val innerBindings = Map(((symbols zip boundArgs) map { case (sym, arg) => sym.name -> arg }): _*)
                                resolveTypeRef(innerRef, innerBindings)
                                    .whenFailed("failed to resolve type at " + in)

                            case poly@sig.PolyType(inner, symbols) =>
                                Okay(sig.TypeRefType(prefix, sisym, boundArgs))

                            case other if args.isEmpty =>
                                Okay(sig.TypeRefType(prefix, sisym, boundArgs))

                            case other if !args.isEmpty =>
                                Failed("expected a TypeRef with arguments to apply to a Poly, but was " + other)

                            case other =>
                                Okay(sig.TypeRefType(prefix, sisym, boundArgs))
                        }

                    case other =>
                        Okay(ref)
                }
            }
        }

        in match {
            case ref: sig.TypeRefType =>
                resolveTypeRef(ref, Map.empty)

            case sig.PolyType(ref: sig.TypeRefType, symbols) =>
                resolveTypeRef(ref, Map.empty)

            case existential@sig.ExistentialType(inner, _) =>
                Failed("saw an existential and got scared")

            case other =>
                Okay(other)
        }
    }

    /** Make a TypeR from a reflective type and a signature type if present */
    def typeRFor(reflectionType: reflect.Type, signatureType: Result[sig.Type]): Result[TypeR] =
        (reflectionType, signatureType flatMap resolveType) match {
            /*
             * discard the erased primitive type in favor of the boxed equivalent in cases where the scala compliler
             * has discarded the type information to make signatures mostly work. see http://www.scala-lang.org/node/9157
             */
            case (ObjectClass, Okay(PrimitiveSigType(which))) => which match {
                case "Boolean" => Okay(ClassTypeR(NameR("java.lang.Boolean"  ),     classOf[java.lang.Boolean      ], signatureType))
                case "Byte"    => Okay(ClassTypeR(NameR("java.lang.Byte"     ),     classOf[java.lang.Byte         ], signatureType))
                case "Char"    => Okay(ClassTypeR(NameR("java.lang.Character"),     classOf[java.lang.Character    ], signatureType))
                case "Double"  => Okay(ClassTypeR(NameR("java.lang.Double"   ),     classOf[java.lang.Double       ], signatureType))
                case "Float"   => Okay(ClassTypeR(NameR("java.lang.Float"    ),     classOf[java.lang.Float        ], signatureType))
                case "Int"     => Okay(ClassTypeR(NameR("java.lang.Integer"  ),     classOf[java.lang.Integer      ], signatureType))
                case "Long"    => Okay(ClassTypeR(NameR("java.lang.Long"     ),     classOf[java.lang.Long         ], signatureType))
                case "Short"   => Okay(ClassTypeR(NameR("java.lang.Short"    ),     classOf[java.lang.Short        ], signatureType))
                case "Unit"    => Okay(ClassTypeR(NameR("scala.runtime.BoxedUnit"), classOf[scala.runtime.BoxedUnit], signatureType))
                case _ => sys.error("unknown primitive type " + which)
            }

            case (_, Okay(sig.ExistentialType(inner, _))) =>
                // skip over existential quantification bindings, as we don't model them and otherwise they'd throw off the tree walk
                typeRFor(reflectionType, Okay(inner))

            case (_, Okay(sig.PolyType(inner, _))) =>
                // skip over poly types, for the same reasons
                typeRFor(reflectionType, Okay(inner))

            case (c: Class[_], _) => Okay(ClassTypeR(NameR(c.getName), c, signatureType))

            case (pt: reflect.ParameterizedType, Okay(sig.TypeRefType(_, symbol, args))) =>
                for {
                    headTypeR <-
                        typeRFor(pt.getRawType, Failed("no signature type for parameterized types")/* FIXME? */)
                            .whenFailed("Failed to model parameterized type with head " + pt.getRawType.toString)

                    argTypeRs <- wrapRefArray(pt.getActualTypeArguments.zip(args).zipWithIndex).mapResult {
                        case ((refTyArg, sigTyArg), index) =>
                            typeRFor(refTyArg, Okay(sigTyArg)) whenFailed (
                                "Failed to model argument " + index + " of parameterized type with head " +
                                pt.getRawType.toString + ": (" + refTyArg + ", " + sigTyArg + ")"
                            )
                    }
                } yield ParameterizedTypeR(headTypeR, argTypeRs.toList, pt, signatureType)

            case (pt: reflect.ParameterizedType, resolvedSigType) =>
                signatureType foreach { sigtype =>
                    logger.warn("Reflective type hierarchy and signature type hierarchy processing have diverged:")
                    logger.warn("Reflective type: " + pt  + " (is a " + pt.getClass + ")")
                    logger.warn("Signature type before resolution: " + sigtype + " (is a " + sigtype.getClass + ")")
                    logger.warn("Signature type after resolution: " + resolvedSigType)
                }

                for {
                    headTypeR <-
                        typeRFor(pt.getRawType, Failed("no signature type for parameterized types")/* FIXME? */)
                            .whenFailed("Failed to model parameterized type with head " + pt.getRawType.toString)

                    argTypeRs <- wrapRefArray(pt.getActualTypeArguments.zipWithIndex).mapResult {
                        case (refTyArg, index) =>
                            typeRFor(refTyArg, Failed("no signature type")) whenFailed (
                                "Failed to model argument " + index + " of parameterized type with head " +
                                pt.getRawType.toString + ": " + refTyArg
                            )
                    }
                } yield ParameterizedTypeR(headTypeR, argTypeRs.toList, pt, signatureType)

            case (wt: reflect.WildcardType, _) =>
                /*
                 * punt on deducing the type bounds of a wildcard type for the moment. following this in the scala side would be a hideous nightmare
                 * due to the existential quantification being outside of the TypeRefType, rather than inside like the java side. hopefully we'll not
                 * need this.
                 */
                Okay(WildcardTypeR(wt, signatureType))

            case _ =>
                logger.trace("Punting on trying to refine (" + reflectionType + ", " + signatureType + ")")
                Okay(OtherTypeR(reflectionType, signatureType))
        }

    /** Get the scala signature for a given class, using the cache if the signature is already cached */
    def sigFor(clazz: Class[_]): Result[sig.ScalaSig] =
        sigCache.get(clazz) getOrElse loadSig(clazz).pass(sigResult => sigCache += (clazz -> sigResult))

    /** Obtain the ClassR for the given Class from the cache or construct a ClassR and return it */
    def classRFor(clazz: Class[_]): Result[ClassR] =
        classCache.get(clazz) getOrElse makeClassR(clazz).pass(classRResult => classCache += (clazz -> classRResult))

    /** Helper method that does the heavy lifting of finding and loading a scala signature for a class */
    private def loadSig(clazz: Class[_]): Result[sig.ScalaSig] = {
        import sig.{ByteCode, ClassFileParser, ScalaSigAttributeParsers, StringBytesPair}

        import ClassFileParser.{Annotation, AnnotationElement, ConstValueIndex}

        val clazzName = NameR(clazz.getName)
        val sigLocationName = NameR(clazzName.pkg, Nil, clazzName.nonPkgs.headOption.getOrElse(clazzName.local), false)
        for {
            signatureClassBytes <- try {
                clazz.getClassLoader.getResourceAsStream(sigLocationName.classFilePath) match {
                    case null => Failed("Class " + sigLocationName.encoded + " could not be read from " +
                                        sigLocationName.classFilePath + " (looking for signature of " + clazzName.qualified + ")")
                    case stream => Okay(withResource(stream)(readBytes))
                }
            } catch {
                case e: Exception =>
                    Failed("Failed to read class " + sigLocationName.encoded +
                           " (looking for signature of " + clazzName.qualified + ")")
            }

            signatureClassFile <- (
                catchingException(ClassFileParser.parse(ByteCode(signatureClassBytes))) whenFailed (
                    "Failed to parse class " + sigLocationName.encoded +
                    " (looking for signature of " + clazzName.qualified + ")"
                )
            )

            signatureAnnotation <- (
                catchingException(signatureClassFile.annotation("Lscala/reflect/ScalaSignature;")) whenFailed (
                    "Failed to parse ScalaSignature annotation from " + sigLocationName.encoded +
                     " (looking for signature of " + clazzName.qualified + ")"
                )
            ) match {
                case Okay(Some(annot)) => Okay(annot)
                case Okay(None) =>
                    Failed("ScalaSignature annotation not found on " + sigLocationName.encoded +
                           " (looking for signature of " + clazzName.qualified + ")")
                case failure =>
                    failure.asA[Annotation]
            }

            annotationConstValue <- signatureAnnotation.elementValuePairs.headOption match {
                case Some(AnnotationElement(_, ConstValueIndex(idx))) => catchingException(signatureClassFile.constantWrapped(idx))
                case Some(_) =>
                    Failed("ScalaSignature annotation from " + sigLocationName.encoded + " does not have a " +
                            "ConstValueIndex as its element value (looking for signature of " + clazzName.qualified + ")")
                case None =>
                    Failed("ScalaSignature annotation from " + sigLocationName.encoded + " does not any  " +
                            "element values (looking for signature of " + clazzName.qualified + ")")
            }

            rawBytes <- annotationConstValue match {
                case StringBytesPair(_, bytes) => Okay(bytes)
                case _ =>
                    Failed("ScalaSignature annotation from " + sigLocationName.encoded + " does not any  " +
                            "element values (looking for signature of " + clazzName.qualified + ")")
            }

            length <- catchingException(ByteCodecs.decode(rawBytes)) // mutates rawBytes!
            sig    <- catchingException(ScalaSigAttributeParsers.parse(ByteCode(rawBytes.take(length))))
        } yield sig
    }

    /** Get the ClassSymbol for a Class token from the signature */
    private def classSymbolFor(clazz: Class[_], signature: sig.ScalaSig): Result[sig.ClassSymbol] = {
        val nameR = NameR(clazz.getName)
        val qualified = nameR.qualified
        signature.symbols.collect {
            case csym: sig.ClassSymbol if pathOf(csym) == qualified && csym.isModule == nameR.isCompanion => csym
        } match {
            case csym :: Nil => Okay(csym)
            case Nil         => Failed("could not find class symbol for " + clazz.getName)
            case csyms       => Failed("found more than one class symbol for " + clazz.getName + "??? got " + csyms.mkString(", "))
        }
    }

    /**
     * Helper method that breaks a method name for a java bean accessor into (isSetter, accessorName), but only if it appears to be a java bean accessor
     * name in the first place.
     *
     * For example:
     *     parseJavaBeanAccessorName("getFoo") -> Some((false, "foo"))
     *     parseJavaBeanAccessorName("setFoo") -> Some((true, "foo"))
     *     parseJavaBeanAccessorName("isFoo") -> Some((false, "foo"))
     *     parseJavaBeanAccessorName("foo") -> None
     */
    private def parseJavaBeanAccessorName(in: String): Option[(Boolean, String)] =
        (in match {
            case _ if in.size <= 2 => None
            case _ if in.startsWith("is") && Character.isUpperCase(in.charAt(2)) =>
                Some((false, Character.toLowerCase(in.charAt(2)) + in.substring(3)))

            case _ if in.size <= 3 || !Character.isUpperCase(in.charAt(3)) => None
            case _ if in.startsWith("get") => Some((false, in.substring(3)))
            case _ if in.startsWith("set") => Some((true,  in.substring(3)))

            case _ => None
        }).map { case (isSetter, name) => (isSetter, Character.toLowerCase(name.charAt(0)) + name.substring(1)) }

    /**
     * Combine a list of ClassMemberRs for a ClassR being generated with those of its superclass and interfaces, with conflicts being resolved towards
     * the left, e.g. current class members win over superclass members which win over each interfaces' members in turn.
     */
    private def combineMemberRs(localMemRs: List[ClassMemberR], superClassR: Option[ClassR], interfaceClassRs: Iterable[ClassR]): List[ClassMemberR] = {
        val candidateMemRs = new ArrayBuffer[ClassMemberR]
        candidateMemRs ++= localMemRs
        superClassR.foreach(candidateMemRs ++= _.members)
        interfaceClassRs.foreach(candidateMemRs ++= _.members)
        // FIXME? this scans the list being built for each new member, so is certainly not appropriate for huge lists of members
        candidateMemRs.foldLeft(Nil: List[ClassMemberR])((memRs, memR) => if (memRs.exists(_.canOverride(memR))) memRs else memR :: memRs).sortWith {
            (a, b) => (a, b) match {
                case (cra: ConstructorR, crb: ConstructorR) => cra.parameters.length < crb.parameters.length
                case (_: ConstructorR, _) => true

                case (pra: PropertyR, prb: PropertyR) => pra.name < prb.name
                case (_: PropertyR, _: ConstructorR) => false
                case (_: PropertyR, _              ) => true

                case (mra: MethodR, mrb: MethodR) if mra.name != mrb.name => mra.name < mrb.name
                case (mra: MethodR, mrb: MethodR) => mra.parameters.length < mrb.parameters.length
                case (_  : MethodR, _           ) => false
            }
        }
    }

    /** Find the reflection Method for a given MethodSymbol */
    private def methodForMethodSymbol(clazz: Class[_], msym: sig.MethodSymbol): Result[reflect.Method] =
        for {
            ty <- catchingException(msym.infoType).whenFailed("failed to get info for " + msym.name + " in " + clazz.getName)

            meth <- clazz.getDeclaredMethods.filter(meth => meth.getName == msym.name && meth.getParameterTypes.length == msym.children.length) match {
                case Array() => Failed("could not find method for " + msym + " with type " + ty + " in " + clazz.getName)
                case meths if areMethodsCompatible(meths) => mostSpecificMethod(meths).toResult whenFailed "could not determine most specific method"
                case _ => Failed("found more than one matching method for " + msym + " with type " + ty + " in " + clazz.getName +
                                  " and the matches are not compatible -- overloads are not presently resolved correctly")
            }
        } yield meth

    /** Find the reflection Constructor for a given MethodSymbol */
    private def constructorForMethodSymbol(clazz: Class[_], msym: sig.MethodSymbol): Result[reflect.Constructor[_]] = {
        def publicCtor(ctor: reflect.Constructor[_]): Boolean = reflect.Modifier.isPublic(ctor.getModifiers)
        clazz.getDeclaredConstructors.filter(ctor => ctor.getParameterTypes.length == msym.children.length) match {
            case Array() => Failed("could not find constructor for " + msym + " in " + clazz.getName)
            case ctors if !ctors.filter(publicCtor).isEmpty && areConstructorsCompatible(ctors.filter(publicCtor)) =>
                mostSpecificConstructor(ctors.filter(publicCtor)).toResult whenFailed "could not determine most specific (public) constructor"
            case ctors if areConstructorsCompatible(ctors) =>
                mostSpecificConstructor(ctors).toResult whenFailed "could not determine most specific constructor"
            case _ => Failed("found more than one matching constructor for " + msym + " in " + clazz.getName +
                              " and the matches are not compatible -- overloads are not presently resolved correctly")
        }
    }

    /** Small ADT to represent either a Method or Constructor */
    sealed abstract class MethodOrConstructor {
        def getGenericParameterTypes: Array[reflect.Type]
        def getParameterAnnotations: Array[Array[JavaAnnotation]]
        def paranamerNames: Result[Iterable[String]]
    }

    final case class WrapMethod(meth: reflect.Method) extends MethodOrConstructor {
        def getGenericParameterTypes = meth.getGenericParameterTypes
        def getParameterAnnotations = meth.getParameterAnnotations
        def paranamerNames = catchingException(paranamer.lookupParameterNames(meth))
    }

    final case class WrapConstructor(ctor: reflect.Constructor[_]) extends MethodOrConstructor {
        def getGenericParameterTypes = ctor.getGenericParameterTypes
        def getParameterAnnotations = ctor.getParameterAnnotations
        def paranamerNames = catchingException(paranamer.lookupParameterNames(ctor))
    }

    /** Helper method used when building class member models to report errors that will cause something being modelled to be ignored */
    private def reportModelBuildingFailed[A](clazz: Class[_], f: A => String)(in: A, failed: Failed): Unit =
        failed match {
            case _ =>
                logger.traceResult (
                    "Failed to build reflection model for " + f(in) + " of class " + clazz.getName + " -- ignoring",
                    failed
                )
        }

    /** Make a list of ParameterRs for a method based on all information at hand */
    private def makeParameterRs(clazz: Class[_], meth: MethodOrConstructor, msymResult: Result[sig.MethodSymbol]): Result[List[ParameterR]] = {
        val genericParameterTypes: Array[reflect.Type]  = meth.getGenericParameterTypes
        val parameterAnnotations: Array[Array[JavaAnnotation]] = meth.getParameterAnnotations
        val sigParamInfo: Result[List[(String, sig.Type)]] =
            msymResult.flatMap(_.children.mapResult {
                psym => catchingException(psym.asInstanceOf[sig.MethodSymbol].infoType).map(sigType => (psym.name, sigType))
            })

        val parameterNamesResult =
            (meth.paranamerNames orElse sigParamInfo.map(_.map(_._1)))
                .whenFailed("Failed to determine parameter names")

        wrapRefArray(genericParameterTypes).leftJoin(sigParamInfo).zipWithIndex.mapResult {
            case ((genTy, sigTyResult), index) =>
                typeRFor(genTy, sigTyResult.map(_._2)) whenFailed ("Failed to model type of parameter " + index)
        }.map(paramTypeRs => {
            wrapRefArray(parameterAnnotations.zip(paramTypeRs)).leftJoin(parameterNamesResult).map {
                case ((annotations, typeR), nameResult) => ParameterR(nameResult.toOption, typeR, annotations.toList)
            }.toList
        })
    }

    /** Make a MethodR using all available information at hand */
    private def makeMethodR(ownerNameR: NameR, clazz: Class[_], meth: reflect.Method, msymResult: Result[sig.MethodSymbol]): Result[MethodR] =
        for {
            paramRs <-
                makeParameterRs(clazz, WrapMethod(meth), msymResult)
                    .whenFailed("Failed to model parameters for method " + meth + " of " + clazz.getName)
            resultTypeR <- typeRFor (
                meth.getGenericReturnType,
                msymResult.flatMap {
                    msym => catchingException {
                        msym.infoType match {
                            case sig.PolyType(sig.MethodType(resultType, _), _) => resultType
                            // it may be that as of 2.9 NullaryMethodType has replaced PolyType(MethodType(...)), but I don't know for sure
                            case sig.NullaryMethodType(resultType) => resultType
                            case sig.MethodType(resultType, _) => resultType
                            case (pt: sig.PolyType) => pt
                            case other => sys.error("don't know what to do with method typed as " + other)
                        }
                    }
                }
            ) whenFailed ("Failed to model return type of method " + meth + " of " + clazz.getName)
        } yield MethodR(
            ownerName       = ownerNameR,
            name            = meth.getName,
            isPublic        = reflect.Modifier.isPublic(meth.getModifiers),
            annotations     = meth.getAnnotations.toList,
            parameters      = paramRs,
            result          = resultTypeR,
            reflectionModel = meth,
            signatureModel  = msymResult
        )

    /** Helper method that does the main work of building the member list of a ClassR, using the scala signature. */
    private def makeMemberRsUsingSig(clazz: Class[_], csym: sig.ClassSymbol): List[ClassMemberR] = {
        val ownerNameR = NameR(clazz.getName)

        /*
         * Partition the MethodSymbol children of the ClassSymbol into three groups:
         *   accessor symbols, along with the deconstructed name.
         *   constructor symbols, if the name of the method symbol is <init>
         *   method symbols for all remaining symbols.
         *
         * We only consider method symbols and don't consider synthetically generated symbols (such as copy defaults, etc)
         */
        val (accessorSymInfo, constructorSyms, methodSyms) = {
            var as: List[(sig.MethodSymbol, Boolean, String)] = Nil
            var cs: List[sig.MethodSymbol] = Nil
            var ms: List[sig.MethodSymbol] = Nil

            /* there used to be a very handy synthetic method flag that tracked this, but it was eliminated in https://lampsvn.epfl.ch/trac/scala/changeset/23668 */
            def isIgnorableMethodName(name: String): Boolean = name match {
                case "canEqual"|"equals"|"hashCode"|"productArity"|"productElement"|"productPrefix"|"toString" if csym.isCase => true
                case "$init$" => true
                case _ => false
            }


            for (sym <- csym.children.collect {
                case ms: sig.MethodSymbol if !ms.isSynthetic && !isIgnorableMethodName(ms.name) && ms.isMethod => ms
            }) {
                parseJavaBeanAccessorName(sym.name) match {
                    case _ if sym.isAccessor       => as ::= ((sym, sym.name.endsWith("_$eq"), sym.name.stripSuffix("_$eq")))
                    case Some((isSetter, name))    => as ::= ((sym, isSetter, name))
                    case _ if sym.name == "<init>" => cs ::= sym
                    case _                         => ms ::= sym
                }
            }

            (as, cs, ms)
        }

        val accessorSymInfoByProperty =
            accessorSymInfo.foldLeft(Map.empty[String, List[(sig.MethodSymbol, Boolean)]].withDefaultValue(Nil))((m, cur) => cur match {
                case (sym, isSetter, name) => m + (name -> ((sym, isSetter) :: m(name)))
            })

        // go through each supposed property and either build Right(PropertyR) or Left(symbols that must be methods since they are not properties)
        val (orphanedSetterSyms, propertyRs) =
            accessorSymInfoByProperty.flattenResult(reportModelBuildingFailed(clazz, "property " + _._1))(tup => {
                val (name, unsortedSyms) = tup

                // sort the symbols so we'll lean towards public accessors, if there's a mix
                val sortedSyms = unsortedSyms.sortWith(
                    (a, b) => (!(a._1.isPrivate || a._1.isProtected), !(b._1.isPrivate || b._1.isProtected), a._1.name, b._1.name) match {
                        case (true, false, _, _) => true // public methods go first
                        case (false, true, _, _) => false // reflexive
                        case (_, _, a, b) if (a == name || a == name + "_$eq") && b != name && b != name + "_$eq" => true // scala accessors first
                        case (_, _, a, b) if (b == name || b == name + "_$eq") && a != name && a != name + "_$eq" => false // reflexive
                        case (_, _, a, b) => a < b
                    }
                )

                sortedSyms.collectFirst { case (sym, isSetter) if !isSetter => sym } match {
                    case Some(getterSym) =>
                        for {
                            getterMeth  <- methodForMethodSymbol(clazz, getterSym)
                            getterMethR <- makeMethodR(ownerNameR, clazz, getterMeth, Okay(getterSym))
                        } yield {
                            val setterMethRResult = for {
                                setterSym   <- sortedSyms.find { case (_, isSetter) => isSetter }.map { case (sym, _) => sym }.toResult
                                                   .whenFailed("no setter found")
                                setterMeth  <- methodForMethodSymbol(clazz, setterSym)
                                setterMethR <- makeMethodR(ownerNameR, clazz, setterMeth, Okay(setterSym))
                            } yield setterMethR

                            val fieldAnnotations: List[JavaAnnotation] =
                                catchingException(clazz.getDeclaredField(name).getAnnotations.toList) getOrElse Nil

                            val otherMethRs = (
                                sortedSyms
                                .map { case (sym, _) => sym }
                                .filter(sym => sym != getterSym && setterMethRResult.map(sym != _.signatureModel.orThrow).getOrElse(true))
                                .map(
                                    sym => methodForMethodSymbol(clazz, sym).flatMap(makeMethodR(ownerNameR, clazz, _, Okay(sym)))
                                )
                                .flatten
                                .sortBy(_.name)
                            )

                            Right(PropertyR(
                                ownerName   = ownerNameR,
                                name        = name,
                                isPublic    = getterMethR.isPublic,
                                typeR       = getterMethR.result,
                                annotations = getterMethR.annotations ++ fieldAnnotations,
                                getter      = getterMethR,
                                setter      = setterMethRResult,
                                others      = otherMethRs
                            ))
                        }

                    case None =>
                        Okay(Left(unsortedSyms.map(_._1))) // orphaned setters should be processed as methods
                }
            }).partitionEither match { case (a, b) => (a.flatten, b) }

        val constructorRs = constructorSyms.flattenResult(reportModelBuildingFailed(clazz, _ => "constructor")) {
            msym =>
            for {
                ctor <- constructorForMethodSymbol(clazz, msym)
                paramRs <- makeParameterRs(clazz, WrapConstructor(ctor), Okay(msym))
            } yield ConstructorR (
                ownerName       = ownerNameR,
                isPublic        = reflect.Modifier.isPublic(ctor.getModifiers),
                annotations     = ctor.getAnnotations.toList,
                parameters      = paramRs,
                reflectionModel = ctor,
                signatureModel  = Okay(msym)
            )
        }

        val methodRs = (methodSyms ++ orphanedSetterSyms).flattenResult(reportModelBuildingFailed(clazz, "method " + _.name)) {
            msym => methodForMethodSymbol(clazz, msym).flatMap(makeMethodR(ownerNameR, clazz, _, Okay(msym)))
        }

        constructorRs.toList ++ propertyRs.toList ++ methodRs.toList
    }

    /** Make a ClassR for a Class using a scala signature */
    private def makeClassRUsingSig(clazz: Class[_], signature: sig.ScalaSig, superClassR: Option[ClassR], interfaceClassRs: Iterable[ClassR]): Result[ClassR] =
        classSymbolFor(clazz, signature).map {
            csym =>
            ClassR (
                name            = NameR(clazz.getName),
                superClass      = superClassR,
                interfaces      = interfaceClassRs.toList,
                members         = combineMemberRs(makeMemberRsUsingSig(clazz, csym), superClassR, interfaceClassRs),
                reflectionModel = clazz,
                signatureModel  = Okay(csym)
            )
        }

    /** Helper method that does the main work of building the mbmer list of a ClassR, using only reflection (when the scala signature is not present) */
    private def makeMemberRsUsingReflectionOnly(clazz: Class[_]): List[ClassMemberR] = {
        val ownerNameR = NameR(clazz.getName)

        /* Partition methods defined by this class into those that look like accessors and those that look like other methods */
        val (accessorMethods, otherMethods) = {
            var ams: List[(reflect.Method, Boolean, String)] = Nil
            var oms: List[reflect.Method] = Nil

            for (meth <- clazz.getDeclaredMethods) {
                parseJavaBeanAccessorName(meth.getName) match {
                    case Some((isSetter, name)) => ams ::= ((meth, isSetter, name))
                    case _ => oms ::= meth
                }
            }

            (ams, oms)
        }

        val propertyRs = accessorMethods.flatMap {
            case (_, true, _) => None // we'll treat setters when processing their associated getters
            case (getterMeth, _, name) => {
                val setterMethResult: Result[reflect.Method] = accessorMethods.find {
                    case (_, true, n) if name == n => true
                    case _ => false
                }.map(_._1).toResult whenFailed ("no setter method found")

                val fieldAnnotations = catchingException(clazz.getDeclaredField(name).getAnnotations.toList) getOrElse Nil

                makeMethodR(ownerNameR, clazz, getterMeth, Failed("no method symbol when using reflection only")) match {
                    case Okay(getterMethR) =>
                        Some(PropertyR (
                            ownerName   = ownerNameR,
                            name        = name,
                            isPublic    = getterMethR.isPublic,
                            typeR       = getterMethR.result,
                            annotations = getterMethR.annotations ++ fieldAnnotations,
                            getter      = getterMethR,
                            setter      = setterMethResult.flatMap(makeMethodR(ownerNameR, clazz, _,
                                              Failed("no method symbol when using reflection only"))),
                            others      = Nil
                        ))

                    case failed@FailedG(_, _) =>
                        reportModelBuildingFailed[Unit](clazz, _ => "property " + name)((), failed)
                        None
                }
            }
        }

        val orphanedSetters = (
            accessorMethods
            .collect { case (meth, isSetter, _) if isSetter => meth }
            .filterNot {
                meth => propertyRs.exists {
                    propR => meth == propR.getter.reflectionModel || (propR.setter.map(meth == _.reflectionModel) getOrElse false)
                }
            }
        )

        val constructorRs = clazz.getConstructors.toList.flattenResult(reportModelBuildingFailed(clazz, _ => "constructor")) {
            ctor => makeParameterRs(clazz, WrapConstructor(ctor), Failed("no class symbol when using reflection only")).map(paramRs => ConstructorR (
                ownerName       = ownerNameR,
                isPublic        = reflect.Modifier.isPublic(ctor.getModifiers),
                annotations     = ctor.getAnnotations.toList,
                parameters      = paramRs,
                reflectionModel = ctor,
                signatureModel  = Failed("no method symbol when using reflection only")
            ))
        }

        val methodRs =
            (otherMethods ::: orphanedSetters).flattenResult(reportModelBuildingFailed(clazz, "method " + _.getName)) {
                meth => makeMethodR(ownerNameR, clazz, meth, Failed("no method symbol when using reflection only"))
            }

        constructorRs.toList ++ propertyRs.toList ++ methodRs.toList
    }

    /** Make a ClassR for a Class using only reflection */
    private def makeClassRUsingReflectionOnly(clazz: Class[_], superClassR: Option[ClassR], interfaceClassRs: Iterable[ClassR]): Result[ClassR] =
        Okay(ClassR(
            name            = NameR(clazz.getName),
            superClass      = superClassR,
            interfaces      = interfaceClassRs.toList,
            members         = combineMemberRs(makeMemberRsUsingReflectionOnly(clazz), superClassR, interfaceClassRs),
            reflectionModel = clazz,
            signatureModel  = Failed("no class symbol when using reflection only")
        ))

    /**
     * Make a ClassR for the given class by getting as much information about the class from reflection and scala signature as possible, and recursing
     * for supertypes to classRFor
     */
    private def makeClassR(clazz: Class[_]): Result[ClassR] = {
        val nameR = NameR(clazz.getName)
        for {
            superClassR <- clazz.getSuperclass match {
                case null => Okay(None)
                case sc => classRFor(sc).map(Some.apply _)
            }

            interfaceClassRs <- wrapRefArray(clazz.getInterfaces).mapResult(classRFor)

            classR <- sigFor(clazz) match {
                case Okay(sig) =>
                    makeClassRUsingSig(clazz, sig, superClassR, interfaceClassRs)
                        .whenFailed("Failed to make ClassR for " + clazz.getName + " using signature")
                case _ =>
                    makeClassRUsingReflectionOnly(clazz, superClassR, interfaceClassRs)
                        .whenFailed("Failed to make ClassR for " + clazz.getName + " using reflection only")
            }
        } yield classR
    }
}
