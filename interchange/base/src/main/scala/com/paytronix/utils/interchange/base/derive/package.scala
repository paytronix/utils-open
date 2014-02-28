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

import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.Universe
import scala.reflect.macros.whitebox.Context

import scalaz.NonEmptyList

import com.paytronix.utils.interchange.base

import NonEmptyList.{nel, nels}
import utils.{
    Property, SimplifiedDeriveAnnotation, Structure,
    addToCompanion, augmentImpl, sequenceResultBindings, simplifyDeriveAnnotation, structureOf
}

/**
 * Trait which implements a family of macros used to generate various coders for some format.
 *
 * Each format implements an object extending this trait and so gets appropriate macros at a lower
 * implementation cost.
 */
trait DeriveCoderMacros {
    /** Type of coders for the given type */
    type Coder[A]

    /**
     * The name to use when attaching implicitly derived coders to companion objects, e.g. `avroCoder` or `jacksonCoder`.
     * To avoid confusion and madness this should always return some constant name, but has to be a function because macros.
     */
    def implicitCoderName(c: Context): c.universe.TermName

    /** Type tree of coder pairs of the given type name */
    def coderType(c: Context)(tpe: c.universe.Tree): c.universe.Tree

    /** Materializer for coder pairs of the given type */
    def materializeCoder(c: Context)(tpe: c.universe.Type, annotations: List[c.universe.Annotation]): c.universe.Tree

    /** Type of encoders of the given type */
    type Encoder[A]

    /** Type tree of encoders of the given type name */
    def encoderType(c: Context)(tpe: c.universe.Tree): c.universe.Tree

    /** Materializer for encoders of the given type */
    def materializeEncoder(c: Context)(tpe: c.universe.Type, annotations: List[c.universe.Annotation]): c.universe.Tree

    /** Type of decoders of the given type */
    type Decoder[A]

    /** Type tree of decoders of the given type name */
    def decoderType(c: Context)(tpe: c.universe.Tree): c.universe.Tree

    /** Materializer of decoders of the given type */
    def materializeDecoder(c: Context)(tpe: c.universe.Type, annotations: List[c.universe.Annotation]): c.universe.Tree

    /** Tree which evaluates to the front end of the macro package, for implicit structure coder pair implementation */
    def makeStructureCoder(c: Context)(target: c.universe.Tree): c.universe.Tree

    /** Generate methods for implementing the encoder type of a structure, giving the model and references to the subsidiary encoders to use */
    def structureEncoderMethods(c: Context)(tpe: c.universe.Type, encoderFor: Property[c.universe.type] => c.universe.Tree, model: Structure[c.universe.type]): List[c.universe.Tree]

    /** Generate methods for implementing the decoder type of a structure, giving the model and references to the subsidiary decoders to use */
    def structureDecoderMethods(c: Context)(tpe: c.universe.Type, decoderFor: Property[c.universe.type] => c.universe.Tree, model: Structure[c.universe.type]): List[c.universe.Tree]

    /** Tree which evaluates to the front end of the macro package, for implicit wrapper coder pair implementation */
    def makeWrapperCoder(c: Context)(target: c.universe.Tree): c.universe.Tree

    /** Generate methods for implementing the decoder type of a wrapper, given an unwrapping function and the single property of the wrapper */
    def wrapperEncoderMethods(c: Context)(targetType: c.universe.Type, property: Property[c.universe.type], unwrap: c.universe.Tree => c.universe.Tree): List[c.universe.Tree]

    /** Generate methods for implementing the decoder type of a wrapper, given a wrapping function and the single property of the wrapper */
    def wrapperDecoderMethods(c: Context)(targetType: c.universe.Type, property: Property[c.universe.type], wrap: c.universe.Tree => c.universe.Tree): List[c.universe.Tree]

    /** Tree which evaluates to the front end of the macro package, for implicit union coder pair implementation */
    def makeUnionCoder(c: Context)(target: c.universe.Tree, alts: Seq[c.universe.Tree]): c.universe.Tree

    /** Generate methods for implementing the decoder type of a union, given a set of subtypes along with names of decoders */
    def unionEncoderMethods(c: Context)(targetType: c.universe.Type, targetSubtypes: NonEmptyList[c.universe.Type], encoderFor: c.universe.Type => c.universe.Tree): List[c.universe.Tree]

    /** Generate methods for implementing the encoder type of a union, given a set of subtypes along with names of encoders */
    def unionDecoderMethods(c: Context)(targetType: c.universe.Type, targetSubtypes: NonEmptyList[c.universe.Type], decoderFor: c.universe.Type => c.universe.Tree): List[c.universe.Tree]

    /** Wrap a core expression which materializes or constructs a coder with additional wrapping based on annotations, used for implementing materialize* */
    def wrapCoderForAnnotations(c: Context) (
        coder: c.universe.Tree,
        annotations: List[c.universe.Annotation],
        nullable: c.universe.Tree => c.universe.Tree,
        default: c.universe.Tree => c.universe.Tree => c.universe.Tree
    ): c.universe.Tree = {
        import c.universe.{Annotation, typeOf}

        val nullableTpe = typeOf[com.paytronix.utils.interchange.base.nullable]
        val defaultTpe = typeOf[com.paytronix.utils.interchange.base.default]
        List (
            annotations.collectFirst { case annot if annot.tree.tpe =:= nullableTpe => annot }.map { _ => nullable },
            annotations.collectFirst { case annot if annot.tree.tpe =:= defaultTpe => annot.tree.children.tail.head }.map { valueTree => default(valueTree) }
        ).foldLeft(coder) { (inside, wrapper) =>
            wrapper match {
                case Some(f) => f(inside)
                case None    =>   inside
            }
        }
    }

    /**
     * Implement the deriveCoder annotation for structures, attaching an automatically derived structure
     * coder pair to the companion of the annotated class
     */
    def deriveImplicitStructureCoderAnnotation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
        addToCompanion(c)(annottees) { (targetName, _) =>
            import c.universe.{Ident, Quasiquote}
            List(q"implicit val ${implicitCoderName(c)}: ${coderType(c)(Ident(targetName))} = ${makeStructureCoder(c)(Ident(targetName))}")
        }

    /** Def macro implementation which derives a coder pair for a structure */
    def structureCoderDefImpl[A: c.WeakTypeTag](c: Context): c.Expr[Coder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName, weakTypeTag}
        val tpe = weakTypeTag[A].tpe
        val name = TermName(tpe.typeSymbol.name.decodedName.toString + "Coder")
        val model = structureOf(c)(tpe)

        val declareCoders = model.properties.flatMap { prop =>
            List (
                q"lazy val ${prop.decoderName} = ${materializeDecoder(c)(prop.tpe, prop.annotations)}",
                q"lazy val ${prop.encoderName} = ${materializeEncoder(c)(prop.tpe, prop.annotations)}"
            )
        }

        c.Expr[Coder[A]](q"""
            {
                object $name extends ${coderType(c)(tq"$tpe")} {
                    ..$declareCoders
                    object decode extends ${decoderType(c)(tq"$tpe")} {
                        ..${structureDecoderMethods(c)(tpe, p => Ident(p.decoderName), model)}
                    }
                    object encode extends ${encoderType(c)(tq"$tpe")} {
                        ..${structureEncoderMethods(c)(tpe, p => Ident(p.encoderName), model)}
                    }
                }
                $name
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /** Def macro implementation which derives a decoder for a structure */
    def structureDecoderDefImpl[A: c.WeakTypeTag](c: Context): c.Expr[Decoder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName, weakTypeTag}
        val tpe = weakTypeTag[A].tpe
        val name = TermName(tpe.typeSymbol.name.decodedName.toString + "Decoder")
        val model = structureOf(c)(tpe)

        val declareDecoders = model.properties.map { prop =>
            q"lazy val ${prop.decoderName} = ${materializeDecoder(c)(prop.tpe, prop.annotations)}"
        }

        c.Expr[Decoder[A]](q"""
            {
                object $name extends ${decoderType(c)(tq"$tpe")} {
                    ..$declareDecoders
                    ..${structureDecoderMethods(c)(tpe, p => Ident(p.decoderName), model)}
                }
                $name
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /** Def macro implementation which derives a encoder for a structure */
    def structureEncoderDefImpl[A: c.WeakTypeTag](c: Context): c.Expr[Encoder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName, weakTypeTag}
        val tpe = weakTypeTag[A].tpe
        val name = TermName(tpe.typeSymbol.name.decodedName.toString + "Encoder")
        val model = structureOf(c)(tpe)

        val declareEncoders = model.properties.map { prop =>
            q"lazy val ${prop.encoderName} = ${materializeEncoder(c)(prop.tpe, prop.annotations)}"
        }

        c.Expr[Encoder[A]](q"""
            {
                object $name extends ${encoderType(c)(tq"$tpe")} {
                    ..$declareEncoders
                    ..${structureEncoderMethods(c)(tpe, p => Ident(p.encoderName), model)}
                }
                $name
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /**
     * Annotation macro implementation which implements a coder pair for a structure where some fields can
     * use an explicit coder pair rather than one from implicit scope
     */
    def structureCoderAnnotation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = try {
        import c.universe.{DefDef, DefDefTag, Ident, Quasiquote, TermName, ValDef, ValDefTag}

        val SimplifiedDeriveAnnotation(targetType, _, _, implDef) = simplifyDeriveAnnotation(c)(annottees: _*)
        val model = structureOf(c)(targetType)

        val overridden = implDef.impl.body.collect {
            case ValDef(_, n, _, _)       if n.decodedName.toString.endsWith("Coder") => n.decodedName.toString.stripSuffix("Coder")
            case DefDef(_, n, _, _, _, _) if n.decodedName.toString.endsWith("Coder") => n.decodedName.toString.stripSuffix("Coder")
        }

        val declareDefaultCoders = model.properties.filterNot(overridden contains _.internalName).map { prop =>
            q"""
                lazy val ${prop.coderName} = ${materializeCoder(c)(prop.tpe, prop.annotations)}
            """
        }

        val newStats = declareDefaultCoders ++ List (
            q"""
                object encode extends ${encoderType(c)(tq"$targetType")} {
                    ..${structureEncoderMethods(c)(targetType, p => q"${p.coderName}.encode", model)}
                }
            """,
            q"""
                object decode extends ${decoderType(c)(tq"$targetType")} {
                    ..${structureDecoderMethods(c)(targetType, p => q"${p.coderName}.decode", model)}
                }
            """
        )

        c.Expr[Any](augmentImpl(c)(implDef, List(coderType(c)(tq"$targetType")), newStats))
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /**
     * Annotation macro implementation which implements an encoder for a structure where some fields can
     * use an explicit encoder rather than one from implicit scope
     */
    def structureEncoderAnnotation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = try {
        import c.universe.{DefDef, DefDefTag, Ident, Quasiquote, TermName, ValDef, ValDefTag}

        val SimplifiedDeriveAnnotation(targetType, _, _, implDef) = simplifyDeriveAnnotation(c)(annottees: _*)
        val model = structureOf(c)(targetType)

        val overridden = implDef.impl.body.collect {
            case ValDef(_, n, _, _)       if n.decodedName.toString.endsWith("Encoder") => n.decodedName.toString.stripSuffix("Encoder")
            case DefDef(_, n, _, _, _, _) if n.decodedName.toString.endsWith("Encoder") => n.decodedName.toString.stripSuffix("Encoder")
        }

        val declareDefaultEncoders = model.properties.filterNot(overridden contains _.internalName).map { prop =>
            q"""
                lazy val ${prop.encoderName} = ${materializeEncoder(c)(prop.tpe, prop.annotations)}
            """
        }

        val newStats = declareDefaultEncoders ++ structureEncoderMethods(c)(targetType, p => Ident(p.encoderName), model)

        c.Expr[Any](augmentImpl(c)(implDef, List(encoderType(c)(tq"$targetType")), newStats))
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /**
     * Annotation macro implementation which implements a decoder for a structure where some fields can
     * use an explicit decoder rather than one from implicit scope
     */
    def structureDecoderAnnotation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = try {
        import c.universe.{DefDef, DefDefTag, Ident, Quasiquote, TermName, ValDef, ValDefTag}

        val SimplifiedDeriveAnnotation(targetType, _, _, implDef) = simplifyDeriveAnnotation(c)(annottees: _*)
        val model = structureOf(c)(targetType)

        val overridden = implDef.impl.body.collect {
            case ValDef(_, n, _, _)       if n.decodedName.toString.endsWith("Decoder") => n.decodedName.toString.stripSuffix("Decoder")
            case DefDef(_, n, _, _, _, _) if n.decodedName.toString.endsWith("Decoder") => n.decodedName.toString.stripSuffix("Decoder")
        }

        val declareDefaultDecoders = model.properties.filterNot(overridden contains _.internalName).map { prop =>
            q"""
                lazy val ${prop.decoderName} = ${materializeDecoder(c)(prop.tpe, prop.annotations)}
            """
        }

        val newStats = declareDefaultDecoders ++ structureDecoderMethods(c)(targetType, p => Ident(p.decoderName), model)

        c.Expr[Any](augmentImpl(c)(implDef, List(decoderType(c)(tq"$targetType")), newStats))
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /**
     * Implement the deriveCoder annotation for wrappers, attaching an automatically derived wrapper
     * coder pair to the companion of the annotated class
     */
    def deriveImplicitWrapperCoderAnnotation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
        addToCompanion(c)(annottees) { (targetName, _) =>
            import c.universe.{Ident, Quasiquote}
            List(q"implicit val ${implicitCoderName(c)}: ${coderType(c)(Ident(targetName))} = ${makeWrapperCoder(c)(Ident(targetName))}")
        }

    /** Def macro implementation which derives a coder pair for a wrapper */
    def wrapperCoderDefImpl[A: c.WeakTypeTag](c: Context): c.Expr[Coder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName}

        val targetType = c.universe.weakTypeTag[A].tpe
        val model = structureOf(c)(targetType)

        model.constructorProperties match {
            case prop :: Nil =>
                val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Coder")
                val declareCoder =
                    List (
                        q"lazy val ${prop.decoderName} = ${materializeDecoder(c)(prop.tpe, prop.annotations)}",
                        q"lazy val ${prop.encoderName} = ${materializeEncoder(c)(prop.tpe, prop.annotations)}"
                    )

                c.Expr[Coder[A]](q"""
                    {
                        object $name extends ${coderType(c)(tq"$targetType")} {
                            ..$declareCoder
                            object decode extends ${decoderType(c)(tq"$targetType")} {
                                ..${wrapperDecoderMethods(c)(targetType, prop, a => model.constructAndAssign(_ => a))}
                            }
                            object encode extends ${encoderType(c)(tq"$targetType")} {
                                ..${wrapperEncoderMethods(c)(targetType, prop, prop.read)}
                            }
                        }
                        $name
                    }
                """)

            case other =>
                sys.error("expected a class with a constructor taking only one explicit argument (the value being wrapped) not " +
                          other.mkString("[", ", ", "]"))
        }
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /** Def macro implementation which derives a encoder for a wrapper */
    def wrapperEncoderDefImpl[A: c.WeakTypeTag](c: Context): c.Expr[Encoder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName}

        val targetType = c.universe.weakTypeTag[A].tpe
        val model = structureOf(c)(targetType)

        model.constructorProperties match {
            case prop :: Nil =>
                val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Encoder")
                val declareEncoder = q"lazy val ${prop.encoderName} = ${materializeEncoder(c)(prop.tpe, prop.annotations)}"

                c.Expr[Encoder[A]](q"""
                    {
                        object $name extends ${encoderType(c)(tq"$targetType")} {
                            $declareEncoder
                            ..${wrapperEncoderMethods(c)(targetType, prop, prop.read)}
                        }
                        $name
                    }
                """)

            case other =>
                sys.error("expected a class with a constructor taking only one explicit argument (the value being wrapped) not " +
                          other.mkString("[", ", ", "]"))
        }
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /** Def macro implementation which derives a decoder for a wrapper */
    def wrapperDecoderDefImpl[A: c.WeakTypeTag](c: Context): c.Expr[Decoder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName}

        val targetType = c.universe.weakTypeTag[A].tpe
        val model = structureOf(c)(targetType)

        model.constructorProperties match {
            case prop :: Nil =>
                val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Decoder")
                val declareDecoder = q"lazy val ${prop.decoderName} = ${materializeDecoder(c)(prop.tpe, prop.annotations)}"

                c.Expr[Decoder[A]](q"""
                    {
                        object $name extends ${decoderType(c)(tq"$targetType")} {
                            $declareDecoder
                            ..${wrapperDecoderMethods(c)(targetType, prop, a => model.constructAndAssign(_ => a))}
                        }
                        $name
                    }
                """)

            case other =>
                sys.error("expected a class with a constructor taking only one explicit argument (the value being wrapped) not " +
                          other.mkString("[", ", ", "]"))
        }
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    private def parseUnionAlternatives(c: Context)(alternativeTrees: Seq[c.universe.Tree]): NonEmptyList[(c.universe.Type, String)] = {
        import c.universe.{Quasiquote, TreeTag}

        val targets = alternativeTrees.toList.map {
            case q"$f[$tpeTree].tag($tag)" => (tpeTree.tpe, c.eval(c.Expr[String](c.untypecheck(tag.duplicate))))
            case q"$f[$tpeTree]"           => (tpeTree.tpe, tpeTree.tpe.typeSymbol.name.decodedName.toString)
            case tree =>
                sys.error("unrecognized union alternative syntax: " + tree + ". expected either alt[Type] or alt[Type].tag(\"Tag\")")
        }

        targets match {
            case Nil => sys.error("union cannot be made with no alternatives!")
            case hd :: tl => nel(hd, tl)
        }
    }

    /**
     * Implement the deriveCoder annotation for wrappers, attaching an automatically derived wrapper
     * coder pair to the companion of the annotated class
     */
    def deriveImplicitUnionCoderAnnotation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
        addToCompanion(c)(annottees) { (targetName, annotations) =>
            import c.universe.{Ident, IdentTag, Name, NameTag, Quasiquote, TermName, TermNameTag, Tree, TreeTag}

            def isUnionAnno(t: Tree): Boolean =
                t match {
                    case Ident(n: Name) => n.decodedName.toString == "union" // the tree isn't yet typechecked and resolved so uhh, this is spotty
                    case _ => false
                }

            val alts =
                annotations.collectFirst { case q"new $anno(..$alts)" if isUnionAnno(anno) => alts }
                    .getOrElse(sys.error("couldn't find union annotation among " + annotations))

            List(q"implicit val ${implicitCoderName(c)}: ${coderType(c)(Ident(targetName))} = ${makeUnionCoder(c)(Ident(targetName), alts)}")
        }

    /** Def macro implementation which derives a coder for a union type */
    def unionCoderDefImpl[A: c.WeakTypeTag](c: Context)(alternatives: c.Expr[base.union.Alternative[A]]*): c.Expr[Coder[A]] = try {
        import c.universe.{Block, BlockTag, Ident, Quasiquote, TermName}

        val targetType = c.universe.weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Coder")
        val targetSubtypes = parseUnionAlternatives(c)(alternatives.map(_.tree)).map(_._1)
        val subtypeEncoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }
        val subtypeDecoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareCoders = (targetSubtypes zip (subtypeEncoderNames zip subtypeDecoderNames)).flatMap { case (tpe, (encoderName, decoderName)) =>
            nels (
                q"lazy val $decoderName = ${materializeDecoder(c)(tpe, Nil)}",
                q"lazy val $encoderName = ${materializeEncoder(c)(tpe, Nil)}"
            )
        }.list

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeEncoderNames).stream
        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeDecoderNames).stream

        c.Expr[Coder[A]](q"""
            {
                object $name extends ${coderType(c)(tq"$targetType")} {
                    ..$declareCoders
                    object decode extends ${decoderType(c)(tq"$targetType")} {
                        ..${unionDecoderMethods(c)(targetType, targetSubtypes, subtypeDecoderNamesByType.mapValues(n => Ident(n)))}
                    }
                    object encode extends ${encoderType(c)(tq"$targetType")} {
                        ..${unionEncoderMethods(c)(targetType, targetSubtypes, subtypeEncoderNamesByType.mapValues(n => Ident(n)))}
                    }
                }
                $name
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /** Def macro implementation which derives a decoder for a union type */
    def unionDecoderDefImpl[A: c.WeakTypeTag](c: Context)(alternatives: c.Expr[base.union.Alternative[A]]*): c.Expr[Decoder[A]] = try {
        import c.universe.{Block, BlockTag, Ident, Quasiquote, TermName, weakTypeTag}

        val targetType = c.universe.weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Decoder")
        val targetSubtypes = parseUnionAlternatives(c)(alternatives.map(_.tree)).map(_._1)
        val subtypeDecoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareDecoders = (targetSubtypes zip subtypeDecoderNames).map { case (tpe, decoderName) =>
            q"lazy val $decoderName = ${materializeDecoder(c)(tpe, Nil)}"
        }.list

        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeDecoderNames).stream

        c.Expr[Decoder[A]](q"""
            {
                object $name extends ${decoderType(c)(tq"$targetType")} {
                    ..$declareDecoders
                    ..${unionDecoderMethods(c)(targetType, targetSubtypes, subtypeDecoderNamesByType.mapValues(n => Ident(n)))}
                }
                $name
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /** Def macro implementation which derives an encoder for a union type */
    def unionEncoderDefImpl[A: c.WeakTypeTag](c: Context)(alternatives: c.Expr[base.union.Alternative[A]]*): c.Expr[Encoder[A]] = try {
        import c.universe.{Block, BlockTag, Ident, Quasiquote, TermName, weakTypeTag}
        val targetType = c.universe.weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Encoder")
        val targetSubtypes = parseUnionAlternatives(c)(alternatives.map(_.tree)).map(_._1)
        val subtypeEncoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareEncoders = (targetSubtypes zip subtypeEncoderNames).map { case (tpe, encoderName) =>
            q"lazy val $encoderName = ${materializeEncoder(c)(tpe, Nil)}"
        }.list

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeEncoderNames).stream

        c.Expr[Encoder[A]](q"""
            {
                object $name extends ${encoderType(c)(tq"$targetType")} {
                    ..$declareEncoders
                    ..${unionEncoderMethods(c)(targetType, targetSubtypes, subtypeEncoderNamesByType.mapValues(n => Ident(n)))}
                }
                $name
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }
}
