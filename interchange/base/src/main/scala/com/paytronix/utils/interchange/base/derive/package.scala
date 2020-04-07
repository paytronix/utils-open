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

import scala.language.higherKinds
import scala.reflect.macros.whitebox.Context

import com.paytronix.utils.interchange.base

/**
 * Trait which implements a family of macros used to generate various coders for some format.
 *
 * Each format implements an object extending this trait and so gets appropriate macros at a lower
 * implementation cost.
 */
trait DeriveCoderMacros extends DeriveUtils {
    val c: Context

    import c.universe.{Annotation, DefDef, DefDefTag, Ident, Quasiquote, TermName, Tree, Type, ValDef, ValDefTag, typeOf, weakTypeTag}

    /** Type of coders for the given type */
    type Coder[A]

    /**
     * The name to use when attaching implicitly derived coders to companion objects, e.g. `avroCoder` or `jacksonCoder`.
     * To avoid confusion and madness this should always return some constant name, but has to be a function because macros.
     */
    //def implicitCoderName: c.universe.TermName

    /** Type tree of coder pairs of the given type name */
    def coderType(tpe: Tree): Tree

    /** Materializer for coder pairs of the given type */
    def materializeCoder(tpe: Type, annotations: List[Annotation]): Tree

    /** Type of encoders of the given type */
    type Encoder[A]

    /** Type tree of encoders of the given type name */
    def encoderType(tpe: Tree): Tree

    /** Materializer for encoders of the given type */
    def materializeEncoder(tpe: Type, annotations: List[Annotation]): Tree

    /** Type of decoders of the given type */
    type Decoder[A]

    /** Type tree of decoders of the given type name */
    def decoderType(tpe: Tree): Tree

    /** Materializer of decoders of the given type */
    def materializeDecoder(tpe: Type, annotations: List[Annotation]): Tree

    /** Tree which evaluates to the front end of the macro package, for implicit structure coder pair implementation */
    def makeStructureCoder(target: Tree): Tree

    /** Generate methods for implementing the encoder type of a structure, giving the model and references to the subsidiary encoders to use */
    def structureEncoderMethods(tpe: Type, encoderFor: Property => Tree, model: Structure): List[Tree]

    /** Generate methods for implementing the decoder type of a structure, giving the model and references to the subsidiary decoders to use */
    def structureDecoderMethods(tpe: Type, decoderFor: Property => Tree, model: Structure): List[Tree]

    /** Tree which evaluates to the front end of the macro package, for implicit wrapper coder pair implementation */
    def makeWrapperCoder(target: Tree): Tree

    /** Generate methods for implementing the decoder type of a wrapper, given an unwrapping function and the single property of the wrapper */
    def wrapperEncoderMethods(targetType: Type, property: Property, unwrap: Tree => Tree): List[Tree]

    /** Generate methods for implementing the decoder type of a wrapper, given a wrapping function and the single property of the wrapper */
    def wrapperDecoderMethods(targetType: Type, property: Property, wrap: Tree => Tree): List[Tree]

    /** Wrap a core expression which materializes or constructs a coder with additional wrapping based on annotations, used for implementing materialize* */
    def wrapCoderForAnnotations (
        coder: Tree,
        annotations: List[Annotation],
        nullable: Tree => Tree,
        default: Tree => Tree => Tree
    ): Tree = {
        val nullableTpe = typeOf[base.nullable]
        val defaultTpe = typeOf[base.default]
        List (
            annotations.collectFirst { case annot if annot.tree.tpe =:= nullableTpe => annot }.map { _ => nullable },
            annotations.collectFirst { case annot if annot.tree.tpe =:= defaultTpe && annot.tree.children.size > 1 =>
                annot.tree.children.tail.head
            }.map { valueTree => default(valueTree) }
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
    /* 2014-08-27 RMM: having multiple annotation macros which addToCompanion causes the compiler to not emit the object class (Blah$) even though
                       it doesn't error at runtime.
    def deriveImplicitStructureCoderAnnotation(annottees: c.Expr[Any]*): c.Expr[Any] =
        addToCompanion(annottees) { (targetName, _) =>
            List(q"implicit val ${implicitCoderName}: ${coderType(Ident(targetName))} = ${makeStructureCoder(Ident(targetName))}")
        }
    */

    /** Def macro implementation which derives a coder pair for a structure */
    def structureCoderDefImpl[A: c.WeakTypeTag]: c.Expr[Coder[A]] = try {
        val tpe = weakTypeTag[A].tpe
        val name = TermName(tpe.typeSymbol.name.decodedName.toString + "Coder")
        val model = structureOf(tpe)

        //System.out.println("structureCoderDefImpl tpe: " + tpe)
        //System.out.println("structureCoderDefImpl name: " + name)
        //System.out.println("structureCoderDefImpl model: " + model)

        val declareCoders = model.properties.flatMap { prop =>
            List (
                q"lazy val ${prop.decoderName} = ${materializeDecoder(prop.tpe, prop.annotations)}",
                q"lazy val ${prop.encoderName} = ${materializeEncoder(prop.tpe, prop.annotations)}"
            )
        }

        c.Expr[Coder[A]](q"""
            {
                object $name extends ${coderType(tq"$tpe")} {
                    ..$declareCoders
                    object decode extends ${decoderType(tq"$tpe")} {
                        ..${structureDecoderMethods(tpe, p => Ident(p.decoderName), model)}
                    }
                    object encode extends ${encoderType(tq"$tpe")} {
                        ..${structureEncoderMethods(tpe, p => Ident(p.encoderName), model)}
                    }
                }
                $name: ${coderType(tq"$tpe")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /** Def macro implementation which derives a decoder for a structure */
    def structureDecoderDefImpl[A: c.WeakTypeTag]: c.Expr[Decoder[A]] = try {
        val tpe = weakTypeTag[A].tpe
        val name = TermName(tpe.typeSymbol.name.decodedName.toString + "Decoder")
        val model = structureOf(tpe)

        val declareDecoders = model.properties.map { prop =>
            q"lazy val ${prop.decoderName} = ${materializeDecoder(prop.tpe, prop.annotations)}"
        }

        c.Expr[Decoder[A]](q"""
            {
                object $name extends ${decoderType(tq"$tpe")} {
                    ..$declareDecoders
                    ..${structureDecoderMethods(tpe, p => Ident(p.decoderName), model)}
                }
                $name: ${decoderType(tq"$tpe")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /** Def macro implementation which derives a encoder for a structure */
    def structureEncoderDefImpl[A: c.WeakTypeTag]: c.Expr[Encoder[A]] = try {
        val tpe = weakTypeTag[A].tpe
        val name = TermName(tpe.typeSymbol.name.decodedName.toString + "Encoder")
        val model = structureOf(tpe)

        val declareEncoders = model.properties.map { prop =>
            q"lazy val ${prop.encoderName} = ${materializeEncoder(prop.tpe, prop.annotations)}"
        }

        c.Expr[Encoder[A]](q"""
            {
                object $name extends ${encoderType(tq"$tpe")} {
                    ..$declareEncoders
                    ..${structureEncoderMethods(tpe, p => Ident(p.encoderName), model)}
                }
                $name: ${encoderType(tq"$tpe")}
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
    def structureCoderAnnotation(annottees: c.Expr[Any]*): c.Expr[Any] = try {
        val SimplifiedDeriveAnnotation(targetType, _, _, implDef) = simplifyDeriveAnnotation(annottees: _*)
        val model = structureOf(targetType)

        val overridden = implDef.impl.body.collect {
            case ValDef(_, n, _, _)       if n.decodedName.toString.endsWith("Coder") => n.decodedName.toString.stripSuffix("Coder")
            case DefDef(_, n, _, _, _, _) if n.decodedName.toString.endsWith("Coder") => n.decodedName.toString.stripSuffix("Coder")
        }

        val declareDefaultCoders = model.properties.filterNot(overridden contains _.internalName).map { prop =>
            q"""
                lazy val ${prop.coderName} = ${materializeCoder(prop.tpe, prop.annotations)}
            """
        }

        val newStats = declareDefaultCoders ++ List (
            q"""
                object encode extends ${encoderType(tq"$targetType")} {
                    ..${structureEncoderMethods(targetType, p => q"${p.coderName}.encode", model)}
                }
            """,
            q"""
                object decode extends ${decoderType(tq"$targetType")} {
                    ..${structureDecoderMethods(targetType, p => q"${p.coderName}.decode", model)}
                }
            """
        )

        c.Expr[Any](augmentImpl(implDef, List(coderType(tq"$targetType")), newStats))
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /**
     * Annotation macro implementation which implements an encoder for a structure where some fields can
     * use an explicit encoder rather than one from implicit scope
     */
    def structureEncoderAnnotation(annottees: c.Expr[Any]*): c.Expr[Any] = try {
        val SimplifiedDeriveAnnotation(targetType, _, _, implDef) = simplifyDeriveAnnotation(annottees: _*)
        val model = structureOf(targetType)

        val overridden = implDef.impl.body.collect {
            case ValDef(_, n, _, _)       if n.decodedName.toString.endsWith("Encoder") => n.decodedName.toString.stripSuffix("Encoder")
            case DefDef(_, n, _, _, _, _) if n.decodedName.toString.endsWith("Encoder") => n.decodedName.toString.stripSuffix("Encoder")
        }

        val declareDefaultEncoders = model.properties.filterNot(overridden contains _.internalName).map { prop =>
            q"""
                lazy val ${prop.encoderName} = ${materializeEncoder(prop.tpe, prop.annotations)}
            """
        }

        val newStats = declareDefaultEncoders ++ structureEncoderMethods(targetType, p => Ident(p.encoderName), model)

        c.Expr[Any](augmentImpl(implDef, List(encoderType(tq"$targetType")), newStats))
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /**
     * Annotation macro implementation which implements a decoder for a structure where some fields can
     * use an explicit decoder rather than one from implicit scope
     */
    def structureDecoderAnnotation(annottees: c.Expr[Any]*): c.Expr[Any] = try {
        val SimplifiedDeriveAnnotation(targetType, _, _, implDef) = simplifyDeriveAnnotation(annottees: _*)
        val model = structureOf(targetType)

        val overridden = implDef.impl.body.collect {
            case ValDef(_, n, _, _)       if n.decodedName.toString.endsWith("Decoder") => n.decodedName.toString.stripSuffix("Decoder")
            case DefDef(_, n, _, _, _, _) if n.decodedName.toString.endsWith("Decoder") => n.decodedName.toString.stripSuffix("Decoder")
        }

        val declareDefaultDecoders = model.properties.filterNot(overridden contains _.internalName).map { prop =>
            q"""
                lazy val ${prop.decoderName} = ${materializeDecoder(prop.tpe, prop.annotations)}
            """
        }

        val newStats = declareDefaultDecoders ++ structureDecoderMethods(targetType, p => Ident(p.decoderName), model)

        c.Expr[Any](augmentImpl(implDef, List(decoderType(tq"$targetType")), newStats))
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    /**
     * Implement the deriveCoder annotation for wrappers, attaching an automatically derived wrapper
     * coder pair to the companion of the annotated class
     */
    /* 2014-08-27 RMM: having multiple annotation macros which addToCompanion causes the compiler to not emit the object class (Blah$) even though
                       it doesn't error at runtime.
    def deriveImplicitWrapperCoderAnnotation(annottees: c.Expr[Any]*): c.Expr[Any] =
        addToCompanion(annottees) { (targetName, _) =>
            List(q"implicit val ${implicitCoderName}: ${coderType(Ident(targetName))} = ${makeWrapperCoder(Ident(targetName))}")
        }
    */

    /** Def macro implementation which derives a coder pair for a wrapper */
    def wrapperCoderDefImpl[A: c.WeakTypeTag]: c.Expr[Coder[A]] = try {
        val targetType = c.universe.weakTypeTag[A].tpe
        val model = structureOf(targetType)

        model.constructorProperties match {
            case prop :: Nil =>
                val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Coder")
                val declareCoder =
                    List (
                        q"lazy val ${prop.decoderName} = ${materializeDecoder(prop.tpe, prop.annotations)}",
                        q"lazy val ${prop.encoderName} = ${materializeEncoder(prop.tpe, prop.annotations)}"
                    )

                c.Expr[Coder[A]](q"""
                    {
                        object $name extends ${coderType(tq"$targetType")} {
                            ..$declareCoder
                            object decode extends ${decoderType(tq"$targetType")} {
                                ..${wrapperDecoderMethods(targetType, prop, a => model.constructAndAssign(_ => a))}
                            }
                            object encode extends ${encoderType(tq"$targetType")} {
                                ..${wrapperEncoderMethods(targetType, prop, prop.read)}
                            }
                        }
                        $name: ${coderType(tq"$targetType")}
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
    def wrapperEncoderDefImpl[A: c.WeakTypeTag]: c.Expr[Encoder[A]] = try {
        val targetType = c.universe.weakTypeTag[A].tpe
        val model = structureOf(targetType)

        model.constructorProperties match {
            case prop :: Nil =>
                val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Encoder")
                val declareEncoder = q"lazy val ${prop.encoderName} = ${materializeEncoder(prop.tpe, prop.annotations)}"

                c.Expr[Encoder[A]](q"""
                    {
                        object $name extends ${encoderType(tq"$targetType")} {
                            $declareEncoder
                            ..${wrapperEncoderMethods(targetType, prop, prop.read)}
                        }
                        $name: ${encoderType(tq"$targetType")}
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
    def wrapperDecoderDefImpl[A: c.WeakTypeTag]: c.Expr[Decoder[A]] = try {
        val targetType = c.universe.weakTypeTag[A].tpe
        val model = structureOf(targetType)

        model.constructorProperties match {
            case prop :: Nil =>
                val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Decoder")
                val declareDecoder = q"lazy val ${prop.decoderName} = ${materializeDecoder(prop.tpe, prop.annotations)}"

                c.Expr[Decoder[A]](q"""
                    {
                        object $name extends ${decoderType(tq"$targetType")} {
                            $declareDecoder
                            ..${wrapperDecoderMethods(targetType, prop, a => model.constructAndAssign(_ => a))}
                        }
                        $name: ${decoderType(tq"$targetType")}
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
}
