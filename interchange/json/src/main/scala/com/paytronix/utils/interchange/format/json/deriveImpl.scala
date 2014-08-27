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

package com.paytronix.utils.interchange.format.json

import scala.reflect.macros.whitebox.Context

import scalaz.NonEmptyList
import scalaz.syntax.foldable.ToFoldableOps

import com.paytronix.utils.interchange.base

import base.derive.DeriveCoderMacros
import base.derive.utils.{Property, Structure, sequenceResultBindings}
import NonEmptyList.{nel, nels}

private[json] object deriveImpl extends DeriveCoderMacros {
    type Coder[A] = JsonCoder[A]

    //def implicitCoderName(c: Context) = c.universe.TermName("jsonCoder")

    def coderType(c: Context)(tpe: c.universe.Tree) = {
        import c.universe.Quasiquote
        tq"com.paytronix.utils.interchange.format.json.JsonCoder[$tpe]"
    }

    def materializeCoder(c: Context)(tpe: c.universe.Type, annotations: List[c.universe.Annotation]): c.universe.Tree = {
        import c.universe.Quasiquote
        q"""
            com.paytronix.utils.interchange.format.json.JsonCoder.make[$tpe] (
                ${materializeEncoder(c)(tpe, annotations)},
                ${materializeDecoder(c)(tpe, annotations)}
            )
        """
    }

    type Encoder[A] = JsonEncoder[A]

    def encoderType(c: Context)(tpe: c.universe.Tree): c.universe.Tree = {
        import c.universe.Quasiquote
        tq"com.paytronix.utils.interchange.format.json.JsonEncoder[$tpe]"
    }

    def materializeEncoder(c: Context)(tpe: c.universe.Type, annotations: List[c.universe.Annotation]): c.universe.Tree = {
        import c.universe.Quasiquote
        wrapCoderForAnnotations(c) (
            coder       = q"com.paytronix.utils.interchange.format.json.JsonEncoder[$tpe]",
            annotations = annotations,
            nullable    = inside => q"com.paytronix.utils.interchange.format.json.container.nullableJsonEncoder($inside)",
            default     = value => inside => inside
        )
    }

    type Decoder[A] = JsonDecoder[A]

    def decoderType(c: Context)(tpe: c.universe.Tree): c.universe.Tree = {
        import c.universe.Quasiquote
        tq"com.paytronix.utils.interchange.format.json.JsonDecoder[$tpe]"
    }

    def materializeDecoder(c: Context)(tpe: c.universe.Type, annotations: List[c.universe.Annotation]): c.universe.Tree = {
        import c.universe.Quasiquote
        wrapCoderForAnnotations(c) (
            coder       = q"com.paytronix.utils.interchange.format.json.JsonDecoder[$tpe]",
            annotations = annotations,
            nullable    = inside => q"com.paytronix.utils.interchange.format.json.container.nullableJsonDecoder($inside)",
            default     = value => inside => q"$inside.default($value)"
        )
    }

    def makeStructureCoder(c: Context)(target: c.universe.Tree): c.universe.Tree = {
        import c.universe.Quasiquote
        q"com.paytronix.utils.interchange.format.json.derive.structure.coder[$target]"
    }

    def structureEncoderMethods(c: Context) (
        tpe: c.universe.Type,
        encoderFor: Property[c.universe.type] => c.universe.Tree,
        model: Structure[c.universe.type]
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote, typeOf}

        val flattenTpe = typeOf[com.paytronix.utils.interchange.format.json.flatten]

        val jsonGenerator = TermName(c.freshName())
        val instance = TermName(c.freshName())
        val encode = sequenceResultBindings(c, model.properties) (
            action = prop =>
                if (prop.annotations.exists(_.tree.tpe =:= flattenTpe))
                    q"""
                        $jsonGenerator.filterNextObject(com.paytronix.utils.interchange.format.json.InterchangeJsonGenerator.ObjectFilter.flatten)
                        com.paytronix.utils.interchange.base.atProperty(${prop.externalName}) {
                            ${encoderFor(prop)}.run(${prop.read(Ident(instance))}, $jsonGenerator)
                        }
                    """
                else
                    q"""
                        $jsonGenerator.writeFieldName(${prop.externalName}) >>
                        com.paytronix.utils.interchange.base.atProperty(${prop.externalName}) {
                            ${encoderFor(prop)}.run(${prop.read(Ident(instance))}, $jsonGenerator)
                        }
                    """,
            accept = (_, _) => q"",
            body = _ => q"com.paytronix.utils.scala.result.Okay.unit"
        )

        List (
            q"val mightBeNull = false",
            q"val codesAsObject = true",
            q"""
                def run($instance: $tpe, $jsonGenerator: com.paytronix.utils.interchange.format.json.InterchangeJsonGenerator) = {
                    $jsonGenerator.writeStartObject() >> $encode >> $jsonGenerator.writeEndObject()
                }
            """
        )
    }

    def structureDecoderMethods(c: Context) (
        tpe: c.universe.Type,
        decoderFor: Property[c.universe.type] => c.universe.Tree,
        model: Structure[c.universe.type]
    ): List[c.universe.Tree] = {
        import c.universe.{Block, BlockTag, Ident, TermName, Quasiquote, typeOf}

        val flattenTpe = typeOf[com.paytronix.utils.interchange.format.json.flatten]

        val allProps = (model.constructorProperties ++ model.mutableProperties).sorted
        val (flattenProps, normalProps) = allProps.partition(_.annotations.exists(_.tree.tpe =:= flattenTpe))

        val jsonParser = TermName(c.freshName())
        val receiver = TermName(c.freshName())
        val propReceiverNames = allProps.map { prop => TermName(c.freshName()) }
        val propReceiverByProp = Map.empty ++ (allProps zip propReceiverNames)
        val propFlagNames = normalProps.map { prop => TermName(c.freshName()) }
        val propFlagByProp = Map.empty ++ (normalProps zip propFlagNames)

        val declarePropReceivers = (propReceiverNames zip allProps).map { case (receiverName, prop) =>
            q"val $receiverName = new com.paytronix.utils.interchange.base.Receiver[${prop.tpe}]()"
        }
        val declarePropFlags = propFlagNames.map { flagName =>
            q"var $flagName = false"
        }

        val fieldDecoders = normalProps.map { prop =>
            cq"""
                ${prop.externalName} =>
                    com.paytronix.utils.interchange.base.atProperty(${prop.externalName}) {
                        ${propFlagByProp(prop)} = true
                        ${decoderFor(prop)}.run($jsonParser, ${propReceiverByProp(prop)})
                    }
            """
        }

        val handleFlattenFields = sequenceResultBindings(c, flattenProps) (
            action = prop => q"""
                $jsonParser.excursion {
                    com.paytronix.utils.interchange.base.atProperty(${prop.externalName}) {
                        ${decoderFor(prop)}.run($jsonParser, ${propReceiverByProp(prop)})
                    }
                }
            """,
            accept = (_, _) => q"",
            body = _ => q"com.paytronix.utils.scala.result.Okay.unit"
        )

        val handleMissing = sequenceResultBindings(c, normalProps) (
            action = prop => q"""

                if (!${propFlagByProp(prop)}) {
                    $jsonParser.currentValueIsMissing()
                    com.paytronix.utils.interchange.base.atProperty(${prop.externalName}) {
                        ${decoderFor(prop)}.run($jsonParser, ${propReceiverByProp(prop)})
                    }
                } else
                    com.paytronix.utils.scala.result.Okay.unit
            """,
            accept = (_, _) => q"",
            body = _ => q"com.paytronix.utils.scala.result.Okay.unit"
        )

        List (
            q"val mightBeNull = false",
            q"val codesAsObject = true",
            q"""
                def run($jsonParser: com.paytronix.utils.interchange.format.json.InterchangeJsonParser,
                        $receiver: com.paytronix.utils.interchange.base.Receiver[$tpe]) = {
                    ..$declarePropReceivers
                    ..$declarePropFlags
                    $jsonParser.require(com.fasterxml.jackson.core.JsonToken.START_OBJECT) >>
                    $handleFlattenFields >>
                    $jsonParser.foreachFields {
                        case ..$fieldDecoders
                        case _ =>
                            $jsonParser.skipToEndOfValue();
                            com.paytronix.utils.scala.result.Okay.unit
                    } >>
                    { $handleMissing } >>
                    $receiver(${model.constructAndAssign(prop => q"${propReceiverByProp(prop)}.value")})
                }
            """
        )
    }

    def makeWrapperCoder(c: Context)(target: c.universe.Tree): c.universe.Tree = {
        import c.universe.Quasiquote
        q"com.paytronix.utils.interchange.format.json.derive.wrapper.coder[$target]"
    }

    def wrapperEncoderMethods(c: Context) (
        targetType: c.universe.Type,
        property: Property[c.universe.type],
        unwrap: c.universe.Tree => c.universe.Tree
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}
        val instance = TermName(c.freshName())
        val jsonGenerator = TermName(c.freshName())
        List (
            q"val mightBeNull = ${property.encoderName}.mightBeNull",
            q"val codesAsObject = ${property.encoderName}.codesAsObject",
            q"""
                def run($instance: $targetType, $jsonGenerator: com.paytronix.utils.interchange.format.json.InterchangeJsonGenerator) =
                    com.paytronix.utils.interchange.base.atProperty(${property.externalName}) {
                        ${property.encoderName}.run(${unwrap(Ident(instance))}, $jsonGenerator)
                    }
            """
        )
    }

    def wrapperDecoderMethods(c: Context) (
        targetType: c.universe.Type,
        property: Property[c.universe.type],
        wrap: c.universe.Tree => c.universe.Tree
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}
        val jsonParser = TermName(c.freshName())
        val out = TermName(c.freshName())
        val receiver = TermName(c.freshName())
        List (
            q"val mightBeNull = ${property.decoderName}.mightBeNull",
            q"val codesAsObject = ${property.decoderName}.codesAsObject",
            q"""
                def run($jsonParser: com.paytronix.utils.interchange.format.json.InterchangeJsonParser, $out: com.paytronix.utils.interchange.base.Receiver[$targetType]) = {
                    val $receiver = new com.paytronix.utils.interchange.base.Receiver[${property.tpe}]
                    com.paytronix.utils.interchange.base.atProperty(${property.externalName}) {
                        ${property.decoderName}.run($jsonParser, $receiver) >> $out(${wrap(q"$receiver.value")})
                    }
                }
            """
        )
    }

    /* 2014-08-27 RMM: having multiple annotation macros which addToCompanion causes the compiler to not emit the object class (Blah$) even though
                       it doesn't error at runtime.
    def deriveImplicitAdHocUnionCoderAnnotation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
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

            val q"new $_($noApplicableAlternates).macroTransform(..$_)" = c.macroApplication

            List(q"""
                implicit val ${implicitCoderName(c)}: ${coderType(c)(Ident(targetName))} =
                    com.paytronix.utils.interchange.format.json.derive.adHocUnion.coder[$targetName]($noApplicableAlternates, ..$alts)
            """)
        }
    */

    private def parseAdHocUnionAlternates(c: Context)(alternateTrees: Seq[c.universe.Tree]): NonEmptyList[c.universe.Type] = {
        import c.universe.{Quasiquote, TreeTag}

        val targets = alternateTrees.toList.map {
            // FIXME? probably this should really typecheck the tree and get the type out from there rather than pattern matching the apply
            case q"$_[$tpeTree]" => tpeTree.tpe
            case tree =>
                sys.error("unrecognized union alternative syntax: " + tree + ". expected alternate[Type]")
        }

        targets match {
            case Nil => sys.error("union cannot be made with no alternates!")
            case hd :: tl => nel(hd, tl)
        }
    }

    def adHocUnionCoderDef[A: c.WeakTypeTag](c: Context)(noApplicableAlternates: c.Tree, alternates: c.Tree*): c.Expr[JsonCoder[A]] = try {
        import c.universe.{Block, BlockTag, Ident, Quasiquote, TermName}

        val targetType = c.universe.weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Coder")
        val targetSubtypes = parseAdHocUnionAlternates(c)(alternates)
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

        c.Expr[JsonCoder[A]](q"""
            {
                object $name extends ${coderType(c)(tq"$targetType")} {
                    ..$declareCoders
                    object decode extends ${decoderType(c)(tq"$targetType")} {
                        ..${adHocUnionDecoderMethods(c)(noApplicableAlternates, targetType, targetSubtypes, subtypeDecoderNamesByType.mapValues(n => Ident(n)))}
                    }
                    object encode extends ${encoderType(c)(tq"$targetType")} {
                        ..${adHocUnionEncoderMethods(c)(noApplicableAlternates, targetType, targetSubtypes, subtypeEncoderNamesByType.mapValues(n => Ident(n)))}
                    }
                }
                $name: ${coderType(c)(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def adHocUnionDecoderDef[A: c.WeakTypeTag](c: Context)(noApplicableAlternates: c.Tree, alternates: c.Tree*): c.Expr[JsonDecoder[A]] = try {
        import c.universe.{Block, BlockTag, Ident, Quasiquote, TermName, weakTypeTag}

        val targetType = c.universe.weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Decoder")
        val targetSubtypes = parseAdHocUnionAlternates(c)(alternates)
        val subtypeDecoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareDecoders = (targetSubtypes zip subtypeDecoderNames).map { case (tpe, decoderName) =>
            q"lazy val $decoderName = ${materializeDecoder(c)(tpe, Nil)}"
        }.list

        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeDecoderNames).stream

        c.Expr[JsonDecoder[A]](q"""
            {
                object $name extends ${decoderType(c)(tq"$targetType")} {
                    ..$declareDecoders
                    ..${adHocUnionDecoderMethods(c)(noApplicableAlternates, targetType, targetSubtypes, subtypeDecoderNamesByType.mapValues(n => Ident(n)))}
                }
                $name: ${decoderType(c)(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def adHocUnionEncoderDef[A: c.WeakTypeTag](c: Context)(noApplicableAlternates: c.Tree, alternates: c.Tree*): c.Expr[JsonEncoder[A]] = try {
        import c.universe.{Block, BlockTag, Ident, Quasiquote, TermName, weakTypeTag}
        val targetType = c.universe.weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Encoder")
        val targetSubtypes = parseAdHocUnionAlternates(c)(alternates)
        val subtypeEncoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareEncoders = (targetSubtypes zip subtypeEncoderNames).map { case (tpe, encoderName) =>
            q"lazy val $encoderName = ${materializeEncoder(c)(tpe, Nil)}"
        }.list

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeEncoderNames).stream

        c.Expr[JsonEncoder[A]](q"""
            {
                object $name extends ${encoderType(c)(tq"$targetType")} {
                    ..$declareEncoders
                    ..${adHocUnionEncoderMethods(c)(noApplicableAlternates, targetType, targetSubtypes, subtypeEncoderNamesByType.mapValues(n => Ident(n)))}
                }
                $name: ${encoderType(c)(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def adHocUnionEncoderMethods(c: Context) (
        noApplicableAlternates: c.Tree,
        targetType: c.universe.Type,
        targetSubtypes: NonEmptyList[c.universe.Type],
        encoderFor: c.universe.Type => c.universe.Tree
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}

        val allEncoders = TermName(c.freshName())
        val jsonGenerator = TermName(c.freshName())
        val instance = TermName(c.freshName())

        val encodeAlts = targetSubtypes.list.zipWithIndex.map { case (subtype, index) =>
            val encoder = encoderFor(subtype)
            val value = TermName(c.freshName())
            cq"""
                $value: $subtype =>
                    com.paytronix.utils.interchange.base.atIndex($index) {
                        com.paytronix.utils.scala.result.tryCatch.resultG(com.paytronix.utils.interchange.base.terminal) {
                            $encoder.run($value, $jsonGenerator)
                        }
                    }
            """
        }

        val value = TermName(c.freshName())

        List (
            q"private val $allEncoders = List(..${targetSubtypes.map(t => encoderFor(t)).list})",
            q"val mightBeNull = $allEncoders.exists(_.mightBeNull)",
            q"val codesAsObject = $allEncoders.forall(_.codesAsObject)",
            q"""
                def run($instance: $targetType, $jsonGenerator: com.paytronix.utils.interchange.format.json.InterchangeJsonGenerator) =
                    $instance match {
                        case ..$encodeAlts
                        case $value =>
                            com.paytronix.utils.scala.result.FailedG (
                                "cannot encode value " + $value + " as it was not configured as a valid union alternative",
                                com.paytronix.utils.interchange.base.CoderFailure.terminal
                            )
                    }
            """
        )
    }

    def adHocUnionDecoderMethods(c: Context) (
        noApplicableAlternates: c.Tree,
        targetType: c.universe.Type,
        targetSubtypes: NonEmptyList[c.universe.Type],
        decoderFor: c.universe.Type => c.universe.Tree
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, Quasiquote, TermName, Tree}

        val allDecoders = TermName(c.freshName())
        val jsonParser = TermName(c.freshName())
        val receiver = TermName(c.freshName())

        val decode = targetSubtypes.foldRight[Tree] (
            q"com.paytronix.utils.scala.result.FailedG($noApplicableAlternates, $jsonParser.terminal)"
        ) { (tpe, rhs) =>
            val subReceiver = TermName(c.freshName())
            q"""
                {
                    val $subReceiver = new com.paytronix.utils.interchange.base.Receiver[$tpe]
                    $jsonParser.excursion(${decoderFor(tpe)}.run($jsonParser, $subReceiver)) >>
                    $jsonParser.skipToEndOfValue() >>
                    $receiver($subReceiver.value)
                } orElse $rhs
            """
        }

        List (
            q"private val $allDecoders = List(..${targetSubtypes.map(decoderFor).list})",
            q"val mightBeNull = $allDecoders.exists(_.mightBeNull)",
            q"val codesAsObject = $allDecoders.forall(_.codesAsObject)",
            q"""
                def run($jsonParser: com.paytronix.utils.interchange.format.json.InterchangeJsonParser,
                        $receiver: com.paytronix.utils.interchange.base.Receiver[$targetType]) =
                    if (!$jsonParser.hasValue) $jsonParser.unexpectedMissingValue
                    else $decode
            """
        )
    }

    /* 2014-08-27 RMM: having multiple annotation macros which addToCompanion causes the compiler to not emit the object class (Blah$) even though
                       it doesn't error at runtime.
    def deriveImplicitTaggedUnionCoderAnnotation(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
        addToCompanion(c)(annottees) { (targetName, annotations) =>
            import c.universe.{Ident, IdentTag, Name, NameTag, Quasiquote, TermName, TermNameTag, Tree, TreeTag}

            def isUnionAnno(t: Tree): Boolean =
                t match {
                    case Ident(n: Name) => n.decodedName.toString == "union" // the tree isn't yet typechecked and resolved so uhh, this is spotty
                    case _ => false
                }

            val q"new $_($determinant).macroTransform(..$_)" = c.macroApplication

            val alts =
                annotations.collectFirst { case q"new $anno(..$alts)" if isUnionAnno(anno) => alts }
                    .getOrElse(sys.error("couldn't find union annotation among " + annotations))

            List(q"""
                implicit val ${implicitCoderName(c)}: ${coderType(c)(Ident(targetName))} =
                    com.paytronix.utils.interchange.format.json.derive.taggedUnion.coder[$targetName]($determinant, ..$alts)
            """)
        }
    */

    private def parseTaggedUnionAlternates(c: Context)(alternateTrees: Seq[c.universe.Tree]): NonEmptyList[(c.universe.Type, String)] = {
        import c.universe.{Quasiquote, TreeTag}

        val targets = alternateTrees.toList.map {
            case q"$_[$tpeTree]($tag)" => (tpeTree.tpe, c.eval(c.Expr[String](c.untypecheck(tag.duplicate))))
            case tree =>
                sys.error("unrecognized union alternative syntax: " + tree + ". expected either alternate[Type](\"Tag\")")
        }

        targets match {
            case Nil => sys.error("union cannot be made with no alternates!")
            case hd :: tl => nel(hd, tl)
        }
    }


    def taggedUnionCoderDef[A: c.WeakTypeTag](c: Context)(determinant: c.Tree, alternates: c.Tree*): c.Expr[JsonCoder[A]] = try {
        import c.universe.{Block, BlockTag, Ident, Quasiquote, TermName}

        val targetType = c.universe.weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Coder")
        val targetSubtypes = parseTaggedUnionAlternates(c)(alternates)
        val subtypeEncoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }
        val subtypeDecoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareCoders = (targetSubtypes zip (subtypeEncoderNames zip subtypeDecoderNames)).flatMap { case ((tpe, _), (encoderName, decoderName)) =>
            nels (
                q"lazy val $decoderName = ${materializeDecoder(c)(tpe, Nil)}",
                q"lazy val $encoderName = ${materializeEncoder(c)(tpe, Nil)}"
            )
        }.list

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeEncoderNames).stream
        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeDecoderNames).stream

        val decoderFor = subtypeDecoderNamesByType.map { case ((tpe, _), name) => (tpe, Ident(name)) }
        val encoderFor = subtypeEncoderNamesByType.map { case ((tpe, _), name) => (tpe, Ident(name)) }

        c.Expr[Coder[A]](q"""
            {
                object $name extends ${coderType(c)(tq"$targetType")} {
                    ..$declareCoders
                    object decode extends ${decoderType(c)(tq"$targetType")} {
                        ..${taggedUnionDecoderMethods(c)(determinant, targetType, targetSubtypes, decoderFor)}
                    }
                    object encode extends ${encoderType(c)(tq"$targetType")} {
                        ..${taggedUnionEncoderMethods(c)(determinant, targetType, targetSubtypes, encoderFor)}
                    }
                }
                $name: ${coderType(c)(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def taggedUnionDecoderDef[A: c.WeakTypeTag](c: Context)(determinant: c.Tree, alternates: c.Tree*): c.Expr[JsonDecoder[A]] = try {
        import c.universe.{Block, BlockTag, Ident, Quasiquote, TermName, weakTypeTag}

        val targetType = c.universe.weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Decoder")
        val targetSubtypes = parseTaggedUnionAlternates(c)(alternates)
        val subtypeDecoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareDecoders = (targetSubtypes zip subtypeDecoderNames).map { case ((tpe, _), decoderName) =>
            q"lazy val $decoderName = ${materializeDecoder(c)(tpe, Nil)}"
        }.list

        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeDecoderNames).stream

        val decoderFor = subtypeDecoderNamesByType.map { case ((tpe, _), name) => (tpe, Ident(name)) }

        c.Expr[Decoder[A]](q"""
            {
                object $name extends ${decoderType(c)(tq"$targetType")} {
                    ..$declareDecoders
                    ..${taggedUnionDecoderMethods(c)(determinant, targetType, targetSubtypes, decoderFor)}
                }
                $name: ${decoderType(c)(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def taggedUnionEncoderDef[A: c.WeakTypeTag](c: Context)(determinant: c.Tree, alternates: c.Tree*): c.Expr[JsonEncoder[A]] = try {
        import c.universe.{Block, BlockTag, Ident, Quasiquote, TermName, weakTypeTag}
        val targetType = c.universe.weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Encoder")
        val targetSubtypes = parseTaggedUnionAlternates(c)(alternates)
        val subtypeEncoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareEncoders = (targetSubtypes zip subtypeEncoderNames).map { case ((tpe, _), encoderName) =>
            q"lazy val $encoderName = ${materializeEncoder(c)(tpe, Nil)}"
        }.list

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeEncoderNames).stream

        val encoderFor = subtypeEncoderNamesByType.map { case ((tpe, _), name) => (tpe, Ident(name)) }

        c.Expr[Encoder[A]](q"""
            {
                object $name extends ${encoderType(c)(tq"$targetType")} {
                    ..$declareEncoders
                    ..${taggedUnionEncoderMethods(c)(determinant, targetType, targetSubtypes, encoderFor)}
                }
                $name: ${encoderType(c)(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def taggedUnionEncoderMethods(c: Context) (
        determinant: c.Tree,
        targetType: c.universe.Type,
        targetSubtypes: NonEmptyList[(c.universe.Type, String)],
        encoderFor: c.universe.Type => c.universe.Tree
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}

        val jsonGenerator = TermName(c.freshName())
        val instance = TermName(c.freshName())

        val encodeAlts = targetSubtypes.list.map { case (tpe, tag) =>
            val encoder = encoderFor(tpe)
            val value = TermName(c.freshName())
            cq"""
                $value: $tpe =>
                    com.paytronix.utils.scala.result.tryCatch.resultG(com.paytronix.utils.interchange.base.terminal) {
                        $jsonGenerator.filterNextObject(new com.paytronix.utils.interchange.format.json.InterchangeJsonGenerator.ObjectFilter {
                            override def beginning() = $jsonGenerator.writeFieldName($determinant) >> $jsonGenerator.writeString($tag)
                        })
                        $encoder.run($value, $jsonGenerator)
                    }
            """
        }

        val value = TermName(c.freshName())

        List (
            q"val mightBeNull = false",
            q"val codesAsObject = true",
            q"""
                def run($instance: $targetType, $jsonGenerator: com.paytronix.utils.interchange.format.json.InterchangeJsonGenerator) =
                    $instance match {
                        case ..$encodeAlts
                        case $value =>
                            com.paytronix.utils.scala.result.FailedG (
                                "cannot encode value " + $value + " as it was not configured as a valid union alternative",
                                com.paytronix.utils.interchange.base.CoderFailure.terminal
                            )
                    }
            """
        )
    }

    def taggedUnionDecoderMethods(c: Context) (
        determinant: c.Tree,
        targetType: c.universe.Type,
        targetSubtypes: NonEmptyList[(c.universe.Type, String)],
        decoderFor: c.universe.Type => c.universe.Tree
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, Quasiquote, TermName}

        val jsonParser = TermName(c.freshName())
        val receiver = TermName(c.freshName())
        val discrimValue = TermName(c.freshName())

        val validTags = targetSubtypes.map(_._2)

        val decodeAlts = targetSubtypes.map { case (tpe, tag) =>
            val subReceiver = TermName(c.freshName())
            cq"""
                $tag =>
                    val $subReceiver = new com.paytronix.utils.interchange.base.Receiver[$tpe]
                    ${decoderFor(tpe)}.run($jsonParser, $subReceiver) >> $receiver($subReceiver.value)
            """
        }.list

        List (
            q"val mightBeNull = false",
            q"val codesAsObject = true",
            q"""
                def run($jsonParser: com.paytronix.utils.interchange.format.json.InterchangeJsonParser,
                        $receiver: com.paytronix.utils.interchange.base.Receiver[$targetType]) =
                    $jsonParser.require(com.fasterxml.jackson.core.JsonToken.START_OBJECT) >>
                    $jsonParser.peekFields(scala.Array($determinant)) >>= {
                        case scala.Array(Some($discrimValue)) =>
                            $discrimValue match {
                                case ..$decodeAlts
                                case _ =>
                                    com.paytronix.utils.interchange.base.atProperty($determinant) {
                                        com.paytronix.utils.scala.result.FailedG (
                                            "unexpected value \"" + $discrimValue + "\" (expected one of " + ${validTags.list.mkString(", ")} + ")",
                                            $jsonParser.terminal
                                        )
                                    }
                            }
                        case _ =>
                            com.paytronix.utils.interchange.base.atProperty($determinant) {
                                com.paytronix.utils.scala.result.FailedG("required but missing", $jsonParser.terminal)
                            }
                    }
            """
        )
    }

    // hack to avoid macro not liking synonyms
    def structureCoderDef[A: c.WeakTypeTag](c: Context): c.Expr[JsonCoder[A]] = structureCoderDefImpl[A](c)
    def structureDecoderDef[A: c.WeakTypeTag](c: Context): c.Expr[JsonDecoder[A]] = structureDecoderDefImpl[A](c)
    def structureEncoderDef[A: c.WeakTypeTag](c: Context): c.Expr[JsonEncoder[A]] = structureEncoderDefImpl[A](c)
    def wrapperCoderDef[A: c.WeakTypeTag](c: Context): c.Expr[JsonCoder[A]] = wrapperCoderDefImpl[A](c)
    def wrapperDecoderDef[A: c.WeakTypeTag](c: Context): c.Expr[JsonDecoder[A]] = wrapperDecoderDefImpl[A](c)
    def wrapperEncoderDef[A: c.WeakTypeTag](c: Context): c.Expr[JsonEncoder[A]] = wrapperEncoderDefImpl[A](c)
}
