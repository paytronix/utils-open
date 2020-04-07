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

package com.paytronix.utils.interchange.format.json

import cats.data.NonEmptyList
import scala.reflect.macros.whitebox.Context

import com.paytronix.utils.interchange.base

import base.derive.DeriveCoderMacros

private[json] class deriveImpl(val c: Context) extends DeriveCoderMacros {
    import c.universe.{Annotation, Ident, Quasiquote, TermName, Tree, TreeTag, Type, typeOf, weakTypeTag}

    type Coder[A] = JsonCoder[A]

    //def implicitCoderName = TermName("jsonCoder")

    def coderType(tpe: Tree) = {
        tq"com.paytronix.utils.interchange.format.json.JsonCoder[$tpe]"
    }

    def materializeCoder(tpe: Type, annotations: List[Annotation]): Tree = {
        q"""
            com.paytronix.utils.interchange.format.json.JsonCoder.make[$tpe] (
                ${materializeEncoder(tpe, annotations)},
                ${materializeDecoder(tpe, annotations)}
            )
        """
    }

    type Encoder[A] = JsonEncoder[A]

    def encoderType(tpe: Tree): Tree = {
        tq"com.paytronix.utils.interchange.format.json.JsonEncoder[$tpe]"
    }

    def materializeEncoder(tpe: Type, annotations: List[Annotation]): Tree = {
        wrapCoderForAnnotations (
            coder       = q"com.paytronix.utils.interchange.format.json.JsonEncoder[$tpe]",
            annotations = annotations,
            nullable    = inside => q"com.paytronix.utils.interchange.format.json.container.nullableJsonEncoder($inside)",
            default     = value => inside => inside
        )
    }

    type Decoder[A] = JsonDecoder[A]

    def decoderType(tpe: Tree): Tree = {
        tq"com.paytronix.utils.interchange.format.json.JsonDecoder[$tpe]"
    }

    def materializeDecoder(tpe: Type, annotations: List[Annotation]): Tree = {
        wrapCoderForAnnotations (
            coder       = q"com.paytronix.utils.interchange.format.json.JsonDecoder[$tpe]",
            annotations = annotations,
            nullable    = inside => q"com.paytronix.utils.interchange.format.json.container.nullableJsonDecoder($inside)",
            default     = value => inside => q"$inside.default($value)"
        )
    }

    def makeStructureCoder(target: Tree): Tree = {
        q"com.paytronix.utils.interchange.format.json.derive.structure.coder[$target]"
    }

    def structureEncoderMethods (
        tpe: Type,
        encoderFor: Property => Tree,
        model: Structure
    ): List[Tree] = {
        val flattenTpe = typeOf[com.paytronix.utils.interchange.format.json.flatten]

        val jsonGenerator = TermName(c.freshName())
        val instance = TermName(c.freshName())
        val encode = sequenceResultBindings(model.properties) (
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

    def structureDecoderMethods (
        tpe: Type,
        decoderFor: Property => Tree,
        model: Structure
    ): List[Tree] = {
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

        val handleFlattenFields = sequenceResultBindings(flattenProps) (
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

        val handleMissing = sequenceResultBindings(normalProps) (
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

    def makeWrapperCoder(target: Tree): Tree = {
        q"com.paytronix.utils.interchange.format.json.derive.wrapper.coder[$target]"
    }

    def wrapperEncoderMethods (
        targetType: Type,
        property: Property,
        unwrap: Tree => Tree
    ): List[Tree] = {
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

    def wrapperDecoderMethods (
        targetType: Type,
        property: Property,
        wrap: Tree => Tree
    ): List[Tree] = {
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
    def deriveImplicitAdHocUnionCoderAnnotation(annottees: c.Expr[Any]*): c.Expr[Any] =
        addToCompanion(annottees) { (targetName, annotations) =>
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
                implicit val ${implicitCoderName}: ${coderType(Ident(targetName))} =
                    com.paytronix.utils.interchange.format.json.derive.adHocUnion.coder[$targetName]($noApplicableAlternates, ..$alts)
            """)
        }
    */

    private def parseAdHocUnionAlternates(alternateTrees: Seq[Tree]): NonEmptyList[Type] = {
        val targets = alternateTrees.toList.map {
            // FIXME? probably this should really typecheck the tree and get the type out from there rather than pattern matching the apply
            case q"$_[$tpeTree]" => tpeTree.tpe
            case tree =>
                sys.error("unrecognized union alternative syntax: " + tree + ". expected alternate[Type]")
        }

        targets match {
            case Nil => sys.error("union cannot be made with no alternates!")
            case hd :: tl => NonEmptyList(hd, tl)
        }
    }

    def adHocUnionCoderDef[A: c.WeakTypeTag](noApplicableAlternates: c.Tree, alternates: c.Tree*): c.Expr[JsonCoder[A]] = try {
        val targetType = weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Coder")
        val targetSubtypes = parseAdHocUnionAlternates(alternates)
        val targetSubtypesList = targetSubtypes.toList
        val subtypeEncoderNames = targetSubtypesList.map { _ => TermName(c.freshName()) }
        val subtypeDecoderNames = targetSubtypesList.map { _ => TermName(c.freshName()) }

        val declareCoders = targetSubtypesList.zip(subtypeEncoderNames.zip(subtypeDecoderNames)).flatMap { case (tpe, (encoderName, decoderName)) =>
            List(
                q"lazy val $decoderName = ${materializeDecoder(tpe, Nil)}",
                q"lazy val $encoderName = ${materializeEncoder(tpe, Nil)}"
            )
        }

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypesList.zip(subtypeEncoderNames)).toStream
        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypesList.zip(subtypeDecoderNames)).toStream

        c.Expr[JsonCoder[A]](q"""
            {
                object $name extends ${coderType(tq"$targetType")} {
                    ..$declareCoders
                    object decode extends ${decoderType(tq"$targetType")} {
                        ..${adHocUnionDecoderMethods(noApplicableAlternates, targetType, targetSubtypes, subtypeDecoderNamesByType.mapValues(n => Ident(n)))}
                    }
                    object encode extends ${encoderType(tq"$targetType")} {
                        ..${adHocUnionEncoderMethods(noApplicableAlternates, targetType, targetSubtypes, subtypeEncoderNamesByType.mapValues(n => Ident(n)))}
                    }
                }
                $name: ${coderType(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def adHocUnionDecoderDef[A: c.WeakTypeTag](noApplicableAlternates: c.Tree, alternates: c.Tree*): c.Expr[JsonDecoder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName, weakTypeTag}

        val targetType = weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Decoder")
        val targetSubtypes = parseAdHocUnionAlternates(alternates)
        val targetSubtypesList = targetSubtypes.toList
        val subtypeDecoderNames = targetSubtypesList.map { _ => TermName(c.freshName()) }

        val declareDecoders = (targetSubtypesList.zip(subtypeDecoderNames)).map { case (tpe, decoderName) =>
            q"lazy val $decoderName = ${materializeDecoder(tpe, Nil)}"
        }

        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypesList.zip(subtypeDecoderNames)).toStream

        c.Expr[JsonDecoder[A]](q"""
            {
                object $name extends ${decoderType(tq"$targetType")} {
                    ..$declareDecoders
                    ..${adHocUnionDecoderMethods(noApplicableAlternates, targetType, targetSubtypes, subtypeDecoderNamesByType.mapValues(n => Ident(n)))}
                }
                $name: ${decoderType(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def adHocUnionEncoderDef[A: c.WeakTypeTag](noApplicableAlternates: c.Tree, alternates: c.Tree*): c.Expr[JsonEncoder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName, weakTypeTag}
        val targetType = weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Encoder")
        val targetSubtypes = parseAdHocUnionAlternates(alternates)
        val targetSubtypesList = targetSubtypes.toList
        val subtypeEncoderNames = targetSubtypesList.map { _ => TermName(c.freshName()) }

        val declareEncoders = (targetSubtypesList.zip(subtypeEncoderNames)).map { case (tpe, encoderName) =>
            q"lazy val $encoderName = ${materializeEncoder(tpe, Nil)}"
        }

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypesList.zip(subtypeEncoderNames)).toStream

        c.Expr[JsonEncoder[A]](q"""
            {
                object $name extends ${encoderType(tq"$targetType")} {
                    ..$declareEncoders
                    ..${adHocUnionEncoderMethods(noApplicableAlternates, targetType, targetSubtypes, subtypeEncoderNamesByType.mapValues(n => Ident(n)))}
                }
                $name: ${encoderType(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def adHocUnionEncoderMethods (
        noApplicableAlternates: c.Tree,
        targetType: Type,
        targetSubtypes: NonEmptyList[Type],
        encoderFor: Type => Tree
    ): List[Tree] = {
        import c.universe.{TermName, Quasiquote}

        val allEncoders = TermName(c.freshName())
        val jsonGenerator = TermName(c.freshName())
        val instance = TermName(c.freshName())
        val targetSubtypesList = targetSubtypes.toList

        val encodeAlts = targetSubtypesList.zipWithIndex.map { case (subtype, index) =>
            val encoder = encoderFor(subtype)
            val value = TermName(c.freshName())
            cq"""
                $value: $subtype =>
                    com.paytronix.utils.interchange.base.atIndex($index) {
                        com.paytronix.utils.scala.result.tryCatchResultG(com.paytronix.utils.interchange.base.terminal) {
                            $encoder.run($value, $jsonGenerator)
                        }
                    }
            """
        }

        val value = TermName(c.freshName())

        List (
            q"private val $allEncoders = List(..${targetSubtypesList.map(t => encoderFor(t))})",
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

    def adHocUnionDecoderMethods (
        noApplicableAlternates: c.Tree,
        targetType: Type,
        targetSubtypes: NonEmptyList[Type],
        decoderFor: Type => Tree
    ): List[Tree] = {
        import c.universe.{Ident, Quasiquote, TermName, Tree}

        val allDecoders = TermName(c.freshName())
        val jsonParser = TermName(c.freshName())
        val receiver = TermName(c.freshName())
        val targetSubtypesList = targetSubtypes.toList

        val decode = targetSubtypesList.foldRight[Tree] (
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
            q"private val $allDecoders = List(..${targetSubtypesList.map(decoderFor)})",
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
    def deriveImplicitTaggedUnionCoderAnnotation(annottees: c.Expr[Any]*): c.Expr[Any] =
        addToCompanion(annottees) { (targetName, annotations) =>
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
                implicit val ${implicitCoderName}: ${coderType(Ident(targetName))} =
                    com.paytronix.utils.interchange.format.json.derive.taggedUnion.coder[$targetName]($determinant, ..$alts)
            """)
        }
    */

    private def parseTaggedUnionAlternates(alternateTrees: Seq[Tree]): NonEmptyList[(Type, String)] = {
        import c.universe.{Quasiquote, TreeTag}

        val targets = alternateTrees.toList.map {
            case q"$_[$tpeTree]($tag)" => (tpeTree.tpe, c.eval(c.Expr[String](c.untypecheck(tag.duplicate))))
            case tree =>
                sys.error("unrecognized union alternative syntax: " + tree + ". expected either alternate[Type](\"Tag\")")
        }

        targets match {
            case Nil => sys.error("union cannot be made with no alternates!")
            case hd :: tl => NonEmptyList(hd, tl)
        }
    }

    def taggedUnionCoderDef[A: c.WeakTypeTag](determinant: c.Tree, alternates: c.Tree*): c.Expr[JsonCoder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName}

        val targetType = weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Coder")
        val targetSubtypes = parseTaggedUnionAlternates(alternates)
        val targetSubtypesList = targetSubtypes.toList
        val subtypeEncoderNames = targetSubtypesList.map { _ => TermName(c.freshName()) }
        val subtypeDecoderNames = targetSubtypesList.map { _ => TermName(c.freshName()) }

        val declareCoders = (targetSubtypesList.zip(subtypeEncoderNames.zip(subtypeDecoderNames))).flatMap { case ((tpe, _), (encoderName, decoderName)) =>
            List(
                q"lazy val $decoderName = ${materializeDecoder(tpe, Nil)}",
                q"lazy val $encoderName = ${materializeEncoder(tpe, Nil)}"
            )
        }

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypesList.zip(subtypeEncoderNames)).toStream
        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypesList.zip(subtypeDecoderNames)).toStream

        val decoderFor = subtypeDecoderNamesByType.map { case ((tpe, _), name) => (tpe, Ident(name)) }
        val encoderFor = subtypeEncoderNamesByType.map { case ((tpe, _), name) => (tpe, Ident(name)) }

        c.Expr[Coder[A]](q"""
            {
                object $name extends ${coderType(tq"$targetType")} {
                    ..$declareCoders
                    object decode extends ${decoderType(tq"$targetType")} {
                        ..${taggedUnionDecoderMethods(determinant, targetType, targetSubtypes, decoderFor)}
                    }
                    object encode extends ${encoderType(tq"$targetType")} {
                        ..${taggedUnionEncoderMethods(determinant, targetType, targetSubtypes, encoderFor)}
                    }
                }
                $name: ${coderType(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def taggedUnionDecoderDef[A: c.WeakTypeTag](determinant: c.Tree, alternates: c.Tree*): c.Expr[JsonDecoder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName, weakTypeTag}

        val targetType = weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Decoder")
        val targetSubtypes = parseTaggedUnionAlternates(alternates)
        val targetSubtypesList = targetSubtypes.toList
        val subtypeDecoderNames = targetSubtypesList.map { _ => TermName(c.freshName()) }

        val declareDecoders = (targetSubtypesList.zip(subtypeDecoderNames)).map { case ((tpe, _), decoderName) =>
            q"lazy val $decoderName = ${materializeDecoder(tpe, Nil)}"
        }

        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypesList.zip(subtypeDecoderNames)).toStream

        val decoderFor = subtypeDecoderNamesByType.map { case ((tpe, _), name) => (tpe, Ident(name)) }

        c.Expr[Decoder[A]](q"""
            {
                object $name extends ${decoderType(tq"$targetType")} {
                    ..$declareDecoders
                    ..${taggedUnionDecoderMethods(determinant, targetType, targetSubtypes, decoderFor)}
                }
                $name: ${decoderType(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def taggedUnionEncoderDef[A: c.WeakTypeTag](determinant: c.Tree, alternates: c.Tree*): c.Expr[JsonEncoder[A]] = try {
        import c.universe.{Ident, Quasiquote, TermName, weakTypeTag}
        val targetType = weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Encoder")
        val targetSubtypes = parseTaggedUnionAlternates(alternates)
        val targetSubtypesList = targetSubtypes.toList
        val subtypeEncoderNames = targetSubtypesList.map { _ => TermName(c.freshName()) }

        val declareEncoders = (targetSubtypesList.zip(subtypeEncoderNames)).map { case ((tpe, _), encoderName) =>
            q"lazy val $encoderName = ${materializeEncoder(tpe, Nil)}"
        }

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypesList.zip(subtypeEncoderNames)).toStream

        val encoderFor = subtypeEncoderNamesByType.map { case ((tpe, _), name) => (tpe, Ident(name)) }

        c.Expr[Encoder[A]](q"""
            {
                object $name extends ${encoderType(tq"$targetType")} {
                    ..$declareEncoders
                    ..${taggedUnionEncoderMethods(determinant, targetType, targetSubtypes, encoderFor)}
                }
                $name: ${encoderType(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def taggedUnionEncoderMethods (
        determinant: c.Tree,
        targetType: Type,
        targetSubtypes: NonEmptyList[(Type, String)],
        encoderFor: Type => Tree
    ): List[Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}

        val jsonGenerator = TermName(c.freshName())
        val instance = TermName(c.freshName())

        val encodeAlts = targetSubtypes.toList.map { case (tpe, tag) =>
            val encoder = encoderFor(tpe)
            val value = TermName(c.freshName())
            cq"""
                $value: $tpe =>
                    com.paytronix.utils.scala.result.tryCatchResultG(com.paytronix.utils.interchange.base.terminal) {
                        $jsonGenerator.filterNextObject( new com.paytronix.utils.interchange.format.json.InterchangeJsonGenerator.ObjectFilter {
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

    def taggedUnionDecoderMethods (
        determinant: c.Tree,
        targetType: Type,
        targetSubtypes: NonEmptyList[(Type, String)],
        decoderFor: Type => Tree
    ): List[Tree] = {
        import c.universe.{Ident, Quasiquote, TermName}

        val jsonParser = TermName(c.freshName())
        val receiver = TermName(c.freshName())
        val discrimValue = TermName(c.freshName())

        val targetSubtypesList = targetSubtypes.toList
        val validTags = targetSubtypesList.map(_._2)

        val decodeAlts = targetSubtypesList.map { case (tpe, tag) =>
            val subReceiver = TermName(c.freshName())
            cq"""
                $tag =>
                    val $subReceiver = new com.paytronix.utils.interchange.base.Receiver[$tpe]
                    ${decoderFor(tpe)}.run($jsonParser, $subReceiver) >> $receiver($subReceiver.value)
            """
        }

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
                                            "unexpected value \"" + $discrimValue + "\" (expected one of " + ${validTags.mkString(", ")} + ")",
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
    def structureCoderDef[A: c.WeakTypeTag]: c.Expr[JsonCoder[A]] = structureCoderDefImpl[A]
    def structureDecoderDef[A: c.WeakTypeTag]: c.Expr[JsonDecoder[A]] = structureDecoderDefImpl[A]
    def structureEncoderDef[A: c.WeakTypeTag]: c.Expr[JsonEncoder[A]] = structureEncoderDefImpl[A]
    def wrapperCoderDef[A: c.WeakTypeTag]: c.Expr[JsonCoder[A]] = wrapperCoderDefImpl[A]
    def wrapperDecoderDef[A: c.WeakTypeTag]: c.Expr[JsonDecoder[A]] = wrapperDecoderDefImpl[A]
    def wrapperEncoderDef[A: c.WeakTypeTag]: c.Expr[JsonEncoder[A]] = wrapperEncoderDefImpl[A]
}
