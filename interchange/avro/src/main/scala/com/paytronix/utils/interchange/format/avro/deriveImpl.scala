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

package com.paytronix.utils.interchange.format.avro

import scala.reflect.macros.whitebox.Context

import scalaz.NonEmptyList

import com.paytronix.utils.interchange.base

import base.derive.DeriveCoderMacros
import utils.sanitizeSchemaName
import NonEmptyList.{nel, nels}

private[avro] class deriveImpl(val c: Context) extends DeriveCoderMacros {
    import c.universe.{Annotation, Block, BlockTag, Ident, Quasiquote, TermName, Tree, TreeTag, Type, weakTypeTag}

    type Coder[A] = AvroCoder[A]

    //def implicitCoderName = TermName("avroCoder")

    def coderType(tpe: Tree) = {
        tq"com.paytronix.utils.interchange.format.avro.AvroCoder[$tpe]"
    }

    def materializeCoder(tpe: Type, annotations: List[Annotation]): Tree = {
        q"""
            com.paytronix.utils.interchange.format.avro.AvroCoder.make[$tpe] (
                ${materializeEncoder(tpe, annotations)},
                ${materializeDecoder(tpe, annotations)}
            )
        """
    }

    type Encoder[A] = AvroEncoder[A]

    def encoderType(tpe: Tree): Tree = {
        tq"com.paytronix.utils.interchange.format.avro.AvroEncoder[$tpe]"
    }

    def materializeEncoder(tpe: Type, annotations: List[Annotation]): Tree = {
        wrapCoderForAnnotations (
            coder       = q"com.paytronix.utils.interchange.format.avro.AvroEncoder[$tpe]",
            annotations = annotations,
            nullable    = inside => q"com.paytronix.utils.interchange.format.avro.container.nullableAvroEncoder($inside)",
            default     = value => inside => q"$inside.default($value)"
        )
    }

    type Decoder[A] = AvroDecoder[A]

    def decoderType(tpe: Tree): Tree = {
        tq"com.paytronix.utils.interchange.format.avro.AvroDecoder[$tpe]"
    }

    def materializeDecoder(tpe: Type, annotations: List[Annotation]): Tree = {
        wrapCoderForAnnotations (
            coder       = q"com.paytronix.utils.interchange.format.avro.AvroDecoder[$tpe]",
            annotations = annotations,
            nullable    = inside => q"com.paytronix.utils.interchange.format.avro.container.nullableAvroDecoder($inside)",
            default     = value => inside => q"$inside.default($value)"
        )
    }

    def makeStructureCoder(target: Tree): Tree = {
        q"com.paytronix.utils.interchange.format.avro.derive.structure.coder[$target]"
    }

    private final case class AvroFieldNaming(addAliases: Tree => List[Tree])
    private def avroFieldNaming (
        prop: Property
    ): AvroFieldNaming = {
        val avroAliasesTpe = c.typeOf[com.paytronix.utils.interchange.format.avro.aliases]

        val addFieldAliases: Tree => List[Tree] = field => prop.annotations
            .collectFirst { case annot if annot.tree.tpe =:= avroAliasesTpe && annot.tree.children.size > 1 => annot }
            .map { annot => annot.tree.children.tail }
            .getOrElse { Nil }
            .map { tree => q"$field.addAlias($tree)" }

        AvroFieldNaming(addFieldAliases)
    }

    private final case class AvroTypeNaming(name: Tree, namespace: Tree, addAliases: Tree => List[Tree])
    private def avroTypeNaming (
        tpe: Type,
        model: Structure
    ): AvroTypeNaming = {
        val avroNameTpe = c.typeOf[com.paytronix.utils.interchange.format.avro.name]
        val avroAliasesTpe = c.typeOf[com.paytronix.utils.interchange.format.avro.aliases]

        val namespace = tpe.typeSymbol.owner.fullName
        val defaultName =
            tpe.typeSymbol.name.decodedName.toString +
            (if (tpe.typeArgs.nonEmpty) "__" + tpe.typeArgs.map(arg => sanitizeSchemaName(arg.typeSymbol.fullName)).mkString("") else "")

        val avroName: Tree = model.annotations
            .collectFirst { case annot if annot.tree.tpe =:= avroNameTpe && annot.tree.children.size > 1 => annot }
            .map { annot => annot.tree.children.tail.head }
            .getOrElse { q"${defaultName}" }
        val addTypeAliases: Tree => List[Tree] = schema => model.annotations
            .collectFirst { case annot if annot.tree.tpe =:= avroAliasesTpe && annot.tree.children.size > 1 => annot }
            .map { annot => annot.tree.children.tail }
            .getOrElse { Nil }
            .map { tree => q"$schema.addAlias($tree)" }

        AvroTypeNaming(avroName, q"$namespace", addTypeAliases)
    }

    private def makeStructureSchema (
        tpe: Type,
        encoderOrDecoderFor: Property => Tree,
        model: Structure
    ): Tree = {
        val schema = TermName(c.freshName())

        val props = (model.constructorProperties ++ model.mutableProperties).sorted
        val fields = props.map { prop =>
            val field = TermName(c.freshName())
            val eod = encoderOrDecoderFor(prop)
            val naming = avroFieldNaming(prop)

            q"""
                {
                    val $field = com.paytronix.utils.interchange.format.avro.utils.makeField(${prop.externalName}, ${eod}.schema, ${eod}.defaultJson)
                    ..${naming.addAliases(q"$field")}
                    $field
                }
            """
        }

        val naming = avroTypeNaming(tpe, model)

        q"""
            {
                val $schema = org.apache.avro.Schema.createRecord(${naming.name}, "", ${naming.namespace}, false)
                ..${naming.addAliases(q"$schema")}
                $schema.setFields(java.util.Arrays.asList(..$fields))
                $schema
            }
        """
    }

    def structureEncoderMethods (
        tpe: Type,
        encoderFor: Property => Tree,
        model: Structure
    ): List[Tree] = {
        val props = (model.constructorProperties ++ model.mutableProperties).sorted // avro doesn't encode read-only fields

        val avroEncoder = TermName(c.freshName())
        val instance = TermName(c.freshName())
        val encode = sequenceResultBindings(props) (
            action = prop => q"${encoderFor(prop)}.run(${prop.read(Ident(instance))}, $avroEncoder)",
            accept = (_, _) => q"",
            body = _ => q"com.paytronix.utils.scala.result.Okay.unit"
        )

        val obj = TermName(c.freshName())
        val encodeDefaultJson = sequenceResultBindings(props) (
            action = prop => q"${encoderFor(prop)}.encodeDefaultJson(${prop.read(Ident(instance))})",
            accept = (prop, value) => q"$obj.put(${prop.externalName}, $value)",
            body = _ => q"com.paytronix.utils.scala.result.Okay($obj)"
        )

        List (
            q"val schema = ${makeStructureSchema(tpe, encoderFor, model)}",
            q"val defaultJson = scala.None",
            q"""
                def encodeDefaultJson($instance: $tpe) = {
                    val $obj = org.codehaus.jackson.node.JsonNodeFactory.instance.objectNode
                    $encodeDefaultJson
                }
            """,
            q"""
                def run($instance: $tpe, $avroEncoder: org.apache.avro.io.Encoder) = $encode
            """
        )
    }

    def structureDecoderMethods (
        tpe: Type,
        decoderFor: Property => Tree,
        model: Structure
    ): List[Tree] = {
        val props = (model.constructorProperties ++ model.mutableProperties).sorted
        val resolvingDecoder = TermName(c.freshName())
        val receiver = TermName(c.freshName())
        val readFieldOrder = TermName(c.freshName())
        val decodeField = TermName(c.freshName())
        val index = TermName(c.freshName())
        val propReceiverNames = props.map { prop => TermName(c.freshName()) }
        val propReceiverByProp = Map.empty ++ (props zip propReceiverNames)
        val declarePropReceivers = (propReceiverNames zip props).map { case (receiverName, prop) =>
            q"val $receiverName = new com.paytronix.utils.interchange.base.Receiver[${prop.tpe}]()"
        }
        val decode = sequenceResultBindings(0 until props.size) (
            action = (i: Int) => q"$decodeField($readFieldOrder($i).pos)",
            accept = (_, _) => q"",
            body = { _ =>
                val v = model.constructAndAssign(prop => q"${propReceiverByProp(prop)}.value")
                q"$receiver($v)"
            }
        )
        val fieldDecoders = (props zip propReceiverNames).zipWithIndex.map { case ((prop, receiverName), propIndex) =>
            cq"""
                $propIndex =>
                    com.paytronix.utils.interchange.base.atProperty(${prop.externalName}) {
                        ${decoderFor(prop)}.run($resolvingDecoder, $receiverName)
                    }
            """
        }

        List (
            q"val schema = ${makeStructureSchema(tpe, decoderFor, model)}",
            q"val defaultJson = scala.None",
            q"""
                def run($resolvingDecoder: org.apache.avro.io.ResolvingDecoder,
                        $receiver: com.paytronix.utils.interchange.base.Receiver[$tpe]) = {
                    ..$declarePropReceivers
                    def $decodeField($index: scala.Int): com.paytronix.utils.interchange.base.CoderResult[scala.Unit] = {
                        $index match {
                            case ..$fieldDecoders
                        }
                    }
                    val $readFieldOrder = $resolvingDecoder.readFieldOrder
                    $decode
                }
            """
        )

    }

    def makeWrapperCoder(target: Tree): Tree = {
        q"com.paytronix.utils.interchange.format.avro.derive.wrapper.coder[$target]"
    }

    def wrapperEncoderMethods (
        targetType: Type,
        property: Property,
        unwrap: Tree => Tree
    ): List[Tree] = {
        val instance = TermName(c.freshName())
        val avroEncoder = TermName(c.freshName())
        List (
            q"val schema = ${property.encoderName}.schema",
            q"val defaultJson = ${property.encoderName}.defaultJson",
            q"""
                def encodeDefaultJson($instance: $targetType) =
                    ${property.encoderName}.encodeDefaultJson(${unwrap(Ident(instance))})
            """,
            q"""
                def run($instance: $targetType, $avroEncoder: org.apache.avro.io.Encoder) =
                    com.paytronix.utils.interchange.base.atProperty(${property.externalName}) {
                        ${property.encoderName}.run(${unwrap(Ident(instance))}, $avroEncoder)
                    }
            """
        )
    }

    def wrapperDecoderMethods (
        targetType: Type,
        property: Property,
        wrap: Tree => Tree
    ): List[Tree] = {
        val resolvingDecoder = TermName(c.freshName())
        val out = TermName(c.freshName())
        val receiver = TermName(c.freshName())
        List (
            q"val schema = ${property.decoderName}.schema",
            q"val defaultJson = ${property.decoderName}.defaultJson",
            q"""
                def run($resolvingDecoder: org.apache.avro.io.ResolvingDecoder, $out: com.paytronix.utils.interchange.base.Receiver[$targetType]) = {
                    val $receiver = new com.paytronix.utils.interchange.base.Receiver[${property.tpe}]
                    com.paytronix.utils.interchange.base.atProperty(${property.externalName}) {
                        ${property.decoderName}.run($resolvingDecoder, $receiver) >> $out(${wrap(q"$receiver.value")})
                    }
                }
            """
        )
    }

    private def makeUnionSchema (
        targetSubtypes: NonEmptyList[Type],
        encoderOrDecoderFor: Type => Tree
    ): Tree = {

        val schemas = targetSubtypes.map(encoderOrDecoderFor).map { coder =>
            q"$coder.schema"
        }.list

        q"org.apache.avro.Schema.createUnion(java.util.Arrays.asList(..$schemas))"
    }

    def unionEncoderMethods (
        targetType: Type,
        targetSubtypes: NonEmptyList[Type],
        encoderFor: Type => Tree
    ): List[Tree] = {
        val avroEncoder = TermName(c.freshName())
        val instance = TermName(c.freshName())

        val encodeAlts = targetSubtypes.list.zipWithIndex.map { case (subtype, index) =>
            val encoder = encoderFor(subtype)
            val value = TermName(c.freshName())
            cq"""
                $value: $subtype =>
                    com.paytronix.utils.interchange.base.atIndex($index) {
                        com.paytronix.utils.scala.result.tryCatchResultG(com.paytronix.utils.interchange.base.terminal) {
                            $avroEncoder.writeIndex($index)
                            $encoder.run($value, $avroEncoder)
                        }
                    }
            """
        }

        val value = TermName(c.freshName())

        List (
            q"val schema = ${makeUnionSchema(targetSubtypes, encoderFor)}",
            q"val defaultJson = scala.None",
            q"""
                def encodeDefaultJson($instance: $targetType) =
                    $instance match {
                        case $value: ${targetSubtypes.head} => ${encoderFor(targetSubtypes.head)}.encodeDefaultJson($value)
                        case _ =>
                            com.paytronix.utils.scala.result.FailedG (
                                "cannot encode value as a default because Avro only supports defaulting to the first alternative of a union",
                                com.paytronix.utils.interchange.base.CoderFailure.terminal
                            )
                    }
            """,
            q"""
                def run($instance: $targetType, $avroEncoder: org.apache.avro.io.Encoder) =
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

    def unionDecoderMethods (
        targetType: Type,
        targetSubtypes: NonEmptyList[Type],
        decoderFor: Type => Tree
    ): List[Tree] = {
        val resolvingDecoder = TermName(c.freshName())
        val receiver = TermName(c.freshName())
        val invalidIndex = TermName(c.freshName())

        val decodeAlts = targetSubtypes.list.zipWithIndex.map { case (subtype, index) =>
            val decoder = decoderFor(subtype)
            val subReceiver = TermName(c.freshName())
            val failed = TermName(c.freshName())
            cq"""
                $index =>
                    com.paytronix.utils.interchange.base.atIndex($index) {
                        com.paytronix.utils.scala.result.tryCatchResultG(com.paytronix.utils.interchange.base.terminal) {
                            val $subReceiver = new com.paytronix.utils.interchange.base.Receiver[$subtype]
                            $decoder.run($resolvingDecoder, $subReceiver) match {
                                case _: com.paytronix.utils.scala.result.Okay[_] =>
                                    $receiver($subReceiver.value)
                                case $failed => $failed
                            }
                        }
                    }
            """
        }

        List (
            q"val schema = ${makeUnionSchema(targetSubtypes, decoderFor)}",
            q"val defaultJson = scala.None",
            q"""
                def run($resolvingDecoder: org.apache.avro.io.ResolvingDecoder,
                        $receiver: com.paytronix.utils.interchange.base.Receiver[$targetType]) =
                    com.paytronix.utils.scala.result.tryCatchResultG(com.paytronix.utils.interchange.base.terminal) {
                        $resolvingDecoder.readIndex() match {
                            case ..$decodeAlts
                            case $invalidIndex =>
                                com.paytronix.utils.scala.result.FailedG (
                                    "invalid union index " + $invalidIndex,
                                    com.paytronix.utils.interchange.base.CoderFailure.terminal
                                )
                        }
                    }
            """
        )

    }

    /* 2014-08-27 RMM: having multiple annotation macros which addToCompanion causes the compiler to not emit the object class (Blah$) even though
                       it doesn't error at runtime.
    def deriveImplicitUnionCoderAnnotation(annottees: c.Expr[Any]*): c.Expr[Any] =
        addToCompanion(annottees) { (targetName, annotations) =>
            def isUnionAnno(t: Tree): Boolean =
                t match {
                    case Ident(n: Name) => n.decodedName.toString == "union" // the tree isn't yet typechecked and resolved so uhh, this is spotty
                    case _ => false
                }

            val alts =
                annotations.collectFirst { case q"new $anno(..$alts)" if isUnionAnno(anno) => alts }
                    .getOrElse(sys.error("couldn't find union annotation among " + annotations))

            List(q"""
                implicit val ${implicitCoderName}: ${coderType(Ident(targetName))} =
                    com.paytronix.utils.interchange.format.avro.derive.union.coder[$targetName](..$alts)
             """)
        }
    */

    private def parseUnionAlternates(alternateTrees: Seq[Tree]): NonEmptyList[Type] = {
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

    def unionCoderDef[A: c.WeakTypeTag](alternates: c.Tree*): c.Expr[AvroCoder[A]] = try {
        val targetType = weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Coder")
        val targetSubtypes = parseUnionAlternates(alternates)
        val subtypeEncoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }
        val subtypeDecoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareCoders = (targetSubtypes zip (subtypeEncoderNames zip subtypeDecoderNames)).flatMap { case (tpe, (encoderName, decoderName)) =>
            nels (
                q"lazy val $decoderName = ${materializeDecoder(tpe, Nil)}",
                q"lazy val $encoderName = ${materializeEncoder(tpe, Nil)}"
            )
        }.list

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeEncoderNames).stream
        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeDecoderNames).stream

        c.Expr[Coder[A]](q"""
            {
                object $name extends ${coderType(tq"$targetType")} {
                    ..$declareCoders
                    object decode extends ${decoderType(tq"$targetType")} {
                        ..${unionDecoderMethods(targetType, targetSubtypes, subtypeDecoderNamesByType.mapValues(n => Ident(n)))}
                    }
                    object encode extends ${encoderType(tq"$targetType")} {
                        ..${unionEncoderMethods(targetType, targetSubtypes, subtypeEncoderNamesByType.mapValues(n => Ident(n)))}
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


    def unionDecoderDef[A: c.WeakTypeTag](alternates: c.Tree*): c.Expr[AvroDecoder[A]] = try {
        val targetType = weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Decoder")
        val targetSubtypes = parseUnionAlternates(alternates)
        val subtypeDecoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareDecoders = (targetSubtypes zip subtypeDecoderNames).map { case (tpe, decoderName) =>
            q"lazy val $decoderName = ${materializeDecoder(tpe, Nil)}"
        }.list

        val subtypeDecoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeDecoderNames).stream

        c.Expr[Decoder[A]](q"""
            {
                object $name extends ${decoderType(tq"$targetType")} {
                    ..$declareDecoders
                    ..${unionDecoderMethods(targetType, targetSubtypes, subtypeDecoderNamesByType.mapValues(n => Ident(n)))}
                }
                $name: ${decoderType(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    def unionEncoderDef[A: c.WeakTypeTag](alternates: c.Tree*): c.Expr[AvroEncoder[A]] = try {
        val targetType = weakTypeTag[A].tpe
        val name = TermName(targetType.typeSymbol.name.decodedName.toString + "Encoder")
        val targetSubtypes = parseUnionAlternates(alternates)
        val subtypeEncoderNames = targetSubtypes.map { _ => TermName(c.freshName()) }

        val declareEncoders = (targetSubtypes zip subtypeEncoderNames).map { case (tpe, encoderName) =>
            q"lazy val $encoderName = ${materializeEncoder(tpe, Nil)}"
        }.list

        val subtypeEncoderNamesByType = Map.empty ++ (targetSubtypes zip subtypeEncoderNames).stream

        c.Expr[Encoder[A]](q"""
            {
                object $name extends ${encoderType(tq"$targetType")} {
                    ..$declareEncoders
                    ..${unionEncoderMethods(targetType, targetSubtypes, subtypeEncoderNamesByType.mapValues(n => Ident(n)))}
                }
                $name: ${encoderType(tq"$targetType")}
            }
        """)
    } catch { case e: Exception =>
        System.err.println("uhoh, macro explosion!")
        e.printStackTrace(System.err)
        null
    }

    // hack to avoid macro not liking synonyms
    def structureCoderDef[A: c.WeakTypeTag]: c.Expr[AvroCoder[A]] = structureCoderDefImpl[A]
    def structureDecoderDef[A: c.WeakTypeTag]: c.Expr[AvroDecoder[A]] = structureDecoderDefImpl[A]
    def structureEncoderDef[A: c.WeakTypeTag]: c.Expr[AvroEncoder[A]] = structureEncoderDefImpl[A]
    def wrapperCoderDef[A: c.WeakTypeTag]: c.Expr[AvroCoder[A]] = wrapperCoderDefImpl[A]
    def wrapperDecoderDef[A: c.WeakTypeTag]: c.Expr[AvroDecoder[A]] = wrapperDecoderDefImpl[A]
    def wrapperEncoderDef[A: c.WeakTypeTag]: c.Expr[AvroEncoder[A]] = wrapperEncoderDefImpl[A]
}
