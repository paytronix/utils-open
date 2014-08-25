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
import base.derive.utils.{Property, Structure, addToCompanion, sequenceResultBindings}
import utils.sanitizeSchemaName
import NonEmptyList.nels

private[avro] object deriveImpl extends DeriveCoderMacros {
    type Coder[A] = AvroCoder[A]

    def implicitCoderName(c: Context) = c.universe.TermName("avroCoder")

    def coderType(c: Context)(tpe: c.universe.Tree) = {
        import c.universe.Quasiquote
        tq"com.paytronix.utils.interchange.format.avro.AvroCoder[$tpe]"
    }

    def materializeCoder(c: Context)(tpe: c.universe.Type, annotations: List[c.universe.Annotation]): c.universe.Tree = {
        import c.universe.Quasiquote
        q"""
            com.paytronix.utils.interchange.format.avro.AvroCoder.make[$tpe] (
                ${materializeEncoder(c)(tpe, annotations)},
                ${materializeDecoder(c)(tpe, annotations)}
            )
        """
    }

    type Encoder[A] = AvroEncoder[A]

    def encoderType(c: Context)(tpe: c.universe.Tree): c.universe.Tree = {
        import c.universe.Quasiquote
        tq"com.paytronix.utils.interchange.format.avro.AvroEncoder[$tpe]"
    }

    def materializeEncoder(c: Context)(tpe: c.universe.Type, annotations: List[c.universe.Annotation]): c.universe.Tree = {
        import c.universe.Quasiquote
        wrapCoderForAnnotations(c) (
            coder       = q"com.paytronix.utils.interchange.format.avro.AvroEncoder[$tpe]",
            annotations = annotations,
            nullable    = inside => q"com.paytronix.utils.interchange.format.avro.container.nullableAvroEncoder($inside)",
            default     = value => inside => q"$inside.default($value)"
        )
    }

    type Decoder[A] = AvroDecoder[A]

    def decoderType(c: Context)(tpe: c.universe.Tree): c.universe.Tree = {
        import c.universe.Quasiquote
        tq"com.paytronix.utils.interchange.format.avro.AvroDecoder[$tpe]"
    }

    def materializeDecoder(c: Context)(tpe: c.universe.Type, annotations: List[c.universe.Annotation]): c.universe.Tree = {
        import c.universe.Quasiquote
        wrapCoderForAnnotations(c) (
            coder       = q"com.paytronix.utils.interchange.format.avro.AvroDecoder[$tpe]",
            annotations = annotations,
            nullable    = inside => q"com.paytronix.utils.interchange.format.avro.container.nullableAvroDecoder($inside)",
            default     = value => inside => q"$inside.default($value)"
        )
    }

    def makeStructureCoder(c: Context)(target: c.universe.Tree): c.universe.Tree = {
        import c.universe.Quasiquote
        q"com.paytronix.utils.interchange.format.avro.derive.structure.coder[$target]"
    }

    private def makeStructureSchema(c: Context) (
        tpe: c.universe.Type,
        encoderOrDecoderFor: Property[c.universe.type] => c.universe.Tree,
        model: Structure[c.universe.type]
    ): c.universe.Tree = {
        import c.universe.Quasiquote

        val namespace = tpe.typeSymbol.owner.fullName
        val name =
            tpe.typeSymbol.name.decodedName.toString +
            (if (tpe.typeArgs.nonEmpty) "__" + tpe.typeArgs.map(arg => sanitizeSchemaName(arg.typeSymbol.fullName)).mkString("") else "")

        val props = model.constructorProperties ++ model.mutableProperties
        val fields = props.map { prop =>
            val eod = encoderOrDecoderFor(prop)
            q"com.paytronix.utils.interchange.format.avro.utils.makeField(${prop.externalName}, ${eod}.schema, ${eod}.defaultJson)"
        }

        q"""
            {
                val s = org.apache.avro.Schema.createRecord($name, "", $namespace, false)
                s.setFields(java.util.Arrays.asList(..$fields))
                s
            }
        """
    }

    def structureEncoderMethods(c: Context) (
        tpe: c.universe.Type,
        encoderFor: Property[c.universe.type] => c.universe.Tree,
        model: Structure[c.universe.type]
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}

        val props = (model.constructorProperties ++ model.mutableProperties).sorted // avro doesn't encode read-only fields

        val avroEncoder = TermName(c.freshName())
        val instance = TermName(c.freshName())
        val encode = sequenceResultBindings(c, props) (
            action = prop => q"${encoderFor(prop)}.run(${prop.read(Ident(instance))}, $avroEncoder)",
            accept = (_, _) => q"",
            body = _ => q"com.paytronix.utils.scala.result.Okay.unit"
        )

        val obj = TermName(c.freshName())
        val encodeDefaultJson = sequenceResultBindings(c, props) (
            action = prop => q"${encoderFor(prop)}.encodeDefaultJson(${prop.read(Ident(instance))})",
            accept = (prop, value) => q"$obj.put(${prop.externalName}, $value)",
            body = _ => q"com.paytronix.utils.scala.result.Okay($obj)"
        )

        List (
            q"val schema = ${makeStructureSchema(c)(tpe, encoderFor, model)}",
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

    def structureDecoderMethods(c: Context) (
        tpe: c.universe.Type,
        decoderFor: Property[c.universe.type] => c.universe.Tree,
        model: Structure[c.universe.type]
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}

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
        val decode = sequenceResultBindings(c, 0 until props.size) (
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
            q"val schema = ${makeStructureSchema(c)(tpe, decoderFor, model)}",
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

    def makeWrapperCoder(c: Context)(target: c.universe.Tree): c.universe.Tree = {
        import c.universe.Quasiquote
        q"com.paytronix.utils.interchange.format.avro.derive.wrapper.coder[$target]"
    }

    def wrapperEncoderMethods(c: Context) (
        targetType: c.universe.Type,
        property: Property[c.universe.type],
        unwrap: c.universe.Tree => c.universe.Tree
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}
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

    def wrapperDecoderMethods(c: Context) (
        targetType: c.universe.Type,
        property: Property[c.universe.type],
        wrap: c.universe.Tree => c.universe.Tree
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}
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

    private def makeUnionSchema(c: Context) (
        targetSubtypes: NonEmptyList[c.universe.Type],
        encoderOrDecoderFor: c.universe.Type => c.universe.Tree
    ): c.universe.Tree = {
        import c.universe.Quasiquote

        val schemas = targetSubtypes.map(encoderOrDecoderFor).map { coder =>
            q"$coder.schema"
        }.list

        q"org.apache.avro.Schema.createUnion(java.util.Arrays.asList(..$schemas))"
    }

    def unionEncoderMethods(c: Context) (
        targetType: c.universe.Type,
        targetSubtypes: NonEmptyList[c.universe.Type],
        encoderFor: c.universe.Type => c.universe.Tree
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}

        val avroEncoder = TermName(c.freshName())
        val instance = TermName(c.freshName())

        val encodeAlts = targetSubtypes.list.zipWithIndex.map { case (subtype, index) =>
            val encoder = encoderFor(subtype)
            val value = TermName(c.freshName())
            cq"""
                $value: $subtype =>
                    com.paytronix.utils.interchange.base.atIndex($index) {
                        com.paytronix.utils.scala.result.tryCatch.resultG(com.paytronix.utils.interchange.base.terminal) {
                            $avroEncoder.writeIndex($index)
                            $encoder.run($value, $avroEncoder)
                        }
                    }
            """
        }

        val value = TermName(c.freshName())

        List (
            q"val schema = ${makeUnionSchema(c)(targetSubtypes, encoderFor)}",
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

    def unionDecoderMethods(c: Context) (
        targetType: c.universe.Type,
        targetSubtypes: NonEmptyList[c.universe.Type],
        decoderFor: c.universe.Type => c.universe.Tree
    ): List[c.universe.Tree] = {
        import c.universe.{Ident, TermName, Quasiquote}

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
                        com.paytronix.utils.scala.result.tryCatch.resultG(com.paytronix.utils.interchange.base.terminal) {
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
            q"val schema = ${makeUnionSchema(c)(targetSubtypes, decoderFor)}",
            q"val defaultJson = scala.None",
            q"""
                def run($resolvingDecoder: org.apache.avro.io.ResolvingDecoder,
                        $receiver: com.paytronix.utils.interchange.base.Receiver[$targetType]) =
                    com.paytronix.utils.scala.result.tryCatch.resultG(com.paytronix.utils.interchange.base.terminal) {
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

            List(q"""
                implicit val ${implicitCoderName(c)}: ${coderType(c)(Ident(targetName))} =
                    com.paytronix.utils.interchange.format.avro.derive.union.coder[$targetName](..$alts)
             """)
        }

    def unionCoderDef[A: c.WeakTypeTag](c: Context)(alternatives: c.Expr[base.union.Alternative[A]]*): c.Expr[AvroCoder[A]] = try {
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


    def unionDecoderDef[A: c.WeakTypeTag](c: Context)(alternatives: c.Expr[base.union.Alternative[A]]*): c.Expr[AvroDecoder[A]] = try {
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

    def unionEncoderDef[A: c.WeakTypeTag](c: Context)(alternatives: c.Expr[base.union.Alternative[A]]*): c.Expr[AvroEncoder[A]] = try {
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

    // hack to avoid macro not liking synonyms
    def structureCoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroCoder[A]] = structureCoderDefImpl[A](c)
    def structureDecoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroDecoder[A]] = structureDecoderDefImpl[A](c)
    def structureEncoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroEncoder[A]] = structureEncoderDefImpl[A](c)
    def wrapperCoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroCoder[A]] = wrapperCoderDefImpl[A](c)
    def wrapperDecoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroDecoder[A]] = wrapperDecoderDefImpl[A](c)
    def wrapperEncoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroEncoder[A]] = wrapperEncoderDefImpl[A](c)
}
