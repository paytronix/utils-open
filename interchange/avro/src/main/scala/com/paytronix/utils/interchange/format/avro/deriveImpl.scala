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
import base.derive.utils.{Property, Structure, sequenceResultBindings}
import utils.sanitizeSchemaName

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

        val avroEncoder = Ident(TermName(c.freshName()))
        val instance = Ident(TermName(c.freshName()))
        val encode = sequenceResultBindings(c, props) (
            action = prop => q"${encoderFor(prop)}.run(${prop.read(instance)}, $avroEncoder)",
            accept = (_, _) => q"",
            body = _ => q"com.paytronix.utils.scala.result.Okay.unit"
        )

        val obj = Ident(TermName(c.freshName()))
        val encodeDefaultJson = sequenceResultBindings(c, props) (
            action = prop => q"${encoderFor(prop)}.encodeDefaultJson(${prop.read(instance)})",
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
        import c.universe.{Block, BlockTag, Ident, TermName, Quasiquote}

        val props = (model.constructorProperties ++ model.mutableProperties).sorted
        val resolvingDecoder = Ident(TermName(c.freshName()))
        val receiver = Ident(TermName(c.freshName()))
        val readFieldOrder = Ident(TermName(c.freshName()))
        val decodeField = TermName(c.freshName())
        val index = Ident(TermName(c.freshName()))
        val propReceiverNames = props.map { prop => Ident(TermName(c.freshName())) }
        val propReceiverByProp = Map.empty ++ (props zip propReceiverNames)
        val declarePropReceivers = (propReceiverNames zip props).map { case (receiverName, prop) =>
            q"val $receiverName = new com.paytronix.utils.interchange.base.Receiver[${prop.tpe}]()"
        }.map { case Block(stats, _) => stats.head } // work around quasiquotes wrapping up the val declarations like { X; () }
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
        val instance = Ident(TermName(c.freshName()))
        val avroEncoder = Ident(TermName(c.freshName()))
        List (
            q"val schema = ${property.encoderName}.schema",
            q"val defaultJson = ${property.encoderName}.defaultJson",
            q"""
                def encodeDefaultJson($instance: $targetType) =
                    ${property.encoderName}.encodeDefaultJson(${unwrap(instance)})
            """,
            q"""
                def run($instance: $targetType, $avroEncoder: org.apache.avro.io.Encoder) =
                    com.paytronix.utils.interchange.base.atProperty(${property.externalName}) {
                        ${property.encoderName}.run(${unwrap(instance)}, $avroEncoder)
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
        val resolvingDecoder = Ident(TermName(c.freshName()))
        val out = Ident(TermName(c.freshName()))
        val receiver = Ident(TermName(c.freshName()))
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

    def makeUnionCoder(c: Context)(target: c.universe.Tree, alts: Seq[c.universe.Tree]): c.universe.Tree = {
        import c.universe.Quasiquote
        q"com.paytronix.utils.interchange.format.avro.derive.union.coder[$target](..$alts)"
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

        val avroEncoder = Ident(TermName(c.freshName()))
        val instance = Ident(TermName(c.freshName()))

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

        val resolvingDecoder = Ident(TermName(c.freshName()))
        val receiver = Ident(TermName(c.freshName()))
        val invalidIndex = TermName(c.freshName())

        val decodeAlts = targetSubtypes.list.zipWithIndex.map { case (subtype, index) =>
            val decoder = decoderFor(subtype)
            val subReceiver = Ident(TermName(c.freshName()))
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

    // hack to avoid macro not liking synonyms
    def structureCoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroCoder[A]] = structureCoderDefImpl[A](c)
    def structureDecoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroDecoder[A]] = structureDecoderDefImpl[A](c)
    def structureEncoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroEncoder[A]] = structureEncoderDefImpl[A](c)
    def wrapperCoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroCoder[A]] = wrapperCoderDefImpl[A](c)
    def wrapperDecoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroDecoder[A]] = wrapperDecoderDefImpl[A](c)
    def wrapperEncoderDef[A: c.WeakTypeTag](c: Context): c.Expr[AvroEncoder[A]] = wrapperEncoderDefImpl[A](c)
    def unionCoderDef[A: c.WeakTypeTag](c: Context)(alternatives: c.Expr[base.union.Alternative[_ <: A]]*): c.Expr[AvroCoder[A]] = unionCoderDefImpl[A](c)(alternatives: _*)
    def unionDecoderDef[A: c.WeakTypeTag](c: Context)(alternatives: c.Expr[base.union.Alternative[_ <: A]]*): c.Expr[AvroDecoder[A]] = unionDecoderDefImpl[A](c)(alternatives: _*)
    def unionEncoderDef[A: c.WeakTypeTag](c: Context)(alternatives: c.Expr[base.union.Alternative[_ <: A]]*): c.Expr[AvroEncoder[A]] = unionEncoderDefImpl[A](c)(alternatives: _*)

}
