//
// Copyright 2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.interchange

import net.liftweb.json.JsonAST.{JBool, JDouble, JField, JInt, JNothing, JNull, JObject, JString, JValue}
import scalaz.{Id, Lens, LensT}

import com.paytronix.utils.scala.result.{Failed, Okay, Result, parameter}

// FIXME, does this really belong in interchange?

object json {
    object lens {
        val id: Lens[JValue, JValue] = LensT.self

        def field(name: String): Lens[JValue, JValue] =
            LensT.lensg (
                _ match {
                    case JObject(jfields) =>
                        _ match {
                            case JNothing => JObject(jfields.filterNot(_.name == name))
                            case newValue =>
                                JObject(JField(name, newValue) :: jfields.filterNot(_.name == name))
                        }
                    case other =>
                        _ match {
                            case JNothing => other
                            case newValue => JObject(List(JField(name, newValue)))
                        }
                },
                _ \ name
            )

        def path(path: String*): Lens[JValue, JValue] =
            path.foldLeft(id)((l, f) => l >=> field(f))

        val boolean: LensT[Result, JValue, Boolean] =
            LensT.lensgT (
                _ => Okay(b => JBool(b)),
                BooleanCoder.decode(getClass.getClassLoader, _).orElse(parameter(())): Result[Boolean]
            )

        val bigDecimal: LensT[Result, JValue, BigDecimal] =
            LensT.lensgT (
                _ => Okay(bd => JString(bd.toString)),
                ScalaBigDecimalCoder.decode(getClass.getClassLoader, _).orElse(parameter(())): Result[BigDecimal]
            )

        val bigInt: LensT[Result, JValue, BigInt] =
            LensT.lensgT (
                _ => Okay(bi => JInt(bi)),
                BigIntCoder.decode(getClass.getClassLoader, _).orElse(parameter(())): Result[BigInt]
            )

        val byte: LensT[Result, JValue, Byte] =
            LensT.lensgT (
                _ => Okay(b => JInt(b)),
                ByteCoder.decode(getClass.getClassLoader, _).orElse(parameter(())): Result[Byte]
            )

        val double: LensT[Result, JValue, Double] =
            LensT.lensgT (
                _ => Okay(d => JDouble(d)),
                DoubleCoder.decode(getClass.getClassLoader, _).orElse(parameter(())): Result[Double]
            )

        val float: LensT[Result, JValue, Float] =
            LensT.lensgT (
                _ => Okay(f => JDouble(f)),
                FloatCoder.decode(getClass.getClassLoader, _).orElse(parameter(())): Result[Float]
            )

        val int: LensT[Result, JValue, Int] =
            LensT.lensgT (
                _ => Okay(i => JInt(BigInt(i))),
                IntCoder.decode(getClass.getClassLoader, _).orElse(parameter(())): Result[Int]
            )

        val long: LensT[Result, JValue, Long] =
            LensT.lensgT (
                _ => Okay(l => JInt(BigInt(l))),
                LongCoder.decode(getClass.getClassLoader, _).orElse(parameter(())): Result[Long]
            )

        val short: LensT[Result, JValue, Short] =
            LensT.lensgT (
                _ => Okay(s => JInt(BigInt(s))),
                ShortCoder.decode(getClass.getClassLoader, _).orElse(parameter(())): Result[Short]
            )

        val string: LensT[Result, JValue, String] =
            LensT.lensgT (
                _ => Okay(s => JString(s)),
                StringCoder.decode(getClass.getClassLoader, _).orElse(parameter(())): Result[String]
            )

    }
}
