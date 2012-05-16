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

package com.paytronix.utils.interchange.test

import java.math.BigInteger
import java.text.DecimalFormat
import java.util.Arrays
import scala.collection.immutable.Set
import scala.collection.mutable.{ArrayBuffer, Buffer, WrappedArray}
import net.liftweb.json.Implicits.{double2jvalue, int2jvalue, string2jvalue}
import net.liftweb.json.JsonAST.{JArray, JBool, JDouble, JField, JInt, JNothing, JNull, JObject, JString, JValue}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import org.slf4j.{Logger, LoggerFactory}
import org.specs2.{SpecificationFeatures, SpecificationWithJUnit}
import org.specs2.execute.{Result => SpecsResult}
import org.specs2.matcher.Matcher
import org.specs2.specification.{Around, Example}
import com.paytronix.utils.interchange._
import com.paytronix.utils.scala.result.{Failed, FailedG, Okay, Result, ResultG}

import ComposableCoder.CoderResult
import Helpers._

object MoreHelpers extends SpecificationFeatures {
    val log = LoggerFactory.getLogger(getClass)

    val percentFormat = new DecimalFormat("#,##0.00")

    val cl = getClass.getClassLoader

    val testObject = ("foo" -> 1) ~ ("bar" -> "baz") ~ ("zip" -> "qux")

    val testCollectionJValue = JArray(JString("one") :: JString("two") :: JString("three") :: Nil)

    def simpleTest[T] (
        name: String,
        inst: Coder[T],
        normalValues: List[(T, JValue)],
        stringDecodeValues: List[(T, String)],
        outOfBoundJValues: List[JValue]
    ) = (name + " should") ^
        normalValues.map { case (from, to) =>
            ("basically encode " + name + " (" + to + ")") ! {
                inst.encode(from) must matchEncodedJson(to)
            }
        } ^
        normalValues.map { case (to, from) =>
            ("basically decode " + name + " (" + from + ")") ! {
                inst.decode(from) must_== Okay(to)
            }
        } ^
        normalValues.map { case (v, _) =>
            ("basically round trip " + name + " via Avro (" + v + ")") ! {
                avroRoundTrip(inst, v)
            }
        } ^ (
            if (!stringDecodeValues.isEmpty) {
                stringDecodeValues.map { case (to, from) =>
                    ("decode JStrings " + name + " (" + from + ")") ! {
                        inst.decode(JString(from)) must_== Okay(to)
                    }
                } ++
                stringDecodeValues.map { case (to, from) =>
                    ("decode Strings " + name + " (" + from + ")") ! {
                        inst.implementation must beLike {
                            case stringInst: StringSafeCoder[_] =>
                                stringInst.decodeString(cl, from) must_== Okay(to)
                        }
                    }
                } ++
                stringDecodeValues.map { case (from, to) =>
                    ("encode Strings " + name + " (" + from + ")") ! {
                        inst.implementation must beLike {
                            case stringInst: StringSafeCoder[_] =>
                                stringInst.encodeString(cl, from) must_== Okay(to)
                        }
                    }
                }
            } else Nil
        ) ^ (
            if (!outOfBoundJValues.isEmpty) {
                outOfBoundJValues.map { outOfBoundJValue =>
                    ("fail to decode invalid JValues " + name + " (" + json(outOfBoundJValue) + ")") ! {
                        def beFailure(checkedValue: JValue): Matcher[Result[_]] =
                            (v: Result[_]) =>
                                if (!v.isDefined) ok("failed decoding " + checkedValue + ", as it should")
                                else ko("should have failed decoding " + checkedValue + ", instead gave " + v)

                        inst.decode(outOfBoundJValue) must beFailure(outOfBoundJValue)
                    }
                }
            } else Nil
        )

    val byteMin  = BigInt(java.lang.Byte.MIN_VALUE.toString)
    val byteMax  = BigInt(java.lang.Byte.MAX_VALUE.toString)
    val intMin   = BigInt(java.lang.Integer.MIN_VALUE.toString)
    val intMax   = BigInt(java.lang.Integer.MAX_VALUE.toString)
    val longMin  = BigInt(java.lang.Long.MIN_VALUE.toString)
    val longMax  = BigInt(java.lang.Long.MAX_VALUE.toString)
    val shortMin = BigInt(java.lang.Short.MIN_VALUE.toString)
    val shortMax = BigInt(java.lang.Short.MAX_VALUE.toString)

    val simpleMapJValue = ("one" -> 1) ~ ("two" -> 2)
    val simpleMapJValue2 = JArray((("key" -> "one") ~ ("value" -> 1)) :: (("key" -> "two") ~ ("value" -> 2)) :: Nil)
    val complexMapJValue = JArray((("key" -> (("foo" -> "bar") ~ ("baz" -> 1))) ~ ("value" -> "foobarbaz1")) ::
                                  (("key" -> (("qux" -> "foo") ~ ("zip" -> 1))) ~ ("value" -> "quxfoozip1")) :: Nil)
}

import MoreHelpers._

class JavaBigDecimalCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="JavaBigDecimalCoder",
        inst=Coder(cl, JavaBigDecimalCoder),
        normalValues=(new java.math.BigDecimal("1.123"), JString("1.123")) :: (new java.math.BigDecimal("-1.123"), JString("-1.123")) :: Nil,
        stringDecodeValues=(new java.math.BigDecimal("1.123"), "1.123") :: (new java.math.BigDecimal("-1.123"), "-1.123") :: Nil,
        outOfBoundJValues=JNothing :: JNull :: JString("foo") :: JBool(false) :: Nil
    )
}

class ScalaBigDecimalCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="ScalaBigDecimalCoder",
        inst=Coder(cl, ScalaBigDecimalCoder),
        normalValues=(scala.math.BigDecimal("1.123"), JString("1.123")) :: (scala.BigDecimal("-1.123"), JString("-1.123")) :: Nil,
        stringDecodeValues=(scala.math.BigDecimal("1.123"), "1.123") :: (scala.math.BigDecimal("-1.123"), "-1.123") :: Nil,
        outOfBoundJValues=JNothing :: JNull :: JString("foo") :: JBool(false) :: Nil
    )
}

class BigIntCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="BigIntCoder",
        inst=Coder(cl, BigIntCoder),
        normalValues=(BigInt("1"), JInt(BigInt("1"))) :: (BigInt("-1"), JInt(BigInt("-1"))) :: Nil,
        stringDecodeValues=(BigInt("1"), "1") :: (BigInt("-1"), "-1") :: Nil,
        outOfBoundJValues=JNothing :: JNull :: JString("foo") :: JBool(false) :: Nil
    )
}

class BigIntegerCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="BigIntegerCoder",
        inst=Coder(cl, BigIntegerCoder),
        normalValues=(new BigInteger("1"), JInt(BigInt("1"))) :: (new BigInteger("-1"), JInt(BigInt("-1"))) :: Nil,
        stringDecodeValues=(new BigInteger("1"), "1") :: (new BigInteger("-1"), "-1") :: Nil,
        outOfBoundJValues=JNothing :: JNull :: JString("foo") :: JBool(false) :: Nil
    )
}

class BooleanCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="BooleanCoder",
        inst=Coder(cl, BooleanCoder),
        normalValues=(true, JBool(true)) :: (false, JBool(false)) :: Nil,
        stringDecodeValues=(true, "true") :: (false, "false") :: Nil,
        outOfBoundJValues=JNothing :: JNull :: JString("foo") :: JInt(BigInt(1)) :: Nil
    )
}

class ByteCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="ByteCoder",
        inst=Coder(cl, ByteCoder),
        normalValues=List[(Byte, JValue)](
            (1: Byte, JInt(1)),
            (-1: Byte, JInt(-1)),
            (java.lang.Byte.MIN_VALUE, JInt(byteMin)),
            (java.lang.Byte.MAX_VALUE, JInt(byteMax))
        ),
        stringDecodeValues=List[(Byte, String)]((1, "1"), (-1, "-1")),
        outOfBoundJValues=JNothing :: JNull :: JInt(byteMin - 1) :: JInt(byteMax + 1) :: JString("foo") :: JBool(false) :: Nil
    )
}

class CharCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="CharCoder",
        inst=Coder(cl, CharCoder),
        normalValues=('a', JString("a")) :: ('\u0000', JString("\u0000")) :: Nil,
        stringDecodeValues=('a', "a") :: ('\u0000', "\u0000") :: Nil,
        outOfBoundJValues=JNothing :: JNull :: JString("foo") :: JBool(false) :: Nil
    )
}

class DoubleCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="DoubleCoder",
        inst=Coder(cl, DoubleCoder),
        normalValues=(1.0, JDouble(1.0)) :: (-1.0, JDouble(-1.0)) :: (0.1, JDouble(0.1)) :: Nil,
        stringDecodeValues=(1.0, "1.0") :: (-1.0, "-1.0") :: (0.1, "0.1") :: Nil,
        outOfBoundJValues=JNothing :: JNull :: JString("foo") :: JBool(false) :: Nil
    )
}

class IntCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="IntCoder",
        inst=Coder(cl, IntCoder),
        normalValues=List[(Int, JValue)](
            (1, JInt(BigInt(1))),
            (-1, JInt(BigInt(-1))),
            (java.lang.Integer.MIN_VALUE, JInt(intMin)),
            (java.lang.Integer.MAX_VALUE, JInt(intMax))
        ),
        stringDecodeValues=List[(Int, String)]((1, "1"), (-1, "-1")),
        outOfBoundJValues=JNothing :: JNull :: JInt(intMin - 1) :: JInt(intMax +1) :: JString("foo") :: JBool(false) :: Nil
    )
}

class LongCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="LongCoder",
        inst=Coder(cl, LongCoder),
        normalValues=List[(Long, JValue)](
            (1L, JInt(BigInt(1))),
            (-1L, JInt(BigInt(-1))),
            (java.lang.Long.MIN_VALUE, JInt(longMin)),
            (java.lang.Long.MAX_VALUE, JInt(longMax))
        ),
        stringDecodeValues=List[(Long, String)]((1L, "1"), (-1L, "-1")),
        outOfBoundJValues=JNothing :: JNull :: JInt(longMin - 1) :: JInt(longMax + 1) :: JString("foo") :: JBool(false) :: Nil
    )
}

class ShortCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="ShortCoder",
        inst=Coder(cl, ShortCoder),
        normalValues=List[(Short, JValue)](
            (1: Short, JInt(BigInt(1))),
            (-1: Short, JInt(BigInt(-1))),
            (java.lang.Short.MIN_VALUE, JInt(shortMin)),
            (java.lang.Short.MAX_VALUE, JInt(shortMax))
        ),
        stringDecodeValues=List[(Short, String)]((1, "1"), (-1, "-1")),
        outOfBoundJValues=JNothing :: JNull :: JInt(shortMin - 1) :: JInt(shortMax + 1) :: JString("foo") :: JBool(false) :: Nil
    )
}

class StringCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="StringCoder",
        inst=Coder(cl, StringCoder),
        normalValues=("foo", JString("foo")) :: ("", JString("")) :: Nil,
        stringDecodeValues=Nil,
        outOfBoundJValues=JNothing :: JNull :: JBool(false) :: Nil
    )
}

class JavaDateCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="JavaDateCoder",
        inst=Coder(cl, JavaDateCoder),
        normalValues=List[(java.util.Date, JString)](
            (new java.text.SimpleDateFormat(JavaDateCoder.defaultFormatString).parse("2010-09-01 11:12:13 -0400"), JString("2010-09-01 11:12:13 -0400"))
        ),
        stringDecodeValues=List[(java.util.Date, String)](
            (new java.text.SimpleDateFormat(JavaDateCoder.defaultFormatString).parse("2010-09-01 11:12:13 -0400"), "2010-09-01 11:12:13 -0400")
        ),
        outOfBoundJValues=JNothing :: JNull :: JString("not a valid date!") :: JBool(false) :: Nil
    )
}

class JavaSqlDateCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="JavaSqlDateCoder",
        inst=Coder(cl, JavaSqlDateCoder),
        normalValues=List[(java.sql.Date, JString)](
            (new java.sql.Date(new java.text.SimpleDateFormat(JavaSqlDateCoder.defaultFormatString).parse("2010-09-01").getTime),
             JString("2010-09-01"))
        ),
        stringDecodeValues=List[(java.sql.Date, String)](
            (new java.sql.Date(new java.text.SimpleDateFormat(JavaSqlDateCoder.defaultFormatString).parse("2010-09-01").getTime),
             "2010-09-01")
        ),
        outOfBoundJValues=JNothing :: JNull :: JString("not a valid date!") :: JBool(false) :: Nil
    )
}

class JavaEnumCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="JavaEnumCoder",
        inst=Coder(cl, fixtures.Coders.javaEnumCoder),
        normalValues=(fixtures.JavaEnum.ONE, JString("ONE")) :: (fixtures.JavaEnum.TWO, JString("TWO")) :: Nil,
        stringDecodeValues=(fixtures.JavaEnum.ONE, "ONE") :: (fixtures.JavaEnum.TWO, "TWO") :: Nil,
        outOfBoundJValues=JNothing :: JNull :: JBool(false) :: JString("FOUR") :: JString("") :: Nil
    )
}

class ScalaEnumCoderSpecTest extends SpecificationWithJUnit {
    def is = simpleTest (
        name="ScalaEnumCoder",
        inst=Coder(cl, fixtures.Coders.scalaEnumCoder),
        normalValues=(fixtures.ScalaEnum.ONE, JString("ONE")) :: (fixtures.ScalaEnum.TWO, JString("TWO")) :: Nil,
        stringDecodeValues=(fixtures.ScalaEnum.ONE, "ONE") :: (fixtures.ScalaEnum.TWO, "TWO") :: Nil,
        outOfBoundJValues=JNothing :: JNull :: JBool(false) :: JString("FOUR") :: JString("") :: Nil
    )
}

class InsecureCoderSpecTest extends SpecificationWithJUnit {
    val testCoder = InsecureCoder(StringCoder, Failed("boom!"))
    def is =
        "InsecureCoder should" ^
        "not work in an insecure context" ! resetCoderSettings {
            CoderSettings.isInsecureContext.set(true)
            testCoder.decode(cl, JString("foo")) must beLike {
                case FailedG(_, _) => ok
                case Okay(_) => ko("should have failed to decode insecure value in insecure context")
            }
        } ^
        "work in a secure context" ! resetCoderSettings {
            testCoder.decode(cl, JString("foo")) must beLike {
                case Okay(_) => ok
                case FailedG(throwable, _) =>
                    throwable.printStackTrace()
                    ko("failed to decode in secure context (see log)")
            }
        }
}

class FailCoderSpecTest extends SpecificationWithJUnit {
    def is =
        "FailCoder should" ^
        "always fail properly" ! {
            Coder(cl, FailCoder[Unit](Failed("test"))).decode(JObject(Nil)) must beLike {
                case FailedG(throwable, _) =>
                    throwable.getCause.getMessage must_== "test"
                case Okay(_) =>
                    ko("FailCoder should have failed")
            }
        }
}

class CollectionCoderSpecTest extends SpecificationWithJUnit {
    val testCoder: CollectionCoder[String, Set[String]] = CollectionCoder(StringCoder)
    val testSet = Set("one", "two", "three")

    def is =
        "CollectionCoder should" ^
        "basically encode" ! {
            testCoder.encode(cl, testSet) must matchEncodedJson(testCollectionJValue)
        } ^
        "basically decode" ! {
            testCoder.decode(cl, testCollectionJValue) must_== Okay(testSet)
        } ^
        "round trip via Avro" ! {
            avroRoundTrip(testCoder, testSet)
        }
}

class JavaListCoderSpecTest extends SpecificationWithJUnit {
    val testCoder = JavaListCoder(StringCoder)
    val testList = Arrays.asList("one", "two", "three")

    def is =
        "JavaListCoder should" ^
        "basically encode" ! {
            testCoder.encode(cl, testList) must matchEncodedJson(testCollectionJValue)
        } ^
        "basically decode" ! {
            testCoder.decode(cl, testCollectionJValue) must_== Okay(testList)
        } ^
        "round trip via Avro" ! {
            avroRoundTrip(testCoder, testList)
        }
}

class JavaMapCoderSpecTest extends SpecificationWithJUnit {
    val simpleMapCoder = JavaMapCoder(StringCoder, IntCoder)
    val complexMapCoder = JavaMapCoder(JValueCoder, StringCoder)

    val simpleMap = new java.util.HashMap[String, Int]
    simpleMap.put("one", 1)
    simpleMap.put("two", 2)
    val complexMap = new java.util.LinkedHashMap[JValue, String]
    complexMap.put(("foo" -> "bar") ~ ("baz" -> 1), "foobarbaz1")
    complexMap.put(("qux" -> "foo") ~ ("zip" -> 1), "quxfoozip1")

    def is =
        "JavaMapCoder should" ^
        "encode simple keys" ! {
            simpleMapCoder.encode(cl, simpleMap) must matchEncodedJson(simpleMapJValue)
        } ^
        "decode simple keys" ! {
            simpleMapCoder.decode(cl, simpleMapJValue) must_== Okay(simpleMap)
        } ^
        "encode complex keys" ! {
            complexMapCoder.encode(cl, complexMap) must matchEncodedJson(complexMapJValue)
        } ^
        "decode complex keys" ! {
            complexMapCoder.decode(cl, complexMapJValue) must_== Okay(complexMap)
        } ^
        "decode pairs even for a simple keys" ! {
            simpleMapCoder.decode(cl, simpleMapJValue2) must_== Okay(simpleMap)
        } ^
        "round trip via Avro for simple keys" ! {
            avroRoundTrip(simpleMapCoder, simpleMap)
        } ^
        "round trip via Avro for complex keys" ! {
            avroRoundTrip(complexMapCoder, complexMap)
        }
}

class ScalaImmutableMapCoderSpecTest extends SpecificationWithJUnit {
    val simpleMapCoder = ScalaImmutableMapCoder(StringCoder, IntCoder)
    val complexMapCoder = ScalaImmutableMapCoder(JValueCoder, StringCoder)

    val simpleMap = scala.collection.immutable.Map[String, Int]("one" -> 1, "two" -> 2)
    val complexMap = scala.collection.immutable.Map[JValue, String]((("foo" -> "bar") ~ ("baz" -> 1)) -> "foobarbaz1",
                                                                    (("qux" -> "foo") ~ ("zip" -> 1)) -> "quxfoozip1")

    def is =
        "ScalaImmutableMapCoder should" ^
        "encode simple keys" ! {
            simpleMapCoder.encode(cl, simpleMap) must matchEncodedJson(simpleMapJValue)
        } ^
        "decode simple keys" ! {
            simpleMapCoder.decode(cl, simpleMapJValue) must_== Okay(simpleMap)
        } ^
        "encode complex keys" ! {
            complexMapCoder.encode(cl, complexMap) must matchEncodedJson(complexMapJValue)
        } ^
        "decode complex keys" ! {
            complexMapCoder.decode(cl, complexMapJValue) must_== Okay(complexMap)
        } ^
        "decode pairs even for a simple keys" ! {
            simpleMapCoder.decode(cl, simpleMapJValue2) must_== Okay(simpleMap)
        } ^
        "round trip via Avro for simple keys" ! {
            avroRoundTrip(simpleMapCoder, simpleMap)
        } ^
        "round trip via Avro for complex keys" ! {
            avroRoundTrip(complexMapCoder, complexMap)
        }
}

class ScalaMutableMapCoderSpecTest extends SpecificationWithJUnit {
    val simpleMapCoder = ScalaMutableMapCoder(StringCoder, IntCoder)
    val complexMapCoder = ScalaMutableMapCoder(JValueCoder, StringCoder)

    val simpleMap = scala.collection.mutable.LinkedHashMap[String, Int]("one" -> 1, "two" -> 2)
    val complexMap = scala.collection.mutable.LinkedHashMap[JValue, String]((("foo" -> "bar") ~ ("baz" -> 1)) -> "foobarbaz1",
                                                                            (("qux" -> "foo") ~ ("zip" -> 1)) -> "quxfoozip1")

    def is =
        "ScalaMutableMapCoder should" ^
        "encode simple keys" ! {
            simpleMapCoder.encode(cl, simpleMap) must matchEncodedJson(simpleMapJValue)
        } ^
        "decode simple keys" ! {
            simpleMapCoder.decode(cl, simpleMapJValue) must_== Okay(simpleMap)
        } ^
        "encode complex keys" ! {
            complexMapCoder.encode(cl, complexMap) must matchEncodedJson(complexMapJValue)
        } ^
        "decode complex keys" ! {
            complexMapCoder.decode(cl, complexMapJValue) must_== Okay(complexMap)
        } ^
        "decode pairs even for a simple keys" ! {
            simpleMapCoder.decode(cl, simpleMapJValue2) must_== Okay(simpleMap)
        } ^
        "round trip via Avro for simple keys" ! {
            avroRoundTrip(simpleMapCoder, simpleMap)
        } ^
        "round trip via Avro for complex keys" ! {
            avroRoundTrip(complexMapCoder, complexMap)
        }
}

class NullCoderSpecTest extends SpecificationWithJUnit {
    val testCoder = NullCoder(StringCoder)

    def is =
        "NullCoder should" ^
        "basically decode" ! {
            { testCoder.decode(cl, JString("foo")) must_== Okay("foo") } and
            { testCoder.decode(cl, JNull) must_== Okay(null) } and
            { testCoder.decode(cl, JNothing) must_== Okay(null) }
        } ^
        "basically encode" ! {
            { testCoder.encode(cl, "foo") must matchEncodedJson(JString("foo")) } and
            { testCoder.encode(cl, null) must matchEncodedJson(JNothing) }
        } ^
        "round trip via Avro" ! {
            { avroRoundTrip(testCoder, "foo") } and
            { avroRoundTrip(testCoder, null) }
        }
}

class OptionCoderSpecTest extends SpecificationWithJUnit {
    val testCoder = OptionCoder(StringCoder)
    val nestedCoder = OptionCoder(OptionCoder(OptionCoder(StringCoder)))
    val nestedListCoder = OptionCoder(OptionCoder(OptionCoder(ScalaListCoder(StringCoder))))

    def is =
        "OptionCoder should" ^
        "basically decode" ! {
            { testCoder.decode(cl, JString("foo")) must_== Okay(Some("foo")) } and
            { testCoder.decode(cl, JNull) must_== Okay(None) } and
            { testCoder.decode(cl, JNothing) must_== Okay(None) }
        } ^
        "basically encode" ! {
            { testCoder.encode(cl, Some("foo")) must matchEncodedJson(JString("foo")) } and
            { testCoder.encode(cl, None) must matchEncodedJson(JNothing) }
        } ^
        "basically round trip via Avro" ! {
            { avroRoundTrip(testCoder, Some("foo")) } and
            { avroRoundTrip(testCoder, None) }
        } ^
        "encode nested options" ! {
            { nestedCoder.encode(cl, Some(Some(Some("foo")))) must matchEncodedJson(JArray(JArray(JString("foo") :: Nil) :: Nil)) } and
            { nestedCoder.encode(cl, Some(Some(None))) must matchEncodedJson(JArray(JArray(JNothing :: Nil) :: Nil)) } and
            { nestedCoder.encode(cl, Some(None)) must matchEncodedJson(JArray(JNothing :: Nil)) } and
            { nestedCoder.encode(cl, None) must matchEncodedJson(JNothing) }
        } ^
        "encode nested options with list terminals" ! {
            { nestedListCoder.encode(cl, Some(Some(Some("foo" :: Nil)))) must matchEncodedJson(JArray(JArray(JArray(JString("foo") :: Nil) :: Nil) :: Nil)) } and
            { nestedListCoder.encode(cl, Some(Some(None))) must matchEncodedJson(JArray(JArray(JNothing :: Nil) :: Nil)) } and
            { nestedListCoder.encode(cl, Some(None)) must matchEncodedJson(JArray(JNothing :: Nil)) } and
            { nestedListCoder.encode(cl, None) must matchEncodedJson(JNothing) }
        } ^
        "decode nested options" ! {
            { nestedCoder.decode(cl, JArray(JArray(JString("foo") :: Nil) :: Nil)) must_== Okay(Some(Some(Some("foo")))) } and
            { nestedCoder.decode(cl, JArray(JArray(JNothing :: Nil) :: Nil)) must_== Okay(Some(Some(None))) } and
            { nestedCoder.decode(cl, JArray(JArray(JNull :: Nil) :: Nil)) must_== Okay(Some(Some(None))) } and
            { nestedCoder.decode(cl, JArray(JArray(Nil) :: Nil)) must_== Okay(Some(Some(None))) } and
            { nestedCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Some(None)) } and
            { nestedCoder.decode(cl, JArray(JNull :: Nil)) must_== Okay(Some(None)) } and
            { nestedCoder.decode(cl, JArray(Nil)) must_== Okay(Some(None)) } and
            { nestedCoder.decode(cl, JNothing) must_== Okay(None) } and
            { nestedCoder.decode(cl, JNull) must_== Okay(None) }
        } ^
        "decode nested options with list terminals" ! {
            { nestedListCoder.decode(cl, JArray(JArray(JArray(JString("foo") :: Nil) :: Nil) :: Nil)) must_== Okay(Some(Some(Some("foo" :: Nil)))) } and
            { nestedListCoder.decode(cl, JArray(JArray(JArray(Nil) :: Nil) :: Nil)) must_== Okay(Some(Some(Some(Nil)))) } and
            { nestedListCoder.decode(cl, JArray(JArray(JNothing :: Nil) :: Nil)) must_== Okay(Some(Some(None))) } and
            { nestedListCoder.decode(cl, JArray(JArray(JNull :: Nil) :: Nil)) must_== Okay(Some(Some(None))) } and
            { nestedListCoder.decode(cl, JArray(JArray(Nil) :: Nil)) must_== Okay(Some(Some(None))) } and
            { nestedListCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Some(None)) } and
            { nestedListCoder.decode(cl, JArray(JNull :: Nil)) must_== Okay(Some(None)) } and
            { nestedListCoder.decode(cl, JArray(Nil)) must_== Okay(Some(None)) } and
            { nestedListCoder.decode(cl, JNothing) must_== Okay(None) } and
            { nestedListCoder.decode(cl, JNull) must_== Okay(None) }
        } ^
        "round trip nested options via Avro" ! {
            { avroRoundTrip(nestedCoder, Some(Some(Some("foo")))) } and
            { avroRoundTrip(nestedCoder, Some(Some(None))) } and
            { avroRoundTrip(nestedCoder, Some(None)) } and
            { avroRoundTrip(nestedCoder, None) }
        }
}

class ResultCoderSpecTest extends SpecificationWithJUnit {
    val testCoder = ResultCoder(UnitCoder, StringCoder)
    val testUnitCoder = ResultCoder(UnitCoder, UnitCoder)
    val nestedCoder = ResultCoder(UnitCoder, ResultCoder(UnitCoder, ResultCoder(UnitCoder, StringCoder)))
    val nestedListCoder = ResultCoder(UnitCoder, ResultCoder(UnitCoder, ResultCoder(UnitCoder, ScalaListCoder(StringCoder))))
    def failedJSON (
        message: String,
        cause: JValue = JNothing,
        param: JValue = JNothing,
        throwableIsA: String = "com.paytronix.utils.scala.result$FailedException"
    ): JObject = {
        var throwable: JObject = ("isA" -> throwableIsA) ~ ("message" -> message)
        if (cause != JNothing) throwable = throwable ~ ("cause" -> (cause \ "throwable"))
        ("throwable" -> throwable) ~ ("param" -> param)
    }

    val failedGWithString = FailedG("failed", Failed("chained failure"), "additional param")
    val failedGWithStringJSON = failedJSON("failed", failedJSON("chained failure"), JString("additional param"))
    val failedGWithStringCoder = ResultCoder(StringCoder, IntCoder)

    val failedGWithInt = FailedG("failed", Failed("chained failure"), 1234)
    val failedGWithIntJSON = failedJSON("failed", failedJSON("chained failure"), 1234: JInt)
    val failedGWithIntCoder = ResultCoder(IntCoder, IntCoder)

    val failedGWithCaseClass = FailedG("failed", Failed("chained failure"), fixtures.CaseClass(1, "foo", Some("bar")))
    val failedGWithCaseClassJSON = failedJSON("failed", failedJSON("chained failure"), (
            ("bar" -> "foo") ~ ("foo" -> 1) ~ ("zip" -> "bar")
    ))
    val failedGWithCaseClassCoder = ResultCoder(fixtures.Coders.caseClassCoder, IntCoder)

    val failedGWithTuple = FailedG("failed", Failed("chained failure"), (1, "foo"))
    val failedGWithTupleJSON = failedJSON("failed", failedJSON("chained failure"), JArray(JInt(1) :: JString("foo") :: Nil))
    val failedGWithTupleCoder = ResultCoder(Tuple2Coder(IntCoder, StringCoder), IntCoder)

    def is = (
        "ResultCoder should" ^
        "decode Okay" ! {
            testCoder.decode(cl, JString("foo")) must_== Okay(Okay("foo"))
        } ^
        "encode Okay" ! {
            testCoder.encode(cl, Okay("foo")) must matchEncodedJson(JString("foo"))
        } ^
        "round trip Okay via Avro" ! {
            avroRoundTrip(testCoder, Okay("foo"))
        } ^
        "encode Failed" ! {
            { testCoder.encode(cl, Failed("failed")) must matchEncodedJson(failedJSON("failed")) } and
            { testCoder.encode(cl, Failed("failed", Failed("chained failure"))) must
                matchEncodedJson(failedJSON("failed", failedJSON("chained failure"))) }
        } ^
        "decode Failed" ! {
            { testCoder.decode(cl, failedJSON("failed")) must_== Okay(Failed("failed")) } and
            { testCoder.decode(cl, failedJSON("failed", failedJSON("chained failure"))) must_==
                Okay(Failed("failed", Failed("chained failure"))) }
        } ^
        "round trip Failed via Avro" ! {
            { avroRoundTrip(testCoder, Failed("failed")) } and
            { avroRoundTrip(testCoder, Failed("failed", Failed("chained failure"))) }
        } ^
        "encode nested results" ! {
            { nestedCoder.encode(cl, Okay(Okay(Okay("foo")))) must matchEncodedJson(JArray(JArray(JString("foo") :: Nil) :: Nil)) } and
            { nestedCoder.encode(cl, Okay(Okay(Failed("failed")))) must matchEncodedJson(JArray(JArray(failedJSON("failed") :: Nil) :: Nil)) } and
            { nestedCoder.encode(cl, Okay(Failed("failed"))) must matchEncodedJson(JArray(failedJSON("failed") :: Nil)) } and
            { nestedCoder.encode(cl, Failed("failed")) must matchEncodedJson(failedJSON("failed")) }
        } ^
        "encode nested results with list terminals" ! {
            { nestedListCoder.encode(cl, Okay(Okay(Okay("foo" :: Nil)))).must(
                matchEncodedJson(JArray(JArray(JArray(JString("foo") :: Nil) :: Nil) :: Nil))
            ) } and
            { nestedListCoder.encode(cl, Okay(Okay(Failed("failed")))) must matchEncodedJson(JArray(JArray(failedJSON("failed") :: Nil) :: Nil)) } and
            { nestedListCoder.encode(cl, Okay(Failed("failed"))) must matchEncodedJson(JArray(failedJSON("failed") :: Nil)) } and
            { nestedListCoder.encode(cl, Failed("failed")) must matchEncodedJson(failedJSON("failed")) }
        } ^
        "decode nested results" ! {
            { nestedCoder.decode(cl, JArray(JArray(JString("foo") :: Nil) :: Nil)) must_== Okay(Okay(Okay(Okay("foo")))) } and
            { nestedCoder.decode(cl, JArray(JArray(JNothing :: Nil) :: Nil)) must_== Okay(Okay(Okay(Failed("unknown failure")))) } and
            { nestedCoder.decode(cl, JArray(JArray(JNull :: Nil) :: Nil)) must_== Okay(Okay(Okay(Failed("unknown failure")))) } and
            { nestedCoder.decode(cl, JArray(JArray(Nil) :: Nil)) must_== Okay(Okay(Okay(Failed("unknown failure")))) } and
            { nestedCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Okay(Failed("unknown failure"))) } and
            { nestedCoder.decode(cl, JArray(JNull :: Nil)) must_== Okay(Okay(Failed("unknown failure"))) } and
            { nestedCoder.decode(cl, JArray(Nil)) must_== Okay(Okay(Failed("unknown failure"))) } and
            { nestedCoder.decode(cl, JNothing) must_== Okay(Failed("unknown failure")) } and
            { nestedCoder.decode(cl, JNull) must_== Okay(Failed("unknown failure")) } and
            { nestedCoder.decode(cl, JArray(JArray(failedJSON("failed") :: Nil) :: Nil)) must_== Okay(Okay(Okay(Failed("failed")))) } and
            { nestedCoder.decode(cl, JArray(failedJSON("failed") :: Nil)) must_== Okay(Okay(Failed("failed"))) } and
            { nestedCoder.decode(cl, failedJSON("failed")) must_== Okay(Failed("failed")) }
        } ^
        "decode nested results with list terminals" ! {
            { nestedListCoder.decode(cl, JArray(JArray(JArray(JString("foo") :: Nil) :: Nil) :: Nil)) must_== Okay(Okay(Okay(Okay("foo" :: Nil)))) } and
            { nestedListCoder.decode(cl, JArray(JArray(JArray(Nil) :: Nil) :: Nil)) must_== Okay(Okay(Okay(Okay(Nil)))) } and
            { nestedListCoder.decode(cl, JArray(JArray(JNothing :: Nil) :: Nil)) must_== Okay(Okay(Okay(Failed("unknown failure")))) } and
            { nestedListCoder.decode(cl, JArray(JArray(JNull :: Nil) :: Nil)) must_== Okay(Okay(Okay(Failed("unknown failure")))) } and
            { nestedListCoder.decode(cl, JArray(JArray(Nil) :: Nil)) must_== Okay(Okay(Okay(Failed("unknown failure")))) } and
            { nestedListCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Okay(Failed("unknown failure"))) } and
            { nestedListCoder.decode(cl, JArray(JNull :: Nil)) must_== Okay(Okay(Failed("unknown failure"))) } and
            { nestedListCoder.decode(cl, JArray(Nil)) must_== Okay(Okay(Failed("unknown failure"))) } and
            { nestedListCoder.decode(cl, JNothing) must_== Okay(Failed("unknown failure")) } and
            { nestedListCoder.decode(cl, JNull) must_== Okay(Failed("unknown failure")) } and
            { nestedListCoder.decode(cl, JArray(JArray(failedJSON("failed") :: Nil) :: Nil)) must_== Okay(Okay(Okay(Failed("failed")))) } and
            { nestedListCoder.decode(cl, JArray(failedJSON("failed") :: Nil)) must_== Okay(Okay(Failed("failed"))) } and
            { nestedListCoder.decode(cl, failedJSON("failed")) must_== Okay(Failed("failed")) }
        } ^
        "round trip nested results via Avro" ! {
            { avroRoundTrip(nestedCoder, Okay(Okay(Okay("foo")))) } and
            { avroRoundTrip(nestedCoder, Okay(Okay(Failed("failed")))) } and
            { avroRoundTrip(nestedCoder, Okay(Failed("failed"))) } and
            { avroRoundTrip(nestedCoder, Failed("failed")) }
        } ^
        "honor hideFailures given at configuration time" ! resetCoderSettings {
            { ResultCoder(UnitCoder, StringCoder, Some(true)).encode(cl, Failed("test")) must_== Okay(JNothing) } and
            { ResultCoder(UnitCoder, StringCoder, Some(false)).encode(cl, Failed("test")) must matchEncodedJson(failedJSON("test")) }
        } ^
        "honor hideFailures given at configuration time even if set otherwise at encode time" ! resetCoderSettings {
            CoderSettings.hideFailures.set(true)

            { ResultCoder(UnitCoder, StringCoder, Some(true)).encode(cl, Failed("test")) must_== Okay(JNothing) } and
            { ResultCoder(UnitCoder, StringCoder, Some(false)).encode(cl, Failed("test")) must matchEncodedJson(failedJSON("test")) }
        } ^
        "honor hideFailures given at encode time" ! resetCoderSettings {
            CoderSettings.hideFailures.set(true)
            ResultCoder(UnitCoder, StringCoder).encode(cl, Failed("test")) must_== Okay(JNothing)
        } ^
        "encode FailedG" ! {
            { failedGWithStringCoder.encode(cl, failedGWithString) must matchEncodedJson(failedGWithStringJSON) } and
            { failedGWithIntCoder.encode(cl, failedGWithInt) must matchEncodedJson(failedGWithIntJSON) } and
            { failedGWithCaseClassCoder.encode(cl, failedGWithCaseClass) must matchEncodedJson(failedGWithCaseClassJSON) } and
            { failedGWithTupleCoder.encode(cl, failedGWithTuple) must matchEncodedJson(failedGWithTupleJSON) }
        } ^
        "decode FailedG" ! {
            { failedGWithStringCoder.decode(cl, failedGWithStringJSON) must_== Okay(failedGWithString) } and
            { failedGWithIntCoder.decode(cl, failedGWithIntJSON) must_== Okay(failedGWithInt) } and
            { failedGWithCaseClassCoder.decode(cl, failedGWithCaseClassJSON) must_== Okay(failedGWithCaseClass) } and
            { failedGWithTupleCoder.decode(cl, failedGWithTupleJSON) must_== Okay(failedGWithTuple) }
        } ^
        "round trip FailedG" ! {
            { avroRoundTrip(failedGWithStringCoder, failedGWithString) } and
            { avroRoundTrip(failedGWithIntCoder, failedGWithInt) } and
            { avroRoundTrip(failedGWithCaseClassCoder, failedGWithCaseClass) } and
            { avroRoundTrip(failedGWithTupleCoder, failedGWithTuple) }
        } ^
        "encode Units" ! {
            testUnitCoder.encode(cl, Okay(())) must matchEncodedJson(JArray(JNothing :: Nil))
        } ^
        "decode Units" ! {
            { testUnitCoder.decode(cl, JArray(JNothing :: Nil)) must_== Okay(Okay(())) } and
            { testUnitCoder.decode(cl, JArray(Nil)) must_== Okay(Okay(())) }
        } ^
        "round trip Units" ! {
            { testUnitCoder.encode(cl, Okay(())).flatMap(testUnitCoder.decode(cl, _)) must_== Okay(Okay(())) } and
            { avroRoundTrip(testUnitCoder, Okay(())) }
        }
    )
}

class EitherCoderSpecTest extends SpecificationWithJUnit {
    val testCoder = EitherCoder(StringCoder, IntCoder)

    def is =
        "EitherCoder should" ^
        "basically decode" ! {
            { testCoder.decode(cl, "left" -> JString("foo")) must_== Okay(Left("foo")) } and
            { testCoder.decode(cl, "right" -> JInt(1)) must_== Okay(Right(1)) }
        } ^
        "basically encode" ! {
            { testCoder.encode(cl, Left("foo")) must matchEncodedJson("left" -> JString("foo")) } and
            { testCoder.encode(cl, Right(1)) must matchEncodedJson("right" -> JInt(1)) }
        } ^
        "basically round trip via Avro" ! {
            { avroRoundTrip(testCoder, Left("foo")) } and
            { avroRoundTrip(testCoder, Right(1)) }
        }
}

class AutomaticUnionCoderSpecTest extends SpecificationWithJUnit {
    import fixtures._
    import Coders._

    def is =
        "automatic UnionCoders should" ^
        "basically decode" ! {
            { autoUnionCoder.decode(cl, "a" -> 1) must_== Okay(AutoUnionFirst(1)) } and
            { autoUnionCoder.decode(cl, "b" -> "foo") must_== Okay(AutoUnionSecond("foo")) }
        } ^
        "basically encode" ! {
            { autoUnionCoder.encode(cl, AutoUnionFirst(1)) must matchEncodedJson("a" -> 1) } and
            { autoUnionCoder.encode(cl, AutoUnionSecond("foo")) must matchEncodedJson("b" -> "foo") }
        } ^
        "round trip via Avro" ! {
            { avroRoundTrip(autoUnionCoder, AutoUnionFirst(1)) } and
            { avroRoundTrip(autoUnionCoder, AutoUnionSecond("foo")) }
        } ^
        "fail to decode invalid inputs" ! {
            autoUnionCoder.decode(cl, "c" -> 1.0) must beLike { case FailedG(_, _) => ok }
        } ^
        "fail to encode invalid inputs" ! {
            autoUnionCoder.encode(cl, AutoUnionInvalid(1.0)) must beLike { case FailedG(_, _) => ok }
        }
}

class ExplicitUnionCoderSpecTest extends SpecificationWithJUnit {
    import fixtures._
    import Coders._

    def is =
        "explicit UnionCoders should" ^
        "basically decode" ! {
            { explicitUnionCoder.decode(cl, ("_type" -> "first") ~ ("a" -> 1)) must_== Okay(ExplicitUnionFirst(1)) } and
            { explicitUnionCoder.decode(cl, ("_type" -> "second") ~ ("b" -> "foo")) must_== Okay(ExplicitUnionSecond("foo")) }
        } ^
        "basically encode" ! {
            { explicitUnionCoder.encode(cl, ExplicitUnionFirst(1)) must matchEncodedJson(("_type" -> "first") ~ ("a" -> 1)) } and
            { explicitUnionCoder.encode(cl, ExplicitUnionSecond("foo")) must matchEncodedJson(("_type" -> "second") ~ ("b" -> "foo")) }
        } ^
        "round trip via Avro" ! {
            { avroRoundTrip(explicitUnionCoder, ExplicitUnionFirst(1)) } and
            { avroRoundTrip(explicitUnionCoder, ExplicitUnionSecond("foo")) } and
            { avroRoundTrip(explicitUnionCoder, ExplicitUnionSingleton) } and
            { avroRoundTrip(explicitUnionCoder, ExplicitUnionSingletonWithProperties) }
        } ^
        "fail to decode invalid inputs" ! {
            { explicitUnionCoder.decode(cl, ("_type" -> "invalid") ~ ("c" -> 1.0)) must beLike { case FailedG(_, _) => ok } } and
            { explicitUnionCoder.decode(cl, "b" -> 1) must beLike { case FailedG(_, _) => ok  } }
        } ^
        "fail to encode invalid inputs" ! {
            explicitUnionCoder.encode(cl, ExplicitUnionInvalid(1.0)) must beLike { case FailedG(_, _) => ok }
        } ^
        "encode singletons as union members" ! {
            { explicitUnionCoder.encode(cl, ExplicitUnionSingleton) must matchEncodedJson("_type" -> "singleton") } and
            { explicitUnionCoder.encode(cl, ExplicitUnionSingletonWithProperties) must matchEncodedJson(("codedProperty" -> "bar") ~ ("_type" -> "singletonWithProperties")) }
        } ^
        "decode singletons as union members" ! {
            { explicitUnionCoder.decode(cl, "_type" -> "singleton") must_== Okay(ExplicitUnionSingleton) } and
            { explicitUnionCoder.decode(cl, "_type" -> "singletonWithProperties") must_== Okay(ExplicitUnionSingletonWithProperties) } and
            { explicitUnionCoder.decode(cl, ("_type" -> "singletonWithProperties") ~ ("codedProperty" -> "bar")) must_== Okay(ExplicitUnionSingletonWithProperties) }
        }
}

class ArgumentArrayCoderSpecTest extends SpecificationWithJUnit {
    val testCoder = ArgumentArrayCoder(false,
                                       ArgumentCoding("foo", IntCoder) ::
                                       ArgumentCoding("bar", NullCoder(StringCoder)) ::
                                       ArgumentCoding("zip", OptionCoder(StringCoder)) :: Nil)

    def matchArrayResult(expected: Array[AnyRef]): Matcher[ResultG[_, Array[AnyRef]]] = {
        def arrayToString(in: Array[AnyRef]): String = in.mkString("[", ", ", "]")

        (actualResult: ResultG[_, Array[AnyRef]]) => {
            actualResult match {
                case Okay(actual) if expected.length == actual.length && !(expected zip actual).exists { case (a, b) => a != b } =>
                    ok

                case _ =>
                    ko(actualResult.map(arrayToString).toString + " does not match Okay(" + arrayToString(expected) + ")")
            }
        }
    }

    def is =
        "ArgumentArrayCoder should" ^
        "basically decode" ! {
            testCoder.decode(cl, testObject) must matchArrayResult(Array[AnyRef](1.asInstanceOf[AnyRef], "baz", Some("qux")))
        } ^
        "basically encode" ! {
            testCoder.encode(cl, Array[AnyRef](1.asInstanceOf[AnyRef], "baz", Some("qux"))) must matchEncodedJson(testObject)
        } ^
        "round trip via Avro" ! {
            val firstArray = Array[AnyRef](1.asInstanceOf[AnyRef], "baz", Some("qux"))
            val secondArray = Array[AnyRef](1.asInstanceOf[AnyRef], null, Some("qux"))

            { avroRoundTripExpect(testCoder, firstArray) { (result: Result[Array[AnyRef]]) => result must matchArrayResult(firstArray) } } and
            { avroRoundTripExpect(testCoder, secondArray) { (result: Result[Array[AnyRef]]) => result must matchArrayResult(secondArray) } }
        } ^
        "support decoding null-ish things" ! {
            { testCoder.decode(cl, "foo" -> 1) must matchArrayResult(Array[AnyRef](1.asInstanceOf[AnyRef], null, None)) } and
            { testCoder.decode(cl, ("foo" -> 1) ~ ("bar" -> "baz")) must matchArrayResult(Array[AnyRef](1.asInstanceOf[AnyRef], "baz", None)) } and
            { testCoder.decode(cl, ("foo" -> 1) ~ ("zip" -> "qux")) must matchArrayResult(Array[AnyRef](1.asInstanceOf[AnyRef], null, Some("qux"))) }
        } ^
        "support encoding nulls" ! {
            { testCoder.encode(cl, Array[AnyRef](1.asInstanceOf[AnyRef], null, None)) must matchEncodedJson("foo" -> 1) } and
            { testCoder.encode(cl, Array[AnyRef](null, "baz", None)) must matchEncodedJson("bar" -> "baz") }
        } ^
        "properly handle flattening arguments" ! {
            import fixtures.Flattened
            import fixtures.Coders.enclosingFlattenedArgumentArrayCoder

            val json = ("a" -> "foo") ~ ("c" -> 1) ~ ("b" -> "bar")
            val args = Array[AnyRef](Flattened("foo", 1), "bar")

            { enclosingFlattenedArgumentArrayCoder.encode(cl, args) must matchEncodedJson(json) } and
            { enclosingFlattenedArgumentArrayCoder.decode(cl, json) must matchArrayResult(args) }
        }
}

class ObjectCoderSpecTest extends SpecificationWithJUnit {
    import fixtures._
    import Coders._

    val caseClass = classOf[CaseClass]
    val basicClass = classOf[BasicClass]
    val pojoClass = classOf[POJOClass]

    val caseClassValue = CaseClass(1, "baz", Some("qux"))
    val basicClassValue = BasicClass.make(1, "baz", Some("qux"))
    val pojoClassValue = POJOClass.make(1, "baz", Some("qux"))

    def is =
        "ObjectCoder should" ^
        "basically decode" ! {
            { caseClassCoder.decode(cl, testObject) must_== Okay(caseClassValue) } and
            { basicClassCoder.decode(cl, testObject) must_== Okay(basicClassValue) } and
            { pojoClassCoder.decode(cl, testObject) must_== Okay(pojoClassValue) }
        } ^
        "basically encode" ! {
            { caseClassCoder.encode(cl, caseClassValue) must matchEncodedJson(testObject) } and
            { basicClassCoder.encode(cl, basicClassValue) must matchEncodedJson(testObject) } and
            { pojoClassCoder.encode(cl, pojoClassValue) must matchEncodedJson(testObject) }
        } ^
        "basically round trip via Avro" ! {
            { avroRoundTrip(caseClassCoder, caseClassValue) } and
            { avroRoundTrip(basicClassCoder, basicClassValue) } and
            { avroRoundTrip(pojoClassCoder, pojoClassValue) }
        } ^
        "encode null-like things" ! {
            { caseClassCoder.encode(cl, CaseClass(1, null, Some("qux"))) must matchEncodedJson(("foo" -> 1) ~ ("zip" -> "qux")) } and
            { basicClassCoder.encode(cl, BasicClass.make(1, null, Some("qux"))) must matchEncodedJson(("foo" -> 1) ~ ("zip" -> "qux")) } and
            { pojoClassCoder.encode(cl, POJOClass.make(1, null, Some("qux"))) must matchEncodedJson(("foo" -> 1) ~ ("zip" -> "qux")) } and
            { caseClassCoder.encode(cl, CaseClass(1, "baz", None)) must matchEncodedJson(("foo" -> 1) ~ ("bar" -> "baz")) } and
            { basicClassCoder.encode(cl, BasicClass.make(1, "baz", None)) must matchEncodedJson(("foo" -> 1) ~ ("bar" -> "baz")) } and
            { pojoClassCoder.encode(cl, POJOClass.make(1, "baz", None)) must matchEncodedJson(("foo" -> 1) ~ ("bar" -> "baz")) }
        } ^
        "decode null-like things" ! {
            { caseClassCoder.decode(cl, ("foo" -> 1) ~ ("zip" -> "qux")) must_== Okay(CaseClass(1, null, Some("qux"))) } and
            { basicClassCoder.decode(cl, ("foo" -> 1) ~ ("zip" -> "qux")) must_== Okay(BasicClass.make(1, null, Some("qux"))) } and
            { pojoClassCoder.decode(cl, ("foo" -> 1) ~ ("zip" -> "qux")) must_== Okay(POJOClass.make(1, null, Some("qux"))) }
        } ^
        "round trip null-like things via Avro" ! {
            { avroRoundTrip(caseClassCoder, CaseClass(1, null, Some("qux"))) } and
            { avroRoundTrip(basicClassCoder, BasicClass.make(1, null, Some("qux"))) } and
            { avroRoundTrip(pojoClassCoder, POJOClass.make(1, null, Some("qux"))) }
        } ^
        "not encode None" ! {
            { caseClassCoder.encode(cl, CaseClass(1, "baz", None)) must matchEncodedJson(("foo" -> 1) ~ ("bar" -> "baz")) } and
            { basicClassCoder.encode(cl, BasicClass.make(1, "baz", None)) must matchEncodedJson(("foo" -> 1) ~ ("bar" -> "baz")) } and
            { pojoClassCoder.encode(cl, POJOClass.make(1, "baz", None)) must matchEncodedJson(("foo" -> 1) ~ ("bar" -> "baz")) }
        } ^
        "round trip null-like things via Avro" ! {
            { avroRoundTrip(caseClassCoder, CaseClass(1, null, Some("qux"))) } and
            { avroRoundTrip(basicClassCoder, BasicClass.make(1, null, Some("qux"))) } and
            { avroRoundTrip(pojoClassCoder, POJOClass.make(1, null, Some("qux"))) }
        } ^
        "properly handle flattening arguments" ! {
            val json = ("a" -> "foo") ~ ("c" -> 1) ~ ("b" -> "bar")
            val obj = EnclosingFlattened(Flattened("foo", 1), "bar")

            { enclosingFlattenedObjectCoder.encode(cl, obj) must matchEncodedJson(json) } and
            { enclosingFlattenedObjectCoder.decode(cl, json) must_== Okay(obj) }
        }
}
