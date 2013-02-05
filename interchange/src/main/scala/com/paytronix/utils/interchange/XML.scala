//
// Copyright 2012-2013 Paytronix Systems, Inc.
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

package com.paytronix.utils.interchange

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.collection.mutable.{Builder, ListBuffer}
import scala.xml.{Comment, Elem, MetaData, Node, NodeSeq, Null, PCData, PrefixedAttribute, Text, TopScope, UnprefixedAttribute}

import net.liftweb.json.JsonAST.{JArray, JBool, JDouble, JField, JInt, JNothing, JNull, JObject, JString, JValue}

/**
 * Utilities to convert JSON to XML and back again, used to support XML over HTTP instead of the (preferred) JSON over HTTP using Interchange.
 *
 * All scalar values (bools, strings, ints, doubles) become text fragments:
 *   true <=> true, "foobar" <=> foobar, 123 <=> 123, 12.34 <=> 12.34
 * Except if it's a string value starting or ending with a space, in which case a PCDATA is emitted.
 * Arrays are reified as <array><item>first</item><item>second</item></array>:
 *   [1,2,3] <=> <array><item>1</item><item>2</item><item>3</item></array>
 * Nulls are reified as <null/>
 *   null <=> <null/>
 * Objects are implicitly created for any element content that consists of tags that aren't special (<null/>, <array/>):
 *   {"foo": "bar", "baz": null, "zippy": 123} => <foo>bar</foo><baz><null/></baz><zippy>123</zippy>
 * As a special case, attributes will be added to a generated object:
 *   <foo bar="baz"><zippy>123</zippy></foo> => {"foo": {"bar": "baz", "zippy": "123"}}
 * Empty objects are represented by the lone tag <empty/>:
 *   {} <=> <empty/>
 *
 * Note that since XML has no type information for scalar values all JSON scalars are turned into strings. Interchange supports strings as input for all
 * basic scalar types, so this shouldn't pose a problem. If you use XML support, you must ensure that any custom coders you use support strings even where integers,
 * floats, etc are preferred.
 */
object XML {
    private val INVALID_ELEM_CHARS      = """[^\w.-]"""
    private val INVALID_ELEM_CHAR_REGEX = INVALID_ELEM_CHARS.r
    private val INVALID_ELEM_NAME_REGEX = (".*" + INVALID_ELEM_CHARS + ".*").r

    /** Convert JSON to XML */
    def fromJSON(in: JValue): NodeSeq =
        in match {
            case JArray(elements) => <array>{ elements.map(e => <item>{ fromJSON(e) }</item>)(breakOut) }</array>
            case JBool(b)         => Text(b.toString)
            case JDouble(d)       => Text(d.toString)
            case JInt(i)          => Text(i.toString)
            case JNothing         => NodeSeq.Empty
            case JNull            => <null />
            case JObject(Nil)     => <empty />
            case JObject(fields)  => fields.flatMap(fromJSON)
            case JString("")      => NodeSeq.Empty
            case JString(s) if Character.isWhitespace(s.charAt(0)) || Character.isWhitespace(s.charAt(s.length-1)) =>
                PCData(s)
            case JString(s) => Text(s)
            case JField(n@INVALID_ELEM_NAME_REGEX(), v) => Elem(null, INVALID_ELEM_CHAR_REGEX.replaceAllIn(n, "_"), Null, TopScope, fromJSON(v): _*)
            case JField(n, v)     => Elem(null, n, Null, TopScope, fromJSON(v): _*)
        }

    /** Convert XML to JSON */
    def toJSON(in: NodeSeq): JValue = {
        def isWhitespace(in: Node): Boolean =
            in match {
                case Text(s) => s.trim() == ""
                case _: Comment => true
                case _ => false
            }

        def trim(ns: Seq[Node]): NodeSeq = trimRight(trimLeft(ns))

        def trimLeft(ns: Seq[Node]): NodeSeq =
            ns.dropWhile(isWhitespace)

        @tailrec
        def trimRight(ns: Seq[Node]): NodeSeq =
            if (ns.lastOption.map(isWhitespace).getOrElse(false))
                trimRight(ns.dropRight(1))
            else
                ns

        def textValue(ns: Seq[Node], withTrimming: Boolean): String = {
            val stringAtoms = ns.filter { case (_: Text)|(_: PCData) => true; case _ => false }

            def trimLeftString(in: String): String =
                in.indexWhere(ch => !Character.isWhitespace(ch)) match {
                    case -1 => ""
                    case pos => in.substring(pos)
                }

            def trimRightString(in: String): String =
                in.lastIndexWhere(ch => !Character.isWhitespace(ch)) match {
                    case -1 => ""
                    case pos => in.substring(0, pos+1)
                }

            def rawTextValue(in: Seq[Node]): String =
                in.view.map {
                    case Text(s)   => s
                    case PCData(s) => s
                    case _ => ""
                }.mkString

            if (!withTrimming)
                rawTextValue(stringAtoms)
            else
                // ugh, there's probably a better way. also this doesn't correctly handle Text(whitespace) ++ Text(with leading spaces) correctly (it leaves the leading spaces)
                stringAtoms match {
                    case Seq(Text(s))   => s.trim
                    case Seq(PCData(s)) => s
                    case other =>
                        val mid = rawTextValue(other.tail.dropRight(1))
                        (other.head, other.last) match {
                            case (Text(s),   Text(s2)  ) => trimLeftString(s) + mid + trimRightString(s2)
                            case (Text(s),   PCData(s2)) => trimLeftString(s) + mid + s2
                            case (PCData(s), Text(s2)  ) => s                 + mid + trimRightString(s2)
                            case _ => sys.error("only Text and PCData should be here due to filter")
                        }
                }
        }

        def fieldsFromAttributes(attrs: MetaData): Seq[JField] =
            attrs.view.map {
                case PrefixedAttribute(_, name, value, _) => JField(name, JString(textValue(trim(value), false)))
                case UnprefixedAttribute(name, value, _)  => JField(name, JString(textValue(trim(value), false)))
                case Null                                 => sys.error("didn't expect Null in attribute view")
            }.toSeq

        def fixupElementValue(attrs: MetaData, children: Seq[Node]): JValue =
            (toJSON(children), fieldsFromAttributes(attrs)) match {
                case (JNothing, Seq())          => JString("")
                case (JNothing, fields)         => JObject(fields.toList)
                case (jvalue, Seq())            => jvalue
                case (JObject(fields), fields2) => JObject(fields ++ fields2)
                case (jvalue, _) =>
                    sys.error("unexpected attributes " + attrs + " cannot be added to scalar or array value enclosed (" + jvalue + ")")
            }

        val ns = trimLeft(in.view)
        lazy val noMore = trimLeft(ns.tail).isEmpty
        lazy val allText = ns.forall(n => n.isInstanceOf[Text] || n.isInstanceOf[PCData])

        if (ns.isEmpty)
            JNothing
        else
            ns.head match {
                case (_: Text)|(_: PCData) if allText =>
                    JString(textValue(trimRight(ns), true))

                case (_: Text)|(_: PCData) =>
                    sys.error("unexpected mix of text and other nodes: " + ns)

                case <empty /> if noMore => JObject(Nil)
                case <null /> if noMore => JNull
                case Elem(_, "array", _, _, children@_*) if noMore =>
                    JArray(children.flatMap {
                        case Elem(_, "item", attrs, _, children@_*) =>
                            Some(fixupElementValue(attrs, children))
                        case _ =>
                            None
                    } (breakOut))

                case _ =>
                    JObject(ns.flatMap {
                        case Elem(_, field, attrs, _, children@_*) =>
                            Some(JField(field, fixupElementValue(attrs, children)))
                        case other if isWhitespace(other) =>
                            None
                        case other =>
                            sys.error("unexpected " + other + " (of type " + other.getClass.getName + ") mixed in with elements in " + ns)
                    } (breakOut))
            }
    }
}


