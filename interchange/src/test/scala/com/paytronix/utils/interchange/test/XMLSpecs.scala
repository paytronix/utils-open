//
// Copyright 2012 Paytronix Systems, Inc.
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

import scala.xml._

import net.liftweb.json.Implicits._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import org.specs2.SpecificationWithJUnit

import com.paytronix.utils.interchange.XML.{fromJSON, toJSON}

class JSONToXMLSpecTest extends SpecificationWithJUnit { def is =
    "JSON to XML conversion" ^
    "simple string"               ! { fromJSON(JString("foobar")) must_== Text("foobar") } ^
    "empty string"                ! { fromJSON(JString("")) must_== NodeSeq.Empty } ^
    "strings with leading spaces" ! { fromJSON(JString("    foo")) must_== PCData("    foo") } ^
    "boolean"                     ! { fromJSON(JBool(true)) must_== Text("true") } ^
    "int"                         ! { fromJSON(JInt(BigInt(123))) must_== Text("123") } ^
    "double"                      ! { fromJSON(JDouble(12.34)) must_== Text("12.34") } ^
    "null"                        ! { fromJSON(JNull) must_== <null /> } ^
    "nothing"                     ! { fromJSON(JNothing) must_== NodeSeq.Empty } ^
    "empty array"                 ! { fromJSON(JArray(Nil)) must_== <array /> } ^
    "nonempty array"              ! { fromJSON(List(1,2,3)) must_== <array><item>1</item><item>2</item><item>3</item></array> } ^
    "empty object"                ! { fromJSON(JObject(Nil)) must_== <empty /> } ^
    "nonempty object"             ! { fromJSON(("foo" -> "bar") ~ ("baz" -> "qux")) must_== (<foo>bar</foo> ++ <baz>qux</baz>) } ^
    "invalid element name" ! {
        fromJSON(
            JField("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ-1234567890./;:'m", JNull)
        ) must_== (
            <abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ-1234567890.____m><null /></abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ-1234567890.____m>
        )
    } ^
    "nontrivial object" ! {
        fromJSON (
            ("enforceUniqueFields" -> JArray(Nil)) ~
            ("setAccountFields" -> JObject(Nil)) ~
            ("setUserFields" -> (
                ("style" -> "typed") ~
                ("firstName" -> List("Ross")) ~
                ("lastName" -> JNull)
            ))
        ) must_== (
            <enforceUniqueFields><array /></enforceUniqueFields> ++
            <setAccountFields><empty /></setAccountFields> ++
            <setUserFields><style>typed</style><firstName><array><item>Ross</item></array></firstName><lastName><null /></lastName></setUserFields>
        )
    }
}

class XMLToJSONSpecTest extends SpecificationWithJUnit { def is =
    "XML to JSON conversion" ^
    "simple string"                                   ! { toJSON(Text("foobar")) must_== JString("foobar") } ^
    "empty string at top level"                       ! { toJSON(Text("")) must_== JNothing } ^
    "string with leading spaces (which are trimmed)"  ! { toJSON(Text("    foo")) must_== JString("foo") } ^
    "string with trailing spaces (which are trimmed)" ! { toJSON(Text("foo    ")) must_== JString("foo") } ^
    "string with medial spaces"                       ! { toJSON(Text("    foo") ++ Text(" ") ++ Text("bar    ")) must_== JString("foo bar") } ^
    "PCData with leading spaces"                      ! { toJSON(Text("    ") ++ PCData("  foo")) must_== JString("  foo") } ^
    "PCData with trailing spaces"                     ! { toJSON(PCData("  foo  ") ++ Text("    ")) must_== JString("  foo  ") } ^
    "mixed text and PCData"                           ! { toJSON(Text("foo") ++ PCData("  foo  ") ++ Text("bar")) must_== JString("foo  foo  bar") } ^
    "null"                                            ! { toJSON(<null />) must_== JNull } ^
    "nothing"                                         ! { toJSON(NodeSeq.Empty) must_== JNothing } ^
    "empty array"                                     ! { toJSON(<array />) must_== JArray(Nil) } ^
    "nonempty array"                                  ! { toJSON(<array><item>1</item><item>2</item></array>) must_== JArray(List(JString("1"), JString("2"))) } ^
    "empty object"                                    ! { toJSON(<empty />) must_== JObject(Nil) } ^
    "nonempty object"                                 ! { toJSON(<foo>bar</foo> ++ <baz>qux</baz>) must_== ((("foo" -> "bar") ~ ("baz" -> "qux")): JObject) } ^
    "not treat keywords specially with other fields"  ! { toJSON(<array>123</array> ++ <empty/>) must_== ((("array" -> "123") ~ ("empty" -> "")): JObject) } ^
    "ignore comments"                                 ! { toJSON(Comment("foo")) must_== JNothing } ^
    "ignore comments (leading)"                       ! { toJSON(Comment("foo") ++ <empty />) must_== JObject(Nil) } ^
    "ignore comments (trailing)"                      ! { toJSON(<empty /> ++ Comment("foo")) must_== JObject(Nil) } ^
    "ignore whitespace"                               ! { toJSON(Text("   \n\r")) must_== JNothing } ^
    "ignore whitespace (leading)"                     ! { toJSON(Text("  \n\r   ") ++ <empty />) must_== JObject(Nil) } ^
    "ignore whitespace (trailing)"                    ! { toJSON(<empty /> ++ Text("   \n\r  ")) must_== JObject(Nil) } ^
    "empty string in value positions"                 ! { toJSON(<foo />) must_== JObject(List(JField("foo", JString("")))) } ^
    "attributes on array items (implicit)"            ! { toJSON(<array><item foo="bar" baz="qux" /></array>) must_== JArray(List(("foo" -> "bar") ~ ("baz" -> "qux"))) } ^
    "attributes on array items (explicit)"            ! { toJSON(<array><item foo="bar" baz="qux"><empty/></item></array>) must_== JArray(List(("foo" -> "bar") ~ ("baz" -> "qux"))) } ^
    "attributes on field values (implicit)"           ! { toJSON(<monkey foo="bar" baz="qux" />) must_== (("monkey" -> (("foo" -> "bar") ~ ("baz" -> "qux"))): JObject) } ^
    "attributes on field values (explicit)"           ! { toJSON(<monkey foo="bar" baz="qux"><empty /></monkey>) must_== (("monkey" -> (("foo" -> "bar") ~ ("baz" -> "qux"))): JObject) } ^
    "nontrivial XML" ! {
        toJSON (
            <enforceUniqueFields><array /></enforceUniqueFields> ++
            <setAccountFields><empty /></setAccountFields> ++
            <setUserFields><style>typed</style><firstName><array><item>Ross</item></array></firstName><lastName><null /></lastName></setUserFields>
        ) must_== (
            (
                ("enforceUniqueFields" -> JArray(Nil)) ~
                ("setAccountFields" -> JObject(Nil)) ~
                ("setUserFields" -> (
                    ("style" -> "typed") ~
                    ("firstName" -> List("Ross")) ~
                    ("lastName" -> JNull)
                ))
            ): JObject
        )
    }
}


