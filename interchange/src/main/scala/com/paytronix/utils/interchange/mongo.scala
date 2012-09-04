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

package com.paytronix.utils.interchange

import com.mongodb.DBObject
import com.paytronix.utils.scala.result.Result

trait CodedMongoObject[T] {
    implicit val manifest: Manifest[T]
    lazy val coder = Coding.forClass[T]

    /** Implicitly convert a coded object to a DBObject, throwing an exception if coding fails */
    implicit def toDBObjectOrThrow(obj: T): DBObject =
        toDBObject(obj).orThrow

    /** Implicitly convert a coded object to a DBObject */
    implicit def toDBObject(obj: T): Result[DBObject] =
        coder.flatMap(_.encodeMongoDB(obj)).asA[DBObject]

    /** Implicitly convert a coded object from a DBObject, throwing an exception if coding fails */
    implicit def fromDBObjectOrThrow(dbo: DBObject): T =
        fromDBObject(dbo).orElse("failed to decode " + dbo).orThrow

    /** Implicitly convert a coded object from a DBObject */
    implicit def fromDBObject(dbo: DBObject): Result[T] =
        coder.flatMap(_.decodeMongoDB(dbo)).orElse("failed to decode " + dbo).asA[T]
}
