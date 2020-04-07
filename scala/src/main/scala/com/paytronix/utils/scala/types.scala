//
// Copyright 2010-2020 Paytronix Systems, Inc.
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

package com.paytronix.utils.scala

package object types {
    /** Identity type function. Resolves to the type argument -- that is, Identity[String] =:= String */
    type Identity[A] = A

    /** Constant type function. Const[A] is a function F[_] which always yields A, i.e. Const[String][Int] =:= String */
    type Const[A] = {
        type F[_] = A
    }
}
