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

package com.paytronix.utils.scala

import scala.reflect.macros.whitebox.Context

/**
 * Macros which implement >>= and >> for `ResultG` which doesn't allocate closures or invoke pattern matching (and therefore allocating Option)
 * but also does not require the optimiser to be turned on, which is pretty buggy (as of 2.11.1) and causes NoClassDefFoundErrors
 */
object resultMacros {
    /** Macro implementation of >>= (bind) which avoids allocation of the closure etc. since the Scala optimizer cannot be trusted */
    def bind(c: Context)(f: c.Tree): c.Tree = {
        import c.universe.{FunctionTag, Quasiquote, TermName, TreeTag, TypeNameTag, TypeTreeTag, ValDef}

        val res = TermName(c.freshName())

        c.macroApplication match {
            case q"$lhs.>>=[$f,$b]($fun)" =>
                c.typecheck(q"""
                    {
                        val $res: ${lhs.tpe} = ${c.untypecheck(lhs)}
                        if ($res.isOkay) {
                            $fun($res.orThrow)
                        } else {
                            $res.asInstanceOf[com.paytronix.utils.scala.result.ResultG[$f, $b]]
                        }
                    }
                """)

            case _ =>
                sys.error(s"did not recognize ${c.macroApplication} as a binding, maybe try flatMap?")
        }
    }

    /** Macro implementation of >> (chain) which avoids allocation of the closure etc. since the Scala optimizer cannot be trusted */
    def chain(c: Context)(consequent: c.Tree): c.Tree = {
        import c.universe.{FunctionTag, Quasiquote, TermName, TreeTag}

        val ok = TermName(c.freshName())
        val ko = TermName(c.freshName())

        c.macroApplication match {
            case q"$lhs.>>[$f,$b]($rhs)" =>
                // upcast to ResultG[_, _] here to avoid "fruitless type test" warnings
                q"""
                    (($lhs: com.paytronix.utils.scala.result.ResultG[_, _]) match {
                        case $ok: com.paytronix.utils.scala.result.Okay[_] =>
                            $rhs
                        case $ko: com.paytronix.utils.scala.result.FailedG[_] =>
                            $ko.asInstanceOf[com.paytronix.utils.scala.result.ResultG[$f, $b]]
                    })
                """

            case _ =>
                sys.error(s"did not recognize ${c.macroApplication} as a binding, maybe try flatMap { _ => … }?")
        }
    }
}
