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
class resultMacros(val c: Context) {
    import c.universe.{EmptyTree, FunctionTag, NoSymbol, Quasiquote, TermName, Transformer, Tree, TreeTag, TypeNameTag, TypeTree, TypeTreeTag, ValDef}

    /** Macro implementation of >>= (bind) which avoids allocation of the closure etc. since the Scala optimizer cannot be trusted */
    def bind(f: c.Tree): c.Tree = {
        val res = TermName(c.freshName())

        c.macroApplication match {
            case q"$lhs.>>=[$f,$b]($fun)" =>
                fun match {
                    /* 2014-08-29 RMM: inlining a function is Fraught with Peril, including but not limited to the mystery of the vanishing compiler,
                                       missing value symbols, type checker exceptions, and strange import renaming issues. thus, this branch is
                                       disabled
                    case q"{$binder => $rhs}" =>
                        // case where we can optimize away an anonymous function literal

                        val ValDef(mods, name, tpt, _) = binder
                        val newBinder = ValDef(mods, name, tpt, q"$res.orThrow")

                        val replaceBinder = new Transformer() {
                            override def transform(tree: Tree): Tree =
                                super.transform {
                                    tree match {
                                        case tree if !tree.canHaveAttrs => tree
                                        case tpt: TypeTree => tpt
                                        case EmptyTree => tree
                                        case _ =>
                                            println(s"this symbol ${tree.symbol} binder ${binder.symbol}, tree $tree")
                                            if (tree.symbol == binder.symbol) {
                                                val dupl = tree.duplicate
                                                // woooo
                                                val m = dupl.getClass.getMethods.filter(_.getName == "symbol_$eq").head
                                                m.invoke(dupl, NoSymbol)
                                                val m2 = dupl.getClass.getMethod("clearType")
                                                m2.invoke(dupl).asInstanceOf[Tree]
                                            } else tree
                                    }
                                }
                        }

                        val out = c.untypecheck(q"""
                            {
                                val $res: ${lhs.tpe} = $lhs
                                if ($res.isInstanceOf[com.paytronix.utils.scala.result.Okay[_]]) {
                                    $newBinder
                                    ${replaceBinder.transform(rhs)}
                                } else {
                                    $res.asInstanceOf[com.paytronix.utils.scala.result.ResultG[$f, $b]]
                                }
                            }
                        """)

                        println(s"in = ${c.macroApplication}")
                        println(s"out = $out")

                        out
                    */

                    case _ =>
                        // case where we can only optimize away the pattern matching

                        q"""
                            {
                                val $res: ${lhs.tpe} = $lhs
                                if ($res.isInstanceOf[com.paytronix.utils.scala.result.Okay[_]]) {
                                    $fun($res.orThrow)
                                } else {
                                    $res.asInstanceOf[com.paytronix.utils.scala.result.ResultG[$f, $b]]
                                }
                            }
                        """
                }

            case _ =>
                sys.error(s"did not recognize ${c.macroApplication} as a binding, maybe try flatMap?")
        }
    }

    /** Macro implementation of >> (chain) which avoids allocation of the closure etc. since the Scala optimizer cannot be trusted */
    def chain(consequent: c.Tree): c.Tree = {
        val res = TermName(c.freshName())

        c.macroApplication match {
            case q"$lhs.>>[$f,$b]($rhs)" =>
                c.typecheck(q"""
                    {
                        val $res: ${lhs.tpe} = $lhs
                        if ($res.isOkay) {
                            $rhs
                        } else {
                            $res.asInstanceOf[com.paytronix.utils.scala.result.ResultG[$f, $b]]
                        }
                    }
                """)

            case _ =>
                sys.error(s"did not recognize ${c.macroApplication} as a binding, maybe try flatMap { _ => … }?")
        }
    }
}
