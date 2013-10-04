//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.common

import scala.collection.immutable.{SortedSet, TreeSet}

import net.liftweb.json.JsonAST.{JArray, JField, JObject, JValue, render}
import net.liftweb.json.Printer.compact

import com.paytronix.utils.extendedreflection.{Builder, ClassTypeR}
import com.paytronix.utils.interchange.{Coding, CollectionCoder, ComposableCoder, ExplicitUnionCoding, FieldCoder}
import com.paytronix.utils.scala.result.{Failed, Okay, Result, iterableResultOps}

/** Trait of actions that can be performed on configuration data to combine some inherited value with a refining context value */
trait InheritanceAction {
    /** Combine a value from a parent node (`inherited`) and a current node (`value`) */
    def combine(inherited: JValue, value: JValue): Result[JValue]
}

object InheritanceAction {
    val coding = Coding.forClass[InheritanceAction]
}

object InheritanceActionCoding extends ExplicitUnionCoding[InheritanceAction] {
    override val determinantField = "action"
    alternative[ExcludeAction.type]("exclude")
    alternative[ReplaceAction.type]("replace")
    alternative[ArrayAppendAction.type]("arrayAppend")
    alternative[ArrayAppendDistinctAction.type]("arrayAppendDistinct")
    alternative[ArrayPrependAction.type]("arrayPrepend")
    alternative[ArrayPrependDistinctAction.type]("arrayPrependDistinct")
    alternative[ObjectMergeAction.type]("objectMerge")
}

case object ExcludeAction extends InheritanceAction {
    def combine(inherited: JValue, value: JValue) =
        Okay(inherited)
    override def toString = "exclude"
}

case object ReplaceAction extends InheritanceAction {
    def combine(inherited: JValue, value: JValue) =
        Okay(value)
    override def toString = "replace"
}

abstract class ArrayInheritanceAction extends InheritanceAction {
    def combineArrays(inherited: JArray, value: JArray): Result[JArray]
    def combine(inherited: JValue, value: JValue): Result[JValue] =
        (inherited, value) match {
            case (a: JArray, b: JArray) => combineArrays(a, b)
            case _ => Failed("cannot combine " + compact(render(inherited)) + " and " + compact(render(value)) + ": expected two arrays")
        }
}

case object ArrayAppendAction extends ArrayInheritanceAction {
    def combineArrays(inherited: JArray, value: JArray): Result[JArray] =
        Okay(JArray(inherited.arr ++ value.arr))
    override def toString = "arrayAppend"
}
case object ArrayAppendDistinctAction extends ArrayInheritanceAction {
    def combineArrays(inherited: JArray, value: JArray): Result[JArray] =
        Okay(JArray((inherited.arr ++ value.arr).distinct))
    override def toString = "arrayAppendDistinct"
}
case object ArrayPrependAction extends ArrayInheritanceAction {
    def combineArrays(inherited: JArray, value: JArray): Result[JArray] =
        Okay(JArray(value.arr ++ inherited.arr))
    override def toString = "arrayPrepend"
}
case object ArrayPrependDistinctAction extends ArrayInheritanceAction {
    def combineArrays(inherited: JArray, value: JArray): Result[JArray] =
        Okay(JArray((value.arr ++ inherited.arr).distinct))
    override def toString = "arrayPrependDistinct"
}

abstract class ObjectInheritanceAction extends InheritanceAction {
    def combineObjects(inherited: JObject, value: JObject): Result[JObject]
    def combine(inherited: JValue, value: JValue): Result[JValue] =
        (inherited, value) match {
            case (a: JObject, b: JObject) => combineObjects(a, b)
            case _ => Failed("cannot combine " + compact(render(inherited)) + " and " + compact(render(value)) + ": expected two objects")
        }
}
case object ObjectMergeAction extends ObjectInheritanceAction {
    def combineObjects(inherited: JObject, value: JObject): Result[JObject] =
        Okay(JObject {
            def toMap(jobj: JObject): Map[String, JField] =
                Map(jobj.obj.map { case jf@JField(n, _) => n -> jf }: _*)
            (toMap(inherited) ++ toMap(value)).values.toList
        })
    override def toString = "objectMerge"
}

/** A single inheritance rule that applies some action to some key path in the contents of a node */
final case class InheritanceRule(keyPath: KeyPath, action: InheritanceAction)

object InheritanceRule {
    implicit val ordering = new Ordering[InheritanceRule] {
        def compare(a: InheritanceRule, b: InheritanceRule): Int = {
            val aspecificity = a.keyPath.size
            val bspecificity = b.keyPath.size
            if (aspecificity == bspecificity)
                Ordering.Iterable[String].compare(a.keyPath, b.keyPath)
            else
                bspecificity - aspecificity
        }
    }
}

import InheritanceRule.ordering

final case class InheritanceRules(rules: SortedSet[InheritanceRule]) {
    def apply(chain: Seq[NodeContents]): Result[Option[NodeContents]] =
        if (chain.isEmpty) Okay(None)
        else {
            // for each pair of (inheritedNode, currentNode) in the inheritance chain, apply all the rules outside-in
            chain.tail.foldLeftResult(chain.head) { (inheritedNode, currentNode) =>
                // determine what root action to use, whether it's one provided for this aspect or the default object
                // merge action
                val (rootAction, restOfRules) = rules.headOption match {
                    case Some(InheritanceRule(Nil, action)) => (action, rules.tail)
                    case _ => (ObjectMergeAction, rules)
                }

                rootAction.combine(inheritedNode, currentNode).flatMap {
                    // apply each rule (pre-sorted at caching time) in turn, walking to the path where the rule applies
                    // in both the very original inherited node and the current state of the node.

                    // in the case where the path is valid (e.g. exists in both the inherited node and current node state)
                    // then apply the inheritance action in question otherwise leave the current node state unchanged

                    restOfRules.foldLeftResult(_) { (currentNode, rule) =>
                        val InheritanceRule(path, action) = rule

                        def tandemWalk(left: JValue, right: JValue, path: KeyPath): Option[(JValue, JValue)] =
                            (left, right, path) match {
                                case (_, _, Nil) => Some((left, right))
                                case (JObject(lfields), JObject(rfields), segment :: rest) =>
                                    (lfields.find(_.name == segment), rfields.find(_.name == segment)) match {
                                        case (Some(left), Some(right)) => tandemWalk(left.value, right.value, rest)
                                        case _ => None
                                    }
                                case _ => None
                            }

                        tandemWalk(inheritedNode, currentNode, path) match {
                            case Some((l, r)) =>
                                action.combine(l, r).map { currentNode.replace(path, _) }
                            case None =>
                                // FIXME? if the path doesn't exist at both the rule is just not applied
                                Okay(currentNode)
                        }
                    }
                }.flatMap {
                    case jo: JObject => Okay(jo)
                    case other => Failed("result of applying inheritance yielded " + other + " rather than JObject")
                }
            }.map(Some.apply)
        }
}

object InheritanceRulesCoding extends Coding {
    // FIXME? this is monkey business, just to make a SortedSet usable
    def makeCoder(default: => Result[ComposableCoder[_]], classTypeR: ClassTypeR)(implicit builder: Builder) =
        Coding.forClassComposable[InheritanceRule].map { case valueCoder =>
            val collectionCoder = CollectionCoder(valueCoder) (
                TreeSet.newCanBuildFrom(InheritanceRule.ordering),
                implicitly[SortedSet[InheritanceRule] => Iterable[InheritanceRule]],
                implicitly[Manifest[SortedSet[_]]].asInstanceOf[Manifest[SortedSet[InheritanceRule]]]
            )

            FieldCoder("rules", collectionCoder).transform {
                rules => Okay(new InheritanceRules(rules))
            } {
                ir => Okay(ir.rules)
            }
        }
}


object InheritanceRules {
    val Aspect = "configuration.InheritanceRules"

    val empty = InheritanceRules(SortedSet.empty)
    val coding = Coding.forClass[InheritanceRules]

    def apply(in: Seq[InheritanceRule]): InheritanceRules =
        InheritanceRules(TreeSet.empty ++ in)
}

object InheritanceRulesDSL extends InheritanceRulesDSL

trait InheritanceRulesDSL {
    val exclude              = ExcludeAction
    val replace              = ReplaceAction
    val arrayAppend          = ArrayAppendAction
    val arrayAppendDistinct  = ArrayAppendDistinctAction
    val arrayPrepend         = ArrayPrependAction
    val arrayPrependDistinct = ArrayPrependDistinctAction
    val objectMerge          = ObjectMergeAction

    implicit def stringToKeyPath(in: String): KeyPath = List(in)

    implicit def stringOps(in: String) = StringOps(in)

    final case class StringOps(s: String) {
        def ==> (action: InheritanceAction) = InheritanceRule(s.split('/').map(_.trim).toList, action)
        def / (field: String): KeyPath = List(s, field)
    }

    implicit def keyPathOps(in: KeyPath) = KeyPathOps(in)

    final case class KeyPathOps(kp: KeyPath) {
        def ==> (action: InheritanceAction) = InheritanceRule(kp, action)
        def / (field: String): KeyPath = kp :+ field
    }

    implicit def ruleToRules(in: InheritanceRule) = InheritanceRules(TreeSet(in))

    implicit def ruleRulesOps(in: InheritanceRule) = InheritanceRulesOps(TreeSet(in))
    implicit def rulesOps(in: SortedSet[InheritanceRule]) = InheritanceRulesOps(in)
    implicit def rulesOps(in: InheritanceRules) = InheritanceRulesOps(in.rules)

    final case class InheritanceRulesOps(rules: SortedSet[InheritanceRule]) {
        def & (rule: InheritanceRule) = InheritanceRules(rules + rule)
        def & (more: InheritanceRules) = InheritanceRules(rules ++ more.rules)
    }
}
