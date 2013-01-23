//
// Copyright 2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.lift

import scala.xml.NodeSeq

import net.liftweb.http.js.{JsCmd, JsExp, JsonCall}
import net.liftweb.http.js.JE.JsNull
import net.liftweb.json.JsonAST.{JValue, render}
import net.liftweb.json.Printer.compact

import JsExp.strToJsExp

/**
 * DSL for constructing JavaScript expressions in a lightweight way, with conversions to allow them to be used where Lift's `JsCmd` or `JsExp` are expected.
 *
 * For example:
 *     `jQuery(renderedEvent).insertBefore(jQueryId(uniqueId + "-events").index(i * eventTemplate.size))`
 * Produces the JavaScript:
 *     `jQuery("<html blob>").insertBefore(jQuery("#uniqueId-events").index(1234))`
 */
object js extends js

trait js {
    /** Base type of JavaScript expressions, such a literal values, arithmetic, function calls, and selections */
    trait Expression {
        /** Convert this expression to JavaScript */
        val toJs: String

        /** Coercion member so literals can be written as `123.expr` */
        def expr: this.type = this

        /** Treat this expression as a function and apply some arguments to it. Equivalent JS: `expr(a1, a2, a3...)` */
        def apply(in: Expression*): Apply = Apply(this, in)

        /** Select a member from this expression. Equivalent JS: `expr.s` */
        def / (s: String): Select = select(Symbol(s))
        /** Select a member from this expression. Equivalent JS: `expr.s` */
        def / (s: Symbol): Select = select(s)

        /** Treat this expression as a statement and sequence it with another expression as a statement. Equivalent JS: `expr; expr2;` */
        def & (e: Expression): Statements = Statements(Vector(SimpleStatement(this), SimpleStatement(e)))

        /** Treat this expression as a statement and sequence it with another statement. Equivalent JS: `expr; stmt;` */
        def & (stmt: Statement): Statements = Statements(Vector(SimpleStatement(this), stmt))

        /** Test for equality with some other expression */
        def === (e: Expression): Equality = Equality(this, e)
        /** Test for inequality with some other expression */
        def !== (e: Expression): Inequality = Inequality(this, e)
        /** Test for strict equality with some other expression */
        def ==== (e: Expression): StrictEquality = StrictEquality(this, e)
        /** Test for strict inequality with some other expression */
        def !=== (e: Expression): StrictInequality = StrictInequality(this, e)

        /**
         * Test this expression is less than some other expression.
         * If the LHS is a constant, make sure to wrap it with `js`, e.g. `js(-1) < 0` otherwise the literal boolean result will be emitted in JavaScript
         */
        def < (e: Expression): LessThan = LessThan(this, e)
        /**
         * Test this expression is less than or equal to some other expression.
         * If the LHS is a constant, make sure to wrap it with `js`, e.g. `js(0) <= 0` otherwise the literal boolean result will be emitted in JavaScript
         */
        def <= (e: Expression): LessThanEqual = LessThanEqual(this, e)
        /**
         * Test this expression is greater than some other expression.
         * If the LHS is a constant, make sure to wrap it with `js`, e.g. `js(1) > 0` otherwise the literal boolean result will be emitted in JavaScript
         */
        def > (e: Expression): GreaterThan = GreaterThan(this, e)
        /**
         * Test this expression is greater than or equal to some other expression.
         * If the LHS is a constant, make sure to wrap it with `js`, e.g. `js(0) >= 0` otherwise the literal boolean result will be emitted in JavaScript
         */
        def >= (e: Expression): GreaterThanEqual = GreaterThanEqual(this, e)

        /** Logically invert this expression */
        def unary_! : Negate = Negate(this)

        /** Logical conjunction of this expression with another. */
        def && (e: Expression): And = And(this, e)
        /** Logical disjunction of this expression with another. */
        def || (e: Expression): Or = Or(this, e)

        /** Select a member from this expression. Equivalent JS: `expr.s` */
        def select(s: String): Select = Select(this, Symbol(s))
        /** Select a member from this expression. Equivalent JS: `expr.s` */
        def select(s: Symbol): Select = Select(this, s)

        /** Call a member function of the object represented by this expression. Equivalent JS: `expr.s(a1, a2, a3...)` */
        def call(s: String, as: Expression*): Apply = Apply(this / s, as)
        /** Call a member function of the object represented by this expression. Equivalent JS: `expr.s(a1, a2, a3...)` */
        def call(s: Symbol, as: Expression*): Apply = Apply(this / s, as)
    }

    trait ExpressionLowPriorityImplicits {
        /** Lift something which is `Numeric` into a `LiteralNumeric` */
        implicit def numericLiteral[A: Numeric](n: A): LiteralNumeric[A] = LiteralNumeric(n)
    }

    object Expression extends ExpressionLowPriorityImplicits {
        /** Lift arrays of things that can be converted `Expression` into an `ArrayExpression`. */
        implicit def arrayExpression[A](es: Seq[A])(implicit f: A => Expression): ArrayExpression =
            ArrayExpression(es.map(f))
        /** Lift sequences of key/value pairs where the value can be converted to `Expression` into an `ObjectExpression`. */
        implicit def objectExpression[A](fs: Seq[(String, A)])(implicit f: A => Expression): ObjectExpression =
            ObjectExpression(fs.map { case (k, v) => (k, f(v)) })
        /** Lift a boolean into a `LiteralBoolean`. */
        implicit def booleanLiteral(b: Boolean): LiteralBoolean = LiteralBoolean(b)
        /** Lift a JSON `JValue` into a `LiteralJSON` */
        implicit def jsonLiteral(jv: JValue): LiteralJSON = LiteralJSON(jv)
        /** Lift a `NodeSeq` to a `LiteralNodeSeq` */
        implicit def nodeSeqLiteral(ns: NodeSeq): LiteralNodeSeq = LiteralNodeSeq(ns)
        /** Lift a `String` into a `LiteralString`. */
        implicit def stringLiteral(s: String): LiteralString = LiteralString(s)
        /** Lift a `Symbol` into a `Reference`. */
        implicit def symbolReference(in: Symbol): Reference = Reference(in)

        /** Convert a Lift `JsonCall` into a `Reference` */
        implicit def jsonCallToReference(in: JsonCall): Reference = Reference(Symbol(in.funcId))

        /** Convert an `Expression` into a `JsExp` (the Lift equivalent) */
        implicit def exprToJsExp(in: Expression): JsExp = new JsExp { def toJsCmd = in.toJs }
        /** Convert an `Expression` into a `JsCmd` (the Lift equivalent of `Statement`) */
        implicit def exprToJsCmd(in: Expression): JsCmd = new JsCmd { def toJsCmd = in.toJs + ";" }
        /** Convert a sequence of `Expression`s into a `JsCmd` (the Lift equivalent of `Statement`) */
        implicit def exprsToJsCmd(in: Seq[Expression]): JsCmd = new JsCmd { def toJsCmd = in.map(_.toJs).mkString("", ";", ";") }
    }

    /** Trait of `Expression`s which can be the left-hand side of an assignment */
    trait Assignable extends Expression {
        /** Assign the expression a new value */
        def := (expression: Expression): Assignment = Assignment(this, expression)
    }

    /** An assignment expression */
    final case class Assignment(assignable: Assignable, expression: Expression) extends Expression {
        val toJs = "(" + assignable.toJs + "=" + expression.toJs + ")"
    }

    /** Trait of simple binary `Expression`s which encode to JS as `lhs op rhs` */
    trait BinaryExpression extends Expression {
        def lhs: Expression
        def rhs: Expression
        def operator: String
        val toJs = "(" + lhs.toJs + operator + rhs.toJs + ")"
    }

    final case class Equality(lhs: Expression, rhs: Expression) extends BinaryExpression { def operator = "==" }
    final case class Inequality(lhs: Expression, rhs: Expression) extends BinaryExpression { def operator = "!=" }
    final case class StrictEquality(lhs: Expression, rhs: Expression) extends BinaryExpression { def operator = "===" }
    final case class StrictInequality(lhs: Expression, rhs: Expression) extends BinaryExpression { def operator = "!==" }

    final case class LessThan(lhs: Expression, rhs: Expression) extends BinaryExpression { def operator = "<" }
    final case class LessThanEqual(lhs: Expression, rhs: Expression) extends BinaryExpression { def operator = "<=" }
    final case class GreaterThan(lhs: Expression, rhs: Expression) extends BinaryExpression { def operator = ">" }
    final case class GreaterThanEqual(lhs: Expression, rhs: Expression) extends BinaryExpression { def operator = ">=" }

    final case class Negate(expression: Expression) extends Expression { val toJs = "!(" + expression + ")" }

    final case class And(lhs: Expression, rhs: Expression) extends BinaryExpression { def operator = "&&" }
    final case class Or(lhs: Expression, rhs: Expression) extends BinaryExpression { def operator = "||" }

    /** A ternary conditional `Expression`, e.g. `p ? c : a` */
    final case class IfExpr(test: Expression)(consequence: Statement)(alternate: Statement) extends Expression {
        val toJs = "if(" + test.toJs + "){" + consequence.toJs + "}else{" + alternate.toJs + "}"
    }

    /** An array display expression. Equivalent JS: `[e1, e2, e3...]` */
    final case class ArrayExpression(elements: Seq[Expression]) extends Expression {
        lazy val toJs = elements.map(_.toJs).mkString("[", ",", "]")
    }

    /** An object display expression. Equivalent JS: `{k: e1, k2: e2, k3: e3...}` */
    final case class ObjectExpression(fields: Seq[(String, Expression)]) extends Expression {
        lazy val toJs = fields.map { case (k, v) => k.toJsCmd + ":" + v.toJs }.mkString("{", ",", "}")
    }

    /** A symbol reference expression (i.e. bareword referring to a var or function or builtin) */
    case class Reference(identifier: Symbol) extends Expression with Assignable { val toJs = identifier.name }

    /** A selection expression. Equivalent JS: `root.selection` */
    case class Select(root: Expression, selection: Symbol) extends Expression with Assignable {
        lazy val toJs = root.toJs + "." + selection.name
    }

    /** A function application expression. Equivalent JS: `func(a1, a2, a3...)` */
    final case class Apply(func: Expression, args: Seq[Expression]) extends Expression {
        lazy val toJs: String = func.toJs + args.map(_.toJs).mkString("(", ",", ")")
    }

    /** An anonymous function expression */
    final case class Lambda(parameters: Symbol*)(body: Statement) extends Expression {
        lazy val toJs = "function(" + parameters.map(_.name).mkString(",") + "){" + body.toJs + "}"
    }

    /** A literal boolean value */
    final case class LiteralBoolean(value: Boolean) extends Expression { val toJs = value.toString }
    /** A literal string value */
    final case class LiteralString(value: String) extends Expression { val toJs = value.toJsCmd }
    /** A literal JSON value */
    final case class LiteralJSON(value: JValue) extends Expression { val toJs = compact(render(value)) }
    /** A HTML fragment literal. Processed via Lift before being serialized to HTML and embedded as a string */
    final case class LiteralNodeSeq(value: NodeSeq) extends Expression { val toJs = JsNull.fixHtmlFunc("inline", value)(identity) }
    /** A literal numeric value */
    final case class LiteralNumeric[A: Numeric](value: A) extends Expression { val toJs = value.toString }
    /** A literal `null` */
    final case object Null extends Expression { val toJs = "null" }

    /** Coercer for Expression type. Used to trigger implicit conversions, such as literal value conversions. */
    def js(in: Expression): Expression =
        if (in == null) Null
        else in

    /** Construct an ArrayExpression */
    def array(exprs: Expression*): ArrayExpression = ArrayExpression(exprs)

    /** Construct an ObjectExpression */
    def obj(pairs: (String, Expression)*): ObjectExpression = ObjectExpression(pairs)


    /** Base type of JavaScript statements */
    trait Statement {
        /** Convert this expression to JavaScript */
        val toJs: String

        /** Sequence this statement with another. Equivalent JS: `stmt; stmt2;` */
        def & (c: Statement): Statements = Statements(Vector(this, c))
    }

    object Statement {
        /** Lift an `Expression` into a `Statement` */
        implicit def fromExpression(in: Expression): Statement = SimpleStatement(in)
        /** Convert a `Statement` into a `JsCmd` (the Lift equivalent) */
        implicit def statementToJsCmd(in: Statement): JsCmd = new JsCmd { def toJsCmd = in.toJs }
        /** Convert a sequence of `Statement`s into into a `JsCmd` (the Lift equivalent) */
        implicit def statementsToJsCmd(in: Seq[Statement]): JsCmd = new JsCmd { def toJsCmd = in.map(_.toJs).mkString("") }
    }

    /** Container for a sequence of statements */
    final case class Statements(statements: Vector[Statement]) extends Statement {
        /** Combine this group of statements with an additional `Statement` */
        override def & (c: Statement): Statements = Statements(statements :+ c)
        /** Combine this group of statements with additional `Statements` */
        def & (cs: Statements): Statements = Statements(statements ++ cs.statements)

        lazy val toJs = statements.map(_.toJsCmd).mkString("")
    }

    /** Simple `Statement` that consists of an `Expression`. */
    final case class SimpleStatement(expression: Expression) extends Statement {
        val toJs = expression.toJs + ";\n"
    }

    /** `return` `Statement` */
    final case class Return(expression: Expression) extends Statement {
        val toJs = "return " + expression.toJs + ";\n"
    }

    /** `var` `Statement` with optional initializer */
    final case class Var(name: Symbol, initial: Expression = Null) extends Statement {
        val toJs = "var " + name.name + "=" + initial.toJs + ";\n"
    }

    /** No-op `Statement` */
    final case object Nop extends Statement {
        val toJs = ""
    }

    /** An `if` `Statement` */
    final case class If(test: Expression)(consequence: Statement)(alternate: Statement) extends Statement {
        val toJs = "if(" + test.toJs + "){\n" + consequence.toJs + "}else{\n" + alternate.toJs + "}\n"
    }

    /**
     * `for` `Statement` that uses an initializer, boundary test, and increment.
     * This always generates a `var` for the initializer, so the initializer must be an assignment.
     * This is more limited than the `for` statement really is, to avoid the `var` special case.
     */
    final case class For(init: Assignment, test: Expression, incr: Expression)(body: Statement) extends Statement {
        val toJs = "for(var " + init.toJs + ";" + test.toJs + ";" + incr.toJs + "){\n" + body.toJs + "}\n"
    }

    /** `for` `Statement` that enumerates over some object */
    final case class Foreach(bind: String, collection: Expression)(body: Statement) extends Statement {
        val toJs = "for( " + bind + " in " + collection.toJs + "){\n" + body.toJs + "}\n"
    }

    /** A named function expression */
    final case class Function(name: Symbol)(parameters: Symbol*)(body: Statement) extends Statement {
        lazy val toJs = "function " + name.name + "(" + parameters.map(_.name).mkString(",") + "){\n" + body.toJs + "}\n"
    }


    /** Enrich `Expression`s with jQuery-specific shortcuts */
    implicit def jqExpression(in: Expression): JqExpression = new JqExpression {
        val underlying = in
    }

    /** jQuery-specific shortcuts */
    trait JqExpression {
        val underlying: Expression

        lazy val add            = underlying select 'add
        lazy val addBack        = underlying select 'addBack
        lazy val addClass       = underlying select 'addClass
        lazy val after          = underlying select 'after
        lazy val ajaxComplete   = underlying select 'ajaxComplete
        lazy val ajaxError      = underlying select 'ajaxError
        lazy val ajaxSend       = underlying select 'ajaxSend
        lazy val ajaxStart      = underlying select 'ajaxStart
        lazy val ajaxStop       = underlying select 'ajaxStop
        lazy val ajaxSuccess    = underlying select 'ajaxSuccess
        lazy val andSelf        = underlying select 'andSelf
        lazy val animate        = underlying select 'animate
        lazy val append         = underlying select 'append
        lazy val appendTo       = underlying select 'appendTo
        lazy val attr           = underlying select 'attr
        lazy val before         = underlying select 'before
        lazy val bind           = underlying select 'bind
        lazy val blur           = underlying select 'blur
        lazy val change         = underlying select 'change
        lazy val children       = underlying select 'children
        lazy val clearQueue     = underlying select 'clearQueue
        lazy val click          = underlying select 'click
        lazy val clone_         = underlying select 'clone
        lazy val closest        = underlying select 'closest
        lazy val contents       = underlying select 'contents
        lazy val css            = underlying select 'css
        lazy val data           = underlying select 'data
        lazy val dblclick       = underlying select 'dblclick
        lazy val delay          = underlying select 'delay
        lazy val delegate       = underlying select 'delegate
        lazy val dequeue        = underlying select 'dequeue
        lazy val detach         = underlying select 'detach
        lazy val die            = underlying select 'die
        lazy val each           = underlying select 'each
        lazy val empty          = underlying select 'empty
        lazy val end            = underlying select 'end
        lazy val eq             = underlying select 'eq
        lazy val error          = underlying select 'error
        lazy val fadeIn         = underlying select 'fadeIn
        lazy val fadeOut        = underlying select 'fadeOut
        lazy val fadeTo         = underlying select 'fadeTo
        lazy val fadeToggle     = underlying select 'fadeToggle
        lazy val filter         = underlying select 'filter
        lazy val find           = underlying select 'find
        lazy val finish         = underlying select 'finish
        lazy val first          = underlying select 'first
        lazy val focus          = underlying select 'focus
        lazy val focusin        = underlying select 'focusin
        lazy val focusout       = underlying select 'focusout
        lazy val get            = underlying select 'get
        lazy val has            = underlying select 'has
        lazy val hasClass       = underlying select 'hasClass
        lazy val height         = underlying select 'height
        lazy val hide           = underlying select 'hide
        lazy val hover          = underlying select 'hover
        lazy val html           = underlying select 'html
        lazy val index          = underlying select 'index
        lazy val innerHeight    = underlying select 'innerHeight
        lazy val innerWidth     = underlying select 'innerWidth
        lazy val insertAfter    = underlying select 'insertAfter
        lazy val insertBefore   = underlying select 'insertBefore
        lazy val is             = underlying select 'is
        lazy val keydown        = underlying select 'keydown
        lazy val keypress       = underlying select 'keypress
        lazy val keyup          = underlying select 'keyup
        lazy val last           = underlying select 'last
        lazy val length         = underlying select 'length
        lazy val live           = underlying select 'live
        lazy val load           = underlying select 'load
        lazy val map            = underlying select 'map
        lazy val mousedown      = underlying select 'mousedown
        lazy val mouseenter     = underlying select 'mouseenter
        lazy val mouseleave     = underlying select 'mouseleave
        lazy val mousemove      = underlying select 'mousemove
        lazy val mouseout       = underlying select 'mouseout
        lazy val mouseover      = underlying select 'mouseover
        lazy val mouseup        = underlying select 'mouseup
        lazy val next           = underlying select 'next
        lazy val nextAll        = underlying select 'nextAll
        lazy val nextUntil      = underlying select 'nextUntil
        lazy val not            = underlying select 'not
        lazy val off            = underlying select 'off
        lazy val offset         = underlying select 'offset
        lazy val offsetParent   = underlying select 'offsetParent
        lazy val on             = underlying select 'on
        lazy val one            = underlying select 'one
        lazy val outerHeight    = underlying select 'outerHeight
        lazy val outerWidth     = underlying select 'outerWidth
        lazy val parent         = underlying select 'parent
        lazy val parents        = underlying select 'parents
        lazy val parentsUntil   = underlying select 'parentsUntil
        lazy val position       = underlying select 'position
        lazy val prepend        = underlying select 'prepend
        lazy val prependTo      = underlying select 'prependTo
        lazy val prev           = underlying select 'prev
        lazy val prevAll        = underlying select 'prevAll
        lazy val prevUntil      = underlying select 'prevUntil
        lazy val promise        = underlying select 'promise
        lazy val prop           = underlying select 'prop
        lazy val pushStack      = underlying select 'pushStack
        lazy val queue          = underlying select 'queue
        lazy val ready          = underlying select 'ready
        lazy val remove         = underlying select 'remove
        lazy val removeAttr     = underlying select 'removeAttr
        lazy val removeClass    = underlying select 'removeClass
        lazy val removeData     = underlying select 'removeData
        lazy val removeProp     = underlying select 'removeProp
        lazy val replaceAll     = underlying select 'replaceAll
        lazy val replaceWith    = underlying select 'replaceWith
        lazy val resize         = underlying select 'resize
        lazy val scroll         = underlying select 'scroll
        lazy val scrollLeft     = underlying select 'scrollLeft
        lazy val scrollTop      = underlying select 'scrollTop
        lazy val select         = underlying select 'select
        lazy val serialize      = underlying select 'serialize
        lazy val serializeArray = underlying select 'serializeArray
        lazy val show           = underlying select 'show
        lazy val siblings       = underlying select 'siblings
        lazy val size           = underlying select 'size
        lazy val slice          = underlying select 'slice
        lazy val slideDown      = underlying select 'slideDown
        lazy val slideToggle    = underlying select 'slideToggle
        lazy val slideUp        = underlying select 'slideUp
        lazy val stop           = underlying select 'stop
        lazy val submit         = underlying select 'submit
        lazy val text           = underlying select 'text
        lazy val toArray        = underlying select 'toArray
        lazy val toggleClass    = underlying select 'toggleClass
        lazy val trigger        = underlying select 'trigger
        lazy val triggerHandler = underlying select 'triggerHandler
        lazy val unbind         = underlying select 'unbind
        lazy val undelegate     = underlying select 'undelegate
        lazy val unload         = underlying select 'unload
        lazy val unwrap         = underlying select 'unwrap
        lazy val val_           = underlying select 'val
        lazy val width          = underlying select 'width
        lazy val wrap           = underlying select 'wrap
        lazy val wrapAll        = underlying select 'wrapAll
        lazy val wrapInner      = underlying select 'wrapInner
    }

    /** Reference to `jQuery` */
    object jQuery extends Reference('jQuery) {
        lazy val ajax          = select('ajax)
        lazy val ajaxPrefilter = select('ajaxPrefilter)
        lazy val ajaxSetup     = select('ajaxSetup)
        lazy val ajaxTransport = select('ajaxTransport)
        lazy val browser       = select('browser)
        lazy val Callbacks     = select('Callbacks)
        lazy val contains      = select('contains)
        lazy val cssHooks      = select('cssHooks)
        lazy val Deferred      = select('Deferred)
        lazy val dequeue       = select('dequeue)
        lazy val each          = select('each)
        lazy val error         = select('error)
        lazy val extend        = select('extend)
        lazy val fx            = select('fx)
        lazy val get           = select('get)
        lazy val getJSON       = select('getJSON)
        lazy val getScript     = select('getScript)
        lazy val globalEval    = select('globalEval)
        lazy val grep          = select('grep)
        lazy val hasData       = select('hasData)
        lazy val holdReady     = select('holdReady)
        lazy val inArray       = select('inArray)
        lazy val isArray       = select('isArray)
        lazy val isEmptyObject = select('isEmptyObject)
        lazy val isFunction    = select('isFunction)
        lazy val isNumeric     = select('isNumeric)
        lazy val isPlainObject = select('isPlainObject)
        lazy val isWindow      = select('isWindow)
        lazy val isXMLDoc      = select('isXMLDoc)
        lazy val makeArray     = select('makeArray)
        lazy val map           = select('map)
        lazy val merge         = select('merge)
        lazy val noConflict    = select('noConflict)
        lazy val noop          = select('noop)
        lazy val now           = select('now)
        lazy val param         = select('param)
        lazy val parseHTML     = select('parseHTML)
        lazy val parseJSON     = select('parseJSON)
        lazy val parseXML      = select('parseXML)
        lazy val post          = select('post)
        lazy val proxy         = select('proxy)
        lazy val queue         = select('queue)
        lazy val removeData    = select('removeData)
        lazy val sub           = select('sub)
        lazy val trim          = select('trim)
        lazy val unique        = select('unique)
        lazy val when          = select('when)
    }

    /** Shorthand for `jQuery("#" + s)` */
    def jQueryId(s: String) = jQuery("#" + s)
}