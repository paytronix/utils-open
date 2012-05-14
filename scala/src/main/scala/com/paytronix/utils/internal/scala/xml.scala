//
// Copyright 2009-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

import java.io.{InputStream, InputStreamReader, PrintStream}
import java.nio.charset.Charset
import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.io.{Position, Source}
import scala.xml.{Atom, Elem, EntityRef, MetaData, Node, NodeSeq, NamespaceBinding, Null, PrefixedAttribute, Text, TopScope, UnprefixedAttribute}
import scala.xml.dtd.{DocType, PublicID}
import scala.xml.parsing.XhtmlParser
import scala.xml.transform.BasicTransformer
import net.liftweb.util.AltXML
import net.liftweb.util.BindHelpers.currentNode
import net.liftweb.util.Helpers.pairToUnprefixed
import com.paytronix.utils.scala.result.{Failed, Okay, Result, tryCatch}
import com.paytronix.utils.scala.io.readChars

/**
 * Helpers for XML manipulation
 */
object xml {
    /** Turn some XML `NodeSeq` into an XHTML text block */
    def xhtmlToString(ns: NodeSeq): String = {
        val sb = new StringBuilder()
        AltXML.sequenceToXML(ns, TopScope, sb, false, false, false)
        sb.toString
    }

    /** Turn some XML `NodeSeq` into an HTML text block with a `DOCTYPE` */
    def xhtmlToHtmlString(ns: NodeSeq): String = {
        val sb = new StringBuilder()
        sb.append(new DocType("html", new PublicID("-//W3C//DTD XHTML 1.0 Transitional//EN", "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"), Seq.empty))
        AltXML.sequenceToXML(ns, TopScope, sb, false, false, false)
        sb.toString
    }

    /** Turn a string containing an XHTML fragment into a `NodeSeq` */
    def stringToXhtmlFragment(s: String): ParseResult[NodeSeq] =
        stringToXhtml("<body>" + s.toString + "</body>").map(_.flatMap(_.child))

    /** Turn a string containing XHTML into XML while logging all the errors */
    def stringToXhtml(s: String): ParseResult[NodeSeq] =
        charsToXhtml(() => s.iterator)

    /** Turn a stream containing XHTML into XML while logging all the errors */
    def streamToXhtml(is: InputStream): ParseResult[NodeSeq] = {
        /*
         * I'm not sure if resetting is a common thing or not, but to properly reset you need to either reopen or seek
         * the input stream, neither of which is possible with InputStream directly. As such, to implement the complete
         * interface of Source, I just have it read the whole file into memory and then parse that. It's probably not
         * too bad since XHTML files are not terribly large on average.
         */
        val chars = readChars(new InputStreamReader(is, Charset.forName("UTF-8")))
        charsToXhtml(() => chars.iterator)
    }

    /** Turn a stream of characters representing XHTML into XML while logging all the errors */
    private def charsToXhtml(chars: () => Iterator[Char]): ParseResult[NodeSeq] = {
        var messages = new ListBuffer[ParseError]
        def mkSource: Source = {
            val characters = chars()
            new Source {
                override def report(pos: Int, msg: String, out: PrintStream): Unit =
                    messages += ParseError.LineAndColumn(Position.line(pos), Position.column(pos), msg)

                override def reset() = mkSource
                val iter = characters
            }
        }
        val source = mkSource

        tryCatch.value(XhtmlParser(source)) match {
            case _ if !messages.isEmpty => ParseFailed(messages)
            case Okay(xml) => ParseSucceeded(if (xml == null) NodeSeq.Empty else xml)
            case Failed(t) => ParseFailed(ParseError.Unlocated(t.toString) :: Nil)
        }
    }

    /** A filtering function which matches any prefixed attribute with a given prefix */
    def attrsWithPrefix(pre: String)(md: MetaData): Boolean = md match {
        case pa: PrefixedAttribute if pa.pre == pre => true
        case _ => false
    }

    /** Strip prefixes from prefixed attributes, leaving unprefixed attributes */
    def stripPrefixes(in: MetaData): MetaData = in.foldLeft[MetaData](Null) {
        (rest, md) => md match {
            case pa: PrefixedAttribute => new UnprefixedAttribute(pa.key, pa.value, rest)
            case upa: UnprefixedAttribute => new UnprefixedAttribute(upa.key, upa.value, rest)
            case Null => rest
            case x => throw new IllegalArgumentException("Not sure how to process " + x + " (of type " + x.getClass.getName + ")")
        }
    }

    /** Add an unprefixed attribute to any nodes in the given sequence with a certain label */
    def addUnprefixedAttributes(label: String, attrs: (String, NodeSeq)*): (NodeSeq) => NodeSeq = _.map {
        case e: Elem if e.label == label => e % attrs.foldLeft[MetaData](Null)((md, att) => new UnprefixedAttribute(att._1, att._2, md))
        case x => x
    }

    /** Convert a series of attribtues into a Seq of pairs */
    def attributesToPairs(in: MetaData): Seq[(String, String)] = in.foldRight[List[(String, String)]](Nil) {
        (md, rest) => md match {
            case upa: UnprefixedAttribute => Pair(upa.key, upa.value.toString) :: rest
            case pa: PrefixedAttribute => Pair(pa.key, pa.value.toString) :: rest
            case Null => rest
            case x => throw new IllegalArgumentException("Not sure how to process " + x + " (of type " + x.getClass.getName + ")")
        }
    }

    /** Transform all attributes of an Elem with the given prefix into a Seq of pairs */
    def prefixedElemAttributesToPairs(prefix: String)(elem: Elem): Seq[(String, String)] =
        attributesToPairs(elem.attributes.filter(attrsWithPrefix(prefix)))

    /** Transform all attributes of a element with the given prefix to unprefixed attributes with the same values, discarding any other attributes */
    def prefixedAttrsToUnprefixed(prefix: String)(attrs: MetaData): MetaData =
        stripPrefixes(attrs.filter(attrsWithPrefix(prefix)))

    /** Transform all unprefixed attributes of an element into prefixed ones with the given prefix */
    def unprefixedAttrsToPrefixed(prefix: String): MetaData => MetaData = {
        def iterate(tail: MetaData, md: MetaData): MetaData = md match {
            case Null => tail
            case upa: UnprefixedAttribute => iterate(new PrefixedAttribute(prefix, upa.key, upa.value, tail), md.next)
            case _ => iterate(tail, md.next)
        }
        iterate(Null, _)
    }

    /**
     * MetaData containing unprefixed versions of all attributes associated with the current binding node that have a given prefix.
     *
     * Usually used to bring along "style attributes" from a template like this:
     *
     *     &lt;snippet:control someattr="foo" input:size="30" /&gt;
     *
     * &rArr;
     *
     *     &lt;input ... size="30" /&gt;
     *
     * With code like:
     *
     * <pre>
     *     bind("snippet", ns, "control" -> SHtml.text(...) %% currentPrefixedAttrs("input"))
     * </pre>
     */
    def currentPrefixedAttrs(prefix: String): MetaData = currentNode.map(_.attributes).map(prefixedAttrsToUnprefixed(prefix)).openOr(Null)

    /** Implicitly convert an Elem to a ElemOps to enable additional functionality */
    implicit def elemOps(e: Elem): ElemOps = ElemOps(e)

    /** Extension on Elem that adds helpful behavior */
    case class ElemOps(e: Elem)
    {
        /** Generate a new Elem with the given additional metadata like Elem's % operator, but uses {@link XMLHelpers#combineMetaData} instead of {@link MetaData#update} */
        def %%(updates: MetaData): Elem = Elem(e.prefix, e.label, combineMetaData(e.attributes, updates, e.scope), e.scope, e.child: _*)
    }


    /** Like {@link MetaData#update} but instead of overwriting class and style attributes, the values are combined with an appropriate format for those attributes. Other attributes are overwrriten per usual. */
    def combineMetaData(md1: MetaData, md2: MetaData, scope: NamespaceBinding): MetaData = {
        def notClassNorStyle(md: MetaData) = md.key match { case ("class"|"style") => false; case _ => true }

        def classAttr(tail: MetaData) = (md1("class"), md2("class")) match {
            case (null, null) => tail
            case (a   , null) => new UnprefixedAttribute("class", Text(a.text), tail)
            case (null, b   ) => new UnprefixedAttribute("class", Text(b.text), tail)
            case (a   , b   ) => new UnprefixedAttribute("class", Text(a.text + " " + b.text), tail)
        }

        def styleAttr(tail: MetaData) = (md1("style"), md2("style")) match {
            case (null, null) => tail
            case (a   , null) => new UnprefixedAttribute("style", Text(a.text), tail)
            case (null, b   ) => new UnprefixedAttribute("style", Text(b.text), tail)
            case (a   , b   ) => new UnprefixedAttribute("style", Text(a.text + "; " + b.text), tail)
        }
        MetaData.update(md1.filter(notClassNorStyle), scope, classAttr(styleAttr(md2.filter(notClassNorStyle))))
    }

    /** Transformer that rewrites XHTML to be more appropriate for plain text, by removing images and rewriting links to be link text &lt;link target&gt; */
    object TextTransformer extends BasicTransformer {
        override def transform(n: Node) = n match {
            case e@(<a>{ img }</a>) if img.label == "img" => NodeSeq.Empty // FIXME? use alt text?
            case e@(<a>{ label }</a>) => Text(label.text + e.attribute("href").map(" <" + _.text + ">").getOrElse(""))
            case e if e.label == "img" => NodeSeq.Empty
            case n => super.transform(n)
        }
    }

    object ReadonlyTransformer extends BasicTransformer {
        override def transform(n: Node) = n match {
            case e: Elem =>
              val attrs = e.attributes.filter(_.key match {
                              case "onblur"|"onkeypress"|"onkeyup"|"onclick" => false
                              case _ => true
                           })
              Elem(e.prefix, e.label, attrs, e.scope, e.child: _* ) % ("disabled" -> "true")
            case other => other
        }
    }

    /** Strip all XML tags from the input NodeSeq, generating a string containing all remaining Text fragments */
    def stripXML(ns: NodeSeq): String =
        ns.flatMap(n => n.descendant.flatMap(n => n match {
            case e: EntityRef   => e.text :: Nil
            case a: Atom[_]     => a.text :: Nil
            case _              => Nil
            })).mkString("")

    /** Convert a NodeSeq into a string suitable for text by composing {@link TextTransformer} with {@link stripXML} */
    def xhtmlToText(ns: NodeSeq): String = stripXML(TextTransformer(<wrapper>{ ns }</wrapper>))
}
