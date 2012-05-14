//
// Copyright 2010-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

import java.sql.{CallableStatement, ResultSet, Statement}

import com.paytronix.utils.scala.resource.withResource

/** Helpers for using JDBC in a more Scala-friendly fashion */
trait JDBCHelpers {
    /** Type alias to indicate that a ResultSet should be used only for the current row (e.g. do not call next) */
    type RowResult = ResultSet

    /**
     * Convert a result set into a lazy stream by applying a function on the result set for each value, usually to create some kind of row object
     *
     * @param f the function to apply to the ResultSet to create values in the stream
     * @param rs the resultset
     * @return a stream of results of applying f to the result set
     */
    def streamResultSet[A](rs: ResultSet)(f: RowResult => A): Stream[A] =
        if (rs.next()) { Stream.cons(f(rs), streamResultSet(rs)(f)) } else Stream.empty

    /** Process result set from a stored procedure call expecting to have just one */
    def withCallResultSet[A](cs: CallableStatement)(f: ResultSet => A): A = {
        cs.execute
        withResultSet(cs)(f)
    }

    /** Apply the functions to the result sets of the statement, in order. Update results are skipped. */
    def withResultSets(s: Statement, fs: List[ResultSet => Unit]): Unit =
        fs match {
            case f :: rest if s.getUpdateCount == -1 => {
                f(s.getResultSet)
                (rest, s.getMoreResults) match {
                    case (Nil, true) => sys.error("expected no more result sets")
                    case (_,   true) => withResultSets(s, rest)
                    case (Nil, false) => {}
                    case (_,   false) => sys.error("expected " + rest.length + " more result sets")
                }
            }

            case _ => if (s.getMoreResults) withResultSets(s, fs)
        }

    /** Process a single result set from the statement with a function */
    def withResultSet[A](s: Statement)(f: ResultSet => A): A = {
        var a: Option[A] = None
        withResultSets(s, List[ResultSet => Unit] (
            { rs => a = Some(f(rs)) }
        ))
        a.get
    }

    /** Process two result sets from the statement with separate functions */
    def with2ResultSets[A, B](s: Statement)(fa: ResultSet => A, fb: ResultSet => B): (A, B) = {
        var a: Option[A] = None
        var b: Option[B] = None
        withResultSets(s, List[ResultSet => Unit] (
            { rs => a = Some(fa(rs)) },
            { rs => b = Some(fb(rs)) }
        ))
        (a.get, b.get)
    }

    /** Process three result sets from the statement with separate functions */
    def with3ResultSets[A, B, C](s: Statement)(fa: ResultSet => A, fb: ResultSet => B, fc: ResultSet => C): (A, B, C) = {
        var a: Option[A] = None
        var b: Option[B] = None
        var c: Option[C] = None
        withResultSets(s, List[ResultSet => Unit] (
            { rs => a = Some(fa(rs)) },
            { rs => b = Some(fb(rs)) },
            { rs => c = Some(fc(rs)) }
        ))
        (a.get, b.get, c.get)
    }

    /** Process four result sets from the statement with separate functions */
    def with4ResultSets[A, B, C, D](s: Statement)(fa: ResultSet => A, fb: ResultSet => B, fc: ResultSet => C, fd: ResultSet => D): (A, B, C, D) = {
        var a: Option[A] = None
        var b: Option[B] = None
        var c: Option[C] = None
        var d: Option[D] = None
        withResultSets(s, List[ResultSet => Unit] (
            { rs => a = Some(fa(rs)) },
            { rs => b = Some(fb(rs)) },
            { rs => c = Some(fc(rs)) },
            { rs => d = Some(fd(rs)) }
        ))
        (a.get, b.get, c.get, d.get)
    }

    /** Process five result sets from the statement with separate functions */
    def with5ResultSets[A, B, C, D, E](s: Statement)(fa: ResultSet => A, fb: ResultSet => B, fc: ResultSet => C, fd: ResultSet => D, fe: ResultSet => E): (A, B, C, D, E) = {
        var a: Option[A] = None
        var b: Option[B] = None
        var c: Option[C] = None
        var d: Option[D] = None
        var e: Option[E] = None
        withResultSets(s, List[ResultSet => Unit] (
            { rs => a = Some(fa(rs)) },
            { rs => b = Some(fb(rs)) },
            { rs => c = Some(fc(rs)) },
            { rs => d = Some(fd(rs)) },
            { rs => e = Some(fe(rs)) }
        ))
        (a.get, b.get, c.get, d.get, e.get)
    }

    /** Process six result sets from the statement with separate functions */
    def with6ResultSets[A, B, C, D, E, F](s: Statement)(fa: ResultSet => A, fb: ResultSet => B, fc: ResultSet => C, fd: ResultSet => D, fe: ResultSet => E, ff: ResultSet => F): (A, B, C, D, E, F) = {
        var a: Option[A] = None
        var b: Option[B] = None
        var c: Option[C] = None
        var d: Option[D] = None
        var e: Option[E] = None
        var f: Option[F] = None
        withResultSets(s, List[ResultSet => Unit] (
            { rs => a = Some(fa(rs)) },
            { rs => b = Some(fb(rs)) },
            { rs => c = Some(fc(rs)) },
            { rs => d = Some(fd(rs)) },
            { rs => e = Some(fe(rs)) },
            { rs => f = Some(ff(rs)) }
        ))
        (a.get, b.get, c.get, d.get, e.get, f.get)
    }

    /** Process seven result sets from the statement with separate functions */
    def with7ResultSets[A, B, C, D, E, F, G](s: Statement)(fa: ResultSet => A, fb: ResultSet => B, fc: ResultSet => C, fd: ResultSet => D, fe: ResultSet => E, ff: ResultSet => F, fg: ResultSet => G): (A, B, C, D, E, F, G) = {
        var a: Option[A] = None
        var b: Option[B] = None
        var c: Option[C] = None
        var d: Option[D] = None
        var e: Option[E] = None
        var f: Option[F] = None
        var g: Option[G] = None
        withResultSets(s, List[ResultSet => Unit] (
            { rs => a = Some(fa(rs)) },
            { rs => b = Some(fb(rs)) },
            { rs => c = Some(fc(rs)) },
            { rs => d = Some(fd(rs)) },
            { rs => e = Some(fe(rs)) },
            { rs => f = Some(ff(rs)) },
            { rs => g = Some(fg(rs)) }
        ))
        (a.get, b.get, c.get, d.get, e.get, f.get, g.get)
    }
}

/** Helpers for using JDBC in a more Scala-friendly fashion */
object jdbc extends JDBCHelpers
