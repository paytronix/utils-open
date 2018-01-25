//
// Copyright 2009-2014 Paytronix Systems, Inc.
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

import org.slf4j.{Logger, LoggerFactory}

import result.{Okay, Result}

/** Helper functions for resource management */
package object resource {
    /** Typeclass of objects that are closeable, e.g. SQL Connections */
    trait Closeable[-A] { def close(a: A): Unit }

    object Closeable {
        implicit def CloseableCloseable[A <: java.io.Closeable] = new Closeable[A] {
            def close(a: A) = a.close()
        }
        implicit val SQLConnectionCloseable = new Closeable[java.sql.Connection] {
            def close(conn: java.sql.Connection) = conn.close()
        }
        implicit val SQLStatementCloseable = new Closeable[java.sql.Statement] {
            def close(statement: java.sql.Statement) = statement.close()
        }
        implicit val ResultSetCloseable = new Closeable[java.sql.ResultSet] {
            def close(rs: java.sql.ResultSet) = rs.close()
        }
        implicit val SourceCloseable = new Closeable[scala.io.Source] {
            def close(s: scala.io.Source) = s.close()
        }
    }

    /** Close some closeable resource, possibly throwing any kind of hideous exception */
    def close[A: Closeable](a: A): Unit = implicitly[Closeable[A]].close(a)

    /** Wrap a Closeable with a for-comprehension compatible object which closes the resource upon exit from the enclosed expressions */
    def openedResource[A: Closeable](closeable: A): OpenedResource[A] =
        OpenedResource(closeable)

    final case class OpenedResource[A: Closeable](closeable: A) {
        def map[B](f: A => B): Result[B] =
            try Okay(f(closeable)) finally {
                try close(closeable) catch { case e: Exception =>
                    LoggerFactory.getLogger(getClass).warn("Failed to close acquired resource [" + String.valueOf(closeable) + "] due to exception (ignoring):", e)
                }
            }

        def flatMap[B](f: A => B): B =
            try f(closeable) finally {
                try close(closeable) catch { case e: Exception =>
                    LoggerFactory.getLogger(getClass).warn("Failed to close acquired resource [" + String.valueOf(closeable) + "] due to exception (ignoring):", e)
                }
            }
    }

    /**
     * Acquire a resource, execute a block against the acquired resource, and then release it (via close) after the block exits, even if an exception is thrown.
     *
     * Example usage:
     *     withResource(dataSource.getConnection) { conn =>
     *         ... use conn ...
     *     }
     *
     * Is equivalent to:
     *     val conn = dataSource.getConnection
     *     try {
     *         ... use conn ...
     *     } finally {
     *         conn.close
     *     }
     */
    def withResource[A: Closeable, U](acquire1: => A)(body: A => U): U = {
        val resource1 = acquire1
        try body(resource1) finally {
            try close(resource1) catch { case e: Exception =>
                LoggerFactory.getLogger(getClass).warn("Failed to close acquired resource [" + String.valueOf(resource1) + "] due to exception (ignoring):", e)
            }
        }
    }

}

