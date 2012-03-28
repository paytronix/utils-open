//
// Copyright 2009-2012 Paytronix Systems, Inc.
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

/**
 * Helper functions for resource management
 */
object resource
{
    private val logger = LoggerFactory.getLogger(getClass)

    /** Type of objects that are closeable, e.g. SQL Connections */
    type Closeable = { def close(): Unit }

    /** Wrap a Closeable with a for-comprehension compatible object which closes the resource upon exit from the enclosed expressions */
    def openedResource[A <: Closeable](closeable: A): OpenedResource[A] =
        OpenedResource(closeable)

    final case class OpenedResource[A <: Closeable](closeable: A) {
        def map[B](f: A => B): Result[B] =
            try {
                Okay(f(closeable))
            } finally {
                try {
                    closeable.close
                } catch {
                    case e: Exception =>
                        logger.warn("Failed to close acquired resource [" + String.valueOf(closeable) + "] due to exception (ignoring):", e)
                }
            }

        def flatMap[B](f: A => B): B =
            try {
                f(closeable)
            } finally {
                try {
                    closeable.close
                } catch {
                    case e: Exception =>
                        logger.warn("Failed to close acquired resource [" + String.valueOf(closeable) + "] due to exception (ignoring):", e)
                }
            }
    }

    /**
     * Acquire a resource, execute a block against the acquired resource, and then release it (via close) after the block exits, even if an exception is thrown.
     *
     * Example usage:
     *     withResource(dataSource.getConnection) {
     *         (conn) =>
     *             ... use conn ...
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
    def withResource[T1 <: Closeable, U]
            (acquire1: => T1)
            (body: (T1) => U): U =
    {
        val resource1 = acquire1
        try {
            body(resource1)
        } finally {
            try {
                resource1.close
            } catch {
                case e: Exception => logger.warn("Failed to close acquired resource [" + String.valueOf(resource1) + "] due to exception (ignoring):", e)
            }
        }
    }

    /**
     * Compose withResource for acquiring a series (or chain) of resources in order, releasing any that have been acquired even if acquiring one fails.
     *
     * For example:
     *     withSeriesOfResources(dataSource.getConnection,
     *                           (c: Connection) => c.createStatement) {
     *         (conn, stmt) =>
     *             stmt.executeUpdate("UPDATE ...")
     *     }
     *
     * Is equivalent to:
     *     withResource(dataSource.getConnection) {
     *         (conn) =>
     *         withResource(conn.createStatement) {
     *             (stmt) =>
     *                 stmt.executeUpdate("UPDATE ...")
     *         }
     *     }
     * but more brief.
     */
    def withSeriesOfResources[T1 <: Closeable, T2 <: Closeable, U]
            (acquire1: => T1, acquire2: (T1) => T2)
            (body: (T1, T2) => U): U =
        withResource(acquire1)({ (resource1) =>
            withResource(acquire2(resource1))({ (resource2) =>
                body(resource1, resource2)
            })
        })

    /**
     * Compose withResource for acquiring a series (or chain) of resources in order, releasing any that have been acquired even if acquiring one fails.
     *
     * For example:
     *     withSeriesOfResources(dataSource.getConnection,
     *                           (c: Connection) => c.createStatement,
     *                           (s: Statement) => s.executeQuery("SELECT ...")) {
     *         (conn, stmt, resultSet) =>
     *             while (resultSet.next) { ... }
     *     }
     *
     * Is equivalent to:
     *     withResource(dataSource.getConnection) {
     *         (conn) =>
     *         withResource(conn.createStatement) {
     *             (stmt) =>
     *             withResource(stmt.executeQuery("SELECT ...")) {
     *                 (resultSet) =>
     *                     while (resultSet.next) { ... }
     *             }
     *         }
     *     }
     * but more brief.
     */
    def withSeriesOfResources[T1 <: Closeable, T2 <: Closeable, T3 <: Closeable, U]
            (acquire1: => T1, acquire2: (T1) => T2, acquire3: (T2) => T3)
            (body: (T1, T2, T3) => U): U =
        withResource(acquire1)({ (resource1) =>
            withResource(acquire2(resource1))({ (resource2) =>
                withResource(acquire3(resource2))({ (resource3) =>
                    body(resource1, resource2, resource3)
                })
            })
        })

    /**
     * Compose withResource for acquiring two separate resources, which do not depend on each other
     *
     * For example:
     *     withResources(transactionalDataSource.getConnection, reportingDataSource.getConnection) {
     *         (transactionalConn, reportingConn) =>
     *             ... use conns ...
     *     }
     *
     * Is equivalent to:
     *     withResource(transactionalDataSource.getConnection) {
     *         (transactionalConn) =>
     *         withResource(reportingDataSource.getConnection) {
     *             (reportingConn) =>
     *                 ... use conns ...
     *         }
     *     }
     * but more brief.
     */
    def withResources[T1 <: Closeable, T2 <: Closeable, U]
            (acquire1: => T1, acquire2: => T2)
            (body: (T1, T2) => U): U =
        withResource(acquire1)({ (resource1) =>
            withResource(acquire2)({ (resource2) =>
                body(resource1, resource2)
            })
        })

    /**
     * Compose withResource for acquiring two separate resources, which do not depend on each other
     *
     * For example:
     *     withResources(directoryDataSource.getConnection,
     *                   transactionalDataSource.getConnection,
     *                   reportingDataSource.getConnection) {
     *         (directoryConn, transactionalConn, reportingConn) =>
     *             ... use conns ...
     *     }
     *
     * Is equivalent to:
     *     withResource(directoryDataSource.getConnection) {
     *         (directoryConn) =>
     *             withResource(transactionalDataSource.getConnection) {
     *                 (transactionalConn) =>
     *                 withResource(reportingDataSource.getConnection) {
     *                     (reportingConn) =>
     *                         ... use conns ...
     *                 }
     *             }
     *         }
     *     }
     * but more brief.
     */
    def withResources[T1 <: Closeable, T2 <: Closeable, T3 <: Closeable, U]
            (acquire1: => T1, acquire2: => T2, acquire3: => T3)
            (body: (T1, T2, T3) => U): U =
        withResource(acquire1)({ (resource1) =>
            withResource(acquire2)({ (resource2) =>
                withResource(acquire3)({ (resource3) =>
                    body(resource1, resource2, resource3)
                })
            })
        })

}

