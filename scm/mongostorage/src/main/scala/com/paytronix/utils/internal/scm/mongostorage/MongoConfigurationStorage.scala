//
// Copyright 2012-2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scm.mongostorage

import java.util.{Timer, TimerTask}
import java.util.regex.Pattern
import scala.annotation.tailrec

import com.mongodb.{DBObject, MongoException, MongoOptions, ReadPreference, ServerAddress, WriteConcern}
import com.mongodb.Bytes.{QUERYOPTION_AWAITDATA, QUERYOPTION_TAILABLE}
import com.mongodb.casbah.{MongoConnection, MongoDB}
import com.mongodb.casbah.commons.{MongoDBList, MongoDBObject}
import com.mongodb.casbah.commons.Implicits.{wrapDBObj, unwrapDBObj}
import com.mongodb.casbah.commons.conversions.scala.RegisterJodaTimeConversionHelpers
import com.mongodb.casbah.query.Imports.{$or, $unset, mongoQueryStatements, mongoNestedDBObjectQueryStatements}
import net.liftweb.json.JsonAST.{JObject, JValue, render}
import net.liftweb.json.Printer.compact
import org.bson.types.{BSONTimestamp, ObjectId}
import org.joda.time.DateTime
import org.slf4j.LoggerFactory

import com.paytronix.utils.internal.scm.common.{
    AspectFilter, AspectName, ConfigurationEngine, ContextFilter, Filter, KeyPath, Node, NodeContents, NodeFilter, Unfiltered
}
import com.paytronix.utils.internal.scm.common.context.{Path, QualifiedSegment, UnqualifiedSegment}
import com.paytronix.utils.internal.scm.engine.ConfigurationStorage
import com.paytronix.utils.interchange.{Coding, ExplicitUnionCoding, MongoUtils}
import com.paytronix.utils.scala.log.resultLoggerOps
import com.paytronix.utils.scala.result.{Failed, Okay, Result, iterableResultOps, optionOps, tryCatch}

/*
{ "ts" : { "t" : 1352690978000, "i" : 1 },
  "h" : NumberLong(0),
  "op" : "n",
  "ns" : "",
  "o" : { "msg" : "initiating set" } }
{ "ts" : { "t" : 1352691014000, "i" : 1 },
  "h" : NumberLong("6532261965744570385"),
  "op" : "n",
  "ns" : "",
  "o" : { "msg" : "Reconfig set", "version" : 2 } }
{ "ts" : { "t" : 1352691015000, "i" : 1 },
  "h" : NumberLong("-601414404918963212"),
  "op" : "n",
  "ns" : "",
  "o" : { "msg" : "Reconfig set", "version" : 3 } }


PRIMARY> db.foobar.insert({"foo":"bar"})
PRIMARY> db.foobar.find()
{ "_id" : ObjectId("50a06dae1121b84215236b8d"), "foo" : "bar" }

{ "ts" : { "t" : 1352691118000, "i" : 1 },
  "h" : NumberLong("-4652036888196607059"),
  "op" : "i",
  "ns" : "test.foobar",
  "o" : { "_id" : ObjectId("50a06dae1121b84215236b8d"), "foo" : "bar" } }


PRIMARY> db.foobar.update({"_id" : ObjectId("50a06dae1121b84215236b8d")}, {"_id" : ObjectId("50a06dae1121b84215236b8d"), "foo": "bar", "biz": "baz"})

{ "ts" : { "t" : 1352691174000, "i" : 1 },
  "h" : NumberLong("-4930451014729848312"),
  "op" : "u",
  "ns" : "test.foobar",
  "o2" : { "_id" : ObjectId("50a06dae1121b84215236b8d") },
  "o" : { "_id" : ObjectId("50a06dae1121b84215236b8d"), "foo" : "bar", "biz" : "baz" } }


PRIMARY> db.foobar.remove({"foo": "bar"})

{ "ts" : { "t" : 1352691189000, "i" : 1 },
  "h" : NumberLong("2230588797083296681"),
  "op" : "d",
  "ns" : "test.foobar",
  "b" : true,  // "replJustOnce"
  "o" : { "_id" : ObjectId("50a06dae1121b84215236b8d") } }
*/

sealed abstract class OplogEntry {
    //val ts: OplogTimestamp // need to add coding for BSONTimestamp to get this
    //val h: Long
    //val v: Int
    val ns: String
}

object OplogEntry {
    val coding = Coding.forClass[OplogEntry]
}

object OplogEntryCoding extends ExplicitUnionCoding[OplogEntry] {
    override val determinantField = "op"
    alternative[OplogInsert]("i")
    alternative[OplogUpdate]("u")
    alternative[OplogDelete]("d")
    alternative[OplogNoop]("n")
    alternative[OplogDbCmd]("c")
}

final case class OplogInsert(ns: String/*, h: Long, v: Int*/, o: DBObject) extends OplogEntry
final case class OplogUpdate(ns: String/*, h: Long, v: Int*/, o: DBObject, o2: DBObject) extends OplogEntry
final case class OplogDelete(ns: String/*, h: Long, v: Int*/, o: DBObject) extends OplogEntry
final case class OplogDbCmd(ns: String/*, h: Long, v: Int*/) extends OplogEntry
final case class OplogNoop(ns: String/*, h: Long, v: Int*/) extends OplogEntry

object MongoConfigurationStorageUtils {
    /**
     * Parse a specification string with one or more Mongo server addresses separated by pipes and possibly with explicit port numbers given with a colon.
     *
     * Example string: mongohost1|mongohost2:12345
     */
    def parseAddressesFromString(spec: String, defaultPort: Option[Int] = None): Result[Seq[ServerAddress]] =
        spec.split('|').map(_.trim).toSeq.mapResult { s =>
            s.split(':') match {
                case Array(host, port)                    => Okay(new ServerAddress(host, Integer.parseInt(port)))
                case Array(host) if defaultPort.isDefined => Okay(new ServerAddress(host, defaultPort.getOrElse(sys.error("default port was defined and now it's not"))))
                case Array(host)                          => Okay(new ServerAddress(host))
                case other => Failed("expected " + s + " to be either host or host:port, not " + other)
            }
        }
}

abstract class MongoConfigurationStorageImpl(serverAddresses: Seq[ServerAddress], database: String) extends ConfigurationStorage {
    protected implicit val logger = LoggerFactory.getLogger(getClass)
    protected def locateConfigurationEngine: Result[ConfigurationEngine]

    val nodesCollectionName                = "configuration.nodes"
    val recentlyDeletedNodesCollectionName = "configuration.recentlyDeletedNodes"
    val auditLogCollectionName             = "configuration.auditLog"
    val oplogCollectionName                = "oplog.rs"

    RegisterJodaTimeConversionHelpers()

    val connection = MongoConnection(serverAddresses.toList)
    val configurationDatabase = connection(database)
    val localDatabase = connection("local")

    val nodesCollectionRO = configurationDatabase(nodesCollectionName)
    nodesCollectionRO.readPreference = ReadPreference.SECONDARY

    val nodesCollection = configurationDatabase(nodesCollectionName)
    nodesCollection.ensureIndex(MongoDBObject("context" -> 1, "aspect" -> 1))
    nodesCollection.ensureIndex(MongoDBObject("aspect" -> 1))
    nodesCollection.writeConcern = WriteConcern.REPLICAS_SAFE

    val recentlyDeletedNodesCollection = configurationDatabase(recentlyDeletedNodesCollectionName)
    recentlyDeletedNodesCollection.writeConcern = WriteConcern.REPLICAS_SAFE
    recentlyDeletedNodesCollection.ensureIndex(MongoDBObject("ts" -> 1))

    val auditLogCollection = configurationDatabase(auditLogCollectionName)
    auditLogCollection.writeConcern = WriteConcern.SAFE

    val oplogCollection = localDatabase(oplogCollectionName)
    oplogCollection.readPreference = ReadPreference.SECONDARY

    val oplogReader = new OplogReader
    val oplogReaderThread = new Thread(oplogReader, "Config oplog reader")

    val recentlyDeletedNodesReaper = new RecentlyDeletedNodesReaper
    val recentlyDeletedNodesReaperTimer = new Timer("Config recently deleted nodes reaper")

    def startWatching() = {
        recentlyDeletedNodesReaperTimer.scheduleAtFixedRate(recentlyDeletedNodesReaper, 1000L * 60L * 60L, 1000L * 60L * 60L)
        oplogReaderThread.start()
    }

    def stopWatching() = {
        oplogReader.continue = false
        oplogReaderThread.interrupt()
        recentlyDeletedNodesReaperTimer.cancel()
    }

    final class OplogReader extends Runnable {
        val expectedNS = configurationDatabase.getName + "." + nodesCollectionName
        val oplogCoding =
            OplogEntry.coding.orElse("OplogEntry coding cannot be generated, oplog reading cannot be performed").orThrow
        var continue = true
        var lastSeen: Option[BSONTimestamp] =
            oplogCollection.find().sort(MongoDBObject("$natural" -> -1)).limit(1).toSeq.headOption map { _.get("ts").asInstanceOf[BSONTimestamp] }

        def query: MongoDBObject =
            lastSeen match {
                case Some(found) => MongoDBObject("ts" -> MongoDBObject("$gt" -> found))
                case _           => MongoDBObject()
            }

        // now invalidate that we've gotten a timestamp
        locateConfigurationEngine.foreach(_.invalidateAll()) // don't care about the result, on first-time startup this will almost always happen

        logger.info("Oplog reader started: " + query)

        @tailrec
        def run() = {
            if (!continue) ()
            else {
                try {
                    val curs = oplogCollection.find(query).sort(MongoDBObject("$natural" -> 1))
                    curs.option = QUERYOPTION_AWAITDATA
                    curs.option = QUERYOPTION_TAILABLE

                    logger.debug("Waiting for oplog updates")
                    try {
                        while (true) {
                            curs.foreach { dbo =>
                                try {
                                    lastSeen = Some(dbo.get("ts").asInstanceOf[BSONTimestamp])
                                    processOplogEntry(dbo)
                                } catch { case e: Exception =>
                                    logger.warn("Exception while processing oplog entry: " + dbo, e)
                                }
                            }

                            try Thread.sleep(100L) catch { case _: InterruptedException => () }
                        }
                    } catch { case dead: MongoException.CursorNotFound => () }
                    logger.debug("Requerying as cursor has timed out")
                } catch {
                    case ie: InterruptedException => ()
                    case e: Exception =>
                        logger.warn("Exception received while processing oplog:", e)
                }
                run()
            }
        }

        private def processOplogEntry(dbo: DBObject): Unit = {
            val entry = oplogCoding.decodeMongoDB(dbo).orElse("Failed to parse oplog entry").orThrow
            // Really noisy
            //if (logger.isDebugEnabled)
            //    logger.debug("Recognized oplog update received: " + entry)
            entry match {
                case OplogInsert(`expectedNS`, o) =>
                    val node = makeNode(o)
                    locateConfigurationEngine.map(_.invalidate(node :: Nil))
                        .logWarn("Failed to notify configuration service of inserted node " + node)

                case OplogUpdate(`expectedNS`, _, o) =>
                    nodesCollectionRO.find(o).limit(1).toSeq.headOption.foreach { dbo =>
                        val node = makeNode(dbo)
                        locateConfigurationEngine.map(_.invalidate(node :: Nil))
                            .logWarn("Failed to notify configuration service of updated node " + node)
                    }

                case OplogDelete(`expectedNS`, o) =>
                    val _id = o.get("_id").asInstanceOf[ObjectId]
                    recentlyDeletedNodesCollection.findOne(MongoDBObject("_id" -> _id)).toList match {
                        case Nil =>
                            logger.warn("Node " + _id + " was deleted but is not present in " + recentlyDeletedNodesCollectionName + " so entire cache has to be invalidated since affected node is not known")
                            locateConfigurationEngine.map(_.invalidateAll)
                                .logWarn("Failed to notify configuration service of unresolved node deletion")
                        case many =>
                            locateConfigurationEngine.map(_.invalidate(many.map(makeNode).toSeq))
                                .logWarn("Failed to notify configuration service of deleted node(s) " + many)
                    }

                case _ =>
                    ()
            }
        }
    }

    final class RecentlyDeletedNodesReaper extends TimerTask {
        def run() = {
            logger.debug("Reaping recently deleted nodes")
            val threshold = new DateTime().hourOfDay.addToCopy(-4)
            recentlyDeletedNodesCollection.remove(MongoDBObject("ts" -> MongoDBObject("$lt" -> threshold)), WriteConcern.NORMAL)
        }
    }

    private def query(filter: Filter): DBObject = {
        if (!filter.inclusive)
            sys.error("exclusive filters not yet supported")

        def contextFilter(path: Path, ancestors: Boolean, descendants: Boolean, self: Boolean): DBObject = {
            val ancestorsAndSelfQ: Option[DBObject] =
                if (ancestors && self)
                    Some("context" $in path.andAncestors.map(_.toString))
                else if (self)
                    Some(MongoDBObject("context" -> path.toString))
                else
                    None

            val descendantsQ: Option[DBObject] =
                if (descendants)
                    Some(MongoDBObject("context" -> ("^" + Pattern.quote(path.toString) + "/").r))
                else
                    None

            List(ancestorsAndSelfQ, descendantsQ).flatten match {
                case Nil =>
                    MongoDBObject()
                case single :: Nil =>
                    single
                case many =>
                    $or(many)
            }
        }

        def aspectFilter(aspect: AspectName): DBObject =
            MongoDBObject("aspect" -> aspect)

        val result: DBObject = filter match {
            case ContextFilter(path, inclusive, ancestors, descendants, self) =>
                contextFilter(path, ancestors, descendants, self)
            case NodeFilter(path, aspect, inclusive, ancestors, descendants, self) =>
                contextFilter(path, ancestors, descendants, self) ++ aspectFilter(aspect)
            case AspectFilter(aspect, inclusive) =>
                aspectFilter(aspect)
            case Unfiltered(true) =>
                MongoDBObject()
            case Unfiltered(false) =>
                MongoDBObject("notafield" -> "notavalue")
        }

        if (logger.isDebugEnabled)
            logger.debug("Compiled " + filter + " to " + result)

        result
    }

    private def query(node: Node): DBObject =
        query(NodeFilter(node.context, node.aspect, inclusive=true, ancestors=false, descendants=false, self=true))
    private def query(nodes: Iterable[Node]): DBObject =
        $or(nodes.map { node =>
            query(NodeFilter(node.context, node.aspect, inclusive=true, ancestors=false, descendants=false, self=true))
        }.toSeq)
    private def query(aspect: AspectName): DBObject =
        query(AspectFilter(aspect, inclusive=true))
    private def query(path: Path): DBObject =
        query(ContextFilter(path, inclusive=true, ancestors=false, descendants=false, self=true))

    private def makePath(a: Any): Path =
        Path.fromString(a.asInstanceOf[String])
    private def makePath(p: Path): Any =
        p.toString
    private def makeNode(dbo: DBObject): Node =
        Node(makePath(dbo.get("context")), dbo.get("aspect").asInstanceOf[String])

    def enumerateNodesUsingFilter(filter: Filter): Result[Seq[Node]] =
        tryCatch.value {
            nodesCollectionRO.find(query(filter), MongoDBObject("context" -> 1, "aspect" -> 1)).map { dbo =>
                makeNode(dbo)
            }.toSeq
        }

    def fetchNode(node: Node): Result[Option[NodeContents]] =
        tryCatch.value {
            val q = query(node)
            if (logger.isDebugEnabled)
                logger.debug("fetchNode(" + node + "): " + nodesCollectionName + "(RO) find " + q + " limit 1")
            nodesCollectionRO.find(q).limit(1).toList.headOption.map { dbo =>
                MongoUtils.toJValue(dbo.get("contents")).asInstanceOf[JObject]
            }
        }

    def fetchNodes(nodes: Iterable[Node]): Result[Seq[Option[NodeContents]]] =
        tryCatch.value {
            val q = query(nodes)
            if (logger.isDebugEnabled)
                logger.debug("fetchNodes(" + nodes.mkString(", ") + ": " + nodesCollectionName + "(RO) find " + q)
            val fetched = Map(nodesCollectionRO.find(q).map { dbo =>
                makeNode(dbo) -> MongoUtils.toJValue(dbo.get("contents")).asInstanceOf[JObject]
            }.toSeq: _*)
            nodes.map(fetched.get).toSeq
        }

    def updateNode(node: Node, upserts: Seq[(KeyPath, JValue)], deletes: Seq[KeyPath], auditInfo: NodeContents): Result[NodeContents] =
        tryCatch.result {
            def kp(in: KeyPath): String = in.mkString(".")

            val sets = MongoDBObject (
                "$set" -> (
                    MongoDBObject(upserts.map { case (k, v) => kp("contents" :: k) -> MongoUtils.fromJValue(v) }: _*) ++
                    ("lastUpdated" -> new DateTime())
                )
            )
            val unsets: DBObject =
                if (deletes.isEmpty) MongoDBObject()
                else $unset(deletes.flatMap { k =>
                    if (upserts.exists(_._1 == k)) None
                    else Some(kp("contents" :: k))
                }: _*)

            val q = query(node)
            val update = sets ++ unsets

            if (logger.isDebugEnabled)
                logger.debug (
                    "updateNode(" + node +
                    ", " + upserts.map { case (k, v) => k.mkString("/") + " -> " + /*compact(render(*/v/*))*/ }.mkString("{", ", ", "}") +
                    ", " + deletes.map(_.mkString("/")).mkString("[", ", ", "]") +
                    ", " + compact(render(auditInfo)) +
                    "): findAndModify(" + q +
                    ", {}, {}, remove=false, update=" + update +
                    ", returnNew=false, upsert=true)"
                )

            val preImage = nodesCollection.findAndModify (
                query=q,
                fields=MongoDBObject(),
                sort=MongoDBObject(),
                remove=false,
                update=update,
                returnNew=false,
                upsert=true
            )

            if (logger.isDebugEnabled)
                logger.debug("updateNode(...): find(" + q + ") limit 1")

            val postImage = nodesCollection.find(q).limit(1).toSeq.headOption

            val before = preImage.map(_.get("contents")).orNull
            val after = postImage.map(_.get("contents")).orNull

            if (before != after)
                auditLogCollection.insert(MongoDBObject (
                    "action" -> (if (before != null) "update" else "insert"),
                    "ts" -> new DateTime(),
                    "context" -> makePath(node.context),
                    "aspect" -> node.aspect,
                    "before" -> before,
                    "after" -> after,
                    "info" -> MongoUtils.fromJValue(auditInfo)
                ))

            postImage.map(_.get("contents")).toResult.orElse("node disappeared after update").map(MongoUtils.toJValue).asA[JObject]
        }

    def replaceNode(node: Node, newContents: NodeContents, auditInfo: NodeContents): Result[Unit] =
        tryCatch.value {
            val q = query(node)
            val dbo = MongoUtils.fromJValue(newContents)
            val update = MongoDBObject (
                "$set" -> MongoDBObject (
                    "contents" -> MongoUtils.fromJValue(newContents),
                    "lastUpdated" -> new DateTime()
                )
            )

            if (logger.isDebugEnabled)
                logger.debug(
                    "replaceNode(" + node + ", " + compact(render(newContents)) + ", " + compact(render(auditInfo)) + "): findAndModify(" +
                    q + ", {}, {}, remove=false, " + update + ", returnNew=false, upsert=true)"
                )

            val preImage = nodesCollection.findAndModify (
                query=q,
                fields=MongoDBObject(),
                sort=MongoDBObject(),
                remove=false,
                update=update,
                returnNew=false,
                upsert=true
            )

            val before = preImage.map(_.get("contents")).orNull

            if (before != dbo)
                auditLogCollection.insert(MongoDBObject (
                    "action" -> (if (before != null) "update" else "insert"),
                    "ts" -> new DateTime(),
                    "context" -> makePath(node.context),
                    "aspect" -> node.aspect,
                    "before" -> before,
                    "after" -> dbo,
                    "info" -> MongoUtils.fromJValue(auditInfo)
                ))
        }

    def deleteNode(node: Node, auditInfo: NodeContents): Result[Unit] =
        tryCatch.value {
            val q = query(node)

            if (logger.isDebugEnabled)
                logger.debug("deleteNode(" + node + ", " + compact(render(auditInfo)) + "): findAndRemove(" + q + ")")

            nodesCollection.findAndRemove(q).foreach { removedNode =>
                val deleteLog = removedNode.filter { case ("_id"|"context"|"aspect", _) => true; case _ => false } ++ ("ts" -> new DateTime())
                recentlyDeletedNodesCollection.insert(deleteLog)

                auditLogCollection.insert(MongoDBObject (
                    "action" -> "delete",
                    "ts" -> new DateTime(),
                    "context" -> makePath(node.context),
                    "aspect" -> node.aspect,
                    "before" -> removedNode.get("contents"),
                    "after" -> null,
                    "info" -> MongoUtils.fromJValue(auditInfo)
                ))
            }
        }
}
