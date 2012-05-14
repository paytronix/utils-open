//
// Copyright 2010-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.scala

import scala.collection.immutable.TreeSet
import net.liftweb.json.JsonAST.{JString, JValue}
import org.apache.avro.Schema
import org.apache.avro.io.{Encoder, ResolvingDecoder}
import org.slf4j.{Logger, LoggerFactory}
import com.paytronix.utils.interchange.{OverrideCoding, StringCoder, StringSafeCoder}
import com.paytronix.utils.scala.result.{Failed, Okay, Result, parameter, tryCatch, tryCatching}

object SoftwareVersion {
    val pattern = "^([^.]+)\\.([^.]+?)(?:\\.([^.]+?))?(?:-\\S+)?(?:\\s+(\\S+))?$".r
    val logger = LoggerFactory.getLogger(classOf[SoftwareVersion])

    /** Extractor that extracts components from a SoftwareVersion rather than the original text */
    object Components {
        def unapply(in: SoftwareVersion): Option[(Result[Option[Int]], Result[Option[Int]], Result[Option[Int]], Result[Option[Int]])] =
            Some((in.major, in.minor, in.micro, in.revision))
    }

    /** Creates a SoftwareVersion from the given string and then asserts that at least the major and minor version are parsed */
    def parse(in: String): Result[SoftwareVersion] = SoftwareVersion(in) match {
        case ver@SoftwareVersion.Components(Okay(Some(_)), Okay(Some(_)), _, _) => Okay(ver)
        case _ => Failed("\"" + in + "\" not a valid software version")
    }
}

/** Wraps a Paytronix version string as stored in JAR manifests and parses it so that it can be compared */
case class SoftwareVersion(text: String) extends Ordered[SoftwareVersion] {
    import SoftwareVersion.logger

    val components@(major, minor, micro, revision): (Result[Option[Int]], Result[Option[Int]], Result[Option[Int]], Result[Option[Int]]) = {
        def asInt(in: String): Result[Option[Int]] =
            if (in == null) {
                Okay(None)
            } else {
                tryCatching[NumberFormatException].value(Some(Integer.parseInt(in))) | Failed("invalid integer " + in)
            }

        text match {
            case SoftwareVersion.pattern(maj, min, mic, rev) => (asInt(maj), asInt(min), asInt(mic), asInt(rev))
            case _ => {
                logger.warn("Failed to parse version \"" + text + "\"")
                (Okay(None), Okay(None), Okay(None), Okay(None))
            }
        }
    }

    override def equals(that: Any): Boolean = that match {
        case that: SoftwareVersion =>
            (major.toOption == that.major.toOption &&
             minor.toOption == that.minor.toOption &&
             micro.toOption == that.micro.toOption &&
             revision.toOption == that.revision.toOption)
        case _ => false
    }

    def compare(that: SoftwareVersion): Int = {
        def comp(aResult: Result[Option[Int]], bResult: Result[Option[Int]]): Option[Int] =
            for (a <- aResult.toOption.flatMap(x => x) orElse Some(0); b <- bResult.toOption.flatMap(x => x) orElse Some(0); if a != b) yield a - b

        (
            comp(major, that.major) orElse
            comp(minor, that.minor) orElse
            comp(micro, that.micro) orElse
            comp(revision, that.revision)
        ) getOrElse 0
    }

    override def toString = "SoftwareVersion(\"" + text + "\" /*" + major + ", " + minor + ", " + micro + ", " + revision + "*/)"
}

object SoftwareVersionCoding extends OverrideCoding(SoftwareVersionCoder)

/** Map a SoftwareVersion to a JString */
object SoftwareVersionCoder extends StringSafeCoder[SoftwareVersion] {
    val mostSpecificClass = classOf[SoftwareVersion]

    def decode(classLoader: ClassLoader, in: JValue) =
        StringCoder.decode(classLoader, in) flatMap { s => SoftwareVersion.parse(s) | parameter(Nil) }
    def encode(classLoader: ClassLoader, in: SoftwareVersion) =
        tryCatch.value(JString(in.text)) | parameter(Nil)

    def decodeString(classLoader: ClassLoader, in: String) =
        SoftwareVersion.parse(in) | parameter(Nil)
    def encodeString(classLoader: ClassLoader, in: SoftwareVersion) =
        tryCatch.value(in.text) | parameter(Nil)

    val avroSchema = Schema.create(Schema.Type.STRING)

    def decodeAvro(classLoader: ClassLoader, in: ResolvingDecoder) =
        tryCatch.value(in.readString(null).toString).flatMap(SoftwareVersion.parse) | parameter(Nil)
    def encodeAvro(classLoader: ClassLoader, in: SoftwareVersion, out: Encoder) =
        tryCatch.value(out.writeString(in.text)) | parameter(Nil)

    def decodeMongoDB(classLoader: ClassLoader, in: AnyRef) =
        StringCoder.decodeMongoDB(classLoader, in).flatMap(s => SoftwareVersion.parse(s) | parameter(Nil))
    def encodeMongoDB(classLoader: ClassLoader, in: SoftwareVersion) =
        tryCatch.value(in.toString) | parameter(Nil)

    override def toString = "SoftwareVersionCoder"
}

/**
 * Class for objects that manage migrating data from version to version.
 *
 * Specifying what migrations are available is done using a mini DSL. For example:
 * {{{
 *     until(SoftwareVersion("8.2.7")) executeFor {
 *         case tr: TransactionReply =>
 *             tr.setIsRegistered(null)
 *             tr.setHasData(null)
 *     }
 * }}}
 *
 * There are five operations that can be performed at a version boundary:
 *
 * Run the given side effects:
 * {{{
 *     execute { in => ... side effecting operations affecting in ...  }
 * }}}
 *
 * Apply side effects only if some cases match, doing nothing if no cases match:
 * {{{
 *     executeFor {
 *         case in if somePropertyAboutIn => ... side effecting operations ...
 *         case in if someOtherProperty => ...
 *     }
 * }}}
 *
 * Pure function transforming the input value:
 * {{{
 *     apply { in => ... generate new value from in ... }
 * }}}
 *
 * Pure function transforming the input value only in certain cases:
 * {{{
 *     applyFor {
 *         case in if somePropertyAboutIn => ... generate new value from in ...
 *         case in if someOtherProperty => ...
 *     }
 * }}}
 *
 * Takes a function that returns Result[DataType], and so allows for failure:
 * {{{
 *     flatMap { in => ... possibly Failing computation to generate a new value ... }
 * }}}
 */
class Migrator[T, V <% Ordered[V]] {
    type Migration = T => Result[T]

    private var migrations: List[(V, Migration)] = Nil

    lazy val upgradeMigrations: List[(V, Migration)] = migrations.sortWith((a, b) => a._1 < b._1)
    lazy val downgradeMigrations: List[(V, Migration)] = migrations.sortWith((a, b) => b._1 < a._1)

    /** Define a new downgrade migration -- for any target version older than the given threshold, apply this migration */
    def until(threshold: V): MigrationDecl = new MigrationDecl(threshold)

    /** Intermediate objects when building a migration */
    class MigrationDecl(threshold: V) {
        private def migrate(f: T => Result[T]): Unit = migrations ::= (threshold, f)

        /** Migration is performed using side effects */
        def execute(f: T => Unit): Unit = migrate { input => f(input); Okay(input) }

        /** Migration is performed using side effects */
        def executeFor(f: PartialFunction[T, Unit]): Unit =
            migrate { input => if (f isDefinedAt input) f(input); Okay(input) }

        /** Migration is performed using a simple function */
        def apply(f: T => T): Unit = migrate { input => Okay(f(input)) }

        /** Migration is performed using a simple partial function */
        def applyFor(f: PartialFunction[T, T]): Unit =
            migrate { input => if (f isDefinedAt input) Okay(f(input)) else Okay(input) }

        /** Migration is performed using a possibly-failing computation */
        def flatMap(f: T => Result[T]): Unit =
            migrate(f)
    }

    /** Upgrade some input data from the given version to the most recent version. */
    def upgrade(fromVersion: V, input: T): Result[T] =
        upgradeMigrations.dropWhile(_._1 <= fromVersion).foldLeft[Result[T]](Okay(input))(_ flatMap _._2)

    /** Downgrade some input data from the most recent version to the given version. */
    def downgrade(toVersion: V, input: T): Result[T] =
        downgradeMigrations.takeWhile(toVersion < _._1).foldLeft[Result[T]](Okay(input))(_ flatMap _._2)
}
