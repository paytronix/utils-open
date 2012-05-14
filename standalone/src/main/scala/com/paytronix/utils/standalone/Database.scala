//
// Copyright 2011 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.standalone

import java.net.URI
import java.sql.DriverManager
import javax.crypto.spec.IvParameterSpec
import scala.util.control.Exception.{allCatch, catching}
import shapeless.{::, HNil}
import net.lag.configgy.ConfigMap
import org.squeryl.{adapters, Session, SessionFactory}
import org.squeryl.internals.DatabaseAdapter
import com.paytronix.utils.internal.java.SecureString
import com.paytronix.utils.validation.base.{Validated, ValidationError, fieldNameOps, validate, validationErrorsToString, validationFunctionOps, valueOps}
import com.paytronix.utils.validation.option.{optional, some}
import com.paytronix.utils.validation.reflection.className
import com.paytronix.utils.validation.string.nonBlank

/** Object with various tools for accessing the primary SQL Server databases managed by the PXS from a standalone tool */
object Database {
    case class Parameters(url: String, user: String, password: String)

    val passwordIV =
        new IvParameterSpec(Array[Byte](0xbe.asInstanceOf[Byte], 0x92.asInstanceOf[Byte], 0xc3.asInstanceOf[Byte], 0x53.asInstanceOf[Byte],
                                        0xe2.asInstanceOf[Byte], 0x75.asInstanceOf[Byte], 0x60.asInstanceOf[Byte], 0x1a.asInstanceOf[Byte]))

    val passwordA1 = Array[Byte](0xa9.asInstanceOf[Byte], 0x81.asInstanceOf[Byte], 0x41.asInstanceOf[Byte], 0x07.asInstanceOf[Byte],
                                 0x92.asInstanceOf[Byte], 0x6e.asInstanceOf[Byte], 0xe6.asInstanceOf[Byte], 0x94.asInstanceOf[Byte],
                                 0x77.asInstanceOf[Byte], 0x72.asInstanceOf[Byte], 0x90.asInstanceOf[Byte], 0x55.asInstanceOf[Byte],
                                 0x8d.asInstanceOf[Byte], 0x53.asInstanceOf[Byte], 0x34.asInstanceOf[Byte], 0x41.asInstanceOf[Byte])
    val passwordA2 = Array[Byte](0x4f.asInstanceOf[Byte], 0xbd.asInstanceOf[Byte], 0xf0.asInstanceOf[Byte], 0xf3.asInstanceOf[Byte],
                                 0x9e.asInstanceOf[Byte], 0xcd.asInstanceOf[Byte], 0xe0.asInstanceOf[Byte], 0x1b.asInstanceOf[Byte],
                                 0xf1.asInstanceOf[Byte], 0xb7.asInstanceOf[Byte], 0x48.asInstanceOf[Byte], 0x57.asInstanceOf[Byte],
                                 0xfa.asInstanceOf[Byte], 0x55.asInstanceOf[Byte], 0xf8.asInstanceOf[Byte], 0x6b.asInstanceOf[Byte])
    val passwordA3 = Array[Byte](0x22.asInstanceOf[Byte], 0x3e.asInstanceOf[Byte], 0xc5.asInstanceOf[Byte], 0x29.asInstanceOf[Byte],
                                 0xc1.asInstanceOf[Byte], 0x12.asInstanceOf[Byte], 0xdb.asInstanceOf[Byte], 0x97.asInstanceOf[Byte],
                                 0xd2.asInstanceOf[Byte], 0xd7.asInstanceOf[Byte], 0x3f.asInstanceOf[Byte], 0x1b.asInstanceOf[Byte],
                                 0x94.asInstanceOf[Byte], 0xef.asInstanceOf[Byte], 0xf9.asInstanceOf[Byte], 0xa2.asInstanceOf[Byte])
    val passwordB1 = Array[Byte](0x95.asInstanceOf[Byte], 0xd1.asInstanceOf[Byte], 0x0d.asInstanceOf[Byte], 0xc1.asInstanceOf[Byte],
                                 0x52.asInstanceOf[Byte], 0x42.asInstanceOf[Byte], 0x57.asInstanceOf[Byte], 0xa1.asInstanceOf[Byte],
                                 0xf9.asInstanceOf[Byte], 0x0b.asInstanceOf[Byte], 0x32.asInstanceOf[Byte], 0x85.asInstanceOf[Byte],
                                 0xed.asInstanceOf[Byte], 0xd9.asInstanceOf[Byte], 0x15.asInstanceOf[Byte], 0xa2.asInstanceOf[Byte])
    val passwordB2 = Array[Byte](0x88.asInstanceOf[Byte], 0x6d.asInstanceOf[Byte], 0x10.asInstanceOf[Byte], 0xb7.asInstanceOf[Byte],
                                 0x88.asInstanceOf[Byte], 0x8b.asInstanceOf[Byte], 0x76.asInstanceOf[Byte], 0x5c.asInstanceOf[Byte],
                                 0x68.asInstanceOf[Byte], 0x7c.asInstanceOf[Byte], 0x87.asInstanceOf[Byte], 0x6c.asInstanceOf[Byte],
                                 0x39.asInstanceOf[Byte], 0x12.asInstanceOf[Byte], 0x76.asInstanceOf[Byte], 0xf9.asInstanceOf[Byte])
    val passwordB3 = Array[Byte](0x00.asInstanceOf[Byte], 0x00.asInstanceOf[Byte], 0x5d.asInstanceOf[Byte], 0x5f.asInstanceOf[Byte],
                                 0x1b.asInstanceOf[Byte], 0x70.asInstanceOf[Byte], 0xc7.asInstanceOf[Byte], 0xb1.asInstanceOf[Byte],
                                 0x12.asInstanceOf[Byte], 0x78.asInstanceOf[Byte], 0xe8.asInstanceOf[Byte], 0xc6.asInstanceOf[Byte],
                                 0x9d.asInstanceOf[Byte], 0x83.asInstanceOf[Byte], 0x2c.asInstanceOf[Byte], 0x95.asInstanceOf[Byte])

    def adapterForURL(url: String): Either[Throwable, DatabaseAdapter] =
        url match {
            case JDBCDriverName("h2")         => Right(new adapters.H2Adapter)
            case JDBCDriverName("mysql")      => Right(new adapters.MySQLAdapter)
            case JDBCDriverName("sqlserver")  => Right(new adapters.MSSQLServer)
            case JDBCDriverName("postgresql") => Right(new adapters.PostgreSqlAdapter)

            case JDBCDriverName(other) => Left(new RuntimeException("unsupported JDBC driver " + other))
            case _ => Left(new RuntimeException("unsupported JDBC URI format " + url))
        }

    def validateDatabaseInstanceParameters(default: Option[Parameters], map: ConfigMap): Validated[Parameters] =
        validate (
            ("class"    from map.getString is optional(className(classOf[java.sql.Driver]))) ::
            ("url"      from map.getString is optional(nonBlank())) ::
            ("user"     from map.getString is optional(nonBlank())) ::
            ("password" from map.getString is optional(nonBlank() and (
                in => catching(classOf[Exception]).either(SecureString.unmaskString(
                    in, passwordIV, passwordA1, passwordA2, passwordA3, passwordB1, passwordB2, passwordB3
                )).left.map(_ => ValidationError("invalid_password", "failed to decrypt password") :: Nil)
            ))) :: HNil
        ).right.flatMap {
            case _ :: Some(url) :: Some(user) :: Some(password) :: HNil =>
                Right(Parameters(url, user, password))
            case _ :: Some(url) :: None :: None :: HNil if default.isDefined =>
                Right(Parameters(url, default.get.user, default.get.password))
            case _ :: Some(url) :: userOpt :: passwordOpt :: HNil if userOpt.isDefined != passwordOpt.isDefined =>
                Left(ValidationError("both user and password must be specified, not only one") :: Nil)
            case _ => Left(ValidationError("url, user, and password are all required") :: Nil)
        }

    def validateDatabaseParameters(configMap: ConfigMap): Either[Throwable, Map[String, Parameters]] =
        (configMap.getConfigMap("database") is some {
            (map: ConfigMap) =>

            validateDatabaseInstanceParameters(None, map).right.flatMap {
                (defaultParams: Parameters) => map.keys.foldLeft[Validated[Map[String, Parameters]]](Right(Map("default" -> defaultParams))) {
                    (validatedParamMap, key) => validatedParamMap.right.flatMap {
                        paramMap => map.getConfigMap(key) match {
                            case Some(subMap) =>
                                validateDatabaseInstanceParameters(Some(defaultParams), subMap).right.map(params => paramMap + (key -> params))
                            case None =>
                                Right(paramMap)
                        }
                    }
                }
            }
        }).left.map {
            errors => new RuntimeException("Failed to configure database:\n" + validationErrorsToString(errors))
        }

    private object JDBCDriverName {
        def unapply(in: String): Option[String] =
            allCatch.opt(Option(new URI(in).getSchemeSpecificPart).flatMap(s => Option(new URI(s).getScheme))).flatMap(x => x)
    }
}

trait DatabaseTool extends StandaloneTool {
    import Database._

    private var _databaseParameters: Option[Map[String, Parameters]] = None

    /** The configured parameters for the default database. Will throw an exception if the database config is not yet loaded. */
    def databaseParameters: Parameters = databaseParametersFor("default") getOrElse sys.error("database configuration parameters not yet loaded")

    /** Get the configured parameters for a particular database instance. */
    def databaseParametersFor(instance: String): Option[Parameters] = _databaseParameters.flatMap(_.get(instance))

    advise {
        f => for {
            params <- validateDatabaseParameters(configMap).right
            adapter <- adapterForURL(params("default").url).right

            result <- {
                val defaultParams = params("default")
                SessionFactory.concreteFactory = Some (
                    () => Session.create(DriverManager.getConnection(defaultParams.url, defaultParams.user, defaultParams.password), adapter)
                )
                _databaseParameters = Some(params)
                f()
            }.right
        } yield {
            _databaseParameters = None
        }
    }
}

object EncryptDatabasePassword {
    import Database._
    def main(args: Array[String]) =
        println(SecureString.maskString(new String(System.console.readPassword("Enter password to encrypt: ")),
                                        passwordIV, passwordA1, passwordA2, passwordA3, passwordB1, passwordB2, passwordB3))
}
