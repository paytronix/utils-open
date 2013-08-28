//
// Copyright 2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.standalone

import java.io.File
import java.net.{JarURLConnection, URLClassLoader}

import org.rogach.scallop.ScallopConf
import org.slf4j.LoggerFactory

import com.paytronix.utils.internal.java.ClassUtils

object scallop {
    private implicit val logger = LoggerFactory.getLogger(getClass)

    def getImplementationVersion(clazz: Class[_]): String =
        try {
            val classFilename = ClassUtils.getUnqualifiedClassName(clazz) + ".class"
            clazz.getResource(classFilename).openConnection match {
                case jarConnection: JarURLConnection =>
                    jarConnection.getJarFile.getManifest.getMainAttributes.getValue("Implementation-Version") match {
                        case null    => "unknown"
                        case version => version
                    }
                case _ =>
                    "version unknown"
            }
        } catch { case e: Exception =>
            logger.warn("Unable to determine version number:", e)
            "version unknown"
        }

    trait BasicOptions extends ScallopConf {
        def programName: String

        version(getImplementationVersion(getClass))

        banner(programName)

        helpWidth(132)

        val debug = opt[Boolean]("debug", descr="Enable verbose debug logging")
        val logFile = opt[String]("log-file", descr="Log output to a file as well as the console") map { new File(_) }
    }
}
