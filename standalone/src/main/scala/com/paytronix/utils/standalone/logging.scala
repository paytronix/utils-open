//
// Copyright 2013 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.standalone

import java.io.File

import ch.qos.logback.classic.{Level, LoggerContext}
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.{ConsoleAppender, FileAppender}
import org.slf4j.{Logger, LoggerFactory}

object logging {
    def setup(logFile: Option[File], debug: Boolean): Unit = {
        val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

        val rootLogger = lc.getLogger(Logger.ROOT_LOGGER_NAME)
        rootLogger.detachAndStopAllAppenders()

        val consolePLE = new PatternLayoutEncoder()
        consolePLE.setContext(lc)
        consolePLE.setPattern("%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n")
        consolePLE.start()

        val consoleAppender = new ConsoleAppender[ILoggingEvent]()
        consoleAppender.setContext(lc)
        consoleAppender.setName("console")
        consoleAppender.setEncoder(consolePLE)
        consoleAppender.start()
        rootLogger.addAppender(consoleAppender)

        logFile.foreach(logToFile)

        rootLogger.setLevel(if (debug) Level.DEBUG else Level.INFO)
    }

    def logToFile(file: File): Unit = {
        val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

        val rootLogger = lc.getLogger(Logger.ROOT_LOGGER_NAME)

        val filePLE = new PatternLayoutEncoder()
        filePLE.setContext(lc)
        filePLE.setPattern("%d{yyyy-MM-dd HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n")
        filePLE.start()

        val fileAppender = new FileAppender[ILoggingEvent]()
        fileAppender.setContext(lc)
        fileAppender.setName("file")
        fileAppender.setFile(file.toString)
        fileAppender.setAppend(true)
        fileAppender.setEncoder(filePLE)
        fileAppender.start()
        rootLogger.addAppender(fileAppender)
    }
}
