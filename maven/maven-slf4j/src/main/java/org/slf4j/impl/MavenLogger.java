/**
 * Copyright (C) 2010-2012 Andrei Pozolotin <Andrei.Pozolotin@gmail.com>
 *
 * All rights reserved. Licensed under the OSI BSD License.
 *
 * http://www.opensource.org/licenses/bsd-license.php
 */
package org.slf4j.impl;

import org.apache.maven.plugin.logging.Log;
import org.slf4j.helpers.FormattingTuple;
import org.slf4j.helpers.MarkerIgnoringBase;
import org.slf4j.helpers.MessageFormatter;

@SuppressWarnings("serial")
class MavenLogger extends MarkerIgnoringBase {

	public static final String LINE_SEPARATOR = System
			.getProperty("line.separator");

	private static String LEVEL_DEBUG = "DEBUG";
	private static String LEVEL_INFO = "INFO";
	private static String LEVEL_WARN = "WARN";
	private static String LEVEL_ERROR = "ERROR";

	/** native maven logger */
	private final Log log;

	/**
	 * Package access allows only {@link MavenLoggerFactory} to instantiate
	 * SimpleLogger instances.
	 */
	MavenLogger(final String name, final Log log) {
		this.name = name;
		this.log = log;
	}

	/**
	 * @return always false
	 */
	@Override
	public boolean isTraceEnabled() {
		return false;
	}

	@Override
	public void trace(final String msg) {
		// NOP
	}

	@Override
	public void trace(final String format, final Object arg) {
		// NOP
	}

	@Override
	public void trace(final String format, final Object arg1, final Object arg2) {
		// NOP
	}

	@Override
	public void trace(final String format, final Object[] argArray) {
		// NOP
	}

	@Override
	public void trace(final String msg, final Throwable t) {
		// NOP
	}

	/**
	 */
	@Override
	public boolean isDebugEnabled() {
		return log.isDebugEnabled();
	}

	/**
	 */
	@Override
	public void debug(final String msg) {
		log(LEVEL_DEBUG, msg, null);
	}

	/**
	 */
	@Override
	public void debug(final String format, final Object arg) {
		formatAndLog(LEVEL_DEBUG, format, arg, null);
	}

	/**
	 * A NOP implementation, as this logger is permanently disabled for the
	 * DEBUG level.
	 */
	@Override
	public void debug(final String format, final Object arg1, final Object arg2) {
		formatAndLog(LEVEL_DEBUG, format, arg1, arg2);
	}

	@Override
	public void debug(final String format, final Object[] argArray) {
		formatAndLog(LEVEL_DEBUG, format, argArray);
	}

	@Override
	public void debug(final String msg, final Throwable t) {
		log(LEVEL_DEBUG, msg, t);
	}

	/**
	 * This is our internal implementation for logging regular
	 * (non-parameterized) log messages.
	 * 
	 * @param level
	 * @param message
	 * @param exception
	 */
	private void log(final String level, final String message,
			final Throwable exception) {

		if (exception == null) {

			if (LEVEL_DEBUG.equals(level)) {
				log.debug(message);
			}

			if (LEVEL_INFO.equals(level)) {
				log.info(message);
			}

			if (LEVEL_WARN.equals(level)) {
				log.warn(message);
			}

			if (LEVEL_ERROR.equals(level)) {
				log.error(message);
			}

		} else {

			if (LEVEL_DEBUG.equals(level)) {
				log.debug(message, exception);
			}

			if (LEVEL_INFO.equals(level)) {
				log.info(message, exception);
			}

			if (LEVEL_WARN.equals(level)) {
				log.warn(message, exception);
			}

			if (LEVEL_ERROR.equals(level)) {
				log.error(message, exception);
			}

		}

	}

	/**
	 * For formatted messages, first substitute arguments and then log.
	 * 
	 * @param level
	 * @param format
	 * @param param1
	 * @param param2
	 */
	private void formatAndLog(final String level, final String format,
			final Object arg1, final Object arg2) {

		final FormattingTuple tp = MessageFormatter.format(format, arg1, arg2);

		log(level, tp.getMessage(), tp.getThrowable());

	}

	/**
	 * For formatted messages, first substitute arguments and then log.
	 * 
	 * @param level
	 * @param format
	 * @param argArray
	 */
	private void formatAndLog(final String level, final String format,
			final Object[] argArray) {

		final FormattingTuple tp = MessageFormatter.arrayFormat(format,
				argArray);

		log(level, tp.getMessage(), tp.getThrowable());

	}

	/**
	 * Always returns true.
	 */
	@Override
	public boolean isInfoEnabled() {
		return log.isInfoEnabled();
	}

	/**
	 * A simple implementation which always logs messages of level INFO
	 * according to the format outlined above.
	 */
	@Override
	public void info(final String msg) {
		log(LEVEL_INFO, msg, null);
	}

	/**
	 * Perform single parameter substitution before logging the message of level
	 * INFO according to the format outlined above.
	 */
	@Override
	public void info(final String format, final Object arg) {
		formatAndLog(LEVEL_INFO, format, arg, null);
	}

	/**
	 * Perform double parameter substitution before logging the message of level
	 * INFO according to the format outlined above.
	 */
	@Override
	public void info(final String format, final Object arg1, final Object arg2) {
		formatAndLog(LEVEL_INFO, format, arg1, arg2);
	}

	/**
	 * Perform double parameter substitution before logging the message of level
	 * INFO according to the format outlined above.
	 */
	@Override
	public void info(final String format, final Object[] argArray) {
		formatAndLog(LEVEL_INFO, format, argArray);
	}

	/**
	 * Log a message of level INFO, including an exception.
	 */
	@Override
	public void info(final String msg, final Throwable t) {
		log(LEVEL_INFO, msg, t);
	}

	/**
	 * Always returns true.
	 */
	@Override
	public boolean isWarnEnabled() {
		return log.isWarnEnabled();
	}

	/**
	 * A simple implementation which always logs messages of level WARN
	 * according to the format outlined above.
	 */
	@Override
	public void warn(final String msg) {
		log(LEVEL_WARN, msg, null);
	}

	/**
	 * Perform single parameter substitution before logging the message of level
	 * WARN according to the format outlined above.
	 */
	@Override
	public void warn(final String format, final Object arg) {
		formatAndLog(LEVEL_WARN, format, arg, null);
	}

	/**
	 * Perform double parameter substitution before logging the message of level
	 * WARN according to the format outlined above.
	 */
	@Override
	public void warn(final String format, final Object arg1, final Object arg2) {
		formatAndLog(LEVEL_WARN, format, arg1, arg2);
	}

	/**
	 * Perform double parameter substitution before logging the message of level
	 * WARN according to the format outlined above.
	 */
	@Override
	public void warn(final String format, final Object[] argArray) {
		formatAndLog(LEVEL_WARN, format, argArray);
	}

	/**
	 * Log a message of level WARN, including an exception.
	 */
	@Override
	public void warn(final String msg, final Throwable t) {
		log(LEVEL_WARN, msg, t);
	}

	/**
	 * Always returns true.
	 */
	@Override
	public boolean isErrorEnabled() {
		return log.isErrorEnabled();
	}

	/**
	 * A simple implementation which always logs messages of level ERROR
	 * according to the format outlined above.
	 */
	@Override
	public void error(final String msg) {
		log(LEVEL_ERROR, msg, null);
	}

	/**
	 * Perform single parameter substitution before logging the message of level
	 * ERROR according to the format outlined above.
	 */
	@Override
	public void error(final String format, final Object arg) {
		formatAndLog(LEVEL_ERROR, format, arg, null);
	}

	/**
	 * Perform double parameter substitution before logging the message of level
	 * ERROR according to the format outlined above.
	 */
	@Override
	public void error(final String format, final Object arg1, final Object arg2) {
		formatAndLog(LEVEL_ERROR, format, arg1, arg2);
	}

	/**
	 * Perform double parameter substitution before logging the message of level
	 * ERROR according to the format outlined above.
	 */
	@Override
	public void error(final String format, final Object[] argArray) {
		formatAndLog(LEVEL_ERROR, format, argArray);
	}

	/**
	 * Log a message of level ERROR, including an exception.
	 */
	@Override
	public void error(final String msg, final Throwable t) {
		log(LEVEL_ERROR, msg, t);
	}

}
