/**
 * Copyright (C) 2010-2012 Andrei Pozolotin <Andrei.Pozolotin@gmail.com>
 *
 * All rights reserved. Licensed under the OSI BSD License.
 *
 * http://www.opensource.org/licenses/bsd-license.php
 */
package org.slf4j.impl;

import java.util.HashMap;
import java.util.Map;

import org.apache.maven.plugin.logging.Log;
import org.slf4j.ILoggerFactory;
import org.slf4j.Logger;

/**
 * An implementation of {@link ILoggerFactory} which always returns
 * {@link MavenLogger} instances.
 *
 */
public class MavenLoggerFactory implements ILoggerFactory {

	final Map<String, MavenLogger> loggerMap;

	public MavenLoggerFactory() {
		loggerMap = new HashMap<String, MavenLogger>();
	}

	public static Logger getLogger(final Class<?> klaz, final Log log) {
		return getLogger(klaz.getName(), log);
	}

	public static Logger getLogger(final String name, final Log log) {

		final MavenLoggerFactory factory = (MavenLoggerFactory) StaticLoggerBinder
				.getSingleton().getLoggerFactory();

		MavenLogger logger = null;

		synchronized (factory) {

			logger = factory.loggerMap.get(name);

			if (logger == null) {
				logger = new MavenLogger(name, log);
				factory.loggerMap.put(name, logger);
			}

		}

		return logger;

	}

	@Override
	public Logger getLogger(final String name) {
        final Log log = StaticLoggerBinder.getSingleton().getLog();
        if (log == null) {
    		throw new UnsupportedOperationException(
    				"you must use another factory method:"
    						+ " getLogger(String name, Log log)");
        } else {
            return getLogger(name, log);
        }
	}

}
