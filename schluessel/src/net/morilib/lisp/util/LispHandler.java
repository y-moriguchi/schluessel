/*
 * Copyright 2009 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.morilib.lisp.util;

import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

public class LispHandler extends FileHandler {
	
	//
	private static boolean loggable2 = false;
	private static Level level2 = Level.OFF;
	
	/**
	 * @throws IOException
	 * @throws SecurityException
	 */
	public LispHandler() throws IOException, SecurityException {
		super();
	}

	/**
	 * @param pattern
	 * @param append
	 * @throws IOException
	 * @throws SecurityException
	 */
	public LispHandler(
			String pattern, boolean append) throws IOException,
			SecurityException {
		super(pattern, append);
	}

	/**
	 * @param pattern
	 * @param limit
	 * @param count
	 * @param append
	 * @throws IOException
	 * @throws SecurityException
	 */
	public LispHandler(
			String pattern, int limit, int count,
			boolean append) throws IOException, SecurityException {
		super(pattern, limit, count, append);
	}

	/**
	 * @param pattern
	 * @param limit
	 * @param count
	 * @throws IOException
	 * @throws SecurityException
	 */
	public LispHandler(String pattern, int limit, int count)
			throws IOException, SecurityException {
		super(pattern, limit, count);
	}

	/**
	 * @param pattern
	 * @throws IOException
	 * @throws SecurityException
	 */
	public LispHandler(
			String pattern) throws IOException, SecurityException {
		super(pattern);
	}

	/**
	 * @return the loggable2
	 */
	public static boolean isLoggable2() {
		return loggable2;
	}

	/**
	 * @param loggable2 the loggable2 to set
	 */
	public static void setLoggable2(boolean loggable2) {
		LispHandler.loggable2 = loggable2;
	}

	/**
	 * @return the level2
	 */
	public static Level isLevel2() {
		return level2;
	}

	/**
	 * @param level2 the level2 to set
	 */
	public static void setLevel2(Level level2) {
		LispHandler.level2 = level2;
	}
	
	/**
	 * 
	 */
	public void publish(LogRecord record) {
		Level l1 = record.getLevel();
		
		if(loggable2 && l1.intValue() >= level2.intValue()) {
			super.publish(record);
			super.flush();
		}
	}
	
}
