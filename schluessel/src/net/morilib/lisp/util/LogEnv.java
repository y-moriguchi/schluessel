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
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class LogEnv {

	//
	private static final String  PGM_ID = "SCHLUSH";
	private static final Formatter FMTER = new SimpleFormatter();

	//
	private static String  PATTERN;
	//private static final Level   LOGLEVEL;
	private static Handler theHandler;

	static {
		String pat, ept;
		//String lev, elv;

		try {
			pat = System.getProperty("mylog.logfile.pattern");
			ept = System.getenv(PGM_ID + "_LOG");
			if(pat != null) {
				PATTERN = pat;
			} else if(ept != null) {
				PATTERN = ept;
			} else {
				PATTERN = null;
			}
		} catch(SecurityException e) {
			PATTERN = null;
		}

		/*lev = System.getProperty("mylog.loglevel");
		elv = System.getenv(PGM_ID + "_LOGLEVEL");
		if(lev != null) {
			LOGLEVEL = Level.parse(lev);
		} else if(elv != null) {
			LOGLEVEL = Level.parse(elv);
		} else {
			LOGLEVEL = Level.OFF;
		}*/
	}

	public synchronized static Logger init(String name) {
		Logger    res = Logger.getLogger(name);

		try {
			Handler[] hns = res.getHandlers();

			for(Handler h : hns) {
				res.removeHandler(h);
			}

			if(theHandler == null) {
				if(PATTERN != null) {
					theHandler = new LispHandler(PATTERN);
					theHandler.setFormatter(FMTER);
				} else {
					theHandler = new NullHandler();
				}
			}

			res.addHandler(theHandler);
			res.setLevel((PATTERN != null) ? Level.ALL : Level.OFF);
			return res;
		} catch(SecurityException e) {
//			res.setLevel(Level.OFF);
			return res;
		} catch(IOException e) {
			throw new RuntimeException(e);
		}
	}

}
