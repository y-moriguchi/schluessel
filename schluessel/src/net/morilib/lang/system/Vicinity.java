/*
 * Copyright 2009-2010 Yuichiro Moriguchi
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
package net.morilib.lang.system;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/26
 */
public final class Vicinity {

	/**
	 * 
	 */
	public static final Set<Character> SUFFIXIES;

	/**
	 * 
	 */
	public static final String USER_HOME;

	/**
	 * 
	 */
	public static final String JAVA_HOME;

	/**
	 * 
	 */
	public static final String WORKING;

	/**
	 * 
	 */
	public static final String PATH_SEPARATOR;

	/**
	 * 
	 */
	public static final Set<String> CLASSPATH;

	/**
	 * 
	 */
	public static final Set<String> PATH;

	//
	private Vicinity() {}

	//
	static {
		Set<Character> suf = new HashSet<Character>();
		String[] cpt;
		String cpt0, pt0;

		USER_HOME = System.getProperty("user.home");
		JAVA_HOME = System.getProperty("java.home");
		WORKING   = System.getProperty("user.dir");
		PATH_SEPARATOR = System.getProperty("path.separator");
		cpt0 = System.getProperty("java.class.path");
		pt0  = System.getProperty("java.library.path");

		if(OSInfo.OS.isUNIX()) {  // include Mac OS X
			suf.add('/');
		} else if(OSInfo.OS.isWindows()) {
			suf.add('/');  suf.add('\\');
		} else if(OSInfo.OS.isClassicMac()) {
			suf.add(':');
		}
		SUFFIXIES = Collections.unmodifiableSet(suf);

		if(cpt0 != null) {
			cpt = cpt0.split(PATH_SEPARATOR);
			CLASSPATH = Collections.unmodifiableSet(
					new HashSet<String>(Arrays.asList(cpt)));
		} else {
			CLASSPATH = Collections.singleton(cpt0);
		}

		if(pt0 != null) {
			cpt = pt0.split(PATH_SEPARATOR);
			PATH = Collections.unmodifiableSet(
					new HashSet<String>(Arrays.asList(cpt)));
		} else {
			PATH = Collections.singleton(pt0);
		}
	}

}
