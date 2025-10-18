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
package net.morilib.lisp.lib.srfi059;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.morilib.lang.system.Vicinity;
import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/15
 */
public class LispVicinity extends Datum2 {

	/**
	 * 
	 */
	public static final LispVicinity EMPTY = new LispVicinity(null);

	/**
	 * 
	 */
	public static final LispVicinity USER_HOME =
		new LispVicinity(new File(Vicinity.USER_HOME));

	/**
	 * 
	 */
	public static final LispVicinity JAVA_HOME =
		new LispVicinity(new File(Vicinity.JAVA_HOME));

	/**
	 * 
	 */
	public static final List<LispVicinity> CLASSPATH;

	/**
	 * 
	 */
	public static final List<LispVicinity> PATH;

	//
	static {
		List<LispVicinity> cl, pt;

		cl = new ArrayList<LispVicinity>();
		for(String s : Vicinity.CLASSPATH) {
			cl.add(new LispVicinity(new File(s)));
		}
		CLASSPATH = Collections.unmodifiableList(cl);

		pt = new ArrayList<LispVicinity>();
		for(String s : Vicinity.PATH) {
			pt.add(new LispVicinity(new File(s)));
		}
		PATH = Collections.unmodifiableList(pt);
	}

	//
	File vicinity;

	/**
	 * 
	 * @param vicinity
	 */
	public LispVicinity(File vicinity) {
		this.vicinity = vicinity;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		if(vicinity != null) {
			buf.append("#<vicinity " + vicinity.toString() + ">");
		} else {
			buf.append("#<vicinity empty>");
		}
	}

}
