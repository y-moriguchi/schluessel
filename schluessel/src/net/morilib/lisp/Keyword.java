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
package net.morilib.lisp;

import java.util.Map;
import java.util.WeakHashMap;

import net.morilib.lisp.sos.LispType;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class Keyword extends Atom {

	//
	private static Map<String, Keyword> flyweight;

	//
	private String name;

	//
	private Keyword(String name) {
		this.name = name;
	}

	/**
	 * 
	 * @param name
	 * @return
	 */
	public static Keyword getKeyword(String name) {
		Keyword res;

		if(name == null) {
			throw new NullPointerException("Keyword");
		}

		synchronized(Keyword.class) {
			if(flyweight == null) {
				flyweight = new WeakHashMap<String, Keyword>();
			}

			res = flyweight.get(name);
			if(res == null) {
				res = new Keyword(name);
				flyweight.put(name, res);
			}
		}
		return res;
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean isEqv(Atom a) {
		return equals(a);
	}

	/**
	 * 
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * 
	 * @return
	 */
	public LispString toLispString() {
		return new LispString(getName());
	}

	/**
	 * 
	 * @return
	 */
	public String print() {
		return ":" + getName();
	}

	/**
	 * 
	 * @return
	 */
	public String getResult() {
		return ":" + getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return "Keyword:" + getName();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.KEYWORD;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append(":" + getName());
	}

}
