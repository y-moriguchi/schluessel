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
package net.morilib.lisp.automata;

import java.util.HashMap;
import java.util.Map;

import net.morilib.automata.NFAState;
import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/06
 */
public final class LispGrammarVariable extends Datum2
implements NFAState {

	//
	private static Map<String, LispGrammarVariable> instances =
			new HashMap<String, LispGrammarVariable>();
	private static int nowgennum = 1;

	//
	private String name;
	private int gennum;

	//
	private LispGrammarVariable(String name) {
		this.name = name;
		this.gennum = -1;
	}

	//
	private LispGrammarVariable(int gennum) {
		this.name = null;
		this.gennum = gennum;
	}

	/**
	 * 
	 * @return
	 */
	public synchronized static LispGrammarVariable genvar() {
		return new LispGrammarVariable(nowgennum++);
	}

	/**
	 * 
	 * @param name
	 * @return
	 */
	public synchronized static LispGrammarVariable getInstance(
			String name) {
		LispGrammarVariable v;

		if((v = instances.get(name)) == null) {
			v = new LispGrammarVariable(name);
			instances.put(name, v);
		}
		return v;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isGrammarVariable()
	 */
	public boolean isGrammarVariable() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		if(name != null) {
			buf.append("<<").append(name).append(">>");
		} else {
			buf.append("<<#").append(gennum).append(">>");
		}
	}

}
