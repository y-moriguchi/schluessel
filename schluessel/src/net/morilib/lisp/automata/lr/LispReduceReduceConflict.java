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
package net.morilib.lisp.automata.lr;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.automata.cfg.LispCFGRule;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/20
 */
public class LispReduceReduceConflict extends Datum2 {

	//
	private Datum state;
	private LispCFGRule reduce1, reduce2;

	/**
	 * 
	 * @param state
	 * @param reduce1
	 * @param reduce2
	 */
	public LispReduceReduceConflict(Datum state, LispCFGRule reduce1,
			LispCFGRule reduce2) {
		this.state   = state;
		this.reduce1 = reduce1;
		this.reduce2 = reduce2;
	}

	/**
	 * @return the state
	 */
	public Datum getState() {
		return state;
	}

	/**
	 * @return the reduce1
	 */
	public LispCFGRule getReduce1() {
		return reduce1;
	}

	/**
	 * @return the reduce1
	 */
	public LispCFGRule getReduce2() {
		return reduce2;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<reduce-reduce-conflict ");
		buf.append("reduce ").append(reduce1).append(" vs ");
		buf.append("reduce ").append(reduce2).append(">");
	}

}
