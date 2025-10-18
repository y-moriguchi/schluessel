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
package net.morilib.lisp.automata.nfa;

import net.morilib.automata.NFA;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/30
 */
public class LispNfa extends Datum2 implements ILispNfa {

	//
	NFA<Datum, Datum, Datum> nfa;

	/**
	 * 
	 * @param nfa
	 */
	public LispNfa(NFA<Datum, Datum, Datum> nfa) {
		this.nfa = nfa;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.nfa.ILispNfa#getNfa()
	 */
	public NFA<Datum, ?, ?> getNFA() {
		return nfa;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.nfa.ILispNfa#getInitialStates()
	 */
	public LispNfaStates getInitial() {
		return new LispNfaStates(this, nfa.getInitialStates());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<nfa>");
	}

}
