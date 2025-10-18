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
package net.morilib.lisp.automata.trie;

import net.morilib.automata.DFA;
import net.morilib.automata.trie.GenericTrie;
import net.morilib.lisp.Datum;
import net.morilib.lisp.automata.dfa.AbstractLispDatumDfa;
import net.morilib.lisp.automata.dfa.LispSimpleDfaState;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/30
 */
public class LispTrie extends AbstractLispDatumDfa {

	//
	GenericTrie<Datum, Datum> trie;

	/**
	 * 
	 * @param t
	 */
	public LispTrie(GenericTrie<Datum, Datum> t) {
		this.trie = t;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfa#getDFA()
	 */
	@Override
	public DFA<Datum, ?, ?> getDFA() {
		return trie;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfa#getInitialState()
	 */
	@Override
	public LispSimpleDfaState getInitial() {
		return new LispSimpleDfaState(trie.getInitialState());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.AbstractLispDatumDfa#isEquvalent(net.morilib.lisp.automata.dfa.ILispDfaState, net.morilib.lisp.automata.dfa.ILispDfaState)
	 */
	@Override
	public boolean isEquvalent(Datum a, Datum b) {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<trie>");
	}

}
