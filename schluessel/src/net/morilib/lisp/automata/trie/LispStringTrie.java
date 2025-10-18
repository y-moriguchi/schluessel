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

import net.morilib.automata.trie.IntegerTrie;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.automata.dfa.ILispDfaState;
import net.morilib.lisp.automata.dfa.LispStringDfaState;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/30
 */
public class LispStringTrie extends Datum2 implements ILispTrie {

	//
	IntegerTrie<Datum> trie;

	/**
	 * 
	 * @param t
	 */
	public LispStringTrie(IntegerTrie<Datum> t) {
		this.trie = t;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfa#getInitialState()
	 */
	@Override
	public ILispDfaState getInitial() {
		return new LispStringDfaState(trie.getInitialState());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<string-trie>");
	}

}
