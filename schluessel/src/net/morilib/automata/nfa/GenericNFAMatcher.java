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
package net.morilib.automata.nfa;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import net.morilib.automata.NFAState;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/05
 */
public final class GenericNFAMatcher<T, A> {

	//
	private NFAObject<T, A, ?> nfa;
	private Set<A>    result = null;
	private List<T>   matched = null;

	/**
	 * 
	 * @param nfa
	 */
	public GenericNFAMatcher(NFAObject<T, A, ?> nfa) {
		this.nfa = nfa;
	}

	//
	private Set<NFAState> getInitialStates() {
		return NFAs.getEpsilonReachable(nfa, nfa.getInitialStates());
	}

	/**
	 * 
	 * @param str
	 * @return
	 */
	public boolean match(T... ts) {
		List<T> matches = new ArrayList<T>();
		Set<NFAState> s = getInitialStates();
		Set<NFAState> t = s;
		int i = 0;

		while(i < ts.length) {
			T a = ts[i];

			matches.add(a);
			t = NFAs.getStates(nfa, s, a);
			s = NFAs.getEpsilonReachable(nfa, t);
			if(s.isEmpty()) {
				return false;
			} else {
				i++;
			}
		}

		if(nfa.isFinalAny(s)) {
			matched = matches;
			result  = NFAs.getAccept(nfa, s);
			return true;
		} else {
			matched = null;
			result  = null;
			return false;
		}
	}

	/**
	 * 
	 * @return
	 */
	public Set<A> getResult() {
		// result is already unmodifiable
		return result;
	}

	/**
	 * 
	 * @return
	 */
	public List<T> getMatched() {
		return (matched != null) ?
				new ArrayList<T>(matched) : null;
	}

}
