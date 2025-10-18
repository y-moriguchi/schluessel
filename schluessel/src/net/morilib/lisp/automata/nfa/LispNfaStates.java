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

import java.util.HashSet;
import java.util.Set;

import net.morilib.automata.NFAState;
import net.morilib.automata.nfa.NFAs;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.automata.ILispConfiguration;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/30
 */
public class LispNfaStates extends Datum2
implements ILispConfiguration {

	//
	ILispNfa nfa;
	Set<NFAState> states;

	//
	LispNfaStates(ILispNfa nfa, Set<NFAState> states) {
		this.nfa    = nfa;
		this.states = states;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispTransitable#go(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public LispNfaStates go(Datum d, LispMessage mesg) {
		Set<NFAState> s;

		s = NFAs.getEpsilonReachable(nfa.getNFA(), states);
		return new LispNfaStates(nfa,
				NFAs.getStates(nfa.getNFA(), s, d));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispTransitable#go(int, net.morilib.lisp.LispMessage)
	 */
	@Override
	public LispNfaStates go(int d, LispMessage mesg) {
		return go(LispInteger.valueOf(d), mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispTransitable#go(char, net.morilib.lisp.LispMessage)
	 */
	@Override
	public LispNfaStates go(char d, LispMessage mesg) {
		return go(LispCharacter.valueOf(d), mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration goSideEffect(Datum d, LispMessage mesg) {
		return go(d, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(int, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration goSideEffect(int d, LispMessage mesg) {
		return go(d, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(char, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration goSideEffect(char d, LispMessage mesg) {
		return go(d, mesg);
	}

	/**
	 * 
	 * @return
	 */
	public Set<Datum> getStates() {
		Set<Datum> r = new HashSet<Datum>();

		for(NFAState s : states)  r.add(new LispNfaState(s));
		return r;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isAccepted() {
		Set<NFAState> s;

		s = NFAs.getEpsilonReachable(nfa.getNFA(), states);
		return NFAs.isAccepted(nfa.getNFA(), s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispState#isInitialState()
	 */
	@Override
	public boolean isInitialState() {
		Set<NFAState> s;

		s = NFAs.getEpsilonReachable(nfa.getNFA(), states);
		return NFAs.isInitialState(nfa.getNFA(), s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispState#isDead()
	 */
	@Override
	public boolean isDead() {
		return states.isEmpty();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<nfa-states>");
	}

}
