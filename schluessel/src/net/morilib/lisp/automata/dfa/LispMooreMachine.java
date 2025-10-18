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
package net.morilib.lisp.automata.dfa;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.morilib.automata.DFA;
import net.morilib.automata.DFAState;
import net.morilib.automata.TextBound;
import net.morilib.automata.dfa.DFAs;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.automata.ILispConfiguration;
import net.morilib.range.Interval;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/05
 */
public class LispMooreMachine extends AbstractLispDatumDfa
implements DFA<Datum, Datum, Datum> {

	//
	class State extends Datum2
	implements ILispDfaState, DFAState<Datum, Datum, Datum> {

		private Map<Datum, Datum> edges;
		private boolean initial;
		private Datum accepted;

		State(Map<Datum, Datum> edges) {
			this.edges = edges;
			this.initial = false;
			this.accepted = null;
		}

		State(Map<Datum, Datum> edges, boolean init, Datum a) {
			this.edges = edges;
			this.initial = init;
			this.accepted = a;
		}

		@Override
		public State go(Datum alphabet) {
			State s = map.get(edges.get(alphabet));

			return (s != null) ? s : DEAD;
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#goInt(int)
		 */
		@Override
		public State goInt(int x) {
			return go(LispInteger.valueOf(x));
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#goChar(char)
		 */
		@Override
		public State goChar(char x) {
			return go(LispCharacter.valueOf(x));
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#goBound(net.morilib.automata.TextBound)
		 */
		@Override
		public DFAState<Datum, Datum, Datum> goBound(TextBound bound) {
			return DFAs.<Datum, Datum, Datum>deadState();
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#isInitialState()
		 */
		@Override
		public boolean isInitialState() {
			return initial;
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#getAccepted()
		 */
		@Override
		public Set<Datum> getAccepted() {
			return (accepted != null) ?
					Collections.<Datum>singleton(accepted) :
						Collections.<Datum>emptySet();
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#isDead()
		 */
		@Override
		public boolean isDead() {
			return edges.isEmpty();
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#isAccepted()
		 */
		@Override
		public boolean isAccepted() {
			return accepted != null;
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#getAlphabets()
		 */
		@Override
		public Set<Datum> getAlphabets() {
			return edges.keySet();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.automata.ILispTransitable#go(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
		 */
		@Override
		public ILispDfaState go(Datum d, LispMessage mesg) {
			return go(d);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.automata.ILispTransitable#go(int, net.morilib.lisp.LispMessage)
		 */
		@Override
		public ILispDfaState go(int d, LispMessage mesg) {
			return goInt(d);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.automata.ILispTransitable#go(char, net.morilib.lisp.LispMessage)
		 */
		@Override
		public ILispDfaState go(char d, LispMessage mesg) {
			return goChar(d);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
		 */
		@Override
		public ILispConfiguration goSideEffect(Datum d,
				LispMessage mesg) {
			return go(d, mesg);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(int, net.morilib.lisp.LispMessage)
		 */
		@Override
		public ILispConfiguration goSideEffect(int d,
				LispMessage mesg) {
			return go(d, mesg);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(char, net.morilib.lisp.LispMessage)
		 */
		@Override
		public ILispConfiguration goSideEffect(char d,
				LispMessage mesg) {
			return go(d, mesg);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<dfa-state-from-table>");
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.DFAState#getAlphabetRanges()
		 */
		@Override
		public Iterable<Interval> getAlphabetRanges() {
			Set<Interval> r = new HashSet<Interval>();

			for(Datum t : getAlphabets()) {
				r.add(Interval.newPoint(t));
			}
			return r;
		}

	}

	//
	final State DEAD = new State(Collections.<Datum, Datum>emptyMap());

	//
	Map<Datum, State> map;
	State initial;

	//
	LispMooreMachine() {}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFiniteAutomaton#getInitial()
	 */
	@Override
	public ILispDfaState getInitial() {
		return initial;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDatumDfa#getDFA()
	 */
	@Override
	public DFA<Datum, Datum, ?> getDFA() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFiniteAutomaton#getInitialState()
	 */
	@Override
	public DFAState<Datum, Datum, Datum> getInitialState() {
		return initial;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<dfa-datum-state>");
	}

}
