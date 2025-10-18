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

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.morilib.automata.DFA;
import net.morilib.automata.DFAState;
import net.morilib.automata.TextBound;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.automata.ILispConfiguration;
import net.morilib.lisp.automata.cfg.LispCFG;
import net.morilib.lisp.automata.dfa.AbstractLispDatumDfa;
import net.morilib.lisp.automata.dfa.ILispDfaState;
import net.morilib.lisp.automata.dfa.LispDfaDeadState;
import net.morilib.range.Interval;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/14
 */
public class LispLR0Automaton extends AbstractLispDatumDfa
implements DFA<Datum, Datum, Datum>, ILispLRGoto {

	//
	private class St extends Datum2
	implements ILispDfaState, DFAState<Datum, Datum, Datum> {

		//
		private LispLR0Items items;

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#go(java.lang.Object)
		 */
		@Override
		public DFAState<Datum, Datum, Datum> go(Datum alphabet) {
			LispLR0Items i;

			i = LispLR0Automaton.this.go(items, alphabet);
			return (i != null) ?
					getinst(i) : LispDfaDeadState.DEAD_STATE;
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#goInt(int)
		 */
		@Override
		public DFAState<Datum, Datum, Datum> goInt(int x) {
			return LispDfaDeadState.DEAD_STATE;
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#goChar(char)
		 */
		@Override
		public DFAState<Datum, Datum, Datum> goChar(char x) {
			return LispDfaDeadState.DEAD_STATE;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.automata.dfa.ILispDfaState#go(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
		 */
		@Override
		public ILispDfaState go(Datum d, LispMessage mesg) {
			LispLR0Items i;

			i = LispLR0Automaton.this.go(items, d);
			return (i != null) ?
					getinst(i) : LispDfaDeadState.DEAD_STATE;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.automata.dfa.ILispDfaState#go(int, net.morilib.lisp.LispMessage)
		 */
		@Override
		public ILispDfaState go(int d, LispMessage mesg) {
			return LispDfaDeadState.DEAD_STATE;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.automata.dfa.ILispDfaState#go(char, net.morilib.lisp.LispMessage)
		 */
		@Override
		public ILispDfaState go(char d, LispMessage mesg) {
			return LispDfaDeadState.DEAD_STATE;
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#goBound(net.morilib.automata.TextBound)
		 */
		@Override
		public DFAState<Datum, Datum, Datum> goBound(TextBound bound) {
			return LispDfaDeadState.DEAD_STATE;
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

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#isInitialState()
		 */
		@Override
		public boolean isInitialState() {
			return items.isStart(cfg);
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#getAccepted()
		 */
		@Override
		public Set<Datum> getAccepted() {
			if(isAccepted()) {
				return Collections.<Datum>singleton(LispBoolean.TRUE);
			} else {
				return Collections.emptySet();
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#isDead()
		 */
		@Override
		public boolean isDead() {
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#isAccepted()
		 */
		@Override
		public boolean isAccepted() {
			return items.isAccepted(cfg);
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#getAlphabets()
		 */
		@Override
		public Set<Datum> getAlphabets() {
			Map<Datum, LispLR0Items> m = graph.get(items);

			return (m != null) ?
					m.keySet() : Collections.<Datum>emptySet();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<state: ").append(items).append(">");
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
	LispCFG cfg;
	Map<LispLR0Items, Map<Datum, LispLR0Items>> graph;
	Map<LispLR0Items, St> states;

	/**
	 * 
	 * @param cfg
	 */
	public LispLR0Automaton(LispCFG cfg) {
		this.cfg    = cfg;
		this.graph  = cfg.lr0Graph();
		this.states = new HashMap<LispLR0Items, St>();
	}

	//
	private synchronized St getinst(LispLR0Items is) {
		St s;

		if((s = states.get(is)) == null) {
			s = new St();
			s.items = is;
			states.put(is, s);
		}
		return s;
	}

	/**
	 * 
	 * @param i
	 * @param x
	 * @return
	 */
	public LispLR0Items go(Datum i, Datum x) {
		Map<Datum, LispLR0Items> m;

		return ((m = graph.get(i)) == null) ? null : m.get(x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDatumDfa#getDFA()
	 */
	@Override
	public DFA<Datum, ?, ?> getDFA() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFA#getInitialState()
	 */
	@Override
	public DFAState<Datum, Datum, Datum> getInitialState() {
		return getinst(cfg.lr0Start());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfa#getInitial()
	 */
	@Override
	public ILispDfaState getInitial() {
		return getinst(cfg.lr0Start());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLR1Goto#getInitialItems()
	 */
	@Override
	public LispLR0Items getInitialItems() {
		return cfg.lr0Start();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLRGoto#getAlphabets(net.morilib.lisp.Datum)
	 */
	@Override
	public Set<Datum> getAlphabets(Datum s) {
		Map<Datum, LispLR0Items> m;

		return ((m = graph.get(s)) == null) ?
				Collections.<Datum>emptySet() : m.keySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLRGoto#getCFG()
	 */
	@Override
	public LispCFG getCFG() {
		return cfg;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<cfg-lr0-goto-automaton>");
	}

}
