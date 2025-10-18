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
import net.morilib.lisp.Datum;
import net.morilib.lisp.automata.cfg.LispCFG;
import net.morilib.lisp.automata.dfa.AbstractLispDatumDfa;
import net.morilib.lisp.automata.dfa.ILispDfaState;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/14
 */
public class LispLALR1Automaton extends AbstractLispDatumDfa
implements DFA<Datum, Datum, Datum>, ILispLR1Goto {

	//
	LispCFG cfg;
	LispLR0Automaton lr0fa;
	Map<LispLR0Items, LispLR1Items> lookaheads;
	private transient Map<LispLR0Items, LispLR1Items> cache;
	private transient Map<LispLR1Items, LispLR0Items> reverse;

	/**
	 * 
	 * @param cfg
	 */
	public LispLALR1Automaton(LispCFG cfg) {
		LispLR1Items h;

		this.cfg        = cfg;
		this.lr0fa      = new LispLR0Automaton(cfg);
		this.lookaheads = LispLR1Items.propergateLookahead(lr0fa);
		this.cache      = new HashMap<LispLR0Items, LispLR1Items>();
		this.reverse    = new HashMap<LispLR1Items, LispLR0Items>();
		for(LispLR0Items s : lookaheads.keySet()) {
			if((h = cache.get(s)) == null) {
				h = lookaheads.get(s).getClosure(cfg);
				cache.put(s, h);
			}
			reverse.put(h, s);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLR1Goto#go(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public LispLR1Items go(Datum i, Datum x) {
		LispLR0Items s = lr0fa.go(reverse.get(i), x);
		LispLR1Items h;

		if(s == null) {
			return null;
		} else if((h = cache.get(s)) == null) {
			h = lookaheads.get(s).getClosure(cfg);
			cache.put(s, h);
		}
		return h;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLR1Goto#getAllStates()
	 */
	public Set<LispLR1Items> getAllStates() {
		Set<LispLR1Items> r = new HashSet<LispLR1Items>();
		LispLR1Items h;

		for(LispLR0Items s : lookaheads.keySet()) {
			if((h = cache.get(s)) == null) {
				h = lookaheads.get(s).getClosure(cfg);
				cache.put(s, h);
			}
			r.add(h);
		}
		return r;
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
		return lr0fa.getInitialState();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfa#getInitial()
	 */
	@Override
	public ILispDfaState getInitial() {
		return lr0fa.getInitial();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLR1Goto#getInitialItems()
	 */
	@Override
	public LispLR1Items getInitialItems() {
		LispLR0Items s = lr0fa.getInitialItems();
		LispLR1Items h;

		if((h = cache.get(s)) == null) {
			h = lookaheads.get(s).getClosure(cfg);
			cache.put(s, h);
		}
		return h;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLRGoto#getAlphabets(net.morilib.lisp.Datum)
	 */
	@Override
	public Set<Datum> getAlphabets(Datum s) {
		LispLR0Items x;

		return ((x = reverse.get(s)) == null) ?
				Collections.<Datum>emptySet() : lr0fa.getAlphabets(x);
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
		buf.append("#<cfg-lalr1-goto-automaton>");
	}

}
