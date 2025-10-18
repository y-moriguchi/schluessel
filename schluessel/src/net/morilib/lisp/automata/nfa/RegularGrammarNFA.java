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

import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.morilib.automata.NFA;
import net.morilib.automata.NFAEdges;
import net.morilib.automata.NFAState;
import net.morilib.automata.TextBound;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.automata.ILispConfiguration;
import net.morilib.lisp.automata.cfg.LispCFG;
import net.morilib.lisp.automata.cfg.LispCFGRule;
import net.morilib.range.Interval;
import net.morilib.range.Range;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/27
 */
public class RegularGrammarNFA extends Datum2
implements ILispNfa, NFA<Datum, Datum, Datum> {

	//
	private LispCFG cfg;

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#isState(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public boolean isState(NFAState o) {
		return cfg.getVariables().contains(o);
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getStates(net.morilib.automata.nfa.NFAState, java.lang.Object)
	 */
	@Override
	public Set<NFAState> getStates(NFAState state, Datum alphabet) {
		Set<NFAState> z = new HashSet<NFAState>();
		List<Datum> l, m;
		Datum a, b, c;

		for(LispCFGRule r : cfg.getRules()) {
			if(!r.getLeftValue().equals(state)) {
				// continue
			} else {
				l = r.getRightValues();
				a = l.get(0);
				if(!(b = l.get(1)).isGrammarVariable()) {
					throw new RuntimeException();
				}

				for(LispCFGRule s : cfg.getRules()) {
					if(!s.getLeftValue().equals(a)) {
						// continue
					} else if((m = s.getRightValues()).isEmpty()) {
						throw new RuntimeException();
					} else if((c = m.get(0)).isGrammarVariable()) {
						throw new RuntimeException();
					} else if(c.equals(alphabet)) {
						z.add((NFAState)b);
					}
				}
			}
		}
		return z;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getStates(net.morilib.automata.nfa.NFAState, net.morilib.range.Range)
	 */
	@Override
	public Set<NFAState> getStates(NFAState state, Range rng) {
		Set<NFAState> z = new HashSet<NFAState>();
		List<Datum> l, m;
		Datum a, b, c;

		for(LispCFGRule r : cfg.getRules()) {
			if(!r.getLeftValue().equals(state)) {
				// continue
			} else {
				l = r.getRightValues();
				a = l.get(0);
				if(!(b = l.get(1)).isGrammarVariable()) {
					throw new RuntimeException();
				}

				for(LispCFGRule s : cfg.getRules()) {
					if(!s.getLeftValue().equals(a)) {
						// continue
					} else if((m = s.getRightValues()).isEmpty()) {
						throw new RuntimeException();
					} else if((c = m.get(0)).isGrammarVariable()) {
						throw new RuntimeException();
					} else if(rng.contains(c)) {
						z.add((NFAState)b);
					}
				}
			}
		}
		return z;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getStates(net.morilib.automata.nfa.NFAState, java.util.EnumSet)
	 */
	@Override
	public Set<NFAState> getStates(NFAState state,
			EnumSet<TextBound> bound) {
		return Collections.emptySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getStatesEpsilon(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public Set<NFAState> getStatesEpsilon(NFAState state) {
		return Collections.emptySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getStatesBound(net.morilib.automata.nfa.NFAState, java.util.EnumSet)
	 */
	@Override
	public Set<NFAState> getStatesBound(NFAState state,
			EnumSet<TextBound> bound) {
		return Collections.emptySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getInitialStates()
	 */
	@Override
	public Set<NFAState> getInitialStates() {
		return Collections.<NFAState>singleton(cfg.getStartVariable());
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#isInitialState(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public boolean isInitialState(NFAState o) {
		return cfg.getStartVariable().equals(o);
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#isFinal(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public boolean isFinal(NFAState state) {
		return isAccepted(state);
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#isFinalAny(java.util.Set)
	 */
	@Override
	public boolean isFinalAny(Set<NFAState> states) {
		for(NFAState s : states) {
			if(isFinal(s))  return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getEdges(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public NFAEdges<Datum> getEdges(NFAState state) {
		final NFAState st = state;

		return new NFAEdges<Datum>() {

			@Override
			public Set<NFAState> goNext(Datum alphabet) {
				return getStates(st, alphabet);
			}

			@Override
			public Set<NFAState> goNext(int alphabet) {
				return getStates(st, LispCharacter.valueOf(alphabet));
			}

			@Override
			public Set<NFAState> goNext(char alphabet) {
				return getStates(st, LispCharacter.valueOf(alphabet));
			}

			@Override
			public Set<NFAState> goNextEpsilon() {
				return getStatesEpsilon(st);
			}

			@Override
			public Set<? extends Range> nextAlphabets() {
				return RegularGrammarNFA.this.nextAlphabets(st);
			}

			@Override
			public boolean isNextEpsilon() {
				return false;
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#nextAlphabets(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public Set<Interval> nextAlphabets(NFAState state) {
		Set<Interval> z = new HashSet<Interval>();

		for(Datum d : nextDiscreteAlphabets(state)) {
			z.add(Interval.newPoint(d));
		}
		return z;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#nextAlphabets(java.util.Set)
	 */
	@Override
	public Iterable<Interval> nextAlphabets(Set<NFAState> states) {
		Set<Interval> z = new HashSet<Interval>();

		for(NFAState s : states)  z.addAll(nextAlphabets(s));
		return z;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#nextDiscreteAlphabets(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public Set<Datum> nextDiscreteAlphabets(NFAState state) {
		Set<Datum> z = new HashSet<Datum>();
		List<Datum> l, m;
		Datum a, c;

		for(LispCFGRule r : cfg.getRules()) {
			if(!r.getLeftValue().equals(state)) {
				// continue
			} else {
				l = r.getRightValues();
				a = l.get(0);
				if(!l.get(1).isGrammarVariable()) {
					throw new RuntimeException();
				}

				for(LispCFGRule s : cfg.getRules()) {
					if(!s.getLeftValue().equals(a)) {
						// continue
					} else if((m = s.getRightValues()).isEmpty()) {
						throw new RuntimeException();
					} else if((c = m.get(0)).isGrammarVariable()) {
						throw new RuntimeException();
					} else {
						z.add(c);
					}
				}
			}
		}
		return z;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#nextDiscreteAlphabets(java.util.Set)
	 */
	@Override
	public Iterable<Datum> nextDiscreteAlphabets(
			Set<NFAState> states) {
		Set<Datum> z = new HashSet<Datum>();

		for(NFAState s : states)  z.addAll(nextDiscreteAlphabets(s));
		return z;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getAcceptedStates()
	 */
	@Override
	public Set<NFAState> getAcceptedStates() {
		Set<NFAState> z = new HashSet<NFAState>();
		List<Datum> l;
		Datum b;

		for(LispCFGRule r : cfg.getRules()) {
			l = r.getRightValues();
			if(!(b = l.get(1)).isGrammarVariable()) {
				throw new RuntimeException();
			}

			for(LispCFGRule s : cfg.getRules()) {
				if(!s.getLeftValue().equals(b)) {
					// continue
				} else if(s.getRightValues().size() == 1) {
					z.add((NFAState)b);
				}
			}
		}
		return z;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getMatchTag(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public Set<Datum> getMatchTag(NFAState state) {
		return Collections.emptySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getMatchTagEnd(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public Set<Datum> getMatchTagEnd(NFAState state) {
		return Collections.emptySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#getAccept(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public Set<Datum> getAccept(NFAState state) {
		if(isAccepted(state)) {
			return Collections.<Datum>singleton(LispBoolean.TRUE);
		} else {
			return Collections.emptySet();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.nfa.NFA#isAccepted(net.morilib.automata.nfa.NFAState)
	 */
	@Override
	public boolean isAccepted(NFAState state) {
		List<Datum> l;
		Datum b;

		for(LispCFGRule r : cfg.getRules()) {
			if(r.getLeftValue().equals(state)) {
				l = r.getRightValues();
				if(!(b = l.get(1)).isGrammarVariable()) {
					throw new RuntimeException();
				}

				for(LispCFGRule s : cfg.getRules()) {
					if(!s.getLeftValue().equals(b)) {
						// continue
					} else if(s.getRightValues().size() == 1) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispStateMachine#getInitial()
	 */
	@Override
	public ILispConfiguration getInitial() {
		return new LispNfaStates(this, getInitialStates());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.nfa.ILispNfa#getNFA()
	 */
	@Override
	public NFA<Datum, ?, ?> getNFA() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<regular-grammar-nfa>");
	}

}
