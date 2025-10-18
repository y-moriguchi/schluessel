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
package net.morilib.automata.dfa;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import net.morilib.automata.CharSequenceHead;
import net.morilib.automata.DFA;
import net.morilib.automata.DFAState;
import net.morilib.automata.TextBound;
import net.morilib.automata.regular.AlternativeBasicRegex;
import net.morilib.automata.regular.BasicRegex;
import net.morilib.automata.regular.BasicRegexUtils;
import net.morilib.automata.regular.ConcatenateBasicRegex;
import net.morilib.automata.regular.ObjectBasicRegex;
import net.morilib.automata.regular.StarClosureBasicRegex;
import net.morilib.range.Interval;
import net.morilib.util.iterator.LookaheadIterator;
import net.morilib.util.iterator.WrappedLookaheadIterator;
import net.morilib.util.set.PairSet;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class DFAs {

	//
	private DFAs() {
		// do nothing
	}

	/**
	 * 
	 */
	public static final DFAState<Object, Object, Object>
	DEAD_STATE = new DFAState<Object, Object, Object>() {

		public Set<Object> getAccepted() {
			return Collections.emptySet();
		}

		public DFAState<Object, Object, Object> go(Object a) {
			return this;
		}

		public DFAState<Object, Object, Object> goBound(TextBound a) {
			return this;
		}

		public boolean isInitialState() {
			return false;
		}

		public boolean isDead() {
			return true;
		}

		@Override
		public DFAState<Object, Object, Object> goInt(int x) {
			return this;
		}

		@Override
		public DFAState<Object, Object, Object> goChar(char x) {
			return this;
		}

		@Override
		public boolean isAccepted() {
			return false;
		}

		@Override
		public Set<Object> getAlphabets() {
			return Collections.emptySet();
		}

		@Override
		public Iterable<Interval> getAlphabetRanges() {
			return Collections.emptySet();
		}

	};

	/**
	 * 
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static<T, A, B> DFAState<T, A, B> deadState() {
		return (DFAState<T, A, B>)DEAD_STATE;
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<A, B> Set<A> input(
			DFAState<Integer, A, B> st, CharSequenceHead seq) {
		DFAState<Integer, A, B> s2 = st;
		int c;

		while(seq.hasNext()) {
			DFAState<Integer, A, B> s3 = s2;
			EnumSet<TextBound> bs = seq.getBounds();

			c = seq.readInt();
			while((s2 = s2.goInt(c)).isDead()) {
				for(TextBound b : bs) {
					s2 = s3.goBound(b);
					if(!s2.isDead()) {
						s3 = s2;
						break;
					}
				}
				if(s2.isDead()) {
					return Collections.emptySet();
				}
			}

		}

		loop: while(true) {
			DFAState<Integer, A, B> s3 = s2;

			for(TextBound b : seq.getBounds()) {
				s3 = s2.goBound(b);
				if(!s3.isDead()) {
					s2 = s3;
					continue loop;
				}
			}
			break;
		}
		return s2.getAccepted();
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<A, B> Set<A> input(
			DFAState<Integer, A, B> st, CharSequence seq) {
		/*DFAState<Integer, A, B> s2 = st;

		for(int i = 0; i < seq.length(); i++) {
			s2 = s2.go((int)seq.charAt(i));
			if(s2.isDead()) {
				return Collections.emptySet();
			}
		}
		return s2.getAccepted();*/
		return input(st, new CharSequenceHead(seq));
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<A, B> Set<A> input(
			DFA<Integer, A, B> dfa, CharSequenceHead seq) {
		return input(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<A, B> Set<A> input(
			DFA<Integer, A, B> dfa, CharSequence seq) {
		return input(dfa, new CharSequenceHead(seq));
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<A, B> Set<A> match(
			DFAState<Integer, A, B> st, CharSequenceHead seq) {
		DFAState<Integer, A, B> s2 = st;
		int c;

		while(seq.hasNext()) {
			DFAState<Integer, A, B> s3 = s2;
			EnumSet<TextBound> bs = seq.getBounds();

			c = seq.readInt();
			while((s2 = s2.goInt(c)).isDead()) {
				for(TextBound b : bs) {
					s2 = s3.goBound(b);
					if(!s2.isDead()) {
						s3 = s2;
						break;
					}
				}

				if(s2.isDead()) {
					seq.unread();
					return s3.getAccepted();
				}
			}
		}

		loop: while(true) {
			DFAState<Integer, A, B> s3 = s2;

			for(TextBound b : seq.getBounds()) {
				s3 = s2.goBound(b);
				if(!s3.isDead()) {
					s2 = s3;
					continue loop;
				}
			}
			break;
		}
		return s2.getAccepted();
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<A, B> Set<A> match(
			DFAState<Integer, A, B> st, CharSequence seq) {
		return match(st, new CharSequenceHead(seq));
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<A, B> Set<A> match(
			DFA<Integer, A, B> dfa, CharSequenceHead seq) {
		return match(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<A, B> Set<A> match(
			DFA<Integer, A, B> dfa, CharSequence seq) {
		return match(dfa, new CharSequenceHead(seq));
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<A, B> boolean isMatched(
			DFAState<Integer, A, B> st, CharSequenceHead seq) {
		return !match(st, seq).isEmpty();
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<A, B> boolean isMatched(
			DFAState<Integer, A, B> st, CharSequence seq) {
		return isMatched(st, new CharSequenceHead(seq));
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<A, B> boolean isMatched(
			DFA<Integer, A, B> dfa, CharSequenceHead seq) {
		return isMatched(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<A, B> boolean isMatched(
			DFA<Integer, A, B> dfa, CharSequence seq) {
		return isMatched(dfa, new CharSequenceHead(seq));
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> match(
			DFAState<T, A, ?> st, LookaheadIterator<T> seq) {
		DFAState<T, A, ?> s2 = st;
		T c;

		while(seq.hasNext()) {
			DFAState<T, A, ?> s3 = s2;

			c = seq.peek();
			if((s2 = s2.go(c)).isDead()) {
				return s3.getAccepted();
			} else {
				seq.next();
			}
		}
		return s2.getAccepted();
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> match(
			DFAState<T, A, ?> st, Collection<T> seq) {
		return match(st,
				new WrappedLookaheadIterator<T>(seq.iterator()));
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> match(DFAState<T, A, ?> st, T... seq) {
		return match(st, Arrays.asList(seq));
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> match(
			DFA<T, A, ?> dfa, LookaheadIterator<T> seq) {
		return match(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> match(
			DFA<T, A, ?> dfa, Collection<T> seq) {
		return match(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> match(DFA<T, A, ?> dfa, T... seq) {
		return match(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T, A> boolean isMatched(
			DFAState<T, A, ?> st, LookaheadIterator<T> seq) {
		return !match(st, seq).isEmpty();
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T, A> boolean isMatched(
			DFAState<T, A, ?> st, Collection<T> seq) {
		return !match(st, seq).isEmpty();
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T, A> boolean isMatched(
			DFAState<T, A, ?> st, T... seq) {
		return !match(st, seq).isEmpty();
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T, A> boolean isMatched(
			DFA<T, A, ?> dfa, LookaheadIterator<T> seq) {
		return !match(dfa, seq).isEmpty();
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T, A> boolean isMatched(
			DFA<T, A, ?> dfa, Collection<T> seq) {
		return !match(dfa, seq).isEmpty();
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T, A> boolean isMatched(DFA<T, A, ?> dfa, T... seq) {
		return !match(dfa, seq).isEmpty();
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> matchAll(DFAState<T, A, ?> st,
			Iterator<T> seq) {
		DFAState<T, A, ?> s2 = st;
		T c;

		while(seq.hasNext()) {
			c = seq.next();
			if((s2 = s2.go(c)).isDead()) {
				return Collections.emptySet();
			}
		}
		return s2.getAccepted();
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> matchAll(
			DFAState<T, A, ?> st, Collection<T> seq) {
		return matchAll(st, seq.iterator());
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> matchAll(DFAState<T, A, ?> st,
			T... seq) {
		return matchAll(st, Arrays.asList(seq));
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> matchAll(DFA<T, A, ?> dfa,
			Iterator<T> seq) {
		return matchAll(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> matchAll(
			DFA<T, A, ?> dfa, Collection<T> seq) {
		return matchAll(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T, A> Set<A> matchAll(DFA<T, A, ?> dfa, T... seq) {
		return matchAll(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T> boolean isMatchAll(DFAState<T, ?, ?> st,
			Iterator<T> seq) {
		DFAState<T, ?, ?> s2 = st;
		T c;

		while(seq.hasNext()) {
			c = seq.next();
			if((s2 = s2.go(c)).isDead()) {
				return false;
			}
		}
		return !s2.getAccepted().isEmpty();
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T> boolean isMatchAll(
			DFAState<T, ?, ?> st, Collection<T> seq) {
		return isMatchAll(st, seq.iterator());
	}

	/**
	 * 
	 * @param st
	 * @param seq
	 * @return
	 */
	public static<T> boolean isMatchAll(DFAState<T, ?, ?> st,
			T... seq) {
		return isMatchAll(st, Arrays.asList(seq));
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T> boolean isMatchAll(DFA<T, ?, ?> dfa,
			Iterator<T> seq) {
		return isMatchAll(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T> boolean isMatchAll(
			DFA<T, ?, ?> dfa, Collection<T> seq) {
		return isMatchAll(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param dfa
	 * @param seq
	 * @return
	 */
	public static<T> boolean isMatchAll(DFA<T, ?, ?> dfa, T... seq) {
		return isMatchAll(dfa.getInitialState(), seq);
	}

	/**
	 * 
	 * @param dfa
	 * @return
	 */
	public static<T> List<DFAState<T, ?, ?>> allStates(
			DFA<T, ?, ?> dfa) {
		List<DFAState<T, ?, ?>> l = new ArrayList<DFAState<T, ?, ?>>();
		DFAState<T, ?, ?> s;

		l.add(dfa.getInitialState());
		for(int i = 0; i < l.size(); i++) {
			for(T o : l.get(i).getAlphabets()) {
				s = l.get(i).go(o);
				if(!l.contains(s))  l.add(s);
			}
		}
		return l;
	}

	// R^(k)_ij
	private static<T> void writere(BasicRegex[][][] tbl,
			List<DFAState<T, ?, ?>> l,
			int m) {
		int n;

		for(int i = 0; i < tbl[m].length; i++) {
			for(T o : l.get(i).getAlphabets()) {
				n = l.indexOf(l.get(i).go(o));
				if(tbl[m][i][n] == null) {
					tbl[m][i][n] = new ObjectBasicRegex(o);
				} else {
					tbl[m][i][n] = new AlternativeBasicRegex(
							new ObjectBasicRegex(o),
							tbl[m][i][n]);
				}
			}
		}
	}

	/**
	 * 
	 * @param dfa
	 * @return
	 */
	public static<T> BasicRegex convertToRegex(DFA<T, ?, ?> dfa) {
		List<DFAState<T, ?, ?>> l = allStates(dfa);
		BasicRegex[][][] tbl =
				new BasicRegex[l.size() + 1][l.size()][l.size()];
		BasicRegex e, f, g, h;
		int k;

		for(int m = 0; m < tbl.length; m++) {
			for(int i = 0; i < tbl[m].length; i++) {
				for(int j = 0; j < tbl[m][i].length; j++) {
					if(m == 0 && i == j) {
						tbl[m][i][j] = BasicRegexUtils.EPSILON;
					} else {
						tbl[m][i][j] = BasicRegexUtils.NIHIL;
					}
				}
			}
		}

		for(int m = 0; m < tbl.length; m++) {
			if(m == 0) {
				writere(tbl, l, m);   // R^(k)_ij
			} else {
				k = m - 1;
				for(int i = 0; i < tbl[m].length; i++) {
					for(int j = 0; j < tbl[m][i].length; j++) {
						tbl[m][i][j] = tbl[k][i][j];
					}
				}

				for(int i = 0; i < tbl[m].length; i++) {
					for(int j = 0; j < tbl[m][i].length; j++) {
						e = tbl[m    ][i][j];
						f = tbl[m - 1][i][k];
						g = tbl[m - 1][k][k];
						h = tbl[m - 1][k][j];
						g = new StarClosureBasicRegex(g);
						f = new ConcatenateBasicRegex(f, g, h);
						e = new AlternativeBasicRegex(e, f);
						tbl[m][i][j] = e.simplify();
					}
				}
			}
		}

		e = BasicRegexUtils.NIHIL;
		for(int i = 0; i < l.size(); i++) {
			if(l.get(i).isAccepted()) {
				f = tbl[tbl.length - 1][0][i];
				e = new AlternativeBasicRegex(f, e);
			}
		}
		return e.simplify();
	}

	/**
	 * 
	 * @param dfa
	 * @return
	 */
	public static<T>
	Set<DFAState<T, ?, ?>> getReachableStatesDiscrete(
			DFA<T, ?, ?> dfa) {
		Set<DFAState<T, ?, ?>> s = new HashSet<DFAState<T, ?, ?>>();
		Stack<DFAState<T, ?, ?>> st = new Stack<DFAState<T, ?, ?>>();
		DFAState<T, ?, ?> x, y;

		s.add(dfa.getInitialState());
		st.push(dfa.getInitialState());
		while(!st.isEmpty()) {
			x = st.pop();
			for(T a : x.getAlphabets()) {
				y = x.go(a);
				if(!y.isDead() && !s.contains(y)) {
					s.add(y);
					st.push(y);
				}
			}
		}
		return s;
	}

	/**
	 * 
	 * @param dfa
	 * @return
	 */
	public static<T> boolean isEmptyDiscrete(DFA<T, ?, ?> dfa) {
		Set<DFAState<T, ?, ?>> s = getReachableStatesDiscrete(dfa);

		for(DFAState<T, ?, ?> t : s) {
			if(t.isAccepted())  return false;
		}
		return true;
	}

	//
	private static<T> boolean finddist(
			Set<PairSet<DFAState<T, ?, ?>>> st,
			DFAState<T, ?, ?> a, DFAState<T, ?, ?> b) {
		if(a.isDead() && b.isDead()) {
			return false;
		} else if(a.isDead() || b.isDead()) {
			return true;
		} else if(a.equals(b)) {
			return false;
		} else {
			for(PairSet<DFAState<T, ?, ?>> p : st) {
				if(p.isPair(a, b))  return false;
			}
			return true;
		}
	}

	//
	static<T> Set<PairSet<DFAState<T, ?, ?>>> fillTable(
			List<DFAState<T, ?, ?>> l) {
		Iterator<PairSet<DFAState<T, ?, ?>>> itr;
		Set<PairSet<DFAState<T, ?, ?>>> r, s;
		PairSet<DFAState<T, ?, ?>> p;
		DFAState<T, ?, ?> a, b;
		boolean f, dirty = true;

		// remove accept and non-accept states
		r = new HashSet<PairSet<DFAState<T, ?, ?>>>();
		for(int i = 0; i < l.size(); i++) {
			for(int j = i + 1; j < l.size(); j++) {
				if(l.get(i).isAccepted() == l.get(j).isAccepted()) {
					r.add(new PairSet<DFAState<T, ?, ?>>(
							l.get(i), l.get(j)));
				}
			}
		}

		// remove distinguishable states
		while(dirty) {
			dirty = false;
			s = new HashSet<PairSet<DFAState<T, ?, ?>>>(r);
			itr = s.iterator();
			while(itr.hasNext()) {
				f = false;
				p = itr.next();
				a = p.get1();  b = p.get2();

				for(T t : a.getAlphabets()) {
					f |= finddist(r, a.go(t), b.go(t));
				}

				for(T t : b.getAlphabets()) {
					f |= finddist(r, a.go(t), b.go(t));
				}

				if(f) {
					itr.remove();
					dirty = true;
				}
			}
			r = new HashSet<PairSet<DFAState<T, ?, ?>>>(s);
		}
		return r;
	}

	/**
	 * 
	 * @param dfa
	 * @return
	 */
	public static<T> Set<PairSet<DFAState<T, ?, ?>>>
	getEquivalentStatesDiscrete(DFA<T, ?, ?> dfa) {
		List<DFAState<T, ?, ?>> l;

		l = new ArrayList<DFAState<T, ?, ?>>(
				getReachableStatesDiscrete(dfa));
		return fillTable(l);
	}

	/**
	 * 
	 * @param dfa1
	 * @param dfa2
	 * @return
	 */
	public static<T> boolean isEquivalentDiscrete(DFA<T, ?, ?> dfa1,
			DFA<T, ?, ?> dfa2) {
		Set<PairSet<DFAState<T, ?, ?>>> r;
		List<DFAState<T, ?, ?>> l;
		Set<DFAState<T, ?, ?>> s, t;

		if(dfa1.equals(dfa2)) {
			return true;
		} else if(isEmptyDiscrete(dfa1)) {
			return isEmptyDiscrete(dfa2);
		} else if(isEmptyDiscrete(dfa2)) {
			return false;
		}

		l = new ArrayList<DFAState<T, ?, ?>>();
		l.addAll(s = getReachableStatesDiscrete(dfa1));
		l.addAll(t = getReachableStatesDiscrete(dfa2));
		r = fillTable(l);

		for(DFAState<T, ?, ?> x : s) {
			for(PairSet<DFAState<T, ?, ?>> a : r) {
				if(a.contains(x))  t.remove(a.getOpposite(x));
			}
		}
		return t.isEmpty();
	}

}
