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
package net.morilib.lisp.automata.cfg;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lang.Hashes;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.automata.ILispFormalGrammar;
import net.morilib.lisp.automata.LispAutomataUtils;
import net.morilib.lisp.automata.LispGrammarVariable;
import net.morilib.lisp.automata.lr.LispLR0Item;
import net.morilib.lisp.automata.lr.LispLR0Items;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/06
 */
public class LispCFG extends Datum2 implements ILispFormalGrammar {

	//
	private LispGrammarVariable start;
	private Set<LispCFGRule> rules;
	private transient Map<Datum, Set<Datum>> firsts = null;
	private transient Map<Datum, Set<Datum>> follows = null;

	/**
	 * 
	 * @param start
	 * @param rules
	 */
	public LispCFG(LispGrammarVariable start, Set<LispCFGRule> rules) {
		this.start = start;
		this.rules = new HashSet<LispCFGRule>(rules);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#getRules()
	 */
	@Override
	public Set<LispCFGRule> getRules() {
		return Collections.unmodifiableSet(rules);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#getStartVariable()
	 */
	@Override
	public LispGrammarVariable getStartVariable() {
		return start;
	}

	//
	boolean isNullableRecursive(Set<Datum> s, Datum v) {
		outer: for(LispCFGRule r : rules) {
			if(r.getLeftValue().equals(v)) {
				for(Datum x : r.getRightValues()) {
					if(!x.isGrammarVariable()) {
						continue outer;
					} else if(s.contains(x)) {
						continue outer;
					}

					s.add(v);
					if(!isNullableRecursive(s, x)) {
						continue outer;
					}
				}
				return true;
			}
		}
		return false;
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public boolean isNullableRecursive(Datum v) {
		return isNullableRecursive(new HashSet<Datum>(), v);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isNullable() {
		return isNullableRecursive(start);
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public boolean isNullable(Datum v) {
		if(!v.isGrammarVariable())  return false;
		for(LispCFGRule r : rules) {
			if(r.getLeftValue().equals(v) &&
					r.getRightValues().isEmpty()) {
				return true;
			}
		}
		return false;
	}

	//
	private Set<LispCFGRule> getNullableR(LispCFGRule r) {
		List<List<Datum>> w = new ArrayList<List<Datum>>();
		Set<LispCFGRule> rt = new HashSet<LispCFGRule>();
		List<Datum> a;
		Datum x;

		w.add(new ArrayList<Datum>());
		a = r.getRightValues();
		for(int i = 0; i < a.size(); i++) {
			x = a.get(i);
			for(List<Datum> l : w)  l.add(x);
			if(isNullable(x)) {
				w.add(new ArrayList<Datum>(a.subList(0, i)));
			}
		}

		for(List<Datum> l : w) {
			rt.add(new LispCFGRule(r.getLeftValue(), l));
		}
		return rt;
	}

	//
	private Set<LispCFGRule> getUnitR(LispCFGRule r) {
		Set<LispCFGRule> rt = new HashSet<LispCFGRule>();
		Datum d;

		for(LispCFGRule s : rules) {
			d = r.getRightValues().get(0);
			if(d.equals(s.getLeftValue()) && !s.isUnitRule()) {
				rt.add(new LispCFGRule(
						r.getLeftValue(), s.getRightValues()));
			}
		}
		return rt;
	}

	/**
	 * 
	 * @return
	 */
	public LispCFG eliminateEpsilonRules() {
		Set<LispCFGRule> z = new HashSet<LispCFGRule>();

		for(LispCFGRule r : rules) {
			if(!r.getRightValues().isEmpty()) {
				z.addAll(getNullableR(r));
			}
		}
		return new LispCFG(start, z);
	}

	/**
	 * 
	 * @return
	 */
	public LispCFG eliminateUnitRules() {
		Set<LispCFGRule> rt = rules, z;
		boolean dirty = true;

		for(; dirty; rt = z) {
			dirty = false;
			z = new HashSet<LispCFGRule>();
			for(LispCFGRule r : rt) {
				if(r.isUnitRule()) {
					z.addAll(getUnitR(r));
					dirty = true;
				} else {
					z.add(r);
				}
			}
		}
		return new LispCFG(start, rt);
	}

	//
	private void substterm(Map<Datum, LispCFGRule> tr, List<Datum> l,
			Set<LispCFGRule> rt) {
		LispCFGRule s;
		Datum x;

		// A -> BcD to A -> BED and E -> c
		for(int i = 0; i < l.size(); i++) {
			x = l.get(i);
			if(!x.isGrammarVariable()) {
				if((s = tr.get(x)) == null) {
					s = new LispCFGRule(
							LispGrammarVariable.genvar(),
							Collections.singletonList(x));
					tr.put(x, s);
					rt.add(s);
				}
				l.set(i, s.getLeftValue());
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	public LispCFG toCNF() {
		Map<Datum, LispCFGRule> tr = new HashMap<Datum, LispCFGRule>();
		Set<LispCFGRule> rt = new HashSet<LispCFGRule>();
		LispGrammarVariable v = null, w = null;
		List<Datum> l, m;
		LispCFG cfg;

		cfg = eliminateEpsilonRules().eliminateUnitRules();
		for(LispCFGRule r : cfg.rules) {
			l = new ArrayList<Datum>(r.getRightValues());

			if(l.size() > 2) {
				substterm(tr, l, rt);

				// A -> BCD to A -> BE_1 and E_1 -> CD
				for(int i = 0; i < l.size() - 1; i++) {
					m = new ArrayList<Datum>();
					if(i == 0) {
						v = LispGrammarVariable.genvar();
						m.add(l.get(i));
						m.add(v);
						rt.add(new LispCFGRule(r.getLeftValue(), m));
					} else if(i == l.size() - 2) {
						m.add(l.get(i));
						m.add(l.get(i + 1));
						rt.add(new LispCFGRule(v, m));
					} else {
						w = LispGrammarVariable.genvar();
						m.add(l.get(i));
						m.add(w);
						rt.add(new LispCFGRule(v, m));
						v = w;
					}
				}
			} else if(l.size() == 2) {
				substterm(tr, l, rt);
				rt.add(new LispCFGRule(r.getLeftValue(), l));
			} else {
				rt.add(r);
			}
		}
		return new LispCFG(start, rt);
	}

	//
	private boolean putfirst(Datum x, Datum y) {
		Set<Datum> s;

		if((s = firsts.get(x)) == null) {
			s = new HashSet<Datum>();
			firsts.put(x, s);
		}
		return s.add(y);
	}

	//
	private Set<Datum> getfirst(Datum x) {
		Set<Datum> s;

		return ((s = firsts.get(x)) != null) ?
				s : Collections.<Datum>emptySet();
	}

	//
	boolean computeFirst1Step() {
		List<Datum> l;
		Set<Datum> s;
		Datum b;
		boolean c = false;

		outer: for(LispCFGRule r : rules) {
			b = r.getLeftValue();
			l = r.getRightValues();
			for(Datum x : l) {
				if(!x.isGrammarVariable()) {
					c |= putfirst(b, x);
					continue outer;
				} else {
					s = getfirst(x);

					// add FIRST(y) except epsilon
					for(Datum y : s) {
						if(!y.equals(LispAutomataUtils.EPSILON)) {
							c |= putfirst(b, y);
						}
					}
					if(!isNullableRecursive(x))  continue outer;
				}
			}
			c |= putfirst(b, LispAutomataUtils.EPSILON);
		}
		return c;
	}

	//
	void computeFirst() {
		firsts = new HashMap<Datum, Set<Datum>>();
		while(computeFirst1Step());
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public Set<Datum> getFirst(Datum d) {
		if(firsts == null)  computeFirst();
		if(d.isGrammarVariable()) {
			return new HashSet<Datum>(getfirst(d));
		} else {
			return Collections.singleton(d);
		}
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public Set<Datum> getFirst(List<Datum> l) {
		Set<Datum> s = new HashSet<Datum>(), t;

		if(firsts == null)  computeFirst();
		for(Datum x : l) {
			if(x != null && !x.isGrammarVariable()) {
				s.add(x);
				return s;
			} else {
				t = getfirst(x);
				for(Datum y : t) {
					if(!y.equals(LispAutomataUtils.EPSILON)) {
						s.add(y);
					}
				}
				if(!isNullableRecursive(x))  return s;
			}
		}
		s.add(LispAutomataUtils.EPSILON);
		return s;
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public Set<Datum> getFirst(Datum... l) {
		return getFirst(Arrays.asList(l));
	}

	//
	private boolean putfollow(Datum x, Set<Datum> y) {
		Set<Datum> s;
		boolean c = false;

		if((s = follows.get(x)) == null) {
			s = new HashSet<Datum>();
			follows.put(x, s);
		}

		for(Datum a : y) {
			if(!a.equals(LispAutomataUtils.EPSILON))  c |= s.add(a);
		}
		return c;
	}

	//
	private Set<Datum> getfollow(Datum x) {
		Set<Datum> s;

		return ((s = follows.get(x)) != null) ?
				s : Collections.<Datum>emptySet();
	}

	//
	boolean computeFollow1Step() {
		List<Datum> l;
		Set<Datum> s;
		Datum b;
		boolean c = false;

		for(LispCFGRule r : rules) {
			b = r.getLeftValue();
			l = r.getRightValues();
			for(int i = 0; i < l.size(); i++) {
				s = getFirst(l.subList(i + 1, l.size()));
				if(i < l.size() - 1) {
					c |= putfollow(l.get(i), s);
				}

				if(s.contains(LispAutomataUtils.EPSILON)) {
					c |= putfollow(l.get(i), getfollow(b));
				}
			}
		}
		return c;
	}

	//
	void computeFollow() {
		follows = new HashMap<Datum, Set<Datum>>();
		putfollow(start,
				Collections.singleton(LispAutomataUtils.ENDMARKER));
		while(computeFollow1Step());
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public Set<Datum> getFollow(Datum d) {
		if(follows == null)  computeFollow();
		if(d.isGrammarVariable()) {
			return new HashSet<Datum>(getfollow(d));
		} else {
			throw new IllegalArgumentException();
		}
	}

	/**
	 * 
	 * @return
	 */
	public Set<LispGrammarVariable> getVariables() {
		Set<LispGrammarVariable> s;

		s = new HashSet<LispGrammarVariable>();
		for(LispCFGRule r : rules) {
			s.add(r.getLeftValue());
			for(Datum x : r.getRightValues()) {
				if(x.isGrammarVariable()) {
					s.add((LispGrammarVariable)x);
				}
			}
		}
		return s;
	}

	/**
	 * 
	 * @return
	 */
	public Set<Datum> getTerminals() {
		Set<Datum> s;

		s = new HashSet<Datum>();
		for(LispCFGRule r : rules) {
			for(Datum x : r.getRightValues()) {
				if(!x.isGrammarVariable())  s.add(x);
			}
		}
		return s;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isValid() {
		Set<LispGrammarVariable> s = getVariables();
		boolean c = true;

		while(c) {
			c = false;
			outer: for(LispCFGRule r : rules) {
				for(Datum x : r.getRightValues()) {
					if(x.isGrammarVariable() && s.contains(x)) {
						// unsolved
						continue outer;
					}
				}
				c = s.remove(r.getLeftValue());
			}
		}
		return s.isEmpty();
	}

	//
	private void getvariables2(List<LispGrammarVariable> l, Datum d) {
		for(LispCFGRule r : rules) {
			if(r.getLeftValue().equals(d)) {
				for(Datum x : r.getRightValues()) {
					if(x.isGrammarVariable() && !l.contains(x)) {
						l.add((LispGrammarVariable)x);
						getvariables2(l, x);
					}
				}
			}
		}
	}

	//
	Set<LispCFGRule> eliminateDirectLeftRecursion(Set<LispCFGRule> rs,
			Datum a) {
		LispGrammarVariable v = LispGrammarVariable.genvar();
		Iterator<LispCFGRule> i;
		Set<LispCFGRule> rt = new HashSet<LispCFGRule>();
		LispCFGRule r, n;
		List<Datum> l, m;

		i = rs.iterator();
		while(i.hasNext()) {
			r = i.next();
			n = null;
			l = new ArrayList<Datum>();
			if(!r.getLeftValue().equals(a)) {
				// do nothing
			} else if(r.isEpsilonRule()) {
				// do nothing
			} else if(r.isLeftRecursive()) {
				m = r.getRightValues();
				l.addAll(m.subList(1, m.size()));
				l.add(v);
				n = new LispCFGRule(v, l);
				i.remove();
			} else {
				l.addAll(r.getRightValues());
				l.add(v);
				n = new LispCFGRule(r.getLeftValue(), l);
				i.remove();
			}
			if(n != null)  rt.add(n);
		}
		rt.add(new LispCFGRule(v, Collections.<Datum>emptyList()));
		return rt;
	}

	/**
	 * 
	 * @return
	 */
	public LispCFG eliminateLeftRecursion() {
		LispCFG ee = eliminateEpsilonRules(), rr;
		Iterator<LispCFGRule> itr;
		Set<LispCFGRule> s, t;
		LispCFGRule r, e;
		List<LispGrammarVariable> ll;
		List<Datum> l, m;
		boolean c;

		ll = new ArrayList<LispGrammarVariable>();
		s  = new HashSet<LispCFGRule>(ee.getRules());
		ll.add(start);
		getvariables2(ll, start);
		for(int i = 0; i < ll.size(); i++) {
			for(int j = 0; j < i; j++) {
				t   = new HashSet<LispCFGRule>();
				itr = s.iterator();
				while(itr.hasNext()) {
					r = itr.next();
					m = r.getRightValues();

					// replace A_i -> A_j z to A_i -> x_1j z ...
					if(r.getLeftValue().equals(ll.get(i)) &&
							m.get(0).equals(ll.get(j))) {
						c = false;
						for(LispCFGRule a : s) {
							if(a.getLeftValue().equals(ll.get(j))) {
								l = new ArrayList<Datum>();
								l.addAll(a.getRightValues());
								l.addAll(m.subList(1, m.size()));
								e = new LispCFGRule(
										r.getLeftValue(), l);
								if(!s.contains(e)) {
									t.add(e);
									c = true;
								}
							}
						}
						if(c)  itr.remove();
					}
				}
				s.addAll(t);
			}
			s.addAll(eliminateDirectLeftRecursion(s, ll.get(i)));
		}
		rr = new LispCFG(start, s);
		rr = rr.eliminateUselessRules();
		rr = rr.addEpsilonRules();
		rr = rr.eliminateCommonRules();
		return rr;
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public boolean isUseless(Datum v) {
		boolean a = false;

		if(!v.isGrammarVariable())  return false;
		for(LispCFGRule r : rules) {
			if(r.getLeftValue().equals(v)) {
				if(r.getRightValues().isEmpty()) {
					a = true;
				} else {
					return false;
				}
			}
		}
		return a;
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public boolean isUnused(Datum v) {
		if(!v.isGrammarVariable())  return false;
		for(LispCFGRule r : rules) {
			if(r.getLeftValue().equals(v))  return false;
		}
		return true;
	}

	/**
	 * 
	 * @return
	 */
	public LispCFG eliminateUselessRules() {
		Set<LispCFGRule> ss = new HashSet<LispCFGRule>();
		List<Datum> l, m;

		for(LispCFGRule r : rules) {
			if(!isUseless(r.getLeftValue())) {
				l = r.getRightValues();
				m = new ArrayList<Datum>(l);
				for(int i = 0; i < l.size(); i++) {
					if(isUseless(l.get(i)))  m.remove(i);
				}
				ss.add(new LispCFGRule(r.getLeftValue(), m));
			}
		}
		return new LispCFG(start, ss);
	}

	/**
	 * 
	 * @return
	 */
	public LispCFG addEpsilonRules() {
		Set<LispCFGRule> ss;
		List<Datum> l, m;

		ss = new HashSet<LispCFGRule>(rules);
		for(LispCFGRule r : rules) {
			l = r.getRightValues();
			outer: for(int i = 0; i < l.size(); i++) {
				if(l.get(i).isGrammarVariable()) {
					m = new ArrayList<Datum>(l);
					m.remove(i);
					for(LispCFGRule t : rules) {
						if(t.getLeftValue().equals(r.getLeftValue()) &&
								t.getRightValues().equals(m)) {
							ss.remove(t);
							ss.add(new LispCFGRule(
									(LispGrammarVariable)l.get(i),
									Collections.<Datum>emptyList()));
							continue outer;
						}
					}
				}
			}
		}
		return new LispCFG(start, ss);
	}

	//
	private static Set<List<Datum>> containsrules(
			Set<LispCFGRule> rules,
			LispGrammarVariable x,
			LispGrammarVariable y) {
		Set<List<Datum>> s, t;

		s = new HashSet<List<Datum>>();
		for(LispCFGRule r : rules) {
			if(r.getLeftValue().equals(x)) {
				s.add(r.getRightValues());
			}
		}

		t = new HashSet<List<Datum>>();
		for(LispCFGRule r : rules) {
			if(r.getLeftValue().equals(y)) {
				t.add(r.getRightValues());
			}
		}
		return s.containsAll(t) ? t : null;
	}

	/**
	 * 
	 * @return
	 */
	public LispCFG eliminateCommonRules() {
		List<LispGrammarVariable> ll;
		Set<LispCFGRule> s;
		Set<List<Datum>> t;
		LispGrammarVariable ii, jj;

		s  = new HashSet<LispCFGRule>(rules);
		ll = new ArrayList<LispGrammarVariable>();
		ll.add(start);
		getvariables2(ll, start);
		for(int i = 0; i < ll.size(); i++) {
			for(int j = 0; j < ll.size(); j++) {
				if(i == j)  continue;
				ii = ll.get(i);
				jj = ll.get(j);
				t  = containsrules(s, ii, jj);
				if(t != null) {
					for(List<Datum> r : t) {
						s.remove(new LispCFGRule(ii, r));
					}
					s.add(new LispCFGRule(
							ii,
							Collections.<Datum>singletonList(jj)));
				}
			}
		}
		return new LispCFG(start, s);
	}

	//
	private static<T> List<T> commlist(List<T> a, List<T> b) {
		List<T> l = new ArrayList<T>();

		for(int i = 0; i < a.size() && i < b.size(); i++) {
			if(a.get(i).equals(b.get(i))) {
				l.add(a.get(i));
			} else {
				return l;
			}
		}
		return l;
	}

	//
	private static<T> List<T> startwith(List<T> a, List<T> b) {
		int i;

		for(i = 0; i < a.size() && i < b.size(); i++) {
			if(!a.get(i).equals(b.get(i)))  return null;
		}
		return (i < b.size()) ? null : a.subList(i, a.size());
	}

	/**
	 * 
	 * @return
	 */
	public LispCFG leftFactor() {
		Set<LispCFGRule> ss, rr;
		List<LispGrammarVariable> ll;
		LispGrammarVariable v;
		List<Datum> l, m;
		int c;

		ss = new HashSet<LispCFGRule>(rules);
		ll = new ArrayList<LispGrammarVariable>();
		ll.add(start);
		getvariables2(ll, start);
		for(int i = 0; i < ll.size(); i++) {
			l = null;
			c = 0;
			for(LispCFGRule t : ss) {
				if(!t.getLeftValue().equals(ll.get(i))) {
					// do nothing
				} else if(l == null) {
					c++;
					l = t.getRightValues();
				} else {
					m = commlist(l, t.getRightValues());
					if(!m.isEmpty()) {
						c++;  l = m;
					}
				}
			}

			if(!l.isEmpty() && c > 1) {
				v  = LispGrammarVariable.genvar();
				rr = new HashSet<LispCFGRule>(ss);
				for(LispCFGRule t : ss) {
					if(!t.getLeftValue().equals(ll.get(i))) {
						// do nothing
					} else {
						m = startwith(t.getRightValues(), l);
						if(m != null) {
							rr.add(new LispCFGRule(v, m));
							rr.remove(t);
						}
					}
				}
				l = new ArrayList<Datum>(l);
				l.add(v);
				rr.add(new LispCFGRule(ll.get(i), l));
				ss = rr;
				if(!ll.contains(v))  ll.add(v);
			}
		}
		return new LispCFG(start, ss);
	}

	/**
	 * 
	 * @return
	 */
	public LispCFG augument() {
		Set<LispCFGRule> ss;
		LispGrammarVariable v = LispGrammarVariable.genvar();

		ss = new HashSet<LispCFGRule>(rules);
		ss.add(new LispCFGRule(
				v, Collections.<Datum>singletonList(start)));
		return new LispCFG(v, ss);
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public Set<LispCFGRule> findRules(LispGrammarVariable a) {
		Set<LispCFGRule> s = new HashSet<LispCFGRule>();

		for(LispCFGRule r : rules) {
			if(r.getLeftValue().equals(a))  s.add(r);
		}
		return s;
	}

	//
	private Set<LispLR0Item> finditems(LispGrammarVariable a) {
		Set<LispLR0Item> s = new HashSet<LispLR0Item>();

		for(LispCFGRule r : rules) {
			if(r.getLeftValue().equals(a)) {
				s.add(new LispLR0Item(r, 0));
			}
		}
		return s;
	}

	/**
	 * 
	 * @param is
	 * @param x
	 * @return
	 */
	public LispLR0Items lr0Goto(LispLR0Items is, Datum x) {
		Set<LispLR0Item> js;

		js = new HashSet<LispLR0Item>();
		for(LispLR0Item i : is.getItems()) {
			if(x.equals(i.getIndicated())) {
				js.add(i.shiftRight());
			}
		}
		return new LispLR0Items(LispLR0Items.getClosure(this, js));
	}

	//
	private void putlr0goto(
			Map<LispLR0Items, Map<Datum, LispLR0Items>> m,
			LispLR0Items i, Datum x, LispLR0Items j) {
		Map<Datum, LispLR0Items> a = m.get(i);

		if(a == null) {
			a = new HashMap<Datum, LispLR0Items>();
			m.put(i, a);
		}
		a.put(x, j);
	}

	/**
	 * 
	 * @return
	 */
	public Map<LispLR0Items, Map<Datum, LispLR0Items>> lr0Graph() {
		Map<LispLR0Items, Map<Datum, LispLR0Items>> g;
		Set<LispLR0Items> sc, sd;
		LispLR0Items g0;
		Set<Datum> vs;
		boolean c = true;

		g  = new HashMap<LispLR0Items, Map<Datum, LispLR0Items>>();
		sc = new HashSet<LispLR0Items>();
		sc.add(lr0Start());
		vs = new HashSet<Datum>();
		vs.addAll(getVariables());
		vs.addAll(getTerminals());
		while(c) {
			c  = false;
			sd = new HashSet<LispLR0Items>(sc);
			for(LispLR0Items i : sc) {
				for(Datum x : vs) {
					g0 = lr0Goto(i, x);
					putlr0goto(g, i, x, g0);
					c |= sd.add(g0);
				}
			}
			sc = sd;
		}
		return g;
	}

	/**
	 * 
	 * @return
	 */
	public LispLR0Items lr0Start() {
		return new LispLR0Items(LispLR0Items.getClosure(
				this, finditems(start)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#isContextSensitive()
	 */
	@Override
	public boolean isContextSensitive() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#isMonotonic()
	 */
	@Override
	public boolean isMonotonic() {
		for(LispCFGRule r : rules) {
			if(r.isEpsilonRule())  return false;
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#isContextFree()
	 */
	@Override
	public boolean isContextFree() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#isRegular()
	 */
	public boolean isRegular() {
		LispCFG cnf = toCNF();
		List<Datum> l;

		for(LispCFGRule r : cnf.rules) {
			l = r.getRightValues();
			if(l.size() == 2) {
				for(LispCFGRule s : cnf.rules) {
					if(s.getLeftValues().equals(l.get(0)) &&
							s.getRightValues().size() == 2) {
						return false;
					}
				}
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#toCFG()
	 */
	@Override
	public LispCFG toCFG() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#toKurodaNormalForm()
	 */
	@Override
	public ILispFormalGrammar toKurodaNormalForm() {
		return toCNF();
	}

	/**
	 * 
	 * @return
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = (r + start.hashCode()) * Hashes.A;
		return r + rules.hashCode();
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public boolean equals(Object o) {
		LispCFG r;

		if(o instanceof LispCFG) {
			r = (LispCFG)o;
			return start.equals(r.start) && rules.equals(r.rules);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<context-free-grammar>");
	}

}
