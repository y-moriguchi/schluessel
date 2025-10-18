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
package net.morilib.lisp.automata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.automata.cfg.LispCFG;
import net.morilib.lisp.automata.cfg.LispCFGRule;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/02/03
 */
public class Lisp0Grammar extends Datum2
implements ILispFormalGrammar {

	//
	private Set<Lisp0GrammarRule> rules;
	private LispGrammarVariable start;

	//
	Lisp0Grammar(LispGrammarVariable start,
			Set<Lisp0GrammarRule> rules, boolean dummy) {
		this.start = start;
		this.rules = rules;
	}

	/**
	 * 
	 * @param start
	 * @param rules
	 */
	public Lisp0Grammar(LispGrammarVariable start,
			Set<Lisp0GrammarRule> rules) {
		this.start = start;
		this.rules = new HashSet<Lisp0GrammarRule>(rules);
	}

	/**
	 * 
	 * @param g
	 * @return
	 */
	public static Set<LispGrammarVariable> getAllVariables(
			ILispFormalGrammar g) {
		Set<LispGrammarVariable> s;

		s = new HashSet<LispGrammarVariable>();
		for(ILispFormalGrammarRule r : g.getRules()) {
			for(Datum x : r.getLeftValues()) {
				if(x.isGrammarVariable()) {
					s.add((LispGrammarVariable)x);
				}
			}

			for(Datum x : r.getRightValues()) {
				if(x.isGrammarVariable()) {
					s.add((LispGrammarVariable)x);
				}
			}
		}
		return s;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#getRules()
	 */
	@Override
	public Set<? extends ILispFormalGrammarRule> getRules() {
		return Collections.unmodifiableSet(rules);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#getStartVariable()
	 */
	@Override
	public LispGrammarVariable getStartVariable() {
		return start;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#getAllVariables()
	 */
	@Override
	public Set<LispGrammarVariable> getVariables() {
		return Lisp0Grammar.getAllVariables(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#isContextSensitive()
	 */
	@Override
	public boolean isContextSensitive() {
		List<Datum> lv, rv;
		Datum d;
		int rs, ls, i, j;

		for(Lisp0GrammarRule r : rules) {
			lv = r.getLeftValues();   ls = lv.size();
			rv = r.getRightValues();  rs = rv.size();

			if(ls == 1 && rs == 0) {
				d = r.getLeftValues().get(0);
				if(!d.equals(start))  return false;
				for(Lisp0GrammarRule s : rules) {
					if(s.getRightValues().contains(d)) {
						return false;
					}
				}
			} else {
				// scan from left
				for(i = 0; true; i++) {
					if(i >= rs || i >= ls) {
						return false;
					} else if(!lv.get(i).equals(rv.get(i))) {
						break;
					}
				}
	
				// scan from right
				for(j = 1; true; j++) {
					if(ls - j == i) {
						break;  // ok
					} else if(!lv.get(ls - j).equals(rv.get(rs - j))) {
						return false;
					}
				}
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#isMonotonic()
	 */
	@Override
	public boolean isMonotonic() {
		List<Datum> lv, rv;

		for(Lisp0GrammarRule r : rules) {
			lv = r.getLeftValues();
			rv = r.getRightValues();
			if(lv.size() > rv.size())  return false;
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#isContextFree()
	 */
	@Override
	public boolean isContextFree() {
		List<Datum> lv;

		for(Lisp0GrammarRule r : rules) {
			lv = r.getLeftValues();
			if(lv.size() != 1 || !lv.get(0).isGrammarVariable()) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#isRegular()
	 */
	@Override
	public boolean isRegular() {
		LispCFG cfg;

		return (cfg = toCFG()) != null && cfg.isRegular();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammar#toCFG()
	 */
	public LispCFG toCFG() {
		Set<LispCFGRule> rs = new HashSet<LispCFGRule>();
		List<Datum> lv, rv;

		for(Lisp0GrammarRule r : rules) {
			lv = r.getLeftValues();
			rv = r.getRightValues();
			if(lv.size() != 1 || !lv.get(0).isGrammarVariable()) {
				return null;
			} else {
				rs.add(new LispCFGRule(
						(LispGrammarVariable)lv.get(0), rv));
			}
		}
		return new LispCFG(start, rs);
	}

	//
	static Set<Lisp0GrammarRule> decreaseDegree(Lisp0GrammarRule r) {
		Set<Lisp0GrammarRule> rl;
		LispGrammarVariable t0, t1;
		Lisp0GrammarRule r0;
		List<Datum> lv, rv, l0, l1;
		int rs, ls;

		lv = r.getLeftValues();   ls = lv.size();
		rv = r.getRightValues();  rs = rv.size();
		if((ls == 2 && rs == 2) || (ls == 1 && (rs == 1 || rs == 2))) {
			return Collections.singleton(r);
		} else if(ls > rs) {
			return null;
		} else if(ls == 1) {
			rl = new HashSet<Lisp0GrammarRule>();
			t0 = LispGrammarVariable.genvar();

			// rule A -> B T0
			l0 = Arrays.asList(rv.get(0), t0);
			r0 = new Lisp0GrammarRule(lv, l0, 1);
			rl.add(r0);

			// rule T0 -> gamma
			l0 = Collections.<Datum>singletonList(t0);
			r0 = new Lisp0GrammarRule(l0, rv.subList(1, rs), 2);
			rl.add(r0);
		} else {
			rl = new HashSet<Lisp0GrammarRule>();
			t0 = LispGrammarVariable.genvar();
			t1 = LispGrammarVariable.genvar();

			// rule A E -> A' E'
			l0 = Arrays.<Datum>asList(t0, t1);
			r0 = new Lisp0GrammarRule(lv.subList(0, 2), l0, 1);
			rl.add(r0);

			// rule A' -> B
			l0 = Collections.<Datum>singletonList(t0);
			l1 = Collections.<Datum>singletonList(rv.get(0));
			r0 = new Lisp0GrammarRule(l0, l1, 3);
			rl.add(r0);

			// rule E' gamma -> C D zeta 
			l0 = new ArrayList<Datum>();
			l0.add(t1);  l0.addAll(lv.subList(2, ls));
			r0 = new Lisp0GrammarRule(l0, rv.subList(1, rs), 1);
			rl.add(r0);
		}
		return rl;
	}

	//
	static Set<Lisp0GrammarRule> decreaseDegree(
			Set<Lisp0GrammarRule> rs) {
		Set<Lisp0GrammarRule> rt, r0;

		rt = new HashSet<Lisp0GrammarRule>();
		for(Lisp0GrammarRule r : rs) {
			if((r0 = decreaseDegree(r)) == null)  return null;
			rt.addAll(r0);
		}
		return rt;
	}

	/**
	 * 
	 * @return
	 */
	public Lisp0Grammar toKurodaNormalForm() {
		Set<Lisp0GrammarRule> rs = rules, rt = null;

		while(rt == null || rt.size() < rs.size()) {
			rt = rs;
			rs = decreaseDegree(rs);
		}
		return new Lisp0Grammar(start, rs, false);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<generic-grammar>");
	}

}
