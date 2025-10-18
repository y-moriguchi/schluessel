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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.automata.cfg.LispCFGRule;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/20
 */
public class LispLR1Table extends Datum2 implements ILispLRTable {

	//
	private Map<LispLR1Items, Map<Datum, LispLR1Items>> shift;
	private Map<LispLR1Items, Map<Datum, Set<LispCFGRule>>> reduce;
	private Map<LispLR1Items, Set<Datum>> accept;
	private Map<LispLR1Items, Map<Datum, LispLR1Items>> ire;
	private LispLR1Items init;
	private List<Datum> conflicts;

	//
	private void addshift(LispLR1Items s, Datum a, LispLR1Items t) {
		Map<Datum, LispLR1Items> m;
		Set<LispCFGRule> z;
		LispCFGRule w;

		if((z = getReduces(s, a)) != null) {
			w = z.iterator().next();
			conflicts.add(new LispShiftReduceConflict(s, a, w));
		}

		if((m = shift.get(s)) == null) {
			m = new HashMap<Datum, LispLR1Items>();
			shift.put(s, m);
		}
		m.put(a, t);
	}

	//
	private void addgoto(LispLR1Items s, Datum a, LispLR1Items t) {
		Map<Datum, LispLR1Items> m;

		if((m = ire.get(s)) == null) {
			m = new HashMap<Datum, LispLR1Items>();
			ire.put(s, m);
		}
		m.put(a, t);
	}

	//
	private void addreduce(LispLR1Items s, Datum a, LispCFGRule t) {
		Map<Datum, Set<LispCFGRule>> m;
		Set<LispCFGRule> z;
		LispCFGRule w;

		if(getShift(s, a) != null) {
			conflicts.add(new LispShiftReduceConflict(s, a, t));
		} else if((z = getReduces(s, a)) != null && !z.isEmpty()) {
			w = z.iterator().next();
			conflicts.add(new LispReduceReduceConflict(s, t, w));
		}

		if((m = reduce.get(s)) == null) {
			m = new HashMap<Datum, Set<LispCFGRule>>();
			reduce.put(s, m);
		}

		if((z = m.get(a)) == null) {
			z = new HashSet<LispCFGRule>();
			m.put(a, z);
		}
		z.add(t);
	}

	//
	private void addaccept(LispLR1Items s, Datum a) {
		Set<Datum> z;

		if((z = accept.get(s)) == null) {
			z = new HashSet<Datum>();
			accept.put(s, z);
		}
		z.add(a);
	}

	//
	private void constr(ILispLR1Goto a) {
		Datum d;

		for(LispLR1Items s : a.getAllStates()) {
			for(LispLR1Item t : s.getItems()) {
				if((d = t.getIndicated()) == null) {
				    if(t.isAccepted(a.getCFG())) {
				    	addaccept(s, t.getLookahead());
				    } else {
						addreduce(s, t.getLookahead(), t.getRule());
				    }
				} else if(!d.isGrammarVariable()) {
					addshift(s, d, a.go(s, d));
				} else {
					addgoto(s, d, a.go(s, d));
				}
			}
		}
	}

	/**
	 * 
	 * @param lr1goto
	 */
	public LispLR1Table(ILispLR1Goto lr1goto) {
		shift  = new HashMap<LispLR1Items, Map<Datum, LispLR1Items>>();
		reduce = new HashMap<LispLR1Items, Map<Datum, Set<LispCFGRule>>>();
		accept = new HashMap<LispLR1Items, Set<Datum>>();
		ire    = new HashMap<LispLR1Items, Map<Datum, LispLR1Items>>();
		conflicts = new ArrayList<Datum>();
		init   = lr1goto.getInitialItems();
		constr(lr1goto);
System.out.println(shift);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLRTable#getInitialState()
	 */
	public Datum getInitialState() {
		return init;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLRTable#getShift(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	@Override
	public LispLR1Items getShift(Datum state, Datum alphabet) {
		Map<Datum, LispLR1Items> m;

		return ((m = shift.get(state)) == null) ?
				null : m.get(alphabet);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLRTable#getReduce(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	@Override
	public Set<LispCFGRule> getReduces(Datum state, Datum alphabet) {
		Map<Datum, Set<LispCFGRule>> m;

		return ((m = reduce.get(state)) == null) ?
				null : m.get(alphabet);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLRTable#getReduces(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	@Override
	public LispCFGRule getReduce(Datum state, Datum alphabet) {
		Set<LispCFGRule> s = getReduces(state, alphabet);

		return (s == null || s.isEmpty()) ? null : s.iterator().next();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLRTable#isAccept(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	@Override
	public boolean isAccept(Datum state, Datum alphabet) {
		Set<Datum> m;

		return (m = accept.get(state)) != null && m.contains(alphabet);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLRTable#getGoto(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	@Override
	public Datum getGoto(Datum state, Datum variable) {
		Map<Datum, LispLR1Items> m;

		return ((m = ire.get(state)) == null) ?
				null : m.get(variable);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.lr.ILispLRTable#getConflicts()
	 */
	@Override
	public List<Datum> getConflicts() {
		return Collections.unmodifiableList(conflicts);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<lr1-table>");
	}

}
