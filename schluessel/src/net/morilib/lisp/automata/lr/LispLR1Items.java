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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.automata.LispAutomataUtils;
import net.morilib.lisp.automata.cfg.LispCFG;
import net.morilib.lisp.automata.cfg.LispCFGRule;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/14
 */
public class LispLR1Items extends Datum2 {

	//
	private static final Datum DUMMY = new Datum2() {

		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<dummy-marker>");
		}

	};

	//
	Set<LispLR1Item> items;

	/**
	 * 
	 * @param items
	 */
	public LispLR1Items(Collection<LispLR1Item> items) {
		this.items = new HashSet<LispLR1Item>(items);
	}

	/**
	 * 
	 * @param items
	 */
	public LispLR1Items(LispLR1Item... items) {
		this(Arrays.asList(items));
	}

	/**
	 * 
	 * @return
	 */
	public Set<LispLR1Item> getItems() {
		return Collections.unmodifiableSet(items);
	}

	/**
	 * 
	 * @param cfg
	 * @return
	 */
	public static Set<LispLR1Item> getKernelItems(LispCFG cfg,
			Set<LispLR1Item> items) {
		Set<LispLR1Item> s = new HashSet<LispLR1Item>();
		Datum st = cfg.getStartVariable();

		for(LispLR1Item i : items) {
			if(i.maybeKernel()) {
				s.add(i);
			} else if(i.getLeftValue().equals(st)) {
				s.add(i);
			}
		}
		return s;
	}

	/**
	 * 
	 * @param cfg
	 * @return
	 */
	public Set<LispLR1Item> getKernelItems(LispCFG cfg) {
		return getKernelItems(cfg, items);
	}

	/**
	 * 
	 * @param cfg
	 * @param si
	 * @return
	 */
	public static Set<LispLR1Item> getClosure(LispCFG cfg,
			Set<LispLR1Item> si) {
		List<Datum> l, m;
		Set<LispLR1Item> sj;
		boolean c = true;
		Datum d;
		int p;

		while(c) {
			c  = false;
			sj = new HashSet<LispLR1Item>(si);
			for(LispLR1Item i : si) {
				if((d = i.getIndicated()) == null)  continue;
				for(LispCFGRule r : cfg.getRules()) {
					if(r.getLeftValue().equals(d)) {
						l = new ArrayList<Datum>();
						m = i.getRightValues();
						p = i.getMark() + 1;
						if(p < m.size()) {
							l.addAll(m.subList(p, m.size()));
						}
						l.add(i.getLookahead());
						for(Datum x : cfg.getFirst(l)) {
							c |= sj.add(new LispLR1Item(r, 0, x));
						}
					}
				}
			}
			si = sj;
		}
		return si;
	}

	/**
	 * 
	 * @param cfg
	 * @return
	 */
	public LispLR1Items getClosure(LispCFG cfg) {
		return new LispLR1Items(getClosure(cfg, items));
	}

	//
	private static boolean putlok(
			Map<LispLR0Items, Set<LispLR1Item>> s,
			LispLR0Items i0, LispLR1Item i1) {
		Set<LispLR1Item> r;

		if((r = s.get(i0)) == null) {
			r = new HashSet<LispLR1Item>();
			s.put(i0, r);
		}
		return r.add(i1);
	}

	//
	private static Set<LispLR1Item> getlok(
			Map<LispLR0Items, Set<LispLR1Item>> s,
			LispLR0Items i0) {
		Set<LispLR1Item> r;

		if((r = s.get(i0)) == null) {
			r = new HashSet<LispLR1Item>();
			s.put(i0, r);
		}
		return r;
	}

	/**
	 * 
	 * @param lr0
	 * @return
	 */
	public static Map<LispLR0Items, LispLR1Items> propergateLookahead(
			LispLR0Automaton lr0) {
		Map<LispLR0Items, LispLR1Items> zz;
		Map<LispLR0Items, Set<LispLR1Item>> rr;
		LispCFG cfg = lr0.cfg;
		LispLR0Items i0;
		Set<LispLR1Item> s1, s2;
		LispLR1Item a1, a2;
		boolean c = true;

		rr = new HashMap<LispLR0Items, Set<LispLR1Item>>();
		i0 = cfg.lr0Start();
		s1 = new HashSet<LispLR1Item>();
		for(LispLR0Item i : i0.getKernelItems(cfg)) {
			if(i.getLeftValue().equals(cfg.getStartVariable())) {
				s1.add(new LispLR1Item(
						i, LispAutomataUtils.ENDMARKER));
			}
		}
		rr.put(i0, s1);

		// calculate spontaneous lookahead
		for(LispLR0Items is : lr0.graph.keySet()) {
			for(LispLR0Item i : is.getKernelItems(cfg)) {
				a1 = new LispLR1Item(i, DUMMY);
				s1 = getClosure(cfg, Collections.singleton(a1));
				for(LispLR1Item j : s1) {
					if(j.getIndicated() != null &&
							j.getLookahead() != DUMMY) {
						i0 = lr0.go(is, j.getIndicated());
						for(LispLR0Item k : i0.items) {
							a2 = new LispLR1Item(k, j.getLookahead());
							if(a2.maybeKernel()) {
								putlok(rr, i0, a2);
							}
						}
					}
				}
			}
		}

		// calculate propagation
		while(c) {
			c = false;
			for(LispLR0Items is : lr0.graph.keySet()) {
				for(LispLR0Item i : is.getKernelItems(cfg)) {
					a1 = new LispLR1Item(i, DUMMY);
					s1 = getClosure(cfg, Collections.singleton(a1));
					for(LispLR1Item j : s1) {
						if(j.getIndicated() != null &&
								j.getLookahead() == DUMMY) {
							i0 = lr0.go(is, j.getIndicated());
							for(LispLR0Item k : i0.items) {
								s2 = new HashSet<LispLR1Item>(
										getlok(rr, is));
								for(LispLR1Item l : s2) {
									a2 = new LispLR1Item(
											k, l.getLookahead());
									if(a2.maybeKernel()) {
										c |= putlok(rr, i0, a2);
									}
								}
							}
						}
					}
				}
			}
		}

		zz = new HashMap<LispLR0Items, LispLR1Items>();
		for(LispLR0Items z : rr.keySet()) {
			zz.put(z, new LispLR1Items(
					getKernelItems(cfg, rr.get(z))));
		}
		return zz;
	}

	/**
	 * 
	 * @return
	 */
	public int hashCode() {
		return items.hashCode();
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public boolean equals(Object o) {
		if(o instanceof LispLR1Items) {
			return items.equals(((LispLR1Items)o).items);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		String d = "";

		buf.append("{");
		for(LispLR1Item i : items) {
			buf.append(d).append(i);
			d = " ";
		}
		buf.append("}");
	}

}
