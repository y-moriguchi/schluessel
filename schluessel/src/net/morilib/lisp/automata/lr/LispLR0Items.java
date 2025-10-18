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

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.automata.cfg.LispCFG;
import net.morilib.lisp.automata.cfg.LispCFGRule;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/14
 */
public class LispLR0Items extends Datum2 {

	//
	Set<LispLR0Item> items;

	/**
	 * 
	 * @param items
	 */
	public LispLR0Items(Collection<LispLR0Item> items) {
		this.items = new HashSet<LispLR0Item>(items);
	}

	/**
	 * 
	 * @param items
	 */
	public LispLR0Items(LispLR0Item... items) {
		this(Arrays.asList(items));
	}

	/**
	 * 
	 * @return
	 */
	public Set<LispLR0Item> getItems() {
		return Collections.unmodifiableSet(items);
	}

	/**
	 * 
	 * @param cfg
	 * @return
	 */
	public Set<LispLR0Item> getKernelItems(LispCFG cfg) {
		Set<LispLR0Item> s = new HashSet<LispLR0Item>();
		Datum st = cfg.getStartVariable();

		for(LispLR0Item i : items) {
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
	 * @param si
	 * @return
	 */
	public static Set<LispLR0Item> getClosure(LispCFG cfg,
			Set<LispLR0Item> si) {
		Set<LispLR0Item> sj;
		boolean c = true;
		Datum d;

		while(c) {
			c  = false;
			sj = new HashSet<LispLR0Item>(si);
			for(LispLR0Item i : si) {
				if((d = i.getIndicated()) == null)  continue;
				for(LispCFGRule r : cfg.getRules()) {
					if(r.getLeftValue().equals(d)) {
						c |= sj.add(new LispLR0Item(r, 0));
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
	public LispLR0Items getClosure(LispCFG cfg) {
		return new LispLR0Items(getClosure(cfg, items));
	}

	/**
	 * 
	 * @return
	 */
	public boolean isStart(LispCFG cfg) {
		for(LispLR0Item s : items) {
			if(s.getLeftValue().equals(cfg.getStartVariable()) &&
					s.getMark() == 0) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 
	 * @param cfg
	 * @return
	 */
	public boolean isAccepted(LispCFG cfg) {
		for(LispLR0Item s : items) {
			if(s.getLeftValue().equals(cfg.getStartVariable()) &&
					s.getIndicated() == null) {
				return true;
			}
		}
		return false;
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
		if(o instanceof LispLR0Items) {
			return items.equals(((LispLR0Items)o).items);
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
		for(LispLR0Item i : items) {
			buf.append(d).append(i);
			d = " ";
		}
		buf.append("}");
	}

}
