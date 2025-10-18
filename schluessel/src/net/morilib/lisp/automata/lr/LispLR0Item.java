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

import java.util.List;

import net.morilib.lang.Hashes;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.automata.LispGrammarVariable;
import net.morilib.lisp.automata.cfg.LispCFGRule;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/14
 */
public class LispLR0Item extends Datum2 {

	//
	LispCFGRule rule;
	int mark;

	/**
	 * 
	 * @param rule
	 * @param mark
	 */
	public LispLR0Item(LispCFGRule rule, int mark) {
		if(mark < 0 || mark > rule.getRightValues().size()) {
			throw new IllegalArgumentException();
		}
		this.rule = rule;
		this.mark = mark;
	}

	/**
	 * 
	 * @return
	 */
	public LispGrammarVariable getLeftValue() {
		return rule.getLeftValue();
	}

	/**
	 * 
	 * @return
	 */
	public List<Datum> getRightValues() {
		return rule.getRightValues();
	}

	/**
	 * 
	 * @return
	 */
	public int getMark() {
		return mark;
	}

	/**
	 * 
	 * @return
	 */
	public boolean maybeKernel() {
		return mark > 0;
	}

	/**
	 * 
	 * @return
	 */
	public Datum getIndicated() {
		List<Datum> l = rule.getRightValues();

		return mark < l.size() ? l.get(mark) : null;
	}

	/**
	 * 
	 * @return
	 */
	public LispLR0Item shiftRight() {
		if(mark < rule.getRightValues().size()) {
			return new LispLR0Item(rule, mark + 1);
		} else {
			throw new IllegalStateException();
		}
	}

	/**
	 * 
	 * @return
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = (r + rule.hashCode()) * Hashes.A;
		return r + mark;
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public boolean equals(Object o) {
		LispLR0Item r;

		if(o instanceof LispLR0Item) {
			r = (LispLR0Item)o;
			return rule.equals(r.rule) && mark == r.mark;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		List<Datum> l = rule.getRightValues();
		String d = "";

		buf.append("{").append(rule.getLeftValue()).append(" -> ");
		for(int i = 0; i < l.size(); i++) {
			buf.append(d);
			if(i == mark)  buf.append("*");
			buf.append(LispUtils.print(l.get(i)));
			d = " ";
		}
		buf.append("}");
	}

}
