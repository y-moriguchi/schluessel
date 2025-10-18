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
import net.morilib.lisp.automata.cfg.LispCFG;
import net.morilib.lisp.automata.cfg.LispCFGRule;
import net.morilib.util.Objects;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/14
 */
public class LispLR1Item extends Datum2 {

	//
	LispCFGRule rule;
	int mark;
	Datum lookahead;

	/**
	 * 
	 * @param rule
	 * @param mark
	 */
	public LispLR1Item(LispCFGRule rule, int mark, Datum lookahead) {
		if(rule == null) {
			throw new NullPointerException();
		} else if(mark < 0 || mark > rule.getRightValues().size()) {
			throw new IllegalArgumentException();
		}
		this.rule = rule;
		this.mark = mark;
		this.lookahead = lookahead;  // lookahead may be null
	}

	/**
	 * 
	 * @param rule
	 * @param mark
	 */
	public LispLR1Item(LispLR0Item item, Datum lookahead) {
		if(item == null) {
			throw new NullPointerException();
		}
		this.rule = item.rule;
		this.mark = item.mark;
		this.lookahead = lookahead;  // lookahead may be null
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
	public LispCFGRule getRule() {
		return rule;
	}

	/**
	 * 
	 * @return
	 */
	public Datum getLookahead() {
		return lookahead;
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
	public LispLR1Item shiftRight() {
		if(mark < rule.getRightValues().size()) {
			return new LispLR1Item(rule, mark + 1, lookahead);
		} else {
			throw new IllegalStateException();
		}
	}

	/**
	 * 
	 * @param cfg
	 * @return
	 */
	public boolean isAccepted(LispCFG cfg) {
		return (rule.getLeftValue().equals(cfg.getStartVariable()) &&
				getIndicated() == null);
	}

	/**
	 * 
	 * @return
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = (r + rule.hashCode()) * Hashes.A;
		r = (r + mark) * Hashes.A;
		return r + Hashes.hashCode(lookahead);
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public boolean equals(Object o) {
		LispLR1Item r;

		if(o instanceof LispLR1Item) {
			r = (LispLR1Item)o;
			return (rule.equals(r.rule) &&
					mark == r.mark &&
					Objects.equals(lookahead, r.lookahead));
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
		buf.append(",").append(LispUtils.print(lookahead)).append("}");
	}

}
