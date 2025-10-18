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
import java.util.Collections;
import java.util.List;

import net.morilib.lang.Hashes;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.automata.ILispFormalGrammarRule;
import net.morilib.lisp.automata.LispGrammarVariable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/06
 */
public class LispCFGRule extends Datum2
implements ILispFormalGrammarRule {

	//
	private LispGrammarVariable lvalue;
	private List<Datum> rvalues;

	/**
	 * 
	 * @param lvalue
	 * @param rvalues
	 */
	public LispCFGRule(LispGrammarVariable lvalue,
			List<Datum> rvalues) {
		this.lvalue  = lvalue;
		this.rvalues = new ArrayList<Datum>(rvalues);
	}

	/**
	 * 
	 * @return
	 */
	public LispGrammarVariable getLeftValue() {
		return lvalue;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammarRule#getLeftValues()
	 */
	@Override
	public List<Datum> getLeftValues() {
		return Collections.<Datum>singletonList(lvalue);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammarRule#getRightValues()
	 */
	@Override
	public List<Datum> getRightValues() {
		return Collections.unmodifiableList(rvalues);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammarRule#isEpsilonRule()
	 */
	@Override
	public boolean isEpsilonRule() {
		return rvalues.isEmpty();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammarRule#isUnitRule()
	 */
	@Override
	public boolean isUnitRule() {
		return (rvalues.size() == 1 &&
				rvalues.get(0).isGrammarVariable());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammarRule#isEquivalentRule(net.morilib.lisp.automata.ILispFormalGrammarRule)
	 */
	@Override
	public boolean isEquivalentRule(ILispFormalGrammarRule r) {
		return (getLeftValues().equals(r.getLeftValues()) &&
				getRightValues().equals(r.getRightValues()));
	}

	/**
	 * 
	 * @return
	 */
	public boolean isLeftRecursive() {
		return rvalues.size() > 0 && lvalue.equals(rvalues.get(0));
	}

	/**
	 * 
	 * @return
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = (r + lvalue.hashCode()) * Hashes.A;
		return r + rvalues.hashCode();
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public boolean equals(Object o) {
		LispCFGRule r;

		if(o instanceof LispCFGRule) {
			r = (LispCFGRule)o;
			return (lvalue.equals(r.lvalue) &&
					rvalues.equals(r.rvalues));
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		String d = "";

		buf.append("#<").append(lvalue).append(" -> ");
		for(Datum x : rvalues) {
			buf.append(d).append(LispUtils.print(x));
			d = " ";
		}
		buf.append(">");
	}

}
