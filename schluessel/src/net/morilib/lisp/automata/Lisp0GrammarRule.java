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
import java.util.Collections;
import java.util.List;

import net.morilib.lang.Hashes;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/02/03
 */
public class Lisp0GrammarRule extends Datum2
implements ILispFormalGrammarRule {

	//
	private List<Datum> lvalues, rvalues;

	//
	Lisp0GrammarRule(List<Datum> lvalues, List<Datum> rvalues,
			int dummy) {
		if(dummy == 1) {
			this.lvalues = new ArrayList<Datum>(lvalues);
			this.rvalues = rvalues;
		} else if(dummy == 2) {
			this.lvalues = lvalues;
			this.rvalues = new ArrayList<Datum>(rvalues);
		} else if(dummy == 3) {
			this.lvalues = lvalues;
			this.rvalues = rvalues;
		} else {
			this.lvalues = new ArrayList<Datum>(lvalues);
			this.rvalues = new ArrayList<Datum>(rvalues);
		}
	}

	/**
	 * 
	 * @param lvalues
	 * @param rvalues
	 */
	Lisp0GrammarRule(List<Datum> lvalues, List<Datum> rvalues) {
		this.lvalues = new ArrayList<Datum>(lvalues);
		this.rvalues = new ArrayList<Datum>(rvalues);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammarRule#getLeftValues()
	 */
	@Override
	public List<Datum> getLeftValues() {
		return Collections.unmodifiableList(lvalues);
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
		return rvalues.size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFormalGrammarRule#isUnitRule()
	 */
	@Override
	public boolean isUnitRule() {
		return (lvalues.size() == 1 &&
				rvalues.size() == 1 &&
				lvalues.get(0).isGrammarVariable() &&
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
	public int hashCode() {
		int r = Hashes.INIT;

		r = (r + lvalues.hashCode()) * Hashes.A;
		return r + rvalues.hashCode();
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public boolean equals(Object o) {
		Lisp0GrammarRule r;

		if(o instanceof Lisp0GrammarRule) {
			r = (Lisp0GrammarRule)o;
			return (lvalues.equals(r.lvalues) &&
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

		buf.append("#<");
		for(Datum x : lvalues) {
			buf.append(d).append(LispUtils.print(x));
			d = " ";
		}
		buf.append(" ->");
		for(Datum x : rvalues) {
			buf.append(" ").append(LispUtils.print(x));
		}
		buf.append(">");
	}

}
