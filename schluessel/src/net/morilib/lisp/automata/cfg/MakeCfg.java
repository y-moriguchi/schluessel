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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.automata.LispGrammarVariable;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/06
 */
public class MakeCfg extends BinaryArgs {

	//
	private static final Symbol Q = Symbol.getSymbol("quote");

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		ConsIterator itr, jtr;
		Set<LispCFGRule> s = new HashSet<LispCFGRule>();
		List<Datum> l;
		String v, s0;
		Datum d, e;

		s0 = SubrUtils.getSymbolName(c1a, mesg);
		itr = new ConsIterator(c2a);
		if(!itr.hasNext()) {
			throw mesg.getError(
					"err.automata.rules.syntaxerror", c2a);
		}

		while(itr.hasNext()) {
			jtr = new ConsIterator(d = itr.next());
			if(!jtr.hasNext()) {
				throw mesg.getError(
						"err.automata.rules.syntaxerror", d);
			}
			v = SubrUtils.getSymbolName(jtr.next(), mesg);

			l = new ArrayList<Datum>();
			while(jtr.hasNext()) {
				e = jtr.next();
				if(e instanceof Symbol) {
					l.add(LispGrammarVariable.getInstance(
							((Symbol)e).getName()));
				} else if(!(e instanceof Cons)) {
					l.add(e);
				} else if(!((Cons)e).getCar().equals(Q)) {
					throw mesg.getError(
							"err.automata.rules.syntaxerror", d);
				} else if((e = ((Cons)e).getCdr()).isNil()) {
					throw mesg.getError(
							"err.automata.rules.syntaxerror", d);
				} else if(!((Cons)e).getCdr().isNil()) {
					throw mesg.getError(
							"err.automata.rules.syntaxerror", d);
				} else {
					l.add(((Cons)e).getCar());
				}
			}
			SubrUtils.checkProper(jtr, d, mesg);
			s.add(new LispCFGRule(
					LispGrammarVariable.getInstance(v), l));
		}
		SubrUtils.checkProper(itr, c2a, mesg);
		return new LispCFG(LispGrammarVariable.getInstance(s0), s);
	}

}
