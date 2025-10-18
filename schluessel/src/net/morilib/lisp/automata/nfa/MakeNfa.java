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
package net.morilib.lisp.automata.nfa;

import java.util.ArrayList;
import java.util.List;

import net.morilib.automata.nfa.NFAAccept;
import net.morilib.automata.nfa.NFAAlternative;
import net.morilib.automata.nfa.NFAConcatenation;
import net.morilib.automata.nfa.NFAObject;
import net.morilib.automata.nfa.NFAOptional;
import net.morilib.automata.nfa.NFARepetition;
import net.morilib.automata.nfa.SingleObjectNFA;
import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/30
 */
public class MakeNfa extends BinaryArgs {

	//
	private static final Symbol ALT = Symbol.getSymbol("|");
	private static final Symbol CAT = Symbol.getSymbol("&");
	private static final Symbol OPT = Symbol.getSymbol("?");
	private static final Symbol RP0 = Symbol.getSymbol("*");
	private static final Symbol RP1 = Symbol.getSymbol("+");

	//
	private static Datum getd(ConsIterator itr, LispMessage mesg) {
		Datum x;

		if(itr.hasNext()) {
			x = itr.next();
			if(itr.hasNext()) {
				throw mesg.getError(
						"err.automata.regex.syntaxerror");
			}
			return x;
		} else {
			throw mesg.getError(
					"err.automata.regex.syntaxerror");
		}
	}

	//
	static NFAObject<Datum, Datum, Datum> makeNFA(Datum d,
			LispMessage mesg) {
		List<NFAObject<Datum, Datum, Datum>> l;
		ConsIterator itr;
		Datum x;

		if(d.isNil()) {
			throw mesg.getError("err.automata.regex.syntaxerror", d);
		} else if(d instanceof Cons) {
			itr = new ConsIterator(d);
			if((x = itr.next()).equals(ALT)) {
				l = new ArrayList<NFAObject<Datum, Datum, Datum>>();
				while(itr.hasNext()) {
					l.add(makeNFA(itr.next(), mesg));
				}
				return NFAAlternative.newInstance(l);
			} else if(x.equals(CAT)) {
				l = new ArrayList<NFAObject<Datum, Datum, Datum>>();
				while(itr.hasNext()) {
					l.add(makeNFA(itr.next(), mesg));
				}
				return NFAConcatenation.newInstance(l);
			} else if(x.equals(OPT)) {
				return NFAOptional.newInstance(
						makeNFA(getd(itr, mesg), mesg));
			} else if(x.equals(RP0)) {
				return NFARepetition.newInstance(
						makeNFA(getd(itr, mesg), mesg), true);
			} else if(x.equals(RP1)) {
				return NFARepetition.newInstance(
						makeNFA(getd(itr, mesg), mesg), false);
			} else {
				l = new ArrayList<NFAObject<Datum, Datum, Datum>>();
				l.add(makeNFA(x, mesg));
				while(itr.hasNext()) {
					l.add(makeNFA(itr.next(), mesg));
				}
				return NFAConcatenation.newInstance(l);
			}
		} else {
			return SingleObjectNFA.newInstance(d);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		return new LispNfa(
				NFAAccept.newInstance(makeNFA(c1a, mesg), c2a));
	}

}
