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
package net.morilib.lisp.automata.dfa;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/05
 */
public class MakeDfaFromTable extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		List<Datum> ac = LispUtils.consToList(c2a, mesg);
		Map<Datum, Datum> eg;
		Map<Datum, LispMooreMachine.State> mp;
		ConsIterator itr = new ConsIterator(c1a), jtr;
		List<Datum> l = new ArrayList<Datum>();
		LispMooreMachine r = new LispMooreMachine();
		LispMooreMachine.State s, ins = null;
		Datum x, q;
		int i;

		mp  = new HashMap<Datum, LispMooreMachine.State>();
		if(!itr.hasNext()) {
			throw mesg.getError("err.automata.require.alphabets");
		}
		jtr = new ConsIterator(itr.next());
		while(jtr.hasNext())  l.add(jtr.next());

		while(itr.hasNext()) {
			jtr = new ConsIterator(x = itr.next());
			eg  = new HashMap<Datum, Datum>();
			if(!jtr.hasNext()) {
				throw mesg.getError(
						"err.automata.table.syntaxerror", x);
			}
			q   = jtr.next();

			for(i = 0; jtr.hasNext(); i++) {
				if(i >= l.size()) {
					throw mesg.getError(
							"err.automata.table.syntaxerror", x);
				}
				eg.put(l.get(i), jtr.next());
			}

			if(i != l.size()) {
				throw mesg.getError(
						"err.automata.table.syntaxerror", x);
			}
			s  = r.new State(eg,
					ins == null,
					LispBoolean.getInstance(ac.contains(q)));
			if(ins == null)  ins = s;
			mp.put(q, s);
		}
		r.initial = ins;
		r.map = mp;
		return r;
	}

}
