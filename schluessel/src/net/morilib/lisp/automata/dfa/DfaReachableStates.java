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

import java.util.Set;

import net.morilib.automata.DFAState;
import net.morilib.automata.dfa.DFAs;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/12
 */
public class DfaReachableStates extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		Set<DFAState<Datum, ?, ?>> s;
		ConsListBuilder b = new ConsListBuilder();

		if(c1a instanceof ILispDatumDfa) {
			s = DFAs.getReachableStatesDiscrete(
					((ILispDatumDfa)c1a).getDFA());
			for(DFAState<Datum, ?, ?> x : s) {
				if(x instanceof Datum) {
					b.append((Datum)x);
				} else {
					b.append(new LispSimpleDfaState(x));
				}
			}
			return b.get();
		} else {
			throw mesg.getError("err.automata.require.dfa", c1a);
		}
	}

}
