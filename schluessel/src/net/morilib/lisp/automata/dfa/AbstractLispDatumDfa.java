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

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import net.morilib.automata.DFAState;
import net.morilib.automata.dfa.DFAs;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.util.set.PairSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/12
 */
public abstract class AbstractLispDatumDfa extends Datum2
implements ILispDatumDfa {

	//
	private transient Set<DFAState<Datum, ?, ?>> st2 = null;
	private transient Set<ILispDfaState> states = null;
	private transient Set<PairSet<DFAState<Datum, ?, ?>>> equiv = null;
	private transient boolean empty = false;

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDatumDfa#getReachableStates()
	 */
	@Override
	public synchronized Set<ILispDfaState> getReachableStates() {
		if(states == null) {
			states  = new HashSet<ILispDfaState>();
			st2 = DFAs.getReachableStatesDiscrete(getDFA());
			empty = DFAs.isEmptyDiscrete(getDFA());
			for(DFAState<Datum, ?, ?> x : st2) {
				if(x instanceof ILispDfaState) {
					states.add((ILispDfaState)x);
				} else {
					states.add(new LispSimpleDfaState(x));
				}
			}
		}
		return Collections.unmodifiableSet(states);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDatumDfa#isEmpty()
	 */
	@Override
	public synchronized boolean isEmpty() {
		getReachableStates();
		return empty;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDatumDfa#isEquvalent(net.morilib.lisp.automata.dfa.ILispDfaState, net.morilib.lisp.automata.dfa.ILispDfaState)
	 */
	@Override
	public synchronized boolean isEquvalent(Datum a, Datum b) {
		Object x, y;

		if(equiv == null) {
			equiv = DFAs.getEquivalentStatesDiscrete(getDFA());
		}

		if(a instanceof LispSimpleDfaState) {
			x = ((LispSimpleDfaState)a).state;
		} else {
			x = a;
		}

		if(b instanceof LispSimpleDfaState) {
			y = ((LispSimpleDfaState)b).state;
		} else {
			y = b;
		}

		getReachableStates();
		if(st2.contains(x) && st2.contains(y) &&
				(empty || x.equals(y))) {
			return true;
		} else {
			for(PairSet<?> s : equiv) {
				if(s.isPair(x, y))  return true;
			}
			return false;
		}
	}

}
