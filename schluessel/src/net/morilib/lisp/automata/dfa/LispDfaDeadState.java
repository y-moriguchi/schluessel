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
import java.util.Set;

import net.morilib.automata.DFAState;
import net.morilib.automata.TextBound;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.automata.ILispConfiguration;
import net.morilib.range.Interval;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/14
 */
public final class LispDfaDeadState extends Datum2
implements ILispDfaState, DFAState<Datum, Datum, Datum> {

	/**
	 * 
	 */
	public static final LispDfaDeadState DEAD_STATE =
			new LispDfaDeadState();

	//
	private LispDfaDeadState() { }

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFAState#go(java.lang.Object)
	 */
	@Override
	public DFAState<Datum, Datum, Datum> go(Datum alphabet) {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFAState#goInt(int)
	 */
	@Override
	public DFAState<Datum, Datum, Datum> goInt(int x) {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFAState#goChar(char)
	 */
	@Override
	public DFAState<Datum, Datum, Datum> goChar(char x) {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfaState#go(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispDfaState go(Datum d, LispMessage mesg) {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfaState#go(int, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispDfaState go(int d, LispMessage mesg) {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfaState#go(char, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispDfaState go(char d, LispMessage mesg) {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration goSideEffect(Datum d, LispMessage mesg) {
		return go(d, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(int, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration goSideEffect(int d, LispMessage mesg) {
		return go(d, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(char, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration goSideEffect(char d, LispMessage mesg) {
		return go(d, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFAState#goBound(net.morilib.automata.TextBound)
	 */
	@Override
	public DFAState<Datum, Datum, Datum> goBound(TextBound bound) {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispState#isInitialState()
	 */
	@Override
	public boolean isInitialState() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFAState#getAccepted()
	 */
	@Override
	public Set<Datum> getAccepted() {
		return Collections.emptySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispState#isDead()
	 */
	@Override
	public boolean isDead() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispState#isAccepted()
	 */
	@Override
	public boolean isAccepted() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFAState#getAlphabets()
	 */
	@Override
	public Set<Datum> getAlphabets() {
		return Collections.emptySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<dead-dfa-state>");
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.DFAState#getAlphabetRanges()
	 */
	@Override
	public Iterable<Interval> getAlphabetRanges() {
		return Collections.emptySet();
	}
	
}