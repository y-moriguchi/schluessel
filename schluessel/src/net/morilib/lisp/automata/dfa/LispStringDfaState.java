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

import net.morilib.automata.DFAState;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispMessage;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/30
 */
public class LispStringDfaState extends Datum2
implements ILispDfaState {

	/**
	 * 
	 */
	protected DFAState<Integer, Datum, ?> state;

	/**
	 * 
	 * @param state
	 */
	public LispStringDfaState(DFAState<Integer, Datum, ?> state) {
		this.state = state;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfaState#go(net.morilib.lisp.Datum)
	 */
	@Override
	public ILispDfaState go(Datum d, LispMessage mesg) {
		if(d instanceof LispCharacter) {
			throw mesg.getError("err.require.char", d);
		}
		return new LispStringDfaState(
				state.goInt(d.getCharacterCodePoint()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfaState#go(int)
	 */
	@Override
	public ILispDfaState go(int d, LispMessage mesg) {
		return new LispStringDfaState(state.goInt(d));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfaState#go(char)
	 */
	@Override
	public ILispDfaState go(char d, LispMessage mesg) {
		return new LispStringDfaState(state.goInt((int)d));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispDfaState goSideEffect(Datum d, LispMessage mesg) {
		return go(d, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(int, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispDfaState goSideEffect(int d, LispMessage mesg) {
		return go(d, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(char, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispDfaState goSideEffect(char d, LispMessage mesg) {
		return go(d, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfaState#isInitialState()
	 */
	@Override
	public boolean isInitialState() {
		return state.isInitialState();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfaState#isDead()
	 */
	@Override
	public boolean isDead() {
		return state.isDead();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.dfa.ILispDfaState#isAccepted()
	 */
	@Override
	public boolean isAccepted() {
		return state.isAccepted();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<dfa-state>");
	}

}
