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
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/13
 */
public class LispParallelDfaState extends Datum2
implements ILispDfaState {

	//
	private ILispDfaState[] states;

	/**
	 * 
	 * @param dfas
	 */
	public LispParallelDfaState(Collection<ILispDfaState> states) {
		this.states = states.toArray(new ILispDfaState[0]);
	}

	/**
	 * 
	 * @param dfas
	 */
	public LispParallelDfaState(ILispDfaState... states) {
		this(Arrays.asList(states));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispState#isInitialState()
	 */
	@Override
	public boolean isInitialState() {
		for(ILispDfaState x : states) {
			if(!x.isInitialState())  return false;
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispState#isDead()
	 */
	@Override
	public boolean isDead() {
		for(ILispDfaState x : states) {
			if(!x.isDead())  return false;
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispState#isAccepted()
	 */
	@Override
	public boolean isAccepted() {
		for(ILispDfaState x : states) {
			if(x.isAccepted())  return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispTransitable#go(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispDfaState go(Datum d, LispMessage mesg) {
		List<ILispDfaState> r = new ArrayList<ILispDfaState>();

		for(ILispDfaState x : states) {
			r.add(x.go(d, mesg));
		}
		return new LispParallelDfaState(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispTransitable#go(int, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispDfaState go(int d, LispMessage mesg) {
		return go(LispInteger.valueOf(d), mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispTransitable#go(char, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispDfaState go(char d, LispMessage mesg) {
		return go(LispCharacter.valueOf(d), mesg);
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
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<dfa-parallel-state>");
	}

}
