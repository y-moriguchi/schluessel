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

import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/13
 */
public class LispParallelDfa extends Datum2 implements ILispDfa {

	//
	ILispDfa[] dfas;

	/**
	 * 
	 * @param dfas
	 */
	public LispParallelDfa(Collection<ILispDfa> dfas) {
		this.dfas = dfas.toArray(new ILispDfa[0]);
	}

	/**
	 * 
	 * @param dfas
	 */
	public LispParallelDfa(ILispDfa... dfas) {
		this(Arrays.asList(dfas));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispFiniteAutomaton#getInitial()
	 */
	@Override
	public ILispDfaState getInitial() {
		List<ILispDfaState> r = new ArrayList<ILispDfaState>();

		for(ILispDfa x : dfas) {
			r.add(x.getInitial());
		}
		return new LispParallelDfaState(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<parallel-dfa>");
	}

}
