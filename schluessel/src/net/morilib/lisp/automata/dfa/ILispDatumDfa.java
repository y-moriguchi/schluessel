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

import net.morilib.automata.DFA;
import net.morilib.lisp.Datum;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/04
 */
public interface ILispDatumDfa extends ILispDfa {

	/**
	 * 
	 * @return
	 */
	public DFA<Datum, ?, ?> getDFA();

	/**
	 * 
	 * @return
	 */
	public Set<ILispDfaState> getReachableStates();

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public boolean isEquvalent(Datum a, Datum b);

	/**
	 * 
	 * @return
	 */
	public boolean isEmpty();

}
