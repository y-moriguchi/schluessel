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
package net.morilib.lisp.automata.lr;

import java.util.List;
import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.automata.cfg.LispCFGRule;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/26
 */
public interface ILispLRTable {

	/**
	 * 
	 * @return
	 */
	public Datum getInitialState();

	/**
	 * 
	 * @param state
	 * @param alphabet
	 * @return
	 */
	public Datum getShift(Datum state, Datum alphabet);

	/**
	 * 
	 * @param state
	 * @param alphabet
	 * @return
	 */
	public Set<LispCFGRule> getReduces(Datum state, Datum alphabet);

	/**
	 * 
	 * @param state
	 * @param alphabet
	 * @return
	 */
	public LispCFGRule getReduce(Datum state, Datum alphabet);

	/**
	 * 
	 * @param state
	 * @param alphabet
	 * @return
	 */
	public boolean isAccept(Datum state, Datum alphabet);

	/**
	 * 
	 * @param state
	 * @param variable
	 * @return
	 */
	public Datum getGoto(Datum state, Datum variable);

	/**
	 * 
	 * @return
	 */
	public List<Datum> getConflicts();

}
