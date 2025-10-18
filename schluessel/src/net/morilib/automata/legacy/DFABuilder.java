/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.automata.legacy;

import net.morilib.automata.DFA;
import net.morilib.util.Tuple2;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/29
 */
public abstract class DFABuilder {

	//
	private static final DFABuilder INSTANCE = new NFADFABuilder();

	//
	/*package*/ DFABuilder() {
		// do nothing
	}

	/**
	 * 
	 * @return
	 */
	public static DFABuilder getInstance() {
		return INSTANCE;
	}

	/**
	 * 
	 * @param regexp
	 * @return
	 */
	public abstract DFA<Integer, Void, Tuple2<Void, Integer>> build(
			String regexp);

	/**
	 * 
	 * @param regexps
	 * @return
	 */
	public abstract
	DFA<Integer, Integer, Tuple2<Integer, Integer>> buildCombined(
			String... regexps);

}
