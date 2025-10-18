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

import java.util.ArrayList;
import java.util.List;

import net.morilib.automata.DFA;
import net.morilib.automata.dfa.ConvertedRangeDFA;
import net.morilib.automata.nfa.CombinedNFA;
import net.morilib.automata.nfa.NFAAccept;
import net.morilib.automata.nfa.NFAObject;
import net.morilib.util.Inclementor;
import net.morilib.util.IntInclementor;
import net.morilib.util.NullInclementor;
import net.morilib.util.Tuple2;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class NFADFABuilder extends DFABuilder {

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFABuilder#build(java.lang.String)
	 */
	@Override
	public DFA<Integer, Void, Tuple2<Void, Integer>> build(
			String regexp) {
		Inclementor<Void> inc = NullInclementor.INSTANCE;
		NFABuilder<Void> bld = NFABuilder.newInstance(inc);
		NFAObject<Integer, Void, Tuple2<Void, Integer>> nfa =
			bld.parse(regexp);
		nfa = NFAAccept.newInstance(nfa, null);

		DFA<Integer, Void, Tuple2<Void, Integer>> dfa =
			ConvertedRangeDFA.convertDFA(nfa);

		return dfa;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFABuilder#buildCombined(java.lang.String[])
	 */
	@Override
	public
	DFA<Integer, Integer, Tuple2<Integer, Integer>> buildCombined(
			String... regexps) {
		NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nfa;
		Inclementor<Integer> inc = new IntInclementor();
		NFABuilder<Integer>  bld = NFABuilder.newInstance(inc);
		List<NFAObject<Integer, Integer, Tuple2<Integer, Integer>>> l =
			new ArrayList
			<NFAObject<Integer, Integer, Tuple2<Integer, Integer>>>();

		for(String re : regexps) {
			NFAObject<Integer, Integer, Tuple2<Integer, Integer>> nf;

			nf = bld.parse(re);
			l.add(NFAAccept.newInstance(nf, inc.getObject()));
			inc.suc();
		}

		nfa = CombinedNFA.newInstance(l);
		DFA<Integer, Integer, Tuple2<Integer, Integer>> dfa =
			ConvertedRangeDFA.convertDFA(nfa);

		return dfa;
	}

}
