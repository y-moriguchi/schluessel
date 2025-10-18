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
package net.morilib.lisp;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.automata.DFA;
import net.morilib.automata.DFAState;
import net.morilib.automata.dfa.ConvertedRangeDFA;
import net.morilib.automata.legacy.NFABuilder;
import net.morilib.automata.nfa.CombinedNFA;
import net.morilib.automata.nfa.NFAAccept;
import net.morilib.automata.nfa.NFAObject;
import net.morilib.util.Inclementor;
import net.morilib.util.IntInclementor;
import net.morilib.util.Tuple2;

/*package*/ final class ParserSharpSyntaxImpl
extends ParserSharpSyntax {
	
	//
	private class Eng implements Engine {
		
		//
		private StringBuilder buf = new StringBuilder();
		private DFAState<Integer, Integer, Tuple2<Integer, Integer>> sta;
		
		private Eng(
				DFAState<Integer, Integer, Tuple2<Integer, Integer>> s) {
			this.sta = s;
		}
		
		private int getMt() {
			Set<Integer> mt = sta.getAccepted();
			int res = -1;
			
			for(Integer r : mt) {
				res = (res < 0 || res > r) ? r : res;
			}
			return res;
		}
		
		public Datum getDatum() {
			int mt = getMt();
			
			return (mt < 0) ? null : data.get(mt);
		}

		public boolean isFollowS() {
			int mt = getMt();
			
			return (mt < 0) ? false : follows.get(mt);
		}

		public boolean isMatch() {
			return !sta.getAccepted().isEmpty();
		}

		public boolean isUseMatch() {
			int mt = getMt();
			
			return (mt < 0) ? false : usemt.get(mt);
		}

		public String getMatchString() {
			return buf.toString();
		}

		public boolean isDead() {
			return sta.isDead();
		}

		public void go(int c) {
			sta = sta.goInt(c);
			buf.append((char)c);
		}

		public boolean isDeadNext(int c) {
			return sta.goInt(c).isDead();
		}
		
	}
	
	//
	private
	List<NFAObject<Integer, Integer, Tuple2<Integer, Integer>>> nfas;
	private DFA<Integer, Integer, Tuple2<Integer, Integer>> dfa;
	private List<Datum> data = new ArrayList<Datum>();
	private BitSet follows = new BitSet();
	private BitSet usemt   = new BitSet();
	private Inclementor<Integer> inc;
	private NFABuilder<Integer>  bld;
	private Map<String, Integer> patno = new HashMap<String, Integer>();
	
	{
		inc = new IntInclementor();
		bld  = NFABuilder.newInstance(inc);
		nfas =
			new ArrayList
			<NFAObject<Integer, Integer, Tuple2<Integer, Integer>>>();
		addRule0("t", LispBoolean.TRUE,  false, false);
		addRule0("f", LispBoolean.FALSE, false, false);
		addRule0("/.*/[iuc]*",
				Symbol.getSymbol("$compile-re"), false, true);
		dfa = ConvertedRangeDFA.convertDFA(CombinedNFA.newInstance(nfas));
	}
	
	//
	private void addRule0(
			String pattern, Datum d, boolean follow, boolean use) {
		String pat = pattern;
		
		if(pat == null || d == null) {
			throw new NullPointerException();
		} else if(pat.equals("")) {
			throw new IllegalArgumentException();
		} else if(pat.charAt(0) == '#') {
			pat = pat.substring(1);
		}
		
		Integer no = patno.get(pat);
		
		if(no == null) {
			nfas.add(NFAAccept.newInstance(
					bld.parse(pat), inc.getObject()));
			data.add(d);
			follows.set(inc.getObject(), follow);
			usemt.set  (inc.getObject(), use);
			patno.put  (pat, inc.getObject());
			inc.suc();
		} else {
			nfas.set(no, NFAAccept.newInstance(bld.parse(pat), no));
			data.set(no, d);
			follows.set(no, follow);
			usemt.set  (no, use);
		}
	}
	
	
	@Override
	public Engine getEngine() {
		return new Eng(dfa.getInitialState());
	}
	
	
	@Override
	public void addRule(
			String pattern, Datum d, boolean follow, boolean use) {
		addRule0(pattern, d, follow, use);
		dfa = ConvertedRangeDFA.convertDFA(CombinedNFA.newInstance(nfas));
	}

}
