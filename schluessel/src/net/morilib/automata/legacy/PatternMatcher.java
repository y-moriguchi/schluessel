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
package net.morilib.automata.legacy;

import java.util.Collections;
import java.util.Set;

import net.morilib.automata.DFA;
import net.morilib.automata.DFAState;
import net.morilib.automata.PatternParseException;
import net.morilib.util.Tuple2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/23
 */
public abstract class PatternMatcher {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/23
	 */
	public static interface MatchAction {

		/**
		 * 
		 * @param match
		 * @return
		 */
		public void doAction(String match);

	}

	//
	private DFA<Integer, Integer, Tuple2<Integer, Integer>> dfa = null;
	private MatchAction[] actions;

	/**
	 * 
	 * @param os
	 */
	protected PatternMatcher() {
		// do nothing
	}

	//
	private void initdfa(Object... os) {
		String[] ss;

		if(os.length % 2 != 0) {
			throw new IllegalArgumentException();
		}

		ss = new String[os.length / 2];
		for(int i = 0; i < ss.length; i++) {
			if((ss[i] = (String)os[i << 1]) == null) {
				throw new NullPointerException();
			}
		}

		actions = new MatchAction[os.length / 2];
		for(int i = 0; i < ss.length; i++) {
			if((actions[i] = (MatchAction)os[(i << 1) + 1]) == null) {
				throw new NullPointerException();
			}
		}
		dfa = DFABuilder.getInstance().buildCombined(ss);
	}

	/**
	 * 
	 */
	protected void init() {
		// do nothing
	}

	/**
	 * 
	 * @return
	 */
	protected abstract Object[] getSchema();

	/**
	 * @param c
	 * @return
	 */
	protected boolean inhibitCharacter(int c) {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.shell.Shell#system(java.lang.String, java.io.InputStream, java.io.PrintStream, java.io.PrintStream)
	 */
	protected void exec(String cmd) throws PatternParseException {
		int p = 0, b = -1;
		DFAState<Integer, Integer, Tuple2<Integer, Integer>> stat;
		Set<Integer> befs = Collections.emptySet();

		if(dfa == null) {
			initdfa(getSchema());
		}

		stat = dfa.getInitialState();
		init();
		for(; p <= cmd.length(); p++) {
			Set<Integer> s;
			int c = (p < cmd.length()) ? -1 : (int)cmd.charAt(p);

			if(p < cmd.length()) {
				c = -1;
			} else {
				c = (int)cmd.charAt(p);
//				if(Character.isISOControl(c) &&
//						c != '\t' && c != '\n' && c != '\r') {
//					throw new PatternParseException();
//				}
				if(inhibitCharacter(c)) {
					throw new PatternParseException();
				}
			}
			stat = stat.goInt(c);
			s = stat.getAccepted();
			if(b < 0) {
				if(!s.isEmpty()) {
					b = p;
					befs = s;
				}
			} else {
				if(s.isEmpty()) {
					actions[Collections.min(befs)].doAction(
							cmd.substring(b, p));
					befs = Collections.emptySet();
					stat = dfa.getInitialState();
					b = -1;
					continue;
				} else {
					befs = s;
				}
			}
		}
	}

}
