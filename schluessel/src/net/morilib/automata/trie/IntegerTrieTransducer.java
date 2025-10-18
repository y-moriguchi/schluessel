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
package net.morilib.automata.trie;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/02/11
 */
public class IntegerTrieTransducer extends TrieTransducer<Integer> {

	//
	private String result = null;

	/**
	 * @param trie
	 * @param iter
	 */
	public IntegerTrieTransducer(Trie<Integer, Integer> trie,
			Iterator<Integer> iter) {
		super(trie, iter);
	}

	/**
	 * @param trie
	 * @param iter
	 */
	public IntegerTrieTransducer(Trie<Integer, Integer> trie,
			String s) {
		super(trie, toiter(s).iterator());
	}

	//
	private static Iterable<Integer> toiter(String s) {
		List<Integer> l;
		int x;

		l = new ArrayList<Integer>();
		for(int i = 0; i < s.length(); i += x > 0xffff ? 2 : 1) {
			l.add(x = s.codePointAt(i));
		}
		return l;
	}

	/**
	 * 
	 * @param trie
	 * @param s
	 * @return
	 */
	public static String transduce(Trie<Integer, Integer> trie,
			String s) {
		return new IntegerTrieTransducer(trie, s).toString();
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public String toString() {
		StringBuffer r;

		if(result != null) {
			return result;
		} else {
			r = new StringBuffer();
			while(hasNext())  r.appendCodePoint(next());
			return result = r.toString();
		}
	}

}
