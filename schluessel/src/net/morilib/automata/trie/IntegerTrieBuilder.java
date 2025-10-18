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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/29
 */
public class IntegerTrieBuilder<A> {

	//
	private int nodeNo = 1;
	private IntegerTrie<A> trie = new IntegerTrie<A>();

	/**
	 * 
	 * @return
	 */
	public IntegerTrie.Node<A> getInitialState() {
		return trie.getInitialState();
	}

	//
	IntegerTrie.Node<A> nlink(IntegerTrie.Node<A> n, int x) {
		IntegerTrie.Node<A> m;

		m = new IntegerTrie.Node<A>(nodeNo++, null, false);
		n.edges.put(x, m);
		return m;
	}

	/**
	 * 
	 * @param s
	 */
	public IntegerTrieBuilder<A> append(String s, A value) {
		IntegerTrie.Node<A> n = trie.getInitialState();
		int c;

		if(value == null)  throw new NullPointerException();
		for(int i = 0; i < s.length(); i += c > 0xffff ? 2 : 1) {
			if(n.goInt(c = s.codePointAt(i)).isDead()) {
				n = nlink(n, c);
			} else {
				n = n.goInt(c);
			}
		}
		n.accepted = true;
		n.value    = value;
		trie.map.put(value, s);
		return this;
	}

	/**
	 * 
	 * @return
	 */
	public IntegerTrie<A> get() {
		return new IntegerTrie<A>(trie);
	}

}
