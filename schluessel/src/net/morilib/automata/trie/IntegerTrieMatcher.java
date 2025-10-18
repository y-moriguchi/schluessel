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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/29
 */
public class IntegerTrieMatcher<A> {

	//
	private Trie<Integer, A> trie;
	private TrieNode<Integer, A> node;
	private Map<TrieNode<Integer, A>, Map<Integer, Integer>> back;
	private
	Map<TrieNode<Integer, A>, Map<Integer, TrieNode<Integer, A>>> fail;

	//
	private StringBuilder buf = new StringBuilder();
	private CharSequence str;
	private int ptr = 0;

	/**
	 * 
	 * @param trie
	 */
	public IntegerTrieMatcher(Trie<Integer, A> trie,
			CharSequence str) {
		this.trie = trie;
		this.str  = str;
		this.node = trie.getInitialState();
		initfail(trie);
	}

	//
	private void initfail(Trie<Integer, A> trie) {
		TrieNode<Integer, A> init = trie.getInitialState();
		List<Integer> lst = new ArrayList<Integer>();

		fail = new HashMap<TrieNode<Integer, A>, Map<Integer, TrieNode<Integer, A>>>();
		back = new HashMap<TrieNode<Integer, A>, Map<Integer, Integer>>();
		for(Integer t : init.getEdges().keySet()) {
			lst.add(t);
			TrieValueMatcher.gennode(
					trie, fail, back, init.go(t), lst);
			lst.remove(lst.size() - 1);
		}
	}

	/**
	 * 
	 * @return
	 */
	public String nextToken() {
		Map<Integer, TrieNode<Integer, A>> x;
		TrieNode<Integer, A> n, m;
		String acc = null;
		int c, b;

		while(true) {
			if(ptr >= str.length())  return acc;
			c = str.charAt(ptr++);
			buf.append((char)c);
			if((n = node.goInt(c)).isDead()) {
				x = fail.get(node);
				if(node.isAccepted() || x == null) {
					node = trie.getInitialState();
					buf  = new StringBuilder();
				} else if((m = x.get(c)) == null) {
					node = trie.getInitialState();
					buf  = new StringBuilder();
				} else {
					b = back.get(node).get(c);
					node = m;
					buf.delete(buf.length() - b - 1, buf.length());
					buf.append((char)c);
				}
				if(!node.goInt(c).isDead())  ptr--;
				if(acc != null)  return acc;
			} else if(n.isAccepted()) {
				acc = buf.toString();
				node = n;
			} else {
				node = n;
			}
		}
	}

}
