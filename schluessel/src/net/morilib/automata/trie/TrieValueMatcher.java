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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/29
 */
public class TrieValueMatcher<T, A> {

	//
	private Trie<T, A> trie;
	private TrieNode<T, A> node;
	private Map<TrieNode<T, A>, Map<T, TrieNode<T, A>>> fail;

	//
	private Iterator<T> itr;
	private T pushback;
	private boolean ispushback = false;

	/**
	 * 
	 * @param trie
	 * @param itr
	 */
	public TrieValueMatcher(Trie<T, A> trie, Iterator<T> itr) {
		this.trie = trie;
		this.itr  = itr;
		this.node = trie.getInitialState();
		initfail(trie);
	}

	/**
	 * 
	 * @param trie
	 * @param ts
	 */
	public TrieValueMatcher(Trie<T, A> trie, T... ts) {
		this(trie, Arrays.asList(ts).iterator());
	}

	//
	static<T, A> Map<T, TrieNode<T, A>> getFailureEdges(
			Trie<T, A> trie, List<T> lst) {
		Map<T, TrieNode<T, A>> m;
		TrieNode<T, A> n;
		T c;

		m = new HashMap<T, TrieNode<T, A>>();
		outer: for(int i = lst.size() - 1; i > 0; i--) {
			n = trie.getInitialState();
			for(int j = i; j < lst.size(); j++) {
				c = lst.get(j);
				if(n.go(c).isDead()) {
					continue outer;
				} else {
					n = n.go(c);
				}
			}
//			m.put(lst.get(i), n);
			m = n.getEdges();
		}
		return m;
	}

	//
	static<T, A> Map<T, Integer> getFailureBacks(Trie<T, A> trie,
			List<T> lst) {
		Map<T, Integer> m;
		TrieNode<T, A> n;
		T c;

		m = new HashMap<T, Integer>();
		outer: for(int i = lst.size() - 1; i > 0; i--) {
			n = trie.getInitialState();
			for(int j = i; j < lst.size(); j++) {
				c = lst.get(j);
				if(n.go(c).isDead()) {
					continue outer;
				} else {
					n = n.go(c);
				}
			}
			for(T t : n.getEdges().keySet())  m.put(t, i);
		}
		return m;
	}

	//
	static<T, A> void gennode(Trie<T, A> trie,
			Map<TrieNode<T, A>, Map<T, TrieNode<T, A>>> nodes,
			Map<TrieNode<T, A>, Map<T, Integer>> backs,
			TrieNode<T, A> n,
			List<T> lst) {
		for(T t : n.getEdges().keySet()) {
			lst.add(t);
			gennode(trie, nodes, backs, n.go(t), lst);
			lst.remove(lst.size() - 1);
		}
		nodes.put(n, getFailureEdges(trie, lst));
		backs.put(n, getFailureBacks(trie, lst));
	}

	//
	static<T, A> void gennode(Trie<T, A> trie,
			Map<TrieNode<T, A>, Map<T, TrieNode<T, A>>> nodes,
			TrieNode<T, A> n,
			List<T> lst) {
		for(T t : n.getEdges().keySet()) {
			lst.add(t);
			gennode(trie, nodes, n.go(t), lst);
			lst.remove(lst.size() - 1);
		}
		nodes.put(n, getFailureEdges(trie, lst));
	}

	//
	private void initfail(Trie<T, A> trie) {
		TrieNode<T, A> init = trie.getInitialState();
		List<T> lst = new ArrayList<T>();

		fail = new HashMap<TrieNode<T, A>, Map<T, TrieNode<T, A>>>();
		for(T t : init.getEdges().keySet()) {
			lst.add(t);
			gennode(trie, fail, init.go(t), lst);
			lst.remove(lst.size() - 1);
		}
	}

	//
	private T next() {
		if(ispushback) {
			ispushback = false;
			return pushback;
		} else {
			return itr.next();
		}
	}

	//
	private boolean hasNext() {
		return itr.hasNext() || ispushback;
	}

	/**
	 * 
	 * @return
	 */
	public A nextValue() {
		Map<T, TrieNode<T, A>> x;
		TrieNode<T, A> n, m;
		A acc = null;
		T c;

		while(true) {
			if(!hasNext())  return acc;
			c = next();
			if((n = node.go(c)).isDead()) {
				x = fail.get(node);
				if(node.isAccepted() || x == null) {
					node = trie.getInitialState();
				} else if((m = x.get(c)) == null) {
					node = trie.getInitialState();
				} else {
					node = m;
				}

				if(!node.go(c).isDead()) {
					ispushback = true;
					pushback = c;
				}
				if(acc != null)  return acc;
			} else if(n.isAccepted()) {
				acc = n.getAccepted().iterator().next();
				node = n;
			} else {
				node = n;
			}
		}
	}

}
