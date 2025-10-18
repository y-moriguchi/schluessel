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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.automata.DFAState;
import net.morilib.automata.Homomorphism;
import net.morilib.automata.TextBound;
import net.morilib.automata.dfa.DFAs;
import net.morilib.range.Interval;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/29
 */
public class GenericTrie<T, A>
implements Trie<T, A>, Homomorphism<A, T> {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/12/29
	 */
	public static class Node<T, A> extends TrieNode<T, A> {

		Map<T, Node<T, A>> edges;
		A value;
		private int nodeNo;

		Node(int nodeNo, A value, boolean acc) {
			super(acc);
			this.nodeNo = nodeNo;
			this.value  = value;
			edges = new HashMap<T, Node<T, A>>();
		}

		private Node(int nodeNo, A value, Map<T, Node<T, A>> e,
				boolean acc) {
			super(acc);
			this.nodeNo = nodeNo;
			this.value  = value;
			edges = e;
		}

		@Override
		public A getState() {
			return value;
		}

		@SuppressWarnings("unchecked")
		@Override
		public Node<T, A> go(T x) {
			Node<T, A> m;

			m = edges.get(x);
			return (m != null) ? m : (Node<T, A>)DEADNODE;
		}

		@Override
		public DFAState<T, A, Void> goBound(TextBound bound) {
			return DFAs.deadState();
		}

		@Override
		public boolean isInitialState() {
			return nodeNo == 0;
		}

		@Override
		public boolean isDead() {
			return nodeNo < 0;
		}

		@Override
		public Map<T, TrieNode<T, A>> getEdges() {
			return Collections.<T, TrieNode<T, A>>unmodifiableMap(edges);
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.trie.TrieNode#willBeFail(java.lang.Object)
		 */
		@Override
		public boolean willFail(T t) {
			return go(t).isDead();
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#getAlphabets()
		 */
		@Override
		public Set<T> getAlphabets() {
			return edges.keySet();
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.DFAState#getAlphabetRanges()
		 */
		@Override
		public Iterable<Interval> getAlphabetRanges() {
			Set<Interval> r = new HashSet<Interval>();

			for(T t : getAlphabets()) {
				r.add(Interval.newPoint(t));
			}
			return r;
		}

	}

	//
	static final Node<Object, Object> DEADNODE =
			new Node<Object, Object>(-1, null, false);

	//
	private Node<T, A> init;
	Map<A, List<T>> map;

	//
	GenericTrie() {
		init = new Node<T, A>(0, (A)null, false);
		map  = new HashMap<A, List<T>>();
	}

	/**
	 * 
	 * @param trie
	 */
	public GenericTrie(GenericTrie<T, A> trie) {
		init = copytrie(trie.init);
		map  = new HashMap<A, List<T>>(trie.map);
	}

	//
	private static<T, A> Node<T, A> copytrie(Node<T, A> n) {
		Map<T, Node<T, A>> e;

		e = new HashMap<T, Node<T, A>>();
		for(T i : n.edges.keySet()) {
			e.put(i, copytrie(n.edges.get(i)));
		}
		return new Node<T, A>(n.nodeNo, n.value, e, n.isAccepted());
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.trie.Trie#getInitialState()
	 */
	@Override
	public Node<T, A> getInitialState() {
		return init;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.Homomorphism#h(java.lang.Object)
	 */
	@Override
	public List<T> get(A a) {
		return map.get(a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.Homomorphism#invert(java.util.List)
	 */
	@Override
	public A invert(List<T> ts) {
		Node<T, A> s = init;

		for(T t : ts) {
			if((s = s.go(t)).isDead())  return null;
		}
		return s.isAccepted() ? s.value : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.Homomorphism#invert(T[])
	 */
	@Override
	public A invert(T... ts) {
		return invert(Arrays.asList(ts));
	}

}
