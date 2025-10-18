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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import net.morilib.automata.DFAState;
import net.morilib.automata.FileFormatException;
import net.morilib.automata.Homomorphism;
import net.morilib.automata.TextBound;
import net.morilib.automata.dfa.DFAs;
import net.morilib.range.Interval;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/29
 */
public class IntegerTrie<A>
implements Trie<Integer, A>, Homomorphism<A, Integer> {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/12/29
	 */
	public static class Node<A> extends TrieNode<Integer, A> {

		Map<Integer, Node<A>> edges;
		private int nodeNo;
		A value;

		Node(int nodeNo, A value, boolean acc) {
			super(acc);
			this.nodeNo = nodeNo;
			this.value  = value;
			edges = new HashMap<Integer, Node<A>>();
		}

		private Node(int nodeNo, A value, Map<Integer, Node<A>> e,
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

		@Override
		public Node<A> go(Integer x) {
			return goInt(x.intValue());
		}

		@SuppressWarnings("unchecked")
		@Override
		public Node<A> goInt(int x) {
			Node<A> m;

			m = edges.get(x);
			return (m != null) ? m : (Node<A>)DEADNODE;
		}

		@Override
		public Node<A> goChar(char x) {
			return goInt((int)x);
		}

		@Override
		public DFAState<Integer, A, Void> goBound(
				TextBound bound) {
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
		public Map<Integer, TrieNode<Integer, A>> getEdges() {
			return Collections.<Integer, TrieNode<Integer, A>>unmodifiableMap(edges);
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.trie.TrieNode#willBeFail(java.lang.Object)
		 */
		@Override
		public boolean willFail(Integer t) {
			return willFail(t.intValue());
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.trie.TrieNode#willFailInt(int)
		 */
		@Override
		public boolean willFail(int x) {
			return goInt(x).isDead();
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.trie.TrieNode#willFailChar(char)
		 */
		@Override
		public boolean willFail(char x) {
			return willFail((int)x);
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.dfa.DFAState#getAlphabets()
		 */
		@Override
		public Set<Integer> getAlphabets() {
			return edges.keySet();
		}

		/* (non-Javadoc)
		 * @see net.morilib.automata.DFAState#getAlphabetRanges()
		 */
		@Override
		public Iterable<Interval> getAlphabetRanges() {
			Set<Interval> r = new HashSet<Interval>();

			for(Integer t : getAlphabets()) {
				r.add(Interval.newPoint(t));
			}
			return r;
		}

	}

	//
	static final Node<Object> DEADNODE =
			new Node<Object>(-1, null, false);

	//
	private Node<A> init;
	Map<A, String> map;

	//
	IntegerTrie() {
		init = new Node<A>(0, (A)null, false);
		map  = new HashMap<A, String>();
	}

	/**
	 * 
	 * @param trie
	 */
	public IntegerTrie(IntegerTrie<A> trie) {
		init = copytrie(trie.init);
		map  = new HashMap<A, String>();
	}

	//
	private static<A> Node<A> copytrie(Node<A> n) {
		Map<Integer, Node<A>> e;

		e = new HashMap<Integer, Node<A>>();
		for(int i : n.edges.keySet()) {
			e.put(i, copytrie(n.edges.get(i)));
		}
		return new Node<A>(n.nodeNo, n.value, e, n.isAccepted());
	}

	/**
	 * 
	 * @param ins
	 * @return
	 * @throws IOException
	 */
	public static IntegerTrie<Integer> loadCharToString(
			InputStream ins) throws IOException {
		IntegerTrieBuilder<Integer> b;
		Properties p = new Properties();
		String s, t;

		b = new IntegerTrieBuilder<Integer>();
		p.load(ins);
		for(Map.Entry<Object, Object> e : p.entrySet()) {
			s = e.getKey().toString();
			t = e.getValue().toString();
			if(s.codePointCount(0, s.length()) != 1) {
				throw new FileFormatException();
			}
			b.append(t, s.codePointAt(0));
		}
		return b.get();
	}

	/**
	 * 
	 * @param ins
	 * @return
	 * @throws IOException
	 */
	public static IntegerTrie<String> loadStringToString(
			InputStream ins) throws IOException {
		IntegerTrieBuilder<String> b;
		Properties p = new Properties();
		String s, t;

		b = new IntegerTrieBuilder<String>();
		p.load(ins);
		for(Map.Entry<Object, Object> e : p.entrySet()) {
			s = e.getKey().toString();
			t = e.getValue().toString();
			b.append(t, s);
		}
		return b.get();
	}

	/**
	 * 
	 * @param file
	 * @return
	 * @throws IOException
	 */
	public static IntegerTrie<Integer> loadCharToString(
			File file) throws IOException {
		InputStream ins = null;

		try {
			ins = new FileInputStream(file);
			return loadCharToString(ins);
		} finally {
			if(ins != null)  ins.close();
		}
	}

	/**
	 * 
	 * @param filename
	 * @return
	 * @throws IOException
	 */
	public static IntegerTrie<Integer> loadCharToString(
			String filename) throws IOException {
		return loadCharToString(new File(filename));
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.trie.Trie#getInitialState()
	 */
	@Override
	public Node<A> getInitialState() {
		return init;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.Homomorphism#h(java.lang.Object)
	 */
	@Override
	public List<Integer> get(A a) {
		List<Integer> l = new ArrayList<Integer>();
		String s = getString(a);
		int c;

		for(int i = 0; i < s.length(); i += c > 0xffff ? 2 : 1) {
			l.add(c = s.codePointAt(i));
		}
		return l;
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public String getString(A a) {
		return map.get(a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.Homomorphism#invert(java.util.List)
	 */
	@Override
	public A invert(List<Integer> ts) {
		Node<A> s = init;

		for(Integer t : ts) {
			if((s = s.go(t)).isDead())  return null;
		}
		return s.isAccepted() ? s.value : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.Homomorphism#invert(T[])
	 */
	@Override
	public A invert(Integer... ts) {
		return invert(Arrays.asList(ts));
	}

	/**
	 * 
	 * @param ts
	 * @return
	 */
	public A invert(String ts) {
		Node<A> s = init;
		int c;

		for(int i = 0; i < ts.length(); i += c > 0xffff ? 2 : 1) {
			if((s = s.go(c = ts.codePointAt(i))).isDead()) {
				return null;
			}
		}
		return s.isAccepted() ? s.value : null;
	}

}
