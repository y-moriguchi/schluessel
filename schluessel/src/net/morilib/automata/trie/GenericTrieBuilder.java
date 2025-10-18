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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/29
 */
public class GenericTrieBuilder<T, A> {

	//
	private int nodeNo = 1;
	private GenericTrie<T, A> trie = new GenericTrie<T, A>();

	/**
	 * 
	 * @return
	 */
	public GenericTrie.Node<T, A> getInitialState() {
		return trie.getInitialState();
	}

	//
	GenericTrie.Node<T, A> nlink(GenericTrie.Node<T, A> n, T x) {
		GenericTrie.Node<T, A> m;

		m = new GenericTrie.Node<T, A>(nodeNo++, null, false);
		n.edges.put(x, m);
		return m;
	}

	/**
	 * 
	 * @param s
	 */
	public GenericTrieBuilder<T, A> append(Iterator<T> t, A value) {
		GenericTrie.Node<T, A> n = trie.getInitialState();
		List<T> l = new ArrayList<T>();
		T c;

		if(value == null)  throw new NullPointerException();
		while(t.hasNext()) {
			if(n.go(c = t.next()).isDead()) {
				n = nlink(n, c);
			} else {
				n = n.go(c);
			}
			l.add(c);
		}
		n.accepted = true;
		n.value    = value;
		trie.map.put(value, Collections.unmodifiableList(l));
		return this;
	}

	/**
	 * 
	 * @param value
	 * @param ts
	 * @return
	 */
	public GenericTrieBuilder<T, A> append(A value, T... ts) {
		return append(Arrays.asList(ts).iterator(), value);
	}

	/**
	 * 
	 * @return
	 */
	public GenericTrie<T, A> get() {
		return new GenericTrie<T, A>(trie);
	}

}
