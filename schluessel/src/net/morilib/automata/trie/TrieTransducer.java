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

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/02/10
 */
public class TrieTransducer<T> implements Iterator<T> {

	//
	enum St { TRIE, BUF };

	//
	Trie<T, T> trie;
	List<T> buf1, buf2;
	TrieNode<T, T> node, prev;
	St stat = St.TRIE;
	Iterator<T> iter;

	/**
	 * 
	 * @param trie
	 */
	protected TrieTransducer(Trie<T, T> trie, Iterator<T> iter) {
		this.trie = trie;
		this.node = trie.getInitialState();
		this.buf1 = new LinkedList<T>();
		this.buf2 = new LinkedList<T>();
		this.iter = iter;
	}

	//
	private T nextSource() {
		return iter.next();
	}

	//
	private boolean hasNextSource() {
		return iter.hasNext();
	}

	//
	private T read() {
		return buf1.isEmpty() ? nextSource() : buf1.remove(0);
	}

	@Override
	public boolean hasNext() {
		return !buf1.isEmpty() || hasNextSource();
	}

	@Override
	public T next() {
		T t;

		switch(stat) {
		case TRIE:
			if(!hasNext()) {
				if(prev != null) {
					t    = prev.getState();
					prev = null;
					return t;
				} else if(buf1.isEmpty()) {
					return null;
				} else {
					stat = St.BUF;
				}
			} else if(!(node = node.go(t = read())).isDead()) {
				buf2.add(t);
				if(node.isAccepted()) {
					buf2.clear();
					prev = node;
				}
				return next();
			} else {
				buf1.clear();  buf1.addAll(buf2);
				buf2.clear();  buf2.add(t);
				if(prev != null) {
					buf1.addAll(buf2);
					buf2.clear();
					t    = prev.getState();
					prev = null;
					node = trie.getInitialState();
					return t;
				} else if(buf1.isEmpty()) {
					node = trie.getInitialState();
					return t;
				} else {
					stat = St.BUF;
					return next();
				}
			}
		case BUF:
			if(!buf1.isEmpty()) {
				return read();
			} else if(hasNext()) {
				buf1.addAll(buf2);
				buf2.clear();
				stat = St.TRIE;
				node = trie.getInitialState();
				prev = null;
				return next();
			} else {
				return null;
			}
		default:  throw new RuntimeException();
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}

}
