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
package net.morilib.util.set;

import java.util.Collection;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public class IdentityHashSet<T> implements Set<T> {

	//
	private IdentityHashMap<T, Object> map;

	/**
	 * 
	 */
	public IdentityHashSet() {
		this.map = new IdentityHashMap<T, Object>();
	}

	/**
	 * 
	 * @param c
	 */
	public IdentityHashSet(Collection<? extends T> c) {
		this();
		addAll(c);
	}

	/* (non-Javadoc)
	 * @see java.util.Set#size()
	 */
	public int size() {
		return map.size();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#isEmpty()
	 */
	public boolean isEmpty() {
		return map.isEmpty();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#contains(java.lang.Object)
	 */
	public boolean contains(Object o) {
		return map.containsKey(o);
	}

	/* (non-Javadoc)
	 * @see java.util.Set#iterator()
	 */
	public Iterator<T> iterator() {
		final Iterator<Map.Entry<T, Object>> i;

		i = map.entrySet().iterator();
		return new Iterator<T>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public T next() {
				return i.next().getKey();
			}

			public void remove() {
				i.remove();
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.Set#toArray()
	 */
	@SuppressWarnings("unchecked")
	public T[] toArray() {
		return (T[])map.keySet().toArray();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#toArray(T[])
	 */
	public<E> E[] toArray(E[] a) {
		return (E[])map.keySet().toArray(a);
	}

	/* (non-Javadoc)
	 * @see java.util.Set#add(java.lang.Object)
	 */
	public boolean add(T e) {
		return map.put(e, e) != null;
	}

	/* (non-Javadoc)
	 * @see java.util.Set#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		return map.remove(o) != null;
	}

	/* (non-Javadoc)
	 * @see java.util.Set#containsAll(java.util.Collection)
	 */
	public boolean containsAll(Collection<?> c) {
		return map.keySet().containsAll(c);
	}

	/* (non-Javadoc)
	 * @see java.util.Set#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends T> c) {
		boolean r = false;

		for(T x : c) {
			r = (map.put(x, x) != null) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.Set#retainAll(java.util.Collection)
	 */
	public boolean retainAll(Collection<?> c) {
		boolean r = false;

		for(T x : this) {
			if(!c.contains(x)) {
				r = (map.remove(x) != null) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.Set#removeAll(java.util.Collection)
	 */
	public boolean removeAll(Collection<?> c) {
		boolean r = false;

		for(Object x : c) {
			r = (map.remove(x) != null) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.Set#clear()
	 */
	public void clear() {
		map.clear();
	}

}
