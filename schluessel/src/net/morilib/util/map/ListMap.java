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
package net.morilib.util.map;

import java.util.AbstractCollection;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lang.number.Numbers;
import net.morilib.util.Iterators;
import net.morilib.util.set.IntegerRangeSet;

public class ListMap<V> extends AbstractMap<Integer, V>
implements java.io.Serializable {
	
	//
	private static final long serialVersionUID = 163106680779272895L;
	
	//
	private class _VView extends AbstractCollection<V> {
		
		public boolean add(V o) {
			throw new UnsupportedOperationException();
		}

		public boolean addAll(Collection<? extends V> c) {
			throw new UnsupportedOperationException();
		}

		public void clear() {
			wrapee.clear();
		}

		public boolean contains(Object o) {
			return wrapee.contains(o);
		}

		public boolean containsAll(Collection<?> c) {
			return wrapee.containsAll(c);
		}

		public boolean isEmpty() {
			return wrapee.isEmpty();
		}

		public Iterator<V> iterator() {
			return Iterators.unmodifiable(wrapee.iterator());
		}

		public boolean remove(Object o) {
			throw new UnsupportedOperationException();
		}

		public boolean removeAll(Collection<?> c) {
			throw new UnsupportedOperationException();
		}

		public boolean retainAll(Collection<?> c) {
			throw new UnsupportedOperationException();
		}

		public int size() {
			return wrapee.size();
		}

		public Object[] toArray() {
			return wrapee.toArray();
		}

		public <T> T[] toArray(T[] a) {
			return wrapee.toArray(a);
		}
		
	}
	
	//
	private class _EEn implements Map.Entry<Integer, V> {
		
		private int now;
		
		private _EEn(int now) {
			this.now = now;
		}
		
		public Integer getKey() {
			return now;
		}

		public V getValue() {
			return wrapee.get(now);
		}

		public V setValue(V value) {
			return wrapee.set(now, value);
		}
		
	}
	
	//
	private class _EIt implements Iterator<Map.Entry<Integer, V>> {
		
		private int now = 0;
		
		public boolean hasNext() {
			return now < wrapee.size();
		}

		public java.util.Map.Entry<Integer, V> next() {
			return new _EEn(now++);
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}
		
	}
	
	//
	private class _ESe extends AbstractSet<Map.Entry<Integer, V>> {

		@Override
		public boolean removeAll(Collection<?> c) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean add(java.util.Map.Entry<Integer, V> o) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean addAll(
				Collection<? extends java.util.Map.Entry<Integer, V>> c) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void clear() {
			wrapee.clear();
		}

		@Override
		public Iterator<java.util.Map.Entry<Integer, V>> iterator() {
			return new _EIt();
		}

		@Override
		public boolean remove(Object o) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean retainAll(Collection<?> c) {
			throw new UnsupportedOperationException();
		}

		@Override
		public int size() {
			return wrapee.size();
		}
		
	}
	
	//
	private List<V> wrapee;
	
	
	public ListMap(List<V> lst) {
		wrapee = lst;
	}
	
	
	public void clear() {
		wrapee.clear();
	}

	public boolean containsKey(Object key) {
		if(key instanceof Number) {
			return Numbers.between((Number)key, 0, wrapee.size());
		}
		return false;
	}

	public boolean containsValue(Object value) {
		return wrapee.contains(value);
	}

	public Set<java.util.Map.Entry<Integer, V>> entrySet() {
		return new _ESe();
	}

	public V get(Object key) {
		if(key instanceof Number) {
			if(Numbers.between((Number)key, 0, wrapee.size())) {
				return wrapee.get(((Number)key).intValue());
			}
		}
		return null;
	}

	public Set<Integer> keySet() {
		return new IntegerRangeSet(0, wrapee.size() - 1);
	}

	public V put(Integer key, V value) {
		int i = ((Number)key).intValue();
		
		if(i >= 0 && i < wrapee.size()) {
			wrapee.set(i, value);
//		} else if(i == wrapee.size()) {
//			wrapee.add(value);
		}
		throw new IllegalArgumentException(key.toString());
	}

	public V remove(Object key) {
		throw new UnsupportedOperationException();
	}

	public int size() {
		return wrapee.size();
	}

	public Collection<V> values() {
		return new _VView();
	}
	
}
