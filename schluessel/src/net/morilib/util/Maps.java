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
package net.morilib.util;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.SortedMap;

import net.morilib.util.string.StringPointer;

/**
 * <i>USEful Implements</i> for maps.
 * <p>Mapに関する便利な関数である.
 * 
 * @author MORIGUCHI, Yuichiro 2005/01/01
 */
public final class Maps {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/01
	 */
	public static interface Transform<V> {

		/**
		 * 
		 * @param o
		 * @return
		 */
		public V transform(V o);

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/01
	 */
	public static interface Transform2<V> {

		/**
		 * 
		 * @param o
		 * @return
		 */
		public V transform(V o, V p);

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/01
	 */
	public static interface TransformEntry<K, V> {

		/**
		 * 
		 * @param k
		 * @param v
		 * @return
		 */
		public V transform(K k, V v);

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/01
	 */
	public static interface Each<V> {

		/**
		 * 
		 * @param v
		 */
		public void each(V v);

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/01
	 */
	public static interface Each2<V> {

		/**
		 * 
		 * @param v
		 */
		public void each(V v, V w);

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/01
	 */
	public static interface EachEntry<K, V> {

		/**
		 * 
		 * @param v
		 */
		public void each(K k, V v);

	}

	/**
	 * 空のSortedMapである.
	 */
	@SuppressWarnings("unchecked")
	public static final SortedMap<Object, Object>
	EMPTY = new SortedMap<Object, Object>() {

		public Object firstKey() {
			throw new NoSuchElementException();
		}

		public Object lastKey() {
			throw new NoSuchElementException();
		}

		public Comparator<Object> comparator() {
			return null;
		}

		public SortedMap<Object, Object> headMap(Object toKey) {
			return this;
		}

		public SortedMap<Object, Object> tailMap(Object fromKey) {
			return this;
		}

		public SortedMap<Object, Object> subMap(
				Object fromKey, Object toKey) {
			return this;
		}

		public int size() {
			return 0;
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public boolean isEmpty() {
			return true;
		}

		public boolean containsKey(Object key) {
			return false;
		}

		public boolean containsValue(Object value) {
			return false;
		}

		public Collection<Object> values() {
			return Collections.EMPTY_SET;
		}

		public void putAll(Map<?, ?> t) {
			throw new UnsupportedOperationException();
		}

		public Set<Map.Entry<Object, Object>> entrySet() {
			return Collections.EMPTY_SET;
		}

		public Set<Object> keySet() {
			return Collections.EMPTY_SET;
		}

		public Object get(Object key) {
			return null;
		}

		public Object remove(Object key) {
			throw new UnsupportedOperationException();
		}

		public Object put(Object key, Object value) {
			throw new UnsupportedOperationException();
		}

		public boolean equals(Object o) {
			if(o instanceof SortedMap) {
				return ((Map<?, ?>)o).isEmpty();
			}
			return false;
		}

		public int hashCode() {
			// 
			return 0;
		}

		public String toString() {
			return "[]";
		}

	};

	//
	private Maps() { }


	@SuppressWarnings("unchecked")
	public static<K, V> Map<K, V> emptySortedMap() {
		return (Map<K, V>)EMPTY;
	}


	/**
	 * Mapを配列から初期化する.
	 * 
	 * @param map  初期化対象のMap
	 * @param data 初期化するデータのある配列
	 * @return  初期化済のMap
	 */
	@SuppressWarnings("unchecked")
	public static<K, V> Map<K, V> init(
			Map<K, V> map, Object[][] data) {
		for(int i = 0; i < data.length; i++) {
			if(data[i].length < 2)
				throw new IllegalArgumentException("too few");

			map.put((K)data[i][0], (V)data[i][1]);
		}
		return map;
	}

	/**
	 * HashMapを配列から初期化する.
	 * HashMapのインスタンスは新しく作成される.
	 * 
	 * @param data 初期化するデータのある配列
	 * @return  初期化済のHashMap
	 */
	public static<K, V> Map<K, V> initHashMap(Object[][] data) {
		return init(new HashMap<K, V>(), data);
	}

	/**
	 * 
	 * @param <T>
	 * @param collection
	 * @return
	 */
	public static<T> Map<T, T> identityMapOf(Set<T> set) {
		final Set<T> s = set;
		final Set<Map.Entry<T, T>> t;

		t = new AbstractSet<Map.Entry<T, T>>() {

			@Override
			public Iterator<Map.Entry<T, T>> iterator() {
				final Iterator<T> itr = s.iterator();

				return new Iterator<Map.Entry<T, T>>() {

					public boolean hasNext() {
						return itr.hasNext();
					}

					public Map.Entry<T, T> next() {
						final T n = itr.next();

						return new Map.Entry<T, T>() {

							public T getKey() {
								return n;
							}

							public T getValue() {
								return n;
							}

							public T setValue(T value) {
								throw new
								UnsupportedOperationException();
							}

						};
					}

					public void remove() {
						itr.remove();
					}

				};
			}

			@Override
			public int size() {
				return s.size();
			}

			@Override
			public boolean contains(Object o) {
				return s.contains(o);
			}

			@Override
			public boolean add(Entry<T, T> e) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean addAll(
					Collection<? extends Entry<T, T>> c) {
				throw new UnsupportedOperationException();
			}

			@Override
			public void clear() {
				s.clear();
			}

		};

		return new AbstractMap<T, T>() {

			@Override
			public Set<Map.Entry<T, T>> entrySet() {
				return t;
			}

			@Override
			public int size() {
				return s.size();
			}

			@Override
			public boolean isEmpty() {
				return s.isEmpty();
			}

			@Override
			public boolean containsValue(Object value) {
				return s.contains(value);
			}

			@Override
			public boolean containsKey(Object key) {
				return s.contains(key);
			}

			@SuppressWarnings("unchecked")
			@Override
			public T get(Object key) {
				return s.contains(key) ? (T)key : null;
			}

			@Override
			public T put(T key, T value) {
				throw new UnsupportedOperationException();
			}

			@SuppressWarnings("unchecked")
			@Override
			public T remove(Object key) {
				return s.remove(key) ? (T)key : null;
			}

			@Override
			public void putAll(Map<? extends T, ? extends T> m) {
				throw new UnsupportedOperationException();
			}

			@Override
			public void clear() {
				s.clear();
			}

			@Override
			public Set<T> keySet() {
				return s;
			}

			@Override
			public Collection<T> values() {
				return s;
			}

		};
	}

	//
	private static<K, V> Map<K, V> _map(
			Transform<V> cnt, Map<K, V> dest,
			boolean removeIfNull, Map<K, V> src) {
		for(Map.Entry<K, V> e : src.entrySet()) {
			V r = cnt.transform(e.getValue());

			if(r != null || !removeIfNull) {
				dest.put(e.getKey(), r);
			}
		}
		return dest;
	}

	//
	private static<K, V> Map<K, V> _mapEntry(
			TransformEntry<K, V> cnt, Map<K, V> dest,
			boolean removeIfNull, Map<K, V> src) {
		for(Map.Entry<K, V> e : src.entrySet()) {
			V r = cnt.transform(e.getKey(), e.getValue());

			if(r != null || !removeIfNull) {
				dest.put(e.getKey(), r);
			}
		}
		return dest;
	}

	//
	private static<K, V> Map<K, V> _map(
			Transform2<V> cnt, Map<K, V> dest,
			boolean removeIfNull,
			Map<K, V> src1, Map<K, V> src2) {
		Set<K> keys = new HashSet<K>();

		keys.addAll(src1.keySet());
		keys.addAll(src2.keySet());
		for(K k : keys) {
			V r = null;

			r = cnt.transform(src1.get(k), src2.get(k));
			if(r != null || !removeIfNull) {
				dest.put(k, r);
			}
		}
		return dest;
	}

	/**
	 * 
	 * @param <K>
	 * @param <V>
	 * @param cnt
	 * @param dest
	 * @param src
	 * @return
	 */
	public static<K, V> Map<K, V> map(
			Transform<V> cnt, Map<K, V> dest, Map<K, V> src) {
		return _map(cnt, dest, false, src);
	}

	/**
	 * 
	 * @param <K>
	 * @param <V>
	 * @param cnt
	 * @param dest
	 * @param src1
	 * @param src2
	 * @return
	 */
	public static<K, V> Map<K, V> map(
			Transform2<V> cnt, Map<K, V> dest,
			Map<K, V> src1, Map<K, V> src2) {
		return _map(cnt, dest, false, src1, src2);
	}

	/**
	 * 
	 * @param <K>
	 * @param <V>
	 * @param cnt
	 * @param dest
	 * @param src
	 * @return
	 */
	public static<K, V> Map<K, V> mapEntry(
			TransformEntry<K, V> cnt,
			Map<K, V> dest, Map<K, V> src) {
		return _mapEntry(cnt, dest, false, src);
	}

	/**
	 * 
	 * @param <K>
	 * @param <V>
	 * @param cnt
	 * @param dest
	 * @param src
	 * @return
	 */
	public static<K, V> Map<K, V> mapRemoveIfNull(
			Transform<V> cnt, Map<K, V> dest, Map<K, V> src) {
		return _map(cnt, dest, true, src);
	}

	/**
	 * 
	 * @param <K>
	 * @param <V>
	 * @param cnt
	 * @param dest
	 * @param src1
	 * @param src2
	 * @return
	 */
	public static<K, V> Map<K, V> mapRemoveIfNull(
			Transform2<V> cnt, Map<K, V> dest,
			Map<K, V> src1, Map<K, V> src2) {
		return _map(cnt, dest, true, src1, src2);
	}

	/**
	 * 
	 * @param <K>
	 * @param <V>
	 * @param cnt
	 * @param dest
	 * @param src
	 * @return
	 */
	public static<K, V> Map<K, V> mapEntryRemoveIfNull(
			TransformEntry<K, V> cnt,
			Map<K, V> dest, Map<K, V> src) {
		return _mapEntry(cnt, dest, true, src);
	}

	/**
	 * 
	 * @param <K>
	 * @param <V>
	 * @param cnt
	 * @param dest
	 * @param src
	 * @return
	 */
	public static<K, V> void each(Each<V> cnt, Map<K, V> src) {
		for(Map.Entry<K, V> e : src.entrySet()) {
			cnt.each(e.getValue());
		}
	}

	/**
	 * 
	 * @param <K>
	 * @param <V>
	 * @param cnt
	 * @param src1
	 * @param src2
	 */
	public static<K, V> void each(
			Each2<V> cnt, Map<K, V> src1, Map<K, V> src2) {
		Set<K> keys = new HashSet<K>();

		keys.addAll(src1.keySet());
		keys.addAll(src2.keySet());
		for(K k : keys) {
			cnt.each(src1.get(k), src2.get(k));
		}
	}

	/**
	 * 
	 * @param <K>
	 * @param <V>
	 * @param cnt
	 * @param dest
	 * @param src
	 * @return
	 */
	public static<K, V> void eachEntry(
			EachEntry<K, V> cnt, Map<K, V> src) {
		for(Map.Entry<K, V> e : src.entrySet()) {
			cnt.each(e.getKey(), e.getValue());
		}
	}

	//
	private static void matchstar(StringPointer p, StringPointer q) {
		for(; q.isValid() && q.charAt() != '.'; q.inclement());
	}

	//
	private static void matchstar2(StringPointer p, StringPointer q) {
		StringPointer s, t, v;

		s = p.clone().add(2);
		t = q.clone().moveEnd();
		v = t.clone().inclement();
		while(q.compareTo(v) < 0 && !matchstr(s, v, null)) {
			for(; t.isValid() && t.charAt() == '.'; t.declement());
			for(; t.isValid() && t.charAt() != '.'; t.declement());
			for(; t.isValid() && t.charAt() == '.'; t.declement());
			s.overwrite(p).add(2);
			v.overwrite(t).inclement();
		}
		q.overwrite(t.inclement());
	}

	//
	private static boolean matchstr(
			StringPointer p, StringPointer q, List<String> r) {
		boolean starok = true;
		int x;

		while(p.isValid()) {
			if(p.charAt() == '.' && p.charAtSafe(1) == '*' &&
					p.charAtSafe(2) == '*' && !p.isValid(3)) {
				String sx = q.getReferent();

				if(!q.isValid()) {
					if(r != null) {
						r.add("");
					}
				} else if(r != null) {
					r.add(sx.substring(q.getIndex() + 1, sx.length()));
				}
				return true;
			} else if(p.charAt() == '*' && p.charAtSafe(1) == '*') {
				x = q.getIndex();
				if(!starok || (p.isValid(2) && p.charAt(2) != '.')) {
					throw new InvalidPatternException();
				}
				matchstar2(p, q);
				if(q.getIndex() < x) {
					if(r != null) {
						r.add("");
					}
				} else if(r != null) {
					r.add(q.getReferent().substring(x, q.getIndex()));
				}
				p.add(2);
			} else if(p.charAt() == '*') {
				x = q.getIndex();
				if(!starok || (p.isValid(1) && p.charAt(1) != '.')) {
					throw new InvalidPatternException();
				}
				matchstar(p, q);
				if(q.getIndex() < x) {
					r.add("");
				} else if(r != null) {
					r.add(q.getReferent().substring(x, q.getIndex()));
				}
				p.add(1);
			} else if(p.charAt() == q.charAtSafe()) {
				starok = p.charAt() == '.';
				q.inclement();
				p.inclement();
			} else {
				return false;
			}
		}
		return q.getIndex() == q.getReferent().length();
	}

	/**
	 * 
	 * @param pat
	 * @param key
	 * @return
	 */
	public static String[] matchPropertyKey(String pat, String key) {
		StringPointer p = new StringPointer(pat);
		StringPointer q = new StringPointer(key);
		List<String>  r = new ArrayList<String>();

		if(pat.matches(".*(\\*\\*\\*+|\\*[^.*]|[^.*]\\*).*")) {
			throw new InvalidPatternException();
		}
		return matchstr(p, q, r) ? r.toArray(new String[0]) : null;
	}

}
