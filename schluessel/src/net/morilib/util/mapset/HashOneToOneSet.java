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
package net.morilib.util.mapset;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.AbstractSet;
import java.util.Collections;
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import net.morilib.util.Objects;
import net.morilib.util.Pair;
import net.morilib.util.Tuple2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/03
 */
public class HashOneToOneSet<K, V>
extends AbstractSet<Pair<K, V>>
implements OneToOneSet<K, V>, java.io.Serializable {

	//
	private static final long serialVersionUID = -336189056797863107L;

	//
	private HashMap<K, V> keyMap = new HashMap<K, V>();
	private transient HashMap<V, K> valueMap = new HashMap<V, K>();
	private transient volatile int modCount = 0;

	/**
	 * 
	 */
	public HashOneToOneSet() { }

	/**
	 * 
	 * @param map
	 */
	public HashOneToOneSet(Map<K, V> map) {
		for(Map.Entry<K, V> e : map.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/**
	 * 
	 * @param objects
	 */
	@SuppressWarnings("unchecked")
	public HashOneToOneSet(Object[]... objects) {
		for(int i = 0; i < objects.length; i++) {
			Object[] os = objects[i];

			if(os.length != 2) {
				throw new IllegalArgumentException();
			}
			put((K)os[0], (V)os[1]);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return keyMap.isEmpty();
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#contains(java.lang.Object)
	 */
	@Override
	public boolean contains(Object o) {
		if(o instanceof Pair) {
			return (keyMap.containsKey  (((Pair<?, ?>)o).getA()) ||
					valueMap.containsKey(((Pair<?, ?>)o).getB()));
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#add(java.lang.Object)
	 */
	@Override
	public boolean add(Pair<K, V> e) {
		if(e == null) {
			throw new NullPointerException();
		}
		return put(e.getA(), e.getB()) != null;
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#remove(java.lang.Object)
	 */
	@Override
	public boolean remove(Object o) {
		if(o instanceof Pair) {
			return remove(
					((Pair<?, ?>)o).getA(),
					((Pair<?, ?>)o).getB());
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#clear()
	 */
	@Override
	public void clear() {
		keyMap.clear();
		valueMap.clear();
		modCount++;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.onetoone.OneToOneSet#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object k) {
		return keyMap.containsKey(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.onetoone.OneToOneSet#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object v) {
		return valueMap.containsKey(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.onetoone.OneToOneSet#getValue(java.lang.Object)
	 */
	public V getValue(Object k) {
		return keyMap.get(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.onetoone.OneToOneSet#getKey(java.lang.Object)
	 */
	public K getKey(Object v) {
		return valueMap.get(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.onetoone.OneToOneSet#keySet()
	 */
	public Set<K> keySet() {
		return Collections.unmodifiableSet(keyMap.keySet());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.onetoone.OneToOneSet#valueSet()
	 */
	public Set<V> valueSet() {
		return Collections.unmodifiableSet(valueMap.keySet());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.onetoone.OneToOneSet#put(java.lang.Object, java.lang.Object)
	 */
	public Pair<K, V> put(K k, V v) {
		final V vb;
		final K kb;

		modCount++;
		vb = keyMap.remove(k);
		kb = valueMap.remove(v);
		keyMap.put(k, v);
		valueMap.put(v, k);

		if(vb != null) {
			return new Tuple2<K, V>(k, vb);
		} else if(kb != null) {
			return new Tuple2<K, V>(kb, v);
		} else {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#put(java.util.Set, java.util.Set)
	 */
	public boolean put(Set<K> ks, Set<V> vs) {
		boolean r = false;

		for(K k : ks) {
			for(V v : vs) {
				r = (put(k, v) != null) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#remove(java.lang.Object, java.lang.Object)
	 */
	public boolean remove(Object k, Object v) {
		modCount++;
		if(keyMap.containsKey(k)) {
			V v2 = keyMap.get(k);

			if(Objects.equals(v, v2)) {
				keyMap.remove(v);
				valueMap.remove(k);
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.onetoone.OneToOneSet#removeKey(java.lang.Object)
	 */
	public boolean removeKey(Object k) {
		boolean r = keyMap.containsKey(k);
		V v = keyMap.remove(k);

		if(v != null) {
			valueMap.remove(v);
		}
		modCount++;
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.onetoone.OneToOneSet#removeValue(java.lang.Object)
	 */
	public boolean removeValue(Object v) {
		boolean r = valueMap.containsKey(v);
		K k = valueMap.remove(v);

		if(k != null) {
			keyMap.remove(k);
		}
		modCount++;
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#iterator()
	 */
	@Override
	public Iterator<Pair<K, V>> iterator() {
		return new Iterator<Pair<K, V>>() {

			//
			private Iterator<Map.Entry<K, V>> iter =
				keyMap.entrySet().iterator();
			private Map.Entry<K, V> cur = null;
			private int expectedModCount = modCount;

			public boolean hasNext() {
				return iter.hasNext();
			}

			public Pair<K, V> next() {
				final Map.Entry<K, V> n = iter.next();

				if(expectedModCount != modCount) {
					throw new ConcurrentModificationException();
				}
				cur = n;
				return new Tuple2<K, V>(n.getKey(), n.getValue());
			}

			public void remove() {
				if(cur == null) {
					throw new IllegalStateException();
				} else if(expectedModCount != modCount) {
					throw new ConcurrentModificationException();
				}
				removeKey(cur.getKey());
				expectedModCount = modCount;
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#size()
	 */
	@Override
	public int size() {
		return keyMap.size();
	}

	//
	private void readObject(
			ObjectInputStream ois
			) throws IOException, ClassNotFoundException {
		ois.defaultReadObject();

		valueMap = new HashMap<V, K>();
		for(Map.Entry<K, V> e : keyMap.entrySet()) {
			valueMap.put(e.getValue(), e.getKey());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#contains(java.lang.Object, java.lang.Object)
	 */
	public boolean contains(Object k, Object v) {
		return containsKey(k) && containsValue(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#getValues(java.lang.Object)
	 */
	public Set<V> getValues(Object k) {
		return Collections.singleton(getValue(k));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#getKeys(java.lang.Object)
	 */
	public Set<K> getKeys(Object v) {
		return Collections.singleton(getKey(v));
	}

}
