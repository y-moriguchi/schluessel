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

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import net.morilib.util.Objects;
import net.morilib.util.SimpleMap;
import net.morilib.util.collection.MappedCollection;
import net.morilib.util.set.MappedSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/15
 */
public class CompositeMap<K, W, V> implements Map<K, V> {
	
	//
	private Map<K, W> wrapee;
	private SimpleMap<W, V> composite;
	private final Tf2 tf2 = new Tf2();
	
	//
	private class En2 implements Map.Entry<K, V> {
		
		private Map.Entry<K, W> wrp;
		
		private En2(Map.Entry<K, W> wrp) {
			this.wrp = wrp;
		}
		
		public K getKey() {
			return wrp.getKey();
		}

		public V getValue() {
			return composite.map(wrp.getValue());
		}

		public V setValue(V value) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	//
	private class Tf2 implements
	SimpleMap<Map.Entry<K, W>, Map.Entry<K, V>> {

		/* (non-Javadoc)
		 * @see net.morilib.util.SimpleMap#map(java.lang.Object)
		 */
		public Map.Entry<K, V> map(Map.Entry<K, W> key) {
			return new En2(key);
		}
		
	}

	/**
	 * @param map
	 */
	public CompositeMap(Map<K, W> map, SimpleMap<W, V> composite) {
		this.wrapee    = map;
		this.composite = composite;
	}
	
	/* (non-Javadoc)
	 * @see java.util.Map#size()
	 */
	public int size() {
		return wrapee.size();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#isEmpty()
	 */
	public boolean isEmpty() {
		return wrapee.isEmpty();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object key) {
		return wrapee.containsKey(key);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		for(W w : wrapee.values()) {
			if(Objects.equals(value, composite.map(w))) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public V get(Object key) {
		return composite.map(wrapee.get(key));
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public V put(K key, V value) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public V remove(Object key) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends K, ? extends V> m) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#clear()
	 */
	public void clear() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public Set<K> keySet() {
		return wrapee.keySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#values()
	 */
	public Collection<V> values() {
		return new MappedCollection<W, V>(wrapee.values(), composite);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		return new MappedSet<Map.Entry<K, W>, Map.Entry<K, V>>(
				wrapee.entrySet(), tf2);
	}

}
