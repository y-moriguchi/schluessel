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

import java.util.AbstractMap;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import net.morilib.util.Objects;
import net.morilib.util.SimpleMap;
import net.morilib.util.Tuple2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/21
 */
public class TupleMap<K, V> extends AbstractMap<K, V>
implements SimpleMap<K, V> {
	
	//
	private Tuple2<K, V> value;
	
	//
	private static class TupleEntry<K, V> implements Map.Entry<K, V> {
		
		//
		private Tuple2<K, V> value;
		
		/**
		 * @param value2
		 */
		public TupleEntry(Tuple2<K, V> value2) {
			this.value = value2;
		}

		/* (non-Javadoc)
		 * @see java.util.Map.Entry#getKey()
		 */
		public K getKey() {
			return value.getA();
		}

		/* (non-Javadoc)
		 * @see java.util.Map.Entry#getValue()
		 */
		public V getValue() {
			return value.getB();
		}

		/* (non-Javadoc)
		 * @see java.util.Map.Entry#setValue(java.lang.Object)
		 */
		public V setValue(V value) {
			throw new UnsupportedOperationException();
		}

		@Override
		public int hashCode() {
			return value.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			if(obj instanceof TupleEntry<?, ?>) {
				return value.equals(((TupleEntry<?, ?>)obj).value);
			}
			return false;
		}

		@Override
		public String toString() {
			return value.toString();
		}
		
	}

	/**
	 * @param var
	 * @param val
	 */
	public TupleMap(K var, V val) {
		value = new Tuple2<K, V>(var, val);
	}


	/* (non-Javadoc)
	 * @see java.util.AbstractMap#entrySet()
	 */
	@Override
	public Set<Map.Entry<K, V>> entrySet() {
		Map.Entry<K, V> e = new TupleEntry<K, V>(value);
		
		return Collections.singleton(e);
	}

	
	@Override
	public int size() {
		return 1;
	}

	
	@Override
	public boolean isEmpty() {
		return false;
	}

	
	@Override
	public boolean containsValue(Object v) {
		return Objects.equals(value.getB(), v);
	}

	
	@Override
	public boolean containsKey(Object key) {
		return Objects.equals(value.getA(), key);
	}

	
	@Override
	public V get(Object key) {
		return Objects.equals(value.getA(), key) ? value.getB() : null;
	}

	
	@Override
	public V put(K key, V value) {
		throw new UnsupportedOperationException();
	}

	
	@Override
	public V remove(Object key) {
		throw new UnsupportedOperationException();
	}
	

	@Override
	public void putAll(Map<? extends K, ? extends V> m) {
		throw new UnsupportedOperationException();
	}

	
	@Override
	public void clear() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.SimpleMap#map(java.lang.Object)
	 */
	public V map(K key) {
		return get(key);
	}

}
