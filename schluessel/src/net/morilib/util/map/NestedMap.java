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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.morilib.util.SimpleMap;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/21
 */
public class NestedMap<K, V> extends AbstractMap<K, V>
implements SimpleMap<K, V> {
	
	//
	private Map<K, V> composite;
	private Map<K, V> elems;
	
	
	public NestedMap(Map<K, V> elems, Map<K, V> composite) {
		this.elems     = elems;
		this.composite = composite;
	}
	
	/* (non-Javadoc)
	 * @see java.util.AbstractMap#entrySet()
	 */
	@Override
	public Set<Map.Entry<K, V>> entrySet() {
		Set<Map.Entry<K, V>> res =
			new HashSet<Map.Entry<K, V>>(elems.entrySet());
		
		if(composite != null) {
			for(Map.Entry<K, V> e : composite.entrySet()) {
				if(!elems.containsKey(e.getKey())) {
					res.add(e);
				}
			}
		}
		return res;
	}

	
	@Override
	public boolean containsKey(Object key) {
		if(elems.containsKey(key)) {
			return true;
		} else if(composite != null) {
			return composite.containsKey(key);
		} else {
			return false;
		}
	}

	
	@Override
	public V get(Object key) {
		if(elems.containsKey(key)) {
			return elems.get(key);
		} else if(composite != null) {
			return composite.get(key);
		} else {
			return null;
		}
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
