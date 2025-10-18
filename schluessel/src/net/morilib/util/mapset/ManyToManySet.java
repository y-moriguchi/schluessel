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

import java.util.Set;

import net.morilib.util.Pair;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/02
 */
public interface ManyToManySet<K, V> extends Set<Pair<K, V>> {
	
	
	public boolean contains(Object k, Object v);
	
	
	public boolean containsKey(Object k);
	
	
	public boolean containsValue(Object v);
	
	
	public Set<V> getValues(Object k);
	
	
	public Set<K> getKeys(Object v);
	
	
	public Set<K> keySet();
	
	
	public Set<V> valueSet();
	
	
	public Pair<K, V> put(K k, V v);
	
	
	public boolean put(Set<K> ks, Set<V> vs);
	
	
	public boolean remove(Object k, Object v);
	
	
	public boolean removeKey(Object k);
	
	
	public boolean removeValue(Object v);
	
}
