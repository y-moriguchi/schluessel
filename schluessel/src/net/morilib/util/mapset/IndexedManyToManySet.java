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

import java.util.AbstractSet;
import java.util.Collections;
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import net.morilib.util.Pair;
import net.morilib.util.Tuple2;
import net.morilib.util.datafactory.DataFactories;
import net.morilib.util.datafactory.SetFactory;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/05
 */
public class IndexedManyToManySet<K, V> extends AbstractSet<Pair<K, V>>
implements ManyToManySet<K, V> {
	
	//
	private SetFactory factory;
	private Map<K, Set<V>> keyindex = new HashMap<K, Set<V>>();
	private transient Map<V, Set<K>> valueindex =
		new HashMap<V, Set<K>>();
	private transient volatile int modCount = 0;
	
	/**
	 * 
	 */
	public IndexedManyToManySet() {
		factory = DataFactories.HASH_SET;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#contains(java.lang.Object, java.lang.Object)
	 */
	public boolean contains(Object k, Object v) {
		return keyindex.containsKey(k) && keyindex.get(k).contains(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object k) {
		return keyindex.containsKey(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object v) {
		return valueindex.containsKey(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#getValues(java.lang.Object)
	 */
	public Set<V> getValues(Object k) {
		Set<V> r = keyindex.get(k);
		
		if(r == null) {
			return Collections.emptySet();
		} else {
			return Collections.unmodifiableSet(r);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#getKeys(java.lang.Object)
	 */
	public Set<K> getKeys(Object v) {
		Set<K> r = valueindex.get(v);
		
		if(r == null) {
			return Collections.emptySet();
		} else {
			return Collections.unmodifiableSet(r);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#keySet()
	 */
	public Set<K> keySet() {
		return keyindex.keySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#valueSet()
	 */
	public Set<V> valueSet() {
		return valueindex.keySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#put(java.lang.Object, java.lang.Object)
	 */
	public Pair<K, V> put(K k, V v) {
		Pair<K, V> res;
		Set<V> vs = keyindex.get(k);
		Set<K> ks;
		
		modCount++;
		if(vs == null) {
			vs = factory.newInstance();
			keyindex.put(k, vs);
		}
		res = vs.add(v) ? new Tuple2<K, V>(k, v) : null;
		
		ks = valueindex.get(v);
		if(ks == null) {
			ks = factory.newInstance();
			valueindex.put(v, ks);
		}
		ks.add(k);
		
		return res;
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
		Set<V> vs;
		
		vs = keyindex.get(k);
		if(vs != null && vs.contains(v)) {
			vs.remove(v);
			if(vs.isEmpty()) {
				keyindex.remove(k);
				valueindex.get(v).remove(k);
				if(valueindex.get(v).isEmpty()) {
					valueindex.remove(v);
				}
			}
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#removeKey(java.lang.Object)
	 */
	public boolean removeKey(Object k) {
		Set<V> vs = keyindex.get(k);
		
		modCount++;
		if(keyindex.containsKey(k)) {
			keyindex.remove(k);
			for(V v : vs) {
				Set<K> ks = valueindex.get(v);
				
				ks.remove(k);
				if(ks.isEmpty()) {
					valueindex.remove(v);
				}
			}
			return true;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.mapset.ManyToManySet#removeValue(java.lang.Object)
	 */
	public boolean removeValue(Object v) {
		Set<K> ks = valueindex.get(v);
		
		modCount++;
		if(valueindex.containsKey(v)) {
			valueindex.remove(v);
			for(K k : ks) {
				Set<V> vs = keyindex.get(k);
				
				vs.remove(k);
				if(vs.isEmpty()) {
					keyindex.remove(k);
				}
			}
			return true;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Set#size()
	 */
	@Override
	public int size() {
		int res = 0;
		
		for(Set<V> vs : keyindex.values()) {
			res += vs.size();
		}
		return res;
	}

	/* (non-Javadoc)
	 * @see java.util.Set#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return keyindex.isEmpty();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#contains(java.lang.Object)
	 */
	@Override
	public boolean contains(Object o) {
		if(o instanceof Pair) {
			contains(((Pair<?, ?>)o).getA(), ((Pair<?, ?>)o).getB());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractSet#iterator()
	 */
	@Override
	public Iterator<Pair<K, V>> iterator() {
		return new Iterator<Pair<K, V>>() {
			
			//
			private Iterator<Map.Entry<K, Set<V>>> iter =
				keyindex.entrySet().iterator();
			private Iterator<V> iter2 = null;
			private K curK = null;
			private V curV = null;
			private int expectedModCount = modCount;
			
			public boolean hasNext() {
				return ((iter2 != null && iter2.hasNext()) ||
						iter.hasNext());
			}

			public Pair<K, V> next() {
				V n;
				
				if(expectedModCount != modCount) {
					throw new ConcurrentModificationException();
				} else if(iter2 == null || !iter2.hasNext()) {
					Map.Entry<K, Set<V>> e0 = iter.next();
					
					curK  = e0.getKey();
					iter2 = e0.getValue().iterator();
				}
				curV = n = iter2.next();
				return new Tuple2<K, V>(curK, n);
			}

			public void remove() {
				if(curV == null) {
					throw new IllegalStateException();
				} else if(expectedModCount != modCount) {
					throw new ConcurrentModificationException();
				}
				IndexedManyToManySet.this.remove(curK, curV);
				expectedModCount = modCount;
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see java.util.Set#add(java.lang.Object)
	 */
	@Override
	public boolean add(Pair<K, V> e) {
		modCount++;
		return put(e.getA(), e.getB()) != null;
	}

	/* (non-Javadoc)
	 * @see java.util.Set#remove(java.lang.Object)
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

	/* (non-Javadoc)
	 * @see java.util.Set#clear()
	 */
	@Override
	public void clear() {
		keyindex.clear();
		valueindex.clear();
		modCount++;
	}

}
