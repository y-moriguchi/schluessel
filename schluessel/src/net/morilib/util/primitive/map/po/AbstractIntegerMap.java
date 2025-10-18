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
package net.morilib.util.primitive.map.po;

import java.util.AbstractCollection;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import net.morilib.util.primitive.AbstractIntegerSet;
import net.morilib.util.primitive.IntegerSet;
import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractIntegerMap<V> implements IntegerMap<V> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient IntegerSet keySet = null;
	private transient Collection<V> values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = intKeyEntrySet().iterator();
		while(i.hasNext()) {
			i.next();
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#inRange(int)
	 */
	public boolean containsValue(Object v) {
		for(PrimitiveEntryK<V> e : intKeyEntrySet()) {
			Object p = e.getValue();
			
			if((v == null && p == null) ||
					(v != null && v.equals(p))) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#get(java.lang.Object)
	 */
	public V getElement(int k) {
		for(PrimitiveEntryK<V> e : intKeyEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractIntegerSet {
		
		//
		private Set<PrimitiveEntryK<V>> eset;
		
		private KSet(Set<PrimitiveEntryK<V>> eset) {
			this.eset = eset;
		}
		
		public IntegerIterator intIterator() {
			final Iterator<PrimitiveEntryK<V>> ei = eset.iterator();
			
			return new IntegerIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public int next() {
					return ei.next().getKey();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractIntegerMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractIntegerMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractIntegerMap.this.clear();
		}

		public boolean addInt(int v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#keySet()
	 */
	public IntegerSet intKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(intKeyEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#putAll(net.morilib.util.primitive.map.op.ToIntegerMap)
	 */
	public void putAllElement(IntegerMap<V> map) {
		for(PrimitiveEntryK<V> e : map.intKeyEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractCollection<V> {
		
		//
		private Set<PrimitiveEntryK<V>> eset;
		
		private VCol(Set<PrimitiveEntryK<V>> eset) {
			this.eset = eset;
		}

		public Iterator<V> iterator() {
			final Iterator<PrimitiveEntryK<V>> ei = eset.iterator();
			
			return new Iterator<V>() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public V next() {
					return ei.next().getValue();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractIntegerMap.this.size();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#values()
	 */
	public Collection<V> values() {
		return (values == null) ?
				(values = new VCol(intKeyEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#isTotal()
	 */
	public boolean isTotal() {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object key) {
		if(key instanceof Integer) {
			return containsKeyElement(((Integer)key).intValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public V get(Object key) {
		if(key instanceof Integer) {
			return getElement(((Integer)key).intValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public V put(Integer key, V value) {
		return putElement(key.intValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public V remove(Object key) {
		if(key instanceof Integer) {
			return removeElement(((Integer)key).intValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Integer, ? extends V> m) {
		for(Map.Entry<? extends Integer, ? extends V> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public Set<Integer> keySet() {
		return intKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Integer, V>> entrySet() {
		final Set<PrimitiveEntryK<V>> e = intKeyEntrySet();
		
		return new AbstractSet<Entry<Integer, V>>() {

			public Iterator<Map.Entry<Integer, V>> iterator() {
				final Iterator<PrimitiveEntryK<V>> i = e.iterator();
				
				return new Iterator<Entry<Integer, V>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Map.Entry<Integer, V> next() {
						final PrimitiveEntryK<V> o = i.next();
						
						return new Map.Entry<Integer, V>() {

							public Integer getKey() {
								return o.getKey();
							}

							public V getValue() {
								return o.getValue();
							}

							public V setValue(V value) {
								return o.setValue(value);
							}
							
						};
					}

					public void remove() {
						i.remove();
					}
					
				};
			}

			public int size() {
				return e.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Integer.MIN_VALUE || k > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((int)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#containsKeyInt(int)
	 */
	public boolean containsKeyElement(int k) {
		for(PrimitiveEntryK<V> e : intKeyEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#get(int)
	 */
	public V get(int k) {
		if(k < Integer.MIN_VALUE || k > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((int)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#put(int, java.lang.Object)
	 */
	public V put(int k, V v) {
		if(k < Integer.MIN_VALUE || k > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((int)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#removeElement(int)
	 */
	public V removeElement(int k) {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = intKeyEntrySet().iterator();
		while(i.hasNext()) {
			PrimitiveEntryK<V> e = i.next();
			
			if(e.getKey() == k) {
				V r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#remove(int)
	 */
	public V remove(int k) {
		if(k < Integer.MIN_VALUE || k > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((int)k);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntryK<V> e : intKeyEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof IntegerMap) {
			IntegerMap<V> m = (IntegerMap<V>)obj;
			Iterator<PrimitiveEntryK<V>> i;
			
			i = intKeyEntrySet().iterator();
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntryK<V> o = i.next();
				V v = m.getElement(o.getKey());
				V p = o.getValue();
				
				if(!m.containsKeyElement(o.getKey()) ||
						!((v == null && p == null) ||
								(v != null && v.equals(p)))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();
		String d = "";
		
		b.append("{");
		for(PrimitiveEntryK<V> e : intKeyEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
