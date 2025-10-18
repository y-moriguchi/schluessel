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

import net.morilib.util.primitive.AbstractLongSet;
import net.morilib.util.primitive.LongSet;
import net.morilib.util.primitive.iterator.LongIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractLongMap<V> implements LongMap<V> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient LongSet keySet = null;
	private transient Collection<V> values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = longKeyEntrySet().iterator();
		while(i.hasNext()) {
			i.next();
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#inRange(long)
	 */
	public boolean containsValue(Object v) {
		for(PrimitiveEntryK<V> e : longKeyEntrySet()) {
			Object p = e.getValue();
			
			if((v == null && p == null) ||
					(v != null && v.equals(p))) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#get(java.lang.Object)
	 */
	public V getElement(long k) {
		for(PrimitiveEntryK<V> e : longKeyEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractLongSet {
		
		//
		private Set<PrimitiveEntryK<V>> eset;
		
		private KSet(Set<PrimitiveEntryK<V>> eset) {
			this.eset = eset;
		}
		
		public LongIterator longIterator() {
			final Iterator<PrimitiveEntryK<V>> ei = eset.iterator();
			
			return new LongIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public long next() {
					return ei.next().getKey();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractLongMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractLongMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractLongMap.this.clear();
		}

		public boolean addLong(long v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#keySet()
	 */
	public LongSet longKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(longKeyEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#putAll(net.morilib.util.primitive.map.op.ToLongMap)
	 */
	public void putAllElement(LongMap<V> map) {
		for(PrimitiveEntryK<V> e : map.longKeyEntrySet()) {
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
			return AbstractLongMap.this.size();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#values()
	 */
	public Collection<V> values() {
		return (values == null) ?
				(values = new VCol(longKeyEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#isTotal()
	 */
	public boolean isTotal() {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object key) {
		if(key instanceof Long) {
			return containsKeyElement(((Long)key).longValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public V get(Object key) {
		if(key instanceof Long) {
			return getElement(((Long)key).longValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public V put(Long key, V value) {
		return putElement(key.longValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public V remove(Object key) {
		if(key instanceof Long) {
			return removeElement(((Long)key).longValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Long, ? extends V> m) {
		for(Map.Entry<? extends Long, ? extends V> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public Set<Long> keySet() {
		return longKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Long, V>> entrySet() {
		final Set<PrimitiveEntryK<V>> e = longKeyEntrySet();
		
		return new AbstractSet<Entry<Long, V>>() {

			public Iterator<Map.Entry<Long, V>> iterator() {
				final Iterator<PrimitiveEntryK<V>> i = e.iterator();
				
				return new Iterator<Entry<Long, V>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Map.Entry<Long, V> next() {
						final PrimitiveEntryK<V> o = i.next();
						
						return new Map.Entry<Long, V>() {

							public Long getKey() {
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
	 * @see net.morilib.util.primitive.map.po.LongMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Long.MIN_VALUE || k > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((long)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#containsKeyLong(long)
	 */
	public boolean containsKeyElement(long k) {
		for(PrimitiveEntryK<V> e : longKeyEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#get(int)
	 */
	public V get(int k) {
		if(k < Long.MIN_VALUE || k > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((long)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#put(int, java.lang.Object)
	 */
	public V put(int k, V v) {
		if(k < Long.MIN_VALUE || k > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((long)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#removeElement(long)
	 */
	public V removeElement(long k) {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = longKeyEntrySet().iterator();
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
	 * @see net.morilib.util.primitive.map.po.LongMap#remove(int)
	 */
	public V remove(int k) {
		if(k < Long.MIN_VALUE || k > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((long)k);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntryK<V> e : longKeyEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof LongMap) {
			LongMap<V> m = (LongMap<V>)obj;
			Iterator<PrimitiveEntryK<V>> i;
			
			i = longKeyEntrySet().iterator();
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
		for(PrimitiveEntryK<V> e : longKeyEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
