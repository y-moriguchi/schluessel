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
package net.morilib.util.primitive.map.op;

import java.util.AbstractCollection;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import net.morilib.util.primitive.AbstractLongCollection;
import net.morilib.util.primitive.LongCollection;
import net.morilib.util.primitive.iterator.LongIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractLongValueMap<K> implements LongValueMap<K> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient Set<K> keySet = null;
	private transient LongCollection values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryV<K>> i = longValueEntrySet().iterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#inDomain(java.lang.Object)
	 */
	public boolean containsKey(Object k) {
		if(k instanceof Long) {
			containsValueElement(((Long)k).longValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#inRange(long)
	 */
	public boolean containsValueElement(long v) {
		for(PrimitiveEntryV<K> e : longValueEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#get(java.lang.Object)
	 */
	public Long get(Object k) {
		if(k instanceof Long) {
			for(PrimitiveEntryV<K> e : longValueEntrySet()) {
				Object l = e.getKey();
				
				if((k == null && l == null) ||
						(k != null && k.equals(l))) {
					return e.getValue();
				}
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#getValue(long)
	 */
	public long getElement(Object k) {
		if(k instanceof Long) {
			for(PrimitiveEntryV<K> e : longValueEntrySet()) {
				Object l = e.getKey();
				
				if((k == null && l == null) ||
						(k != null && k.equals(l))) {
					return e.getValue();
				}
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractSet<K> {
		
		//
		private Set<PrimitiveEntryV<K>> eset;
		
		private KSet(Set<PrimitiveEntryV<K>> eset) {
			this.eset = eset;
		}
		
		public Iterator<K> iterator() {
			final Iterator<PrimitiveEntryV<K>> ei = eset.iterator();
			
			return new Iterator<K>() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public K next() {
					return ei.next().getKey();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractLongValueMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractLongValueMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractLongValueMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#keySet()
	 */
	public Set<K> keySet() {
		return (keySet == null) ?
				(keySet = new KSet(longValueEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#put(java.lang.Object, long)
	 */
	public Long put(K k, long v) {
		boolean b = containsKey(k);
		long r = putElement(k, v);
		
		return b ? r : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#putAll(net.morilib.util.primitive.map.op.ToLongMap)
	 */
	public void putAllElement(LongValueMap<K> map) {
		for(PrimitiveEntryV<K> e : map.longValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractLongCollection {
		
		//
		private Set<PrimitiveEntryV<K>> eset;
		
		private VCol(Set<PrimitiveEntryV<K>> eset) {
			this.eset = eset;
		}

		public boolean addLong(long v) {
			throw new UnsupportedOperationException();
		}

		public LongIterator longIterator() {
			final Iterator<PrimitiveEntryV<K>> ei = eset.iterator();
			
			return new LongIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public long next() {
					return ei.next().getValue();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractLongValueMap.this.size();
		}

		public boolean isInfinite() {
			return false;
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#values()
	 */
	public LongCollection longValues() {
		return (values == null) ?
				(values = new VCol(longValueEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Long.MIN_VALUE || v > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((long)v);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Long) {
			return containsValueElement(
					((Long)value).longValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Long put(K key, Long value) {
		if(value instanceof Long) {
			return putElement(key, ((Long)value).longValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends K, ? extends Long> m) {
		for(PrimitiveEntryV<K> e : longValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#values()
	 */
	public Collection<Long> values() {
		final Iterator<PrimitiveEntryV<K>> i = longValueEntrySet().iterator();
		
		return new AbstractCollection<Long>() {

			public Iterator<Long> iterator() {
				return new Iterator<Long>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Long next() {
						return i.next().getValue();
					}

					public void remove() {
						i.remove();
					}
					
				};
			}

			public int size() {
				return AbstractLongValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<K, Long>> entrySet() {
		final Iterator<PrimitiveEntryV<K>> i = longValueEntrySet().iterator();
		
		return new AbstractSet<Entry<K, Long>>() {

			public Iterator<Entry<K, Long>> iterator() {
				return new Iterator<Entry<K, Long>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Entry<K, Long> next() {
						final PrimitiveEntryV<K> e = i.next();
						
						return new Map.Entry<K, Long>() {

							public K getKey() {
								return e.getKey();
							}

							public Long getValue() {
								return e.getValue();
							}

							public Long setValue(Long value) {
								return e.setValue(value);
							}
							
						};
					}

					public void remove() {
						i.remove();
					}
					
				};
			}

			public int size() {
				return AbstractLongValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#remove(java.lang.Object)
	 */
	public Long remove(Object k) {
		Iterator<PrimitiveEntryV<K>> i = longValueEntrySet().iterator();
		
		while(i.hasNext()) {
			PrimitiveEntryV<K> e = i.next();
			Object l = e.getKey();
			
			if((k == null && l == null) ||
					(k != null && k.equals(l))) {
				long r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntryV<K> e : longValueEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof LongValueMap) {
			LongValueMap<K> m = (LongValueMap<K>)obj;
			Iterator<PrimitiveEntryV<K>> i = longValueEntrySet().iterator();
			
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntryV<K> o = i.next();
				
				if(!m.containsKey(o.getKey()) ||
						m.getElement(o.getKey()) != o.getValue()) {
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
		for(PrimitiveEntryV<K> e : longValueEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
