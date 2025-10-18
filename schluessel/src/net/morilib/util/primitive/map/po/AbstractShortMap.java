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

import net.morilib.util.primitive.AbstractShortSet;
import net.morilib.util.primitive.ShortSet;
import net.morilib.util.primitive.iterator.ShortIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractShortMap<V> implements ShortMap<V> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient ShortSet keySet = null;
	private transient Collection<V> values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = shortKeyEntrySet().iterator();
		while(i.hasNext()) {
			i.next();
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#inRange(short)
	 */
	public boolean containsValue(Object v) {
		for(PrimitiveEntryK<V> e : shortKeyEntrySet()) {
			Object p = e.getValue();
			
			if((v == null && p == null) ||
					(v != null && v.equals(p))) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#get(java.lang.Object)
	 */
	public V getElement(short k) {
		for(PrimitiveEntryK<V> e : shortKeyEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractShortSet {
		
		//
		private Set<PrimitiveEntryK<V>> eset;
		
		private KSet(Set<PrimitiveEntryK<V>> eset) {
			this.eset = eset;
		}
		
		public ShortIterator shortIterator() {
			final Iterator<PrimitiveEntryK<V>> ei = eset.iterator();
			
			return new ShortIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public short next() {
					return ei.next().getKey();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractShortMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractShortMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractShortMap.this.clear();
		}

		public boolean addShort(short v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#keySet()
	 */
	public ShortSet shortKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(shortKeyEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#putAll(net.morilib.util.primitive.map.op.ToShortMap)
	 */
	public void putAllElement(ShortMap<V> map) {
		for(PrimitiveEntryK<V> e : map.shortKeyEntrySet()) {
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
			return AbstractShortMap.this.size();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#values()
	 */
	public Collection<V> values() {
		return (values == null) ?
				(values = new VCol(shortKeyEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#isTotal()
	 */
	public boolean isTotal() {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object key) {
		if(key instanceof Short) {
			return containsKeyElement(((Short)key).shortValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public V get(Object key) {
		if(key instanceof Short) {
			return getElement(((Short)key).shortValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public V put(Short key, V value) {
		return putElement(key.shortValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public V remove(Object key) {
		if(key instanceof Short) {
			return removeElement(((Short)key).shortValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Short, ? extends V> m) {
		for(Map.Entry<? extends Short, ? extends V> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public Set<Short> keySet() {
		return shortKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Short, V>> entrySet() {
		final Set<PrimitiveEntryK<V>> e = shortKeyEntrySet();
		
		return new AbstractSet<Entry<Short, V>>() {

			public Iterator<Map.Entry<Short, V>> iterator() {
				final Iterator<PrimitiveEntryK<V>> i = e.iterator();
				
				return new Iterator<Entry<Short, V>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Map.Entry<Short, V> next() {
						final PrimitiveEntryK<V> o = i.next();
						
						return new Map.Entry<Short, V>() {

							public Short getKey() {
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
	 * @see net.morilib.util.primitive.map.po.ShortMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Short.MIN_VALUE || k > Short.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((short)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ShortMap#containsKeyShort(short)
	 */
	public boolean containsKeyElement(short k) {
		for(PrimitiveEntryK<V> e : shortKeyEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ShortMap#get(int)
	 */
	public V get(int k) {
		if(k < Short.MIN_VALUE || k > Short.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((short)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ShortMap#put(int, java.lang.Object)
	 */
	public V put(int k, V v) {
		if(k < Short.MIN_VALUE || k > Short.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((short)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ShortMap#removeElement(short)
	 */
	public V removeElement(short k) {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = shortKeyEntrySet().iterator();
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
	 * @see net.morilib.util.primitive.map.po.ShortMap#remove(int)
	 */
	public V remove(int k) {
		if(k < Short.MIN_VALUE || k > Short.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((short)k);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntryK<V> e : shortKeyEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof ShortMap) {
			ShortMap<V> m = (ShortMap<V>)obj;
			Iterator<PrimitiveEntryK<V>> i;
			
			i = shortKeyEntrySet().iterator();
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
		for(PrimitiveEntryK<V> e : shortKeyEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
