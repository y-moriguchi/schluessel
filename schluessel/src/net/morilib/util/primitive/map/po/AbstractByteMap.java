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

import net.morilib.util.primitive.AbstractByteSet;
import net.morilib.util.primitive.ByteSet;
import net.morilib.util.primitive.iterator.ByteIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractByteMap<V> implements ByteMap<V> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient ByteSet keySet = null;
	private transient Collection<V> values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = byteKeyEntrySet().iterator();
		while(i.hasNext()) {
			i.next();
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#inRange(byte)
	 */
	public boolean containsValue(Object v) {
		for(PrimitiveEntryK<V> e : byteKeyEntrySet()) {
			Object p = e.getValue();
			
			if((v == null && p == null) ||
					(v != null && v.equals(p))) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#get(java.lang.Object)
	 */
	public V getElement(byte k) {
		for(PrimitiveEntryK<V> e : byteKeyEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractByteSet {
		
		//
		private Set<PrimitiveEntryK<V>> eset;
		
		private KSet(Set<PrimitiveEntryK<V>> eset) {
			this.eset = eset;
		}
		
		public ByteIterator byteIterator() {
			final Iterator<PrimitiveEntryK<V>> ei = eset.iterator();
			
			return new ByteIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public byte next() {
					return ei.next().getKey();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractByteMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractByteMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractByteMap.this.clear();
		}

		public boolean addByte(byte v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#keySet()
	 */
	public ByteSet byteKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(byteKeyEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#putAll(net.morilib.util.primitive.map.op.ToByteMap)
	 */
	public void putAllElement(ByteMap<V> map) {
		for(PrimitiveEntryK<V> e : map.byteKeyEntrySet()) {
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
			return AbstractByteMap.this.size();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#values()
	 */
	public Collection<V> values() {
		return (values == null) ?
				(values = new VCol(byteKeyEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#isTotal()
	 */
	public boolean isTotal() {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object key) {
		if(key instanceof Byte) {
			return containsKeyElement(((Byte)key).byteValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public V get(Object key) {
		if(key instanceof Byte) {
			return getElement(((Byte)key).byteValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public V put(Byte key, V value) {
		return putElement(key.byteValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public V remove(Object key) {
		if(key instanceof Byte) {
			return removeElement(((Byte)key).byteValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Byte, ? extends V> m) {
		for(Map.Entry<? extends Byte, ? extends V> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public Set<Byte> keySet() {
		return byteKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Byte, V>> entrySet() {
		final Set<PrimitiveEntryK<V>> e = byteKeyEntrySet();
		
		return new AbstractSet<Entry<Byte, V>>() {

			public Iterator<Map.Entry<Byte, V>> iterator() {
				final Iterator<PrimitiveEntryK<V>> i = e.iterator();
				
				return new Iterator<Entry<Byte, V>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Map.Entry<Byte, V> next() {
						final PrimitiveEntryK<V> o = i.next();
						
						return new Map.Entry<Byte, V>() {

							public Byte getKey() {
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
	 * @see net.morilib.util.primitive.map.po.ByteMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Byte.MIN_VALUE || k > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((byte)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ByteMap#containsKeyByte(byte)
	 */
	public boolean containsKeyElement(byte k) {
		for(PrimitiveEntryK<V> e : byteKeyEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ByteMap#get(int)
	 */
	public V get(int k) {
		if(k < Byte.MIN_VALUE || k > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((byte)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ByteMap#put(int, java.lang.Object)
	 */
	public V put(int k, V v) {
		if(k < Byte.MIN_VALUE || k > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((byte)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ByteMap#removeElement(byte)
	 */
	public V removeElement(byte k) {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = byteKeyEntrySet().iterator();
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
	 * @see net.morilib.util.primitive.map.po.ByteMap#remove(int)
	 */
	public V remove(int k) {
		if(k < Byte.MIN_VALUE || k > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((byte)k);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntryK<V> e : byteKeyEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof ByteMap) {
			ByteMap<V> m = (ByteMap<V>)obj;
			Iterator<PrimitiveEntryK<V>> i;
			
			i = byteKeyEntrySet().iterator();
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
		for(PrimitiveEntryK<V> e : byteKeyEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
