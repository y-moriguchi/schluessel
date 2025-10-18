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

import net.morilib.util.primitive.AbstractByteCollection;
import net.morilib.util.primitive.ByteCollection;
import net.morilib.util.primitive.iterator.ByteIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractByteValueMap<K> implements ByteValueMap<K> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient Set<K> keySet = null;
	private transient ByteCollection values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryV<K>> i = byteValueEntrySet().iterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#inDomain(java.lang.Object)
	 */
	public boolean containsKey(Object k) {
		if(k instanceof Byte) {
			containsValueElement(((Byte)k).byteValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#inRange(byte)
	 */
	public boolean containsValueElement(byte v) {
		for(PrimitiveEntryV<K> e : byteValueEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#get(java.lang.Object)
	 */
	public Byte get(Object k) {
		if(k instanceof Byte) {
			for(PrimitiveEntryV<K> e : byteValueEntrySet()) {
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
	 * @see net.morilib.util.primitive.map.op.ToByteMap#getValue(byte)
	 */
	public byte getElement(Object k) {
		if(k instanceof Byte) {
			for(PrimitiveEntryV<K> e : byteValueEntrySet()) {
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
	 * @see net.morilib.util.primitive.map.op.ToByteMap#isEmpty()
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
			return AbstractByteValueMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractByteValueMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractByteValueMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#keySet()
	 */
	public Set<K> keySet() {
		return (keySet == null) ?
				(keySet = new KSet(byteValueEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#put(java.lang.Object, byte)
	 */
	public Byte put(K k, byte v) {
		boolean b = containsKey(k);
		byte r = putElement(k, v);
		
		return b ? r : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#putAll(net.morilib.util.primitive.map.op.ToByteMap)
	 */
	public void putAllElement(ByteValueMap<K> map) {
		for(PrimitiveEntryV<K> e : map.byteValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractByteCollection {
		
		//
		private Set<PrimitiveEntryV<K>> eset;
		
		private VCol(Set<PrimitiveEntryV<K>> eset) {
			this.eset = eset;
		}

		public boolean addByte(byte v) {
			throw new UnsupportedOperationException();
		}

		public ByteIterator byteIterator() {
			final Iterator<PrimitiveEntryV<K>> ei = eset.iterator();
			
			return new ByteIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public byte next() {
					return ei.next().getValue();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractByteValueMap.this.size();
		}

		public boolean isInfinite() {
			return false;
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#values()
	 */
	public ByteCollection byteValues() {
		return (values == null) ?
				(values = new VCol(byteValueEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Byte.MIN_VALUE || v > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((byte)v);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Byte) {
			return containsValueElement(
					((Byte)value).byteValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Byte put(K key, Byte value) {
		if(value instanceof Byte) {
			return putElement(key, ((Byte)value).byteValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends K, ? extends Byte> m) {
		for(PrimitiveEntryV<K> e : byteValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#values()
	 */
	public Collection<Byte> values() {
		final Iterator<PrimitiveEntryV<K>> i = byteValueEntrySet().iterator();
		
		return new AbstractCollection<Byte>() {

			public Iterator<Byte> iterator() {
				return new Iterator<Byte>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Byte next() {
						return i.next().getValue();
					}

					public void remove() {
						i.remove();
					}
					
				};
			}

			public int size() {
				return AbstractByteValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<K, Byte>> entrySet() {
		final Iterator<PrimitiveEntryV<K>> i = byteValueEntrySet().iterator();
		
		return new AbstractSet<Entry<K, Byte>>() {

			public Iterator<Entry<K, Byte>> iterator() {
				return new Iterator<Entry<K, Byte>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Entry<K, Byte> next() {
						final PrimitiveEntryV<K> e = i.next();
						
						return new Map.Entry<K, Byte>() {

							public K getKey() {
								return e.getKey();
							}

							public Byte getValue() {
								return e.getValue();
							}

							public Byte setValue(Byte value) {
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
				return AbstractByteValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#remove(java.lang.Object)
	 */
	public Byte remove(Object k) {
		Iterator<PrimitiveEntryV<K>> i = byteValueEntrySet().iterator();
		
		while(i.hasNext()) {
			PrimitiveEntryV<K> e = i.next();
			Object l = e.getKey();
			
			if((k == null && l == null) ||
					(k != null && k.equals(l))) {
				byte r = e.getValue();
				
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
		
		for(PrimitiveEntryV<K> e : byteValueEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof ByteValueMap) {
			ByteValueMap<K> m = (ByteValueMap<K>)obj;
			Iterator<PrimitiveEntryV<K>> i = byteValueEntrySet().iterator();
			
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
		for(PrimitiveEntryV<K> e : byteValueEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
