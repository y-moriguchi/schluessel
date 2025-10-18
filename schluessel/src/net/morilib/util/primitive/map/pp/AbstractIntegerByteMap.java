/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, byteersion 2.0 (the "License");
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
package net.morilib.util.primitive.map.pp;

import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import net.morilib.util.primitive.AbstractIntegerSet;
import net.morilib.util.primitive.AbstractByteCollection;
import net.morilib.util.primitive.IntegerSet;
import net.morilib.util.primitive.ByteCollection;
import net.morilib.util.primitive.iterator.IntegerIterator;
import net.morilib.util.primitive.iterator.ByteIterator;
import net.morilib.util.primitive.map.op.ByteValueMap;
import net.morilib.util.primitive.map.po.IntegerMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractIntegerByteMap
implements IntegerByteMap {
	
	//
	private transient IntegerSet keySet = null;
	private transient ByteCollection values = null;
	private transient Set<PrimitiveEntryK<Byte>> entrySetK = null;
	private transient Set<PrimitiveEntryV<Integer>>  entrySetV = null;
	private transient Set<Map.Entry<Integer, Byte>> entrySet  = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntry> i;
		
		i = primitiveEntrySet().iterator();
		while(i.hasNext()) {
			i.next();
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#inRange(byte)
	 */
	public boolean containsValueElement(byte v) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#get(java.lang.Object)
	 */
	public byte f(int k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.pp.IntegerByteMap#getElement(int)
	 */
	public Byte getElement(int k) {
		return f(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractIntegerSet {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private KSet(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}
		
		public IntegerIterator intIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
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
			return AbstractIntegerByteMap.this.size();
		}

		public boolean containsInt(int o) {
			return containsKeyElement(o);
		}

		public boolean remove(Object o) {
			return AbstractIntegerByteMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractIntegerByteMap.this.clear();
		}

		public boolean addInt(int v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#keySet()
	 */
	public IntegerSet intKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(primitiveEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#putAll(net.morilib.util.primitive.map.op.ToByteMap)
	 */
	public void putAllElement(IntegerByteMap map) {
		for(PrimitiveEntry e : map.primitiveEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractByteCollection {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private VCol(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}

		public ByteIterator byteIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
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
			return AbstractIntegerByteMap.this.size();
		}

		public boolean addByte(byte v) {
			throw new UnsupportedOperationException();
		}

		public boolean isInfinite() {
			return false;
		}

		public boolean containsByte(byte o) {
			return containsValueElement(o);
		}

		public boolean remove(Object o) {
			return AbstractIntegerByteMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractIntegerByteMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToByteMap#values()
	 */
	public ByteCollection values() {
		return (values == null) ?
				(values = new VCol(primitiveEntrySet())) : values;
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
			return containsKeyElement(((Integer)key).intValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public Byte get(Object key) {
		if(key instanceof Byte) {
			return getElement(((Integer)key).intValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Byte put(Integer key, byte value) {
		return putElement(key.intValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public Byte remove(Object key) {
		if(key instanceof Byte) {
			return removeElement(((Integer)key).intValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Integer, ? extends Byte> m) {
		for(Map.Entry<? extends Integer, ? extends Byte> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public IntegerSet keySet() {
		return intKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Integer, Byte>> entrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySet == null) {
			entrySet = new AbstractSet<Entry<Integer, Byte>>() {
	
				public Iterator<Map.Entry<Integer, Byte>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<Entry<Integer, Byte>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public Map.Entry<Integer, Byte> next() {
							final PrimitiveEntry o = i.next();
							
							return new Map.Entry<Integer, Byte>() {
	
								public Integer getKey() {
									return o.getKey();
								}
	
								public Byte getValue() {
									return o.getValue();
								}
	
								public Byte setValue(Byte value) {
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
		return entrySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ByteMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Integer.MIN_VALUE || k > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((int)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ByteMap#containsKeyByte(byte)
	 */
	public boolean containsKeyElement(int k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ByteMap#get(int)
	 */
	public Byte get(int k) {
		if(k < Byte.MIN_VALUE || k > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((byte)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ByteMap#put(int, java.lang.Object)
	 */
	public Byte put(int k, Byte v) {
		if(k < Byte.MIN_VALUE || k > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((int)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ByteMap#removeElement(byte)
	 */
	public Byte removeElement(int k) {
		Iterator<PrimitiveEntry> i;
		
		i = primitiveEntrySet().iterator();
		while(i.hasNext()) {
			PrimitiveEntry e = i.next();
			
			if(e.getKey() == k) {
				byte r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ByteMap#remove(int)
	 */
	public Byte remove(int k) {
		if(k < Byte.MIN_VALUE || k > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((int)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#intEntrySet()
	 */
	public Set<PrimitiveEntryK<Byte>> intKeyEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetK == null) {
			entrySetK = new AbstractSet<PrimitiveEntryK<Byte>>() {
	
				public Iterator<PrimitiveEntryK<Byte>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryK<Byte>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryK<Byte> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryK<Byte>() {
	
								public int getKey() {
									return o.getKey();
								}
	
								public Byte getValue() {
									return o.getValue();
								}
	
								public Byte setValue(Byte value) {
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
		return entrySetK;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#putElement(int, java.lang.Object)
	 */
	public Byte putElement(int k, Byte v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#putAllElement(net.morilib.util.primitive.map.po.IntegerMap)
	 */
	public void putAllElement(IntegerMap<Byte> map) {
		Iterator<IntegerMap.PrimitiveEntryK<Byte>> i;
		
		i = map.intKeyEntrySet().iterator();
		while(i.hasNext()) {
			IntegerMap.PrimitiveEntryK<Byte> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ByteValueMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((byte)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ByteValueMap#byteValueEntrySet()
	 */
	public Set<PrimitiveEntryV<Integer>> byteValueEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetV == null) {
			entrySetV = new AbstractSet<PrimitiveEntryV<Integer>>() {
	
				public Iterator<PrimitiveEntryV<Integer>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryV<Integer>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryV<Integer> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryV<Integer>() {
	
								public Integer getKey() {
									return o.getKey();
								}
	
								public byte getValue() {
									return o.getValue();
								}
	
								public byte setValue(byte value) {
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
		return entrySetV;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ByteValueMap#getElement(java.lang.Object)
	 */
	public byte getElement(Object k) {
		if(k instanceof Integer) {
			return f(((Integer)k).intValue());
		}
		throw new IllegalArgumentException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ByteValueMap#putElement(java.lang.Object, byte)
	 */
	public byte putElement(Integer k, byte v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ByteValueMap#putAllElement(net.morilib.util.primitive.map.op.ByteValueMap)
	 */
	public void putAllElement(ByteValueMap<Integer> map) {
		Iterator<ByteValueMap.PrimitiveEntryV<Integer>> i;
		
		i = map.byteValueEntrySet().iterator();
		while(i.hasNext()) {
			ByteValueMap.PrimitiveEntryV<Integer> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ByteValueMap#byteValues()
	 */
	public ByteCollection byteValues() {
		return values();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Byte) {
			return containsValueElement(((Byte)value).byteValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Byte put(Integer key, Byte value) {
		return putElement(key.intValue(), value.byteValue());
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntry e : primitiveEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof IntegerByteMap) {
			IntegerByteMap m = (IntegerByteMap)obj;
			Iterator<PrimitiveEntry> i;
			
			i = primitiveEntrySet().iterator();
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntry o = i.next();
				byte v = m.getElement(o.getKey());
				byte p = o.getValue();
				
				if(!m.containsKeyElement(o.getKey()) || v != p) {
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
		for(PrimitiveEntry e : primitiveEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
