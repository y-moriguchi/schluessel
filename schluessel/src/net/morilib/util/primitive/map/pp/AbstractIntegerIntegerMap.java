/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, intersion 2.0 (the "License");
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
import net.morilib.util.primitive.AbstractIntegerCollection;
import net.morilib.util.primitive.IntegerSet;
import net.morilib.util.primitive.IntegerCollection;
import net.morilib.util.primitive.iterator.IntegerIterator;
import net.morilib.util.primitive.map.op.IntegerValueMap;
import net.morilib.util.primitive.map.po.IntegerMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractIntegerIntegerMap
implements IntegerIntegerMap {
	
	//
	private transient IntegerSet keySet = null;
	private transient IntegerCollection values = null;
	private transient Set<PrimitiveEntryK<Integer>> entrySetK = null;
	private transient Set<PrimitiveEntryV<Integer>>  entrySetV = null;
	private transient Set<Map.Entry<Integer, Integer>> entrySet  = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#clear()
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
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#inRange(int)
	 */
	public boolean containsValueElement(int v) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#get(java.lang.Object)
	 */
	public int f(int k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.pp.IntegerIntegerMap#getElement(int)
	 */
	public Integer getElement(int k) {
		return f(k);
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
			return AbstractIntegerIntegerMap.this.size();
		}

		public boolean containsInt(int o) {
			return containsKeyElement(o);
		}

		public boolean remove(Object o) {
			return AbstractIntegerIntegerMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractIntegerIntegerMap.this.clear();
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
				(keySet = new KSet(primitiveEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#putAll(net.morilib.util.primitive.map.op.ToIntegerMap)
	 */
	public void putAllElement(IntegerIntegerMap map) {
		for(PrimitiveEntry e : map.primitiveEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractIntegerCollection {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private VCol(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}

		public IntegerIterator intIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
			return new IntegerIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public int next() {
					return ei.next().getValue();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractIntegerIntegerMap.this.size();
		}

		public boolean addInt(int v) {
			throw new UnsupportedOperationException();
		}

		public boolean isInfinite() {
			return false;
		}

		public boolean containsInt(int o) {
			return containsValueElement(o);
		}

		public boolean remove(Object o) {
			return AbstractIntegerIntegerMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractIntegerIntegerMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#values()
	 */
	public IntegerCollection values() {
		return (values == null) ?
				(values = new VCol(primitiveEntrySet())) : values;
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
	public Integer get(Object key) {
		if(key instanceof Integer) {
			return getElement(((Integer)key).intValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Integer put(Integer key, int value) {
		return putElement(key.intValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public Integer remove(Object key) {
		if(key instanceof Integer) {
			return removeElement(((Integer)key).intValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Integer, ? extends Integer> m) {
		for(Map.Entry<? extends Integer, ? extends Integer> e : m.entrySet()) {
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
	public Set<Entry<Integer, Integer>> entrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySet == null) {
			entrySet = new AbstractSet<Entry<Integer, Integer>>() {
	
				public Iterator<Map.Entry<Integer, Integer>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<Entry<Integer, Integer>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public Map.Entry<Integer, Integer> next() {
							final PrimitiveEntry o = i.next();
							
							return new Map.Entry<Integer, Integer>() {
	
								public Integer getKey() {
									return o.getKey();
								}
	
								public Integer getValue() {
									return o.getValue();
								}
	
								public Integer setValue(Integer value) {
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
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#get(int)
	 */
	public Integer get(int k) {
		if(k < Integer.MIN_VALUE || k > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((int)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#put(int, java.lang.Object)
	 */
	public Integer put(int k, Integer v) {
		if(k < Integer.MIN_VALUE || k > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((int)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#removeElement(int)
	 */
	public Integer removeElement(int k) {
		Iterator<PrimitiveEntry> i;
		
		i = primitiveEntrySet().iterator();
		while(i.hasNext()) {
			PrimitiveEntry e = i.next();
			
			if(e.getKey() == k) {
				int r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#remove(int)
	 */
	public Integer remove(int k) {
		if(k < Integer.MIN_VALUE || k > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((int)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#intEntrySet()
	 */
	public Set<PrimitiveEntryK<Integer>> intKeyEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetK == null) {
			entrySetK = new AbstractSet<PrimitiveEntryK<Integer>>() {
	
				public Iterator<PrimitiveEntryK<Integer>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryK<Integer>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryK<Integer> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryK<Integer>() {
	
								public int getKey() {
									return o.getKey();
								}
	
								public Integer getValue() {
									return o.getValue();
								}
	
								public Integer setValue(Integer value) {
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
	public Integer putElement(int k, Integer v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#putAllElement(net.morilib.util.primitive.map.po.IntegerMap)
	 */
	public void putAllElement(IntegerMap<Integer> map) {
		Iterator<IntegerMap.PrimitiveEntryK<Integer>> i;
		
		i = map.intKeyEntrySet().iterator();
		while(i.hasNext()) {
			IntegerMap.PrimitiveEntryK<Integer> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.IntegerValueMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((int)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.IntegerValueMap#intValueEntrySet()
	 */
	public Set<PrimitiveEntryV<Integer>> intValueEntrySet() {
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
	
								public int getValue() {
									return o.getValue();
								}
	
								public int setValue(int value) {
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
	 * @see net.morilib.util.primitive.map.op.IntegerValueMap#getElement(java.lang.Object)
	 */
	public int getElement(Object k) {
		if(k instanceof Integer) {
			return f(((Integer)k).intValue());
		}
		throw new IllegalArgumentException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.IntegerValueMap#putElement(java.lang.Object, int)
	 */
	public int putElement(Integer k, int v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.IntegerValueMap#putAllElement(net.morilib.util.primitive.map.op.IntegerValueMap)
	 */
	public void putAllElement(IntegerValueMap<Integer> map) {
		Iterator<IntegerValueMap.PrimitiveEntryV<Integer>> i;
		
		i = map.intValueEntrySet().iterator();
		while(i.hasNext()) {
			IntegerValueMap.PrimitiveEntryV<Integer> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.IntegerValueMap#intValues()
	 */
	public IntegerCollection intValues() {
		return values();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Integer) {
			return containsValueElement(((Integer)value).intValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Integer put(Integer key, Integer value) {
		return putElement(key.intValue(), value.intValue());
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
		if(obj instanceof IntegerIntegerMap) {
			IntegerIntegerMap m = (IntegerIntegerMap)obj;
			Iterator<PrimitiveEntry> i;
			
			i = primitiveEntrySet().iterator();
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntry o = i.next();
				int v = m.getElement(o.getKey());
				int p = o.getValue();
				
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
