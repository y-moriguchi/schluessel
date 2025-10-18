/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, longersion 2.0 (the "License");
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

import net.morilib.util.primitive.AbstractDoubleSet;
import net.morilib.util.primitive.AbstractLongCollection;
import net.morilib.util.primitive.DoubleSet;
import net.morilib.util.primitive.LongCollection;
import net.morilib.util.primitive.iterator.DoubleIterator;
import net.morilib.util.primitive.iterator.LongIterator;
import net.morilib.util.primitive.map.op.LongValueMap;
import net.morilib.util.primitive.map.po.DoubleMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractDoubleLongMap
implements DoubleLongMap {
	
	//
	private transient DoubleSet keySet = null;
	private transient LongCollection values = null;
	private transient Set<PrimitiveEntryK<Long>> entrySetK = null;
	private transient Set<PrimitiveEntryV<Double>>  entrySetV = null;
	private transient Set<Map.Entry<Double, Long>> entrySet  = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#clear()
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
	 * @see net.morilib.util.primitive.map.op.ToLongMap#inRange(long)
	 */
	public boolean containsValueElement(long v) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#get(java.lang.Object)
	 */
	public long f(double k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.pp.DoubleLongMap#getElement(double)
	 */
	public Long getElement(double k) {
		return f(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractDoubleSet {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private KSet(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}
		
		public DoubleIterator doubleIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
			return new DoubleIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public double next() {
					return ei.next().getKey();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractDoubleLongMap.this.size();
		}

		public boolean containsDouble(double o) {
			return containsKeyElement(o);
		}

		public boolean remove(Object o) {
			return AbstractDoubleLongMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractDoubleLongMap.this.clear();
		}

		public boolean addDouble(double v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#keySet()
	 */
	public DoubleSet doubleKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(primitiveEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#putAll(net.morilib.util.primitive.map.op.ToLongMap)
	 */
	public void putAllElement(DoubleLongMap map) {
		for(PrimitiveEntry e : map.primitiveEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractLongCollection {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private VCol(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}

		public LongIterator longIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
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
			return AbstractDoubleLongMap.this.size();
		}

		public boolean addLong(long v) {
			throw new UnsupportedOperationException();
		}

		public boolean isInfinite() {
			return false;
		}

		public boolean containsLong(long o) {
			return containsValueElement(o);
		}

		public boolean remove(Object o) {
			return AbstractDoubleLongMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractDoubleLongMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToLongMap#values()
	 */
	public LongCollection values() {
		return (values == null) ?
				(values = new VCol(primitiveEntrySet())) : values;
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
			return containsKeyElement(((Double)key).doubleValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public Long get(Object key) {
		if(key instanceof Long) {
			return getElement(((Double)key).doubleValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Long put(Double key, long value) {
		return putElement(key.doubleValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public Long remove(Object key) {
		if(key instanceof Long) {
			return removeElement(((Double)key).doubleValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Double, ? extends Long> m) {
		for(Map.Entry<? extends Double, ? extends Long> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public DoubleSet keySet() {
		return doubleKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Double, Long>> entrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySet == null) {
			entrySet = new AbstractSet<Entry<Double, Long>>() {
	
				public Iterator<Map.Entry<Double, Long>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<Entry<Double, Long>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public Map.Entry<Double, Long> next() {
							final PrimitiveEntry o = i.next();
							
							return new Map.Entry<Double, Long>() {
	
								public Double getKey() {
									return o.getKey();
								}
	
								public Long getValue() {
									return o.getValue();
								}
	
								public Long setValue(Long value) {
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
	 * @see net.morilib.util.primitive.map.po.LongMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Double.MIN_VALUE || k > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((double)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#containsKeyLong(long)
	 */
	public boolean containsKeyElement(double k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#get(int)
	 */
	public Long get(int k) {
		if(k < Long.MIN_VALUE || k > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((long)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#put(int, java.lang.Object)
	 */
	public Long put(int k, Long v) {
		if(k < Long.MIN_VALUE || k > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((double)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#removeElement(long)
	 */
	public Long removeElement(double k) {
		Iterator<PrimitiveEntry> i;
		
		i = primitiveEntrySet().iterator();
		while(i.hasNext()) {
			PrimitiveEntry e = i.next();
			
			if(e.getKey() == k) {
				long r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#remove(int)
	 */
	public Long remove(int k) {
		if(k < Long.MIN_VALUE || k > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((double)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#doubleEntrySet()
	 */
	public Set<PrimitiveEntryK<Long>> doubleKeyEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetK == null) {
			entrySetK = new AbstractSet<PrimitiveEntryK<Long>>() {
	
				public Iterator<PrimitiveEntryK<Long>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryK<Long>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryK<Long> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryK<Long>() {
	
								public double getKey() {
									return o.getKey();
								}
	
								public Long getValue() {
									return o.getValue();
								}
	
								public Long setValue(Long value) {
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
	 * @see net.morilib.util.primitive.map.po.DoubleMap#putElement(double, java.lang.Object)
	 */
	public Long putElement(double k, Long v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#putAllElement(net.morilib.util.primitive.map.po.DoubleMap)
	 */
	public void putAllElement(DoubleMap<Long> map) {
		Iterator<DoubleMap.PrimitiveEntryK<Long>> i;
		
		i = map.doubleKeyEntrySet().iterator();
		while(i.hasNext()) {
			DoubleMap.PrimitiveEntryK<Long> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.LongValueMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Double.MIN_VALUE || v > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((long)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.LongValueMap#longValueEntrySet()
	 */
	public Set<PrimitiveEntryV<Double>> longValueEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetV == null) {
			entrySetV = new AbstractSet<PrimitiveEntryV<Double>>() {
	
				public Iterator<PrimitiveEntryV<Double>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryV<Double>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryV<Double> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryV<Double>() {
	
								public Double getKey() {
									return o.getKey();
								}
	
								public long getValue() {
									return o.getValue();
								}
	
								public long setValue(long value) {
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
	 * @see net.morilib.util.primitive.map.op.LongValueMap#getElement(java.lang.Object)
	 */
	public long getElement(Object k) {
		if(k instanceof Double) {
			return f(((Double)k).doubleValue());
		}
		throw new IllegalArgumentException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.LongValueMap#putElement(java.lang.Object, long)
	 */
	public long putElement(Double k, long v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.LongValueMap#putAllElement(net.morilib.util.primitive.map.op.LongValueMap)
	 */
	public void putAllElement(LongValueMap<Double> map) {
		Iterator<LongValueMap.PrimitiveEntryV<Double>> i;
		
		i = map.longValueEntrySet().iterator();
		while(i.hasNext()) {
			LongValueMap.PrimitiveEntryV<Double> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.LongValueMap#longValues()
	 */
	public LongCollection longValues() {
		return values();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Long) {
			return containsValueElement(((Long)value).longValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Long put(Double key, Long value) {
		return putElement(key.doubleValue(), value.longValue());
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
		if(obj instanceof DoubleLongMap) {
			DoubleLongMap m = (DoubleLongMap)obj;
			Iterator<PrimitiveEntry> i;
			
			i = primitiveEntrySet().iterator();
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntry o = i.next();
				long v = m.getElement(o.getKey());
				long p = o.getValue();
				
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
