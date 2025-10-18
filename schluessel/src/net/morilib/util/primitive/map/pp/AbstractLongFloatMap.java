/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, floatersion 2.0 (the "License");
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

import net.morilib.util.primitive.AbstractLongSet;
import net.morilib.util.primitive.AbstractFloatCollection;
import net.morilib.util.primitive.LongSet;
import net.morilib.util.primitive.FloatCollection;
import net.morilib.util.primitive.iterator.LongIterator;
import net.morilib.util.primitive.iterator.FloatIterator;
import net.morilib.util.primitive.map.op.FloatValueMap;
import net.morilib.util.primitive.map.po.LongMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractLongFloatMap
implements LongFloatMap {
	
	//
	private transient LongSet keySet = null;
	private transient FloatCollection values = null;
	private transient Set<PrimitiveEntryK<Float>> entrySetK = null;
	private transient Set<PrimitiveEntryV<Long>>  entrySetV = null;
	private transient Set<Map.Entry<Long, Float>> entrySet  = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#clear()
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
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#inRange(float)
	 */
	public boolean containsValueElement(float v) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#get(java.lang.Object)
	 */
	public float f(long k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.pp.LongFloatMap#getElement(long)
	 */
	public Float getElement(long k) {
		return f(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractLongSet {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private KSet(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}
		
		public LongIterator longIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
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
			return AbstractLongFloatMap.this.size();
		}

		public boolean containsLong(long o) {
			return containsKeyElement(o);
		}

		public boolean remove(Object o) {
			return AbstractLongFloatMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractLongFloatMap.this.clear();
		}

		public boolean addLong(long v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#keySet()
	 */
	public LongSet longKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(primitiveEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#putAll(net.morilib.util.primitive.map.op.ToFloatMap)
	 */
	public void putAllElement(LongFloatMap map) {
		for(PrimitiveEntry e : map.primitiveEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractFloatCollection {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private VCol(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}

		public FloatIterator floatIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
			return new FloatIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public float next() {
					return ei.next().getValue();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractLongFloatMap.this.size();
		}

		public boolean addFloat(float v) {
			throw new UnsupportedOperationException();
		}

		public boolean isInfinite() {
			return false;
		}

		public boolean containsFloat(float o) {
			return containsValueElement(o);
		}

		public boolean remove(Object o) {
			return AbstractLongFloatMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractLongFloatMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#values()
	 */
	public FloatCollection values() {
		return (values == null) ?
				(values = new VCol(primitiveEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#isTotal()
	 */
	public boolean isTotal() {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object key) {
		if(key instanceof Float) {
			return containsKeyElement(((Long)key).longValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public Float get(Object key) {
		if(key instanceof Float) {
			return getElement(((Long)key).longValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Float put(Long key, float value) {
		return putElement(key.longValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public Float remove(Object key) {
		if(key instanceof Float) {
			return removeElement(((Long)key).longValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Long, ? extends Float> m) {
		for(Map.Entry<? extends Long, ? extends Float> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public LongSet keySet() {
		return longKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Long, Float>> entrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySet == null) {
			entrySet = new AbstractSet<Entry<Long, Float>>() {
	
				public Iterator<Map.Entry<Long, Float>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<Entry<Long, Float>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public Map.Entry<Long, Float> next() {
							final PrimitiveEntry o = i.next();
							
							return new Map.Entry<Long, Float>() {
	
								public Long getKey() {
									return o.getKey();
								}
	
								public Float getValue() {
									return o.getValue();
								}
	
								public Float setValue(Float value) {
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
	 * @see net.morilib.util.primitive.map.po.FloatMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Long.MIN_VALUE || k > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((long)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.FloatMap#containsKeyFloat(float)
	 */
	public boolean containsKeyElement(long k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.FloatMap#get(int)
	 */
	public Float get(int k) {
		if(k < Float.MIN_VALUE || k > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((float)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.FloatMap#put(int, java.lang.Object)
	 */
	public Float put(int k, Float v) {
		if(k < Float.MIN_VALUE || k > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((long)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.FloatMap#removeElement(float)
	 */
	public Float removeElement(long k) {
		Iterator<PrimitiveEntry> i;
		
		i = primitiveEntrySet().iterator();
		while(i.hasNext()) {
			PrimitiveEntry e = i.next();
			
			if(e.getKey() == k) {
				float r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.FloatMap#remove(int)
	 */
	public Float remove(int k) {
		if(k < Float.MIN_VALUE || k > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((long)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#longEntrySet()
	 */
	public Set<PrimitiveEntryK<Float>> longKeyEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetK == null) {
			entrySetK = new AbstractSet<PrimitiveEntryK<Float>>() {
	
				public Iterator<PrimitiveEntryK<Float>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryK<Float>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryK<Float> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryK<Float>() {
	
								public long getKey() {
									return o.getKey();
								}
	
								public Float getValue() {
									return o.getValue();
								}
	
								public Float setValue(Float value) {
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
	 * @see net.morilib.util.primitive.map.po.LongMap#putElement(long, java.lang.Object)
	 */
	public Float putElement(long k, Float v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.LongMap#putAllElement(net.morilib.util.primitive.map.po.LongMap)
	 */
	public void putAllElement(LongMap<Float> map) {
		Iterator<LongMap.PrimitiveEntryK<Float>> i;
		
		i = map.longKeyEntrySet().iterator();
		while(i.hasNext()) {
			LongMap.PrimitiveEntryK<Float> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.FloatValueMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Long.MIN_VALUE || v > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((float)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.FloatValueMap#floatValueEntrySet()
	 */
	public Set<PrimitiveEntryV<Long>> floatValueEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetV == null) {
			entrySetV = new AbstractSet<PrimitiveEntryV<Long>>() {
	
				public Iterator<PrimitiveEntryV<Long>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryV<Long>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryV<Long> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryV<Long>() {
	
								public Long getKey() {
									return o.getKey();
								}
	
								public float getValue() {
									return o.getValue();
								}
	
								public float setValue(float value) {
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
	 * @see net.morilib.util.primitive.map.op.FloatValueMap#getElement(java.lang.Object)
	 */
	public float getElement(Object k) {
		if(k instanceof Long) {
			return f(((Long)k).longValue());
		}
		throw new IllegalArgumentException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.FloatValueMap#putElement(java.lang.Object, float)
	 */
	public float putElement(Long k, float v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.FloatValueMap#putAllElement(net.morilib.util.primitive.map.op.FloatValueMap)
	 */
	public void putAllElement(FloatValueMap<Long> map) {
		Iterator<FloatValueMap.PrimitiveEntryV<Long>> i;
		
		i = map.floatValueEntrySet().iterator();
		while(i.hasNext()) {
			FloatValueMap.PrimitiveEntryV<Long> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.FloatValueMap#floatValues()
	 */
	public FloatCollection floatValues() {
		return values();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Float) {
			return containsValueElement(((Float)value).floatValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Float put(Long key, Float value) {
		return putElement(key.longValue(), value.floatValue());
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
		if(obj instanceof LongFloatMap) {
			LongFloatMap m = (LongFloatMap)obj;
			Iterator<PrimitiveEntry> i;
			
			i = primitiveEntrySet().iterator();
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntry o = i.next();
				float v = m.getElement(o.getKey());
				float p = o.getValue();
				
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
