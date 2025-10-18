/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, doubleersion 2.0 (the "License");
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
import net.morilib.util.primitive.AbstractDoubleCollection;
import net.morilib.util.primitive.IntegerSet;
import net.morilib.util.primitive.DoubleCollection;
import net.morilib.util.primitive.iterator.IntegerIterator;
import net.morilib.util.primitive.iterator.DoubleIterator;
import net.morilib.util.primitive.map.op.DoubleValueMap;
import net.morilib.util.primitive.map.po.IntegerMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractIntegerDoubleMap
implements IntegerDoubleMap {
	
	//
	private transient IntegerSet keySet = null;
	private transient DoubleCollection values = null;
	private transient Set<PrimitiveEntryK<Double>> entrySetK = null;
	private transient Set<PrimitiveEntryV<Integer>>  entrySetV = null;
	private transient Set<Map.Entry<Integer, Double>> entrySet  = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#clear()
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
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#inRange(double)
	 */
	public boolean containsValueElement(double v) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#get(java.lang.Object)
	 */
	public double f(int k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.pp.IntegerDoubleMap#getElement(int)
	 */
	public Double getElement(int k) {
		return f(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#isEmpty()
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
			return AbstractIntegerDoubleMap.this.size();
		}

		public boolean containsInt(int o) {
			return containsKeyElement(o);
		}

		public boolean remove(Object o) {
			return AbstractIntegerDoubleMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractIntegerDoubleMap.this.clear();
		}

		public boolean addInt(int v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#keySet()
	 */
	public IntegerSet intKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(primitiveEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#putAll(net.morilib.util.primitive.map.op.ToDoubleMap)
	 */
	public void putAllElement(IntegerDoubleMap map) {
		for(PrimitiveEntry e : map.primitiveEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractDoubleCollection {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private VCol(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}

		public DoubleIterator doubleIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
			return new DoubleIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public double next() {
					return ei.next().getValue();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractIntegerDoubleMap.this.size();
		}

		public boolean addDouble(double v) {
			throw new UnsupportedOperationException();
		}

		public boolean isInfinite() {
			return false;
		}

		public boolean containsDouble(double o) {
			return containsValueElement(o);
		}

		public boolean remove(Object o) {
			return AbstractIntegerDoubleMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractIntegerDoubleMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#values()
	 */
	public DoubleCollection values() {
		return (values == null) ?
				(values = new VCol(primitiveEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#isTotal()
	 */
	public boolean isTotal() {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object key) {
		if(key instanceof Double) {
			return containsKeyElement(((Integer)key).intValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public Double get(Object key) {
		if(key instanceof Double) {
			return getElement(((Integer)key).intValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Double put(Integer key, double value) {
		return putElement(key.intValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public Double remove(Object key) {
		if(key instanceof Double) {
			return removeElement(((Integer)key).intValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Integer, ? extends Double> m) {
		for(Map.Entry<? extends Integer, ? extends Double> e : m.entrySet()) {
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
	public Set<Entry<Integer, Double>> entrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySet == null) {
			entrySet = new AbstractSet<Entry<Integer, Double>>() {
	
				public Iterator<Map.Entry<Integer, Double>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<Entry<Integer, Double>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public Map.Entry<Integer, Double> next() {
							final PrimitiveEntry o = i.next();
							
							return new Map.Entry<Integer, Double>() {
	
								public Integer getKey() {
									return o.getKey();
								}
	
								public Double getValue() {
									return o.getValue();
								}
	
								public Double setValue(Double value) {
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
	 * @see net.morilib.util.primitive.map.po.DoubleMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Integer.MIN_VALUE || k > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((int)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#containsKeyDouble(double)
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
	 * @see net.morilib.util.primitive.map.po.DoubleMap#get(int)
	 */
	public Double get(int k) {
		if(k < Double.MIN_VALUE || k > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((double)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#put(int, java.lang.Object)
	 */
	public Double put(int k, Double v) {
		if(k < Double.MIN_VALUE || k > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((int)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#removeElement(double)
	 */
	public Double removeElement(int k) {
		Iterator<PrimitiveEntry> i;
		
		i = primitiveEntrySet().iterator();
		while(i.hasNext()) {
			PrimitiveEntry e = i.next();
			
			if(e.getKey() == k) {
				double r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#remove(int)
	 */
	public Double remove(int k) {
		if(k < Double.MIN_VALUE || k > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((int)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#intEntrySet()
	 */
	public Set<PrimitiveEntryK<Double>> intKeyEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetK == null) {
			entrySetK = new AbstractSet<PrimitiveEntryK<Double>>() {
	
				public Iterator<PrimitiveEntryK<Double>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryK<Double>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryK<Double> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryK<Double>() {
	
								public int getKey() {
									return o.getKey();
								}
	
								public Double getValue() {
									return o.getValue();
								}
	
								public Double setValue(Double value) {
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
	public Double putElement(int k, Double v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.IntegerMap#putAllElement(net.morilib.util.primitive.map.po.IntegerMap)
	 */
	public void putAllElement(IntegerMap<Double> map) {
		Iterator<IntegerMap.PrimitiveEntryK<Double>> i;
		
		i = map.intKeyEntrySet().iterator();
		while(i.hasNext()) {
			IntegerMap.PrimitiveEntryK<Double> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.DoubleValueMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((double)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.DoubleValueMap#doubleValueEntrySet()
	 */
	public Set<PrimitiveEntryV<Integer>> doubleValueEntrySet() {
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
	
								public double getValue() {
									return o.getValue();
								}
	
								public double setValue(double value) {
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
	 * @see net.morilib.util.primitive.map.op.DoubleValueMap#getElement(java.lang.Object)
	 */
	public double getElement(Object k) {
		if(k instanceof Integer) {
			return f(((Integer)k).intValue());
		}
		throw new IllegalArgumentException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.DoubleValueMap#putElement(java.lang.Object, double)
	 */
	public double putElement(Integer k, double v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.DoubleValueMap#putAllElement(net.morilib.util.primitive.map.op.DoubleValueMap)
	 */
	public void putAllElement(DoubleValueMap<Integer> map) {
		Iterator<DoubleValueMap.PrimitiveEntryV<Integer>> i;
		
		i = map.doubleValueEntrySet().iterator();
		while(i.hasNext()) {
			DoubleValueMap.PrimitiveEntryV<Integer> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.DoubleValueMap#doubleValues()
	 */
	public DoubleCollection doubleValues() {
		return values();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Double) {
			return containsValueElement(((Double)value).doubleValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Double put(Integer key, Double value) {
		return putElement(key.intValue(), value.doubleValue());
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
		if(obj instanceof IntegerDoubleMap) {
			IntegerDoubleMap m = (IntegerDoubleMap)obj;
			Iterator<PrimitiveEntry> i;
			
			i = primitiveEntrySet().iterator();
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntry o = i.next();
				double v = m.getElement(o.getKey());
				double p = o.getValue();
				
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
