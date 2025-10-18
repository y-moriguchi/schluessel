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

import net.morilib.util.primitive.AbstractDoubleCollection;
import net.morilib.util.primitive.DoubleCollection;
import net.morilib.util.primitive.iterator.DoubleIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractDoubleValueMap<K> implements DoubleValueMap<K> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient Set<K> keySet = null;
	private transient DoubleCollection values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryV<K>> i = doubleValueEntrySet().iterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#inDomain(java.lang.Object)
	 */
	public boolean containsKey(Object k) {
		if(k instanceof Double) {
			containsValueElement(((Double)k).doubleValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#inRange(double)
	 */
	public boolean containsValueElement(double v) {
		for(PrimitiveEntryV<K> e : doubleValueEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#get(java.lang.Object)
	 */
	public Double get(Object k) {
		if(k instanceof Double) {
			for(PrimitiveEntryV<K> e : doubleValueEntrySet()) {
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
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#getValue(double)
	 */
	public double getElement(Object k) {
		if(k instanceof Double) {
			for(PrimitiveEntryV<K> e : doubleValueEntrySet()) {
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
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#isEmpty()
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
			return AbstractDoubleValueMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractDoubleValueMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractDoubleValueMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#keySet()
	 */
	public Set<K> keySet() {
		return (keySet == null) ?
				(keySet = new KSet(doubleValueEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#put(java.lang.Object, double)
	 */
	public Double put(K k, double v) {
		boolean b = containsKey(k);
		double r = putElement(k, v);
		
		return b ? r : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#putAll(net.morilib.util.primitive.map.op.ToDoubleMap)
	 */
	public void putAllElement(DoubleValueMap<K> map) {
		for(PrimitiveEntryV<K> e : map.doubleValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractDoubleCollection {
		
		//
		private Set<PrimitiveEntryV<K>> eset;
		
		private VCol(Set<PrimitiveEntryV<K>> eset) {
			this.eset = eset;
		}

		public boolean addDouble(double v) {
			throw new UnsupportedOperationException();
		}

		public DoubleIterator doubleIterator() {
			final Iterator<PrimitiveEntryV<K>> ei = eset.iterator();
			
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
			return AbstractDoubleValueMap.this.size();
		}

		public boolean isInfinite() {
			return false;
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#values()
	 */
	public DoubleCollection doubleValues() {
		return (values == null) ?
				(values = new VCol(doubleValueEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Double.MIN_VALUE || v > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((double)v);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Double) {
			return containsValueElement(
					((Double)value).doubleValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Double put(K key, Double value) {
		if(value instanceof Double) {
			return putElement(key, ((Double)value).doubleValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends K, ? extends Double> m) {
		for(PrimitiveEntryV<K> e : doubleValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#values()
	 */
	public Collection<Double> values() {
		final Iterator<PrimitiveEntryV<K>> i = doubleValueEntrySet().iterator();
		
		return new AbstractCollection<Double>() {

			public Iterator<Double> iterator() {
				return new Iterator<Double>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Double next() {
						return i.next().getValue();
					}

					public void remove() {
						i.remove();
					}
					
				};
			}

			public int size() {
				return AbstractDoubleValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<K, Double>> entrySet() {
		final Iterator<PrimitiveEntryV<K>> i = doubleValueEntrySet().iterator();
		
		return new AbstractSet<Entry<K, Double>>() {

			public Iterator<Entry<K, Double>> iterator() {
				return new Iterator<Entry<K, Double>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Entry<K, Double> next() {
						final PrimitiveEntryV<K> e = i.next();
						
						return new Map.Entry<K, Double>() {

							public K getKey() {
								return e.getKey();
							}

							public Double getValue() {
								return e.getValue();
							}

							public Double setValue(Double value) {
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
				return AbstractDoubleValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#remove(java.lang.Object)
	 */
	public Double remove(Object k) {
		Iterator<PrimitiveEntryV<K>> i = doubleValueEntrySet().iterator();
		
		while(i.hasNext()) {
			PrimitiveEntryV<K> e = i.next();
			Object l = e.getKey();
			
			if((k == null && l == null) ||
					(k != null && k.equals(l))) {
				double r = e.getValue();
				
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
		
		for(PrimitiveEntryV<K> e : doubleValueEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof DoubleValueMap) {
			DoubleValueMap<K> m = (DoubleValueMap<K>)obj;
			Iterator<PrimitiveEntryV<K>> i = doubleValueEntrySet().iterator();
			
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
		for(PrimitiveEntryV<K> e : doubleValueEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
