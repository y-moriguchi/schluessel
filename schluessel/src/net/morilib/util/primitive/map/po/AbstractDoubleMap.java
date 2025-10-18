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

import net.morilib.util.primitive.AbstractDoubleSet;
import net.morilib.util.primitive.DoubleSet;
import net.morilib.util.primitive.iterator.DoubleIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractDoubleMap<V> implements DoubleMap<V> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient DoubleSet keySet = null;
	private transient Collection<V> values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = doubleKeyEntrySet().iterator();
		while(i.hasNext()) {
			i.next();
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#inRange(double)
	 */
	public boolean containsValue(Object v) {
		for(PrimitiveEntryK<V> e : doubleKeyEntrySet()) {
			Object p = e.getValue();
			
			if((v == null && p == null) ||
					(v != null && v.equals(p))) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#get(java.lang.Object)
	 */
	public V getElement(double k) {
		for(PrimitiveEntryK<V> e : doubleKeyEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractDoubleSet {
		
		//
		private Set<PrimitiveEntryK<V>> eset;
		
		private KSet(Set<PrimitiveEntryK<V>> eset) {
			this.eset = eset;
		}
		
		public DoubleIterator doubleIterator() {
			final Iterator<PrimitiveEntryK<V>> ei = eset.iterator();
			
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
			return AbstractDoubleMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractDoubleMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractDoubleMap.this.clear();
		}

		public boolean addDouble(double v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#keySet()
	 */
	public DoubleSet doubleKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(doubleKeyEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#putAll(net.morilib.util.primitive.map.op.ToDoubleMap)
	 */
	public void putAllElement(DoubleMap<V> map) {
		for(PrimitiveEntryK<V> e : map.doubleKeyEntrySet()) {
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
			return AbstractDoubleMap.this.size();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#values()
	 */
	public Collection<V> values() {
		return (values == null) ?
				(values = new VCol(doubleKeyEntrySet())) : values;
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
			return containsKeyElement(((Double)key).doubleValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public V get(Object key) {
		if(key instanceof Double) {
			return getElement(((Double)key).doubleValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public V put(Double key, V value) {
		return putElement(key.doubleValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public V remove(Object key) {
		if(key instanceof Double) {
			return removeElement(((Double)key).doubleValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Double, ? extends V> m) {
		for(Map.Entry<? extends Double, ? extends V> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public Set<Double> keySet() {
		return doubleKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Double, V>> entrySet() {
		final Set<PrimitiveEntryK<V>> e = doubleKeyEntrySet();
		
		return new AbstractSet<Entry<Double, V>>() {

			public Iterator<Map.Entry<Double, V>> iterator() {
				final Iterator<PrimitiveEntryK<V>> i = e.iterator();
				
				return new Iterator<Entry<Double, V>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Map.Entry<Double, V> next() {
						final PrimitiveEntryK<V> o = i.next();
						
						return new Map.Entry<Double, V>() {

							public Double getKey() {
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
	 * @see net.morilib.util.primitive.map.po.DoubleMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Double.MIN_VALUE || k > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((double)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#containsKeyDouble(double)
	 */
	public boolean containsKeyElement(double k) {
		for(PrimitiveEntryK<V> e : doubleKeyEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#get(int)
	 */
	public V get(int k) {
		if(k < Double.MIN_VALUE || k > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((double)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#put(int, java.lang.Object)
	 */
	public V put(int k, V v) {
		if(k < Double.MIN_VALUE || k > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((double)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#removeElement(double)
	 */
	public V removeElement(double k) {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = doubleKeyEntrySet().iterator();
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
	 * @see net.morilib.util.primitive.map.po.DoubleMap#remove(int)
	 */
	public V remove(int k) {
		if(k < Double.MIN_VALUE || k > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((double)k);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntryK<V> e : doubleKeyEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof DoubleMap) {
			DoubleMap<V> m = (DoubleMap<V>)obj;
			Iterator<PrimitiveEntryK<V>> i;
			
			i = doubleKeyEntrySet().iterator();
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
		for(PrimitiveEntryK<V> e : doubleKeyEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
