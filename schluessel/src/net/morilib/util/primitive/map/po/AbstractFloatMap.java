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

import net.morilib.util.primitive.AbstractFloatSet;
import net.morilib.util.primitive.FloatSet;
import net.morilib.util.primitive.iterator.FloatIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractFloatMap<V> implements FloatMap<V> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient FloatSet keySet = null;
	private transient Collection<V> values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = floatKeyEntrySet().iterator();
		while(i.hasNext()) {
			i.next();
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#inRange(float)
	 */
	public boolean containsValue(Object v) {
		for(PrimitiveEntryK<V> e : floatKeyEntrySet()) {
			Object p = e.getValue();
			
			if((v == null && p == null) ||
					(v != null && v.equals(p))) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#get(java.lang.Object)
	 */
	public V getElement(float k) {
		for(PrimitiveEntryK<V> e : floatKeyEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractFloatSet {
		
		//
		private Set<PrimitiveEntryK<V>> eset;
		
		private KSet(Set<PrimitiveEntryK<V>> eset) {
			this.eset = eset;
		}
		
		public FloatIterator floatIterator() {
			final Iterator<PrimitiveEntryK<V>> ei = eset.iterator();
			
			return new FloatIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public float next() {
					return ei.next().getKey();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractFloatMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractFloatMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractFloatMap.this.clear();
		}

		public boolean addFloat(float v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#keySet()
	 */
	public FloatSet floatKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(floatKeyEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#putAll(net.morilib.util.primitive.map.op.ToFloatMap)
	 */
	public void putAllElement(FloatMap<V> map) {
		for(PrimitiveEntryK<V> e : map.floatKeyEntrySet()) {
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
			return AbstractFloatMap.this.size();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#values()
	 */
	public Collection<V> values() {
		return (values == null) ?
				(values = new VCol(floatKeyEntrySet())) : values;
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
			return containsKeyElement(((Float)key).floatValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public V get(Object key) {
		if(key instanceof Float) {
			return getElement(((Float)key).floatValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public V put(Float key, V value) {
		return putElement(key.floatValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public V remove(Object key) {
		if(key instanceof Float) {
			return removeElement(((Float)key).floatValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Float, ? extends V> m) {
		for(Map.Entry<? extends Float, ? extends V> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public Set<Float> keySet() {
		return floatKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Float, V>> entrySet() {
		final Set<PrimitiveEntryK<V>> e = floatKeyEntrySet();
		
		return new AbstractSet<Entry<Float, V>>() {

			public Iterator<Map.Entry<Float, V>> iterator() {
				final Iterator<PrimitiveEntryK<V>> i = e.iterator();
				
				return new Iterator<Entry<Float, V>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Map.Entry<Float, V> next() {
						final PrimitiveEntryK<V> o = i.next();
						
						return new Map.Entry<Float, V>() {

							public Float getKey() {
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
	 * @see net.morilib.util.primitive.map.po.FloatMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Float.MIN_VALUE || k > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((float)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.FloatMap#containsKeyFloat(float)
	 */
	public boolean containsKeyElement(float k) {
		for(PrimitiveEntryK<V> e : floatKeyEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.FloatMap#get(int)
	 */
	public V get(int k) {
		if(k < Float.MIN_VALUE || k > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((float)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.FloatMap#put(int, java.lang.Object)
	 */
	public V put(int k, V v) {
		if(k < Float.MIN_VALUE || k > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((float)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.FloatMap#removeElement(float)
	 */
	public V removeElement(float k) {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = floatKeyEntrySet().iterator();
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
	 * @see net.morilib.util.primitive.map.po.FloatMap#remove(int)
	 */
	public V remove(int k) {
		if(k < Float.MIN_VALUE || k > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((float)k);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntryK<V> e : floatKeyEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof FloatMap) {
			FloatMap<V> m = (FloatMap<V>)obj;
			Iterator<PrimitiveEntryK<V>> i;
			
			i = floatKeyEntrySet().iterator();
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
		for(PrimitiveEntryK<V> e : floatKeyEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
