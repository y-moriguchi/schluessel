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

import net.morilib.util.primitive.AbstractFloatCollection;
import net.morilib.util.primitive.FloatCollection;
import net.morilib.util.primitive.iterator.FloatIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractFloatValueMap<K> implements FloatValueMap<K> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient Set<K> keySet = null;
	private transient FloatCollection values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryV<K>> i = floatValueEntrySet().iterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#inDomain(java.lang.Object)
	 */
	public boolean containsKey(Object k) {
		if(k instanceof Float) {
			containsValueElement(((Float)k).floatValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#inRange(float)
	 */
	public boolean containsValueElement(float v) {
		for(PrimitiveEntryV<K> e : floatValueEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#get(java.lang.Object)
	 */
	public Float get(Object k) {
		if(k instanceof Float) {
			for(PrimitiveEntryV<K> e : floatValueEntrySet()) {
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
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#getValue(float)
	 */
	public float getElement(Object k) {
		if(k instanceof Float) {
			for(PrimitiveEntryV<K> e : floatValueEntrySet()) {
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
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#isEmpty()
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
			return AbstractFloatValueMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractFloatValueMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractFloatValueMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#keySet()
	 */
	public Set<K> keySet() {
		return (keySet == null) ?
				(keySet = new KSet(floatValueEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#put(java.lang.Object, float)
	 */
	public Float put(K k, float v) {
		boolean b = containsKey(k);
		float r = putElement(k, v);
		
		return b ? r : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#putAll(net.morilib.util.primitive.map.op.ToFloatMap)
	 */
	public void putAllElement(FloatValueMap<K> map) {
		for(PrimitiveEntryV<K> e : map.floatValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractFloatCollection {
		
		//
		private Set<PrimitiveEntryV<K>> eset;
		
		private VCol(Set<PrimitiveEntryV<K>> eset) {
			this.eset = eset;
		}

		public boolean addFloat(float v) {
			throw new UnsupportedOperationException();
		}

		public FloatIterator floatIterator() {
			final Iterator<PrimitiveEntryV<K>> ei = eset.iterator();
			
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
			return AbstractFloatValueMap.this.size();
		}

		public boolean isInfinite() {
			return false;
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#values()
	 */
	public FloatCollection floatValues() {
		return (values == null) ?
				(values = new VCol(floatValueEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Float.MIN_VALUE || v > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((float)v);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Float) {
			return containsValueElement(
					((Float)value).floatValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Float put(K key, Float value) {
		if(value instanceof Float) {
			return putElement(key, ((Float)value).floatValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends K, ? extends Float> m) {
		for(PrimitiveEntryV<K> e : floatValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#values()
	 */
	public Collection<Float> values() {
		final Iterator<PrimitiveEntryV<K>> i = floatValueEntrySet().iterator();
		
		return new AbstractCollection<Float>() {

			public Iterator<Float> iterator() {
				return new Iterator<Float>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Float next() {
						return i.next().getValue();
					}

					public void remove() {
						i.remove();
					}
					
				};
			}

			public int size() {
				return AbstractFloatValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<K, Float>> entrySet() {
		final Iterator<PrimitiveEntryV<K>> i = floatValueEntrySet().iterator();
		
		return new AbstractSet<Entry<K, Float>>() {

			public Iterator<Entry<K, Float>> iterator() {
				return new Iterator<Entry<K, Float>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Entry<K, Float> next() {
						final PrimitiveEntryV<K> e = i.next();
						
						return new Map.Entry<K, Float>() {

							public K getKey() {
								return e.getKey();
							}

							public Float getValue() {
								return e.getValue();
							}

							public Float setValue(Float value) {
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
				return AbstractFloatValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#remove(java.lang.Object)
	 */
	public Float remove(Object k) {
		Iterator<PrimitiveEntryV<K>> i = floatValueEntrySet().iterator();
		
		while(i.hasNext()) {
			PrimitiveEntryV<K> e = i.next();
			Object l = e.getKey();
			
			if((k == null && l == null) ||
					(k != null && k.equals(l))) {
				float r = e.getValue();
				
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
		
		for(PrimitiveEntryV<K> e : floatValueEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof FloatValueMap) {
			FloatValueMap<K> m = (FloatValueMap<K>)obj;
			Iterator<PrimitiveEntryV<K>> i = floatValueEntrySet().iterator();
			
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
		for(PrimitiveEntryV<K> e : floatValueEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
