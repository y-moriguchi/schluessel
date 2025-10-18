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

import net.morilib.util.primitive.AbstractIntegerCollection;
import net.morilib.util.primitive.IntegerCollection;
import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractIntegerValueMap<K> implements IntegerValueMap<K> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient Set<K> keySet = null;
	private transient IntegerCollection values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryV<K>> i = intValueEntrySet().iterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#inDomain(java.lang.Object)
	 */
	public boolean containsKey(Object k) {
		if(k instanceof Integer) {
			containsValueElement(((Integer)k).intValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#inRange(int)
	 */
	public boolean containsValueElement(int v) {
		for(PrimitiveEntryV<K> e : intValueEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#get(java.lang.Object)
	 */
	public Integer get(Object k) {
		if(k instanceof Integer) {
			for(PrimitiveEntryV<K> e : intValueEntrySet()) {
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
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#getValue(int)
	 */
	public int getElement(Object k) {
		if(k instanceof Integer) {
			for(PrimitiveEntryV<K> e : intValueEntrySet()) {
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
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#isEmpty()
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
			return AbstractIntegerValueMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractIntegerValueMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractIntegerValueMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#keySet()
	 */
	public Set<K> keySet() {
		return (keySet == null) ?
				(keySet = new KSet(intValueEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#put(java.lang.Object, int)
	 */
	public Integer put(K k, int v) {
		boolean b = containsKey(k);
		int r = putElement(k, v);
		
		return b ? r : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#putAll(net.morilib.util.primitive.map.op.ToIntegerMap)
	 */
	public void putAllElement(IntegerValueMap<K> map) {
		for(PrimitiveEntryV<K> e : map.intValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractIntegerCollection {
		
		//
		private Set<PrimitiveEntryV<K>> eset;
		
		private VCol(Set<PrimitiveEntryV<K>> eset) {
			this.eset = eset;
		}

		public boolean addInt(int v) {
			throw new UnsupportedOperationException();
		}

		public IntegerIterator intIterator() {
			final Iterator<PrimitiveEntryV<K>> ei = eset.iterator();
			
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
			return AbstractIntegerValueMap.this.size();
		}

		public boolean isInfinite() {
			return false;
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#values()
	 */
	public IntegerCollection intValues() {
		return (values == null) ?
				(values = new VCol(intValueEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((int)v);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Integer) {
			return containsValueElement(
					((Integer)value).intValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Integer put(K key, Integer value) {
		if(value instanceof Integer) {
			return putElement(key, ((Integer)value).intValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends K, ? extends Integer> m) {
		for(PrimitiveEntryV<K> e : intValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#values()
	 */
	public Collection<Integer> values() {
		final Iterator<PrimitiveEntryV<K>> i = intValueEntrySet().iterator();
		
		return new AbstractCollection<Integer>() {

			public Iterator<Integer> iterator() {
				return new Iterator<Integer>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Integer next() {
						return i.next().getValue();
					}

					public void remove() {
						i.remove();
					}
					
				};
			}

			public int size() {
				return AbstractIntegerValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<K, Integer>> entrySet() {
		final Iterator<PrimitiveEntryV<K>> i = intValueEntrySet().iterator();
		
		return new AbstractSet<Entry<K, Integer>>() {

			public Iterator<Entry<K, Integer>> iterator() {
				return new Iterator<Entry<K, Integer>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Entry<K, Integer> next() {
						final PrimitiveEntryV<K> e = i.next();
						
						return new Map.Entry<K, Integer>() {

							public K getKey() {
								return e.getKey();
							}

							public Integer getValue() {
								return e.getValue();
							}

							public Integer setValue(Integer value) {
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
				return AbstractIntegerValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#remove(java.lang.Object)
	 */
	public Integer remove(Object k) {
		Iterator<PrimitiveEntryV<K>> i = intValueEntrySet().iterator();
		
		while(i.hasNext()) {
			PrimitiveEntryV<K> e = i.next();
			Object l = e.getKey();
			
			if((k == null && l == null) ||
					(k != null && k.equals(l))) {
				int r = e.getValue();
				
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
		
		for(PrimitiveEntryV<K> e : intValueEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof IntegerValueMap) {
			IntegerValueMap<K> m = (IntegerValueMap<K>)obj;
			Iterator<PrimitiveEntryV<K>> i = intValueEntrySet().iterator();
			
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
		for(PrimitiveEntryV<K> e : intValueEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
