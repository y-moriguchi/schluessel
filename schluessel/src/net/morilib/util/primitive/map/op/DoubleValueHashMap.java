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

import java.io.IOException;
import java.util.AbstractSet;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

import net.morilib.util.primitive.AbstractDoubleCollection;
import net.morilib.util.primitive.DoubleCollection;
import net.morilib.util.primitive.iterator.DoubleIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public class DoubleValueHashMap<K> extends AbstractDoubleValueMap<K>
implements java.io.Serializable {
	
	//
	private static final long serialVersionUID = 4823620077843167065L;
	private static final int MAXIMUM_CAPACITY = 1 << 30;
	
	//
	private static class Ent<K> implements PrimitiveEntryV<K> {
		
		//
		private K key;
		private double value;
		private Ent<?> next;
		
		//
		private Ent(K key, double value) {
			this.key   = key;
			this.value = value;
		}
		
		public K getKey() {
			return key;
		}

		public double getValue() {
			return value;
		}

		public double setValue(double v) {
			double r = value;
			
			value = v;
			return r;
		}

		public int hashCode() {
			return (((key == null) ? 0 : key.hashCode()) ^
					(int)value);
		}

		public boolean equals(Object obj) {
			if(obj instanceof Ent) {
				Ent<?> e = (Ent<?>)obj;
				
				return (((key == null && e.key == null) ||
						 (key != null && key.equals(e.key)) &&
						(value == e.value)));
			}
			return false;
		}

		public String toString() {
			return key + "=" + value;
		}
		
		//
		private Ent<?> searchEntry(Object k) {
			for(Ent<?> f = this; f != null; f = f.next) {
				if((f.key == null && k == null) ||
						(k != null && k.equals(f.key))) {
					return f;
				}
			}
			return null;
		}
		
		private Ent<?> removeEntry(Object k) {
			Ent<?> g = this;
			
			for(Ent<?> f = this.next; f != null; g = f, f = f.next) {
				if((f.key == null && k == null) ||
						(k != null && k.equals(f.key))) {
					g.next = f.next;
					return f;
				}
			}
			return null;
		}
		
		//
		private Double putEntry(K k, double v) {
			Ent<?> g = this;
			
			for(Ent<?> f = this.next; f != null; g = f, f = f.next) {
				if((f.key == null && k == null) ||
						(k != null && k.equals(f.key))) {
					Double r = Double.valueOf(f.value);
					
					f.value = v;
					return r;
				}
			}
			
			// add
			g.next = new Ent<K>(k, v);
			return null;
		}
		
	}
	
	//
	@SuppressWarnings("rawtypes")
	private transient Ent[] ent;
	private transient int size;
	private final float loadFactor;
	private int threshold;
	private transient volatile int modCount = 0;
	private transient Set<PrimitiveEntryV<K>> entrySet = null;
	private transient Set<K> keySet = null;
	private transient DoubleCollection values = null;
	
	public DoubleValueHashMap(int initialCapacity, float loadFactor) {
		int cp = 1;
		
		if(initialCapacity < 0) {
			throw new IllegalArgumentException(
					"Illegal initial capacity: " + initialCapacity);
		} else if(loadFactor <= 0.0f || Float.isNaN(loadFactor)) {
			throw new IllegalArgumentException(
					"Illegal load factor: " + loadFactor);
		}
		
		if(initialCapacity >= MAXIMUM_CAPACITY) {
			cp = MAXIMUM_CAPACITY;
		} else {
			while(cp < initialCapacity) {
				cp <<= 1;
			}
		}
		ent  = new Ent[cp];
		size = 0;
		threshold = (int)(cp * loadFactor + 1);
		this.loadFactor = loadFactor;
	}
	
	public DoubleValueHashMap(int initialCapacity) {
		this(initialCapacity, 0.75f);
	}
	
	public DoubleValueHashMap(float loadFactor) {
		this(16, loadFactor);
	}
	
	public DoubleValueHashMap() {
		this(16, 0.75f);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#clear()
	 */
	public void clear() {
		modCount++;
		size = 0;
		for(int i = 0; i < ent.length; i++) {
			ent[i] = null;
		}
	}
	
	/**
	 * 
	 * @param k
	 * @return
	 */
	protected int getHash(Object k) {
		return (k == null) ? 0 : k.hashCode();
	}
	
	//
	private Ent<?> _get(Object k) {
		int    h = getHash(k) % ent.length;
		
		return (ent[h] != null) ? ent[h].searchEntry(k) : null;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#inDomain(java.lang.Object)
	 */
	public boolean containsKey(Object k) {
		return _get(k) != null;
	}
	//
	private class ESet extends AbstractSet<PrimitiveEntryV<K>> {

		public Iterator<PrimitiveEntryV<K>> iterator() {
			return new Iterator<PrimitiveEntryV<K>>() {
				
				//
				private int     ptr = 0;
				private Ent<?>  e   = _next();
				private Ent<?>  remove = null;
				private int     exModCount = modCount;
				
				private Ent<?> _next() {
					if(e.next != null) {
						return e.next;
					} else {
						while(++ptr < ent.length) {
							if(ent[ptr] != null) {
								return ent[ptr];
							}
						}
						return null;
					}
				}
				
				public boolean hasNext() {
					return e != null;
				}

				@SuppressWarnings("unchecked")
				public DoubleValueMap.PrimitiveEntryV<K> next() {
					remove = e;
					if(e == null) {
						throw new NoSuchElementException();
					} else if(exModCount != modCount) {
						throw new ConcurrentModificationException();
					}
					e = _next();
					return (PrimitiveEntryV<K>)remove;
				}

				public void remove() {
					if(remove == null) {
						throw new IllegalStateException();
					}
					DoubleValueHashMap.this.remove(e.key);
					exModCount = modCount;
					remove = null;
				}
				
			};
		}

		public int size() {
			return size;
		}

		public boolean contains(Object o) {
			if(o instanceof Ent) {
				return containsKey(((Ent<?>)o).key);
			}
			return false;
		}

		public boolean remove(Object o) {
			if(o instanceof Ent) {
				return _remove(((Ent<?>)o).getKey()) != null;
			}
			return false;
		}

		public void clear() {
			DoubleValueHashMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#entrySet()
	 */
	public Set<DoubleValueMap.PrimitiveEntryV<K>> doubleValueEntrySet() {
		return (entrySet != null) ?
				entrySet : (entrySet = new ESet());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#get(java.lang.Object)
	 */
	public Double get(Object k) {
		Ent<?> e = _get(k);
		
		if(e == null) {
			throw new NoSuchElementException();
		}
		return e.value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#getValue(double)
	 */
	public double getElement(Object k) {
		Ent<?> e = _get(k);
		
		return (e == null) ? null : e.value;
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
			return size;
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return DoubleValueHashMap.this.remove(o) != null;
		}

		public void clear() {
			DoubleValueHashMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#keySet()
	 */
	public Set<K> keySet() {
		return (keySet == null) ?
				(keySet = new KSet(doubleValueEntrySet())) : keySet;
	}
	
	//
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private void resize() {
		if(size > threshold && ent.length < MAXIMUM_CAPACITY) {
			int ns = ent.length;
			Ent[] ne;
			
			while(size > threshold) {
				ns <<= 1;
				threshold = (int)(ns * loadFactor + 1);
			}
			ne = new Ent[ns];
			
			for(PrimitiveEntryV<K> e : doubleValueEntrySet()) {
				int h = getHash(e.getKey()) % ns;
				
				if(ne[h] == null) {
					ne[h] = new Ent<K>((K)e.getKey(), e.getValue());
				} else {
					ne[h].putEntry((K)e.getKey(), e.getValue());
				}
			}
		}
	}

	//
	@SuppressWarnings("unchecked")
	private void _putEntry(K k, double v) {
		int h = getHash(k) % ent.length;
		
		if(ent[h] == null) {
			ent[h] = new Ent<K>(k, v);
		} else {
			ent[h].putEntry(k, v);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#put(java.lang.Object, double)
	 */
	@SuppressWarnings("unchecked")
	public double putElement(K k, double v) {
		int h = getHash(k) % ent.length;
		
		modCount++;
		if(ent[h] == null) {
			size++;
			resize();
			ent[h] = new Ent<K>(k, v);
			return 0;
		} else {
			Double r = ent[h].putEntry(k, v);
			
			if(r == null) {
				size++;
				resize();
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#putAll(net.morilib.util.primitive.map.op.ToDoubleMap)
	 */
	@SuppressWarnings("unchecked")
	public void putAllElement(DoubleValueMap<K> map) {
		int nd = map.size();
		
		modCount++;
		if(nd == 0) {
			return;
		}
		size += nd;
		resize();
		
		for(PrimitiveEntryV<K> e : doubleValueEntrySet()) {
			int h = getHash(e.getKey()) % ent.length;
			
			if(ent[h] == null) {
				ent[h] = new Ent<K>(e.getKey(), e.getValue());
			} else {
				ent[h].putEntry(e.getKey(), e.getValue());
			}
		}
	}
	
	//
	private Ent<?> _remove(Object k) {
		int h = getHash(k) % ent.length;
		
		if(ent[h] == null) {
			return null;
		} else if(
				(k == null && ent[h].key == null) ||
				(k != null && k.equals(ent[h].key))) {
			Ent<?> r = ent[h];
			
			ent[h] = r.next;
			return r;
		} else {
			return ent[h].removeEntry(k);
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#remove(java.lang.Object)
	 */
	public Double remove(Object k) {
		Ent<?> e = _remove(k);
		
		return (e == null) ? null : e.value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#size()
	 */
	public int size() {
		return size;
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
			return size;
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
	
	//
	private void writeObject(
			java.io.ObjectOutputStream s) throws IOException {
		Iterator<PrimitiveEntryV<K>> i;
		
		s.defaultWriteObject();
		s.writeInt(ent.length);
		s.writeInt(size);
		
		i = doubleValueEntrySet().iterator();
		while(i.hasNext()) {
			PrimitiveEntryV<K> e = i.next();
			
			s.writeObject(e.getKey());
			s.writeDouble(e.getValue());
		}
	}
	
	//
	@SuppressWarnings("unchecked")
	private void readObject(
			java.io.ObjectInputStream s
			) throws IOException, ClassNotFoundException {
		s.defaultReadObject();
		
		int cp = s.readInt();
		ent = new Ent[cp];
		
		size = s.readInt();
		for(int i = 0; i < size; i++) {
			_putEntry((K)s.readObject(), s.readDouble());
		}
	}
	
}
