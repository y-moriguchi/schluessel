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

import java.io.IOException;
import java.util.AbstractSet;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public class DoubleHashMap<V> extends AbstractDoubleMap<V>
implements java.io.Serializable {
	
	//
	private static final long serialVersionUID = 4823620077843167065L;
	private static final int MAXIMUM_CAPACITY = 1 << 30;
	private static final Object MARK_A = new Object();
	
	//
	private static class Ent<V> implements PrimitiveEntryK<V> {
		
		//
		private double  key;
		private V      value;
		private Ent<V> next;
		
		//
		private Ent(double key, V value) {
			this.key   = key;
			this.value = value;
		}
		
		public int hashCode() {
			return (int)key;
		}

		@SuppressWarnings("rawtypes")
		public boolean equals(Object obj) {
			if(obj instanceof Ent) {
				Ent e = (Ent)obj;
				
				return key == e.key;
			}
			return false;
		}

		public String toString() {
			return Double.toString(key) + "=" + value;
		}
		
		//
		@SuppressWarnings("rawtypes")
		private Ent searchEntry(double k) {
			for(Ent f = this; f != null; f = f.next) {
				if(k == f.key) {
					return f;
				}
			}
			return null;
		}
		
		@SuppressWarnings({ "rawtypes", "unchecked" })
		private Ent removeEntry(double k) {
			Ent g = this;
			
			for(Ent f = this.next; f != null; g = f, f = f.next) {
				if(k == f.key) {
					g.next = f.next;
					return f;
				}
			}
			return null;
		}
		
		//
		@SuppressWarnings({ "rawtypes", "unchecked" })
		private Object putEntry(double k, V v) {
			Ent g = this;
			
			for(Ent f = this.next; f != null; g = f, f = f.next) {
				if(k == f.key) {
					Object r = f.value;
					
					f.value = v;
					return r;
				}
			}
			
			// add
			g.next = new Ent(k, v);
			return MARK_A;
		}

		public double getKey() {
			return key;
		}

		public V getValue() {
			return value;
		}

		public V setValue(V v) {
			V r = value;
			
			value = v;
			return r;
		}
		
	}
	
	//
	@SuppressWarnings("rawtypes")
	private transient Ent[] ent;
	private transient int size;
	private final float loadFactor;
	private int threshold;
	private transient volatile int modCount = 0;
	private transient Set<PrimitiveEntryK<V>> entrySet = null;
	
	/**
	 * 
	 * @param initialCapacity
	 * @param loadFactor
	 */
	public DoubleHashMap(int initialCapacity, float loadFactor) {
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
	
	/**
	 * 
	 * @param initialCapacity
	 */
	public DoubleHashMap(int initialCapacity) {
		this(initialCapacity, 0.75f);
	}
	
	/**
	 * 
	 * @param loadFactor
	 */
	public DoubleHashMap(float loadFactor) {
		this(16, loadFactor);
	}
	
	/**
	 * 
	 */
	public DoubleHashMap() {
		this(16, 0.75f);
	}

	/**
	 * @param abstractDoubleSet
	 */
	public DoubleHashMap(DoubleMap<V> mp) {
		this(16, 0.75f);
		putAllElement(mp);
	}

	//
	private Iterator<PrimitiveEntryK<V>> doubleEntrySetIterator() {
		return new Iterator<PrimitiveEntryK<V>>() {
			
			//
			private int     ptr = -1;
			@SuppressWarnings("rawtypes")
			private Ent     e   = _next();
			@SuppressWarnings("rawtypes")
			private Ent     remove = null;
			private int     exModCount = modCount;
			
			@SuppressWarnings("rawtypes")
			private Ent _next() {
				e = (ptr < 0) ? null : e.next;
				
				if(e != null) {
					return e;
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

			@SuppressWarnings({ "unchecked" })
			public PrimitiveEntryK<V> next() {
				remove = e;
				if(e == null) {
					throw new NoSuchElementException();
				} else if(exModCount != modCount) {
					throw new ConcurrentModificationException();
				}
				e = _next();
				return remove;
			}

			public void remove() {
				if(remove == null) {
					throw new IllegalStateException();
				}
				_remove(remove.key);
				exModCount = modCount;
				remove = null;
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#doubleEntrySet()
	 */
	public Set<DoubleMap.PrimitiveEntryK<V>> doubleKeyEntrySet() {
		if(entrySet == null) {
			entrySet = new AbstractSet<PrimitiveEntryK<V>>() {

				public Iterator<DoubleMap.PrimitiveEntryK<V>> iterator() {
					return doubleEntrySetIterator();
				}

				public int size() {
					return DoubleHashMap.this.size();
				}
				
			};
		}
		return entrySet;
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
	protected int getHash(double k) {
		// %HASHCODE% double
		return (int)Double.doubleToLongBits(k);
	}
	
	//
	@SuppressWarnings("rawtypes")
	private Ent _get(double k) {
		int    h = getHash(k) % ent.length;
		
		return (ent[h] != null) ? ent[h].searchEntry(k) : null;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.AbstractDoubleMap#getElement(double)
	 */
	@SuppressWarnings("unchecked")
	public V getElement(double k) {
		return (V)_get(k).value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#inDomain(java.lang.Object)
	 */
	public boolean containsKeyElement(double k) {
		return _get(k) != null;
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
			
			for(PrimitiveEntryK<V> e : doubleKeyEntrySet()) {
				double b = e.getKey();
				int   h = getHash(b) % ns;
				
				if(ne[h] == null) {
					ne[h] = new Ent(b, e.getValue());
				} else {
					ne[h].putEntry(b, e.getValue());
				}
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#put(java.lang.Object, double)
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public V putElement(double k, V v) {
		int h = getHash(k) % ent.length;
		
		modCount++;
		if(ent[h] == null) {
			size++;
			resize();
			ent[h] = new Ent(k, v);
			return null;
		} else {
			Object r = ent[h].putEntry(k, v);
			
			if(r == MARK_A) {
				size++;
				resize();
				return null;
			} else {
				return (V)r;
			}
		}
	}
	
	//
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private boolean _putEntry(double k, V v) {
		int h = getHash(k) % ent.length;
		
		if(ent[h] == null) {
			ent[h] = new Ent(k, v);
			return true;
		} else {
			return ent[h].putEntry(k, v) != MARK_A;
		}
	}
	
	//
	@SuppressWarnings("rawtypes")
	private Ent _remove(double k) {
		int h = getHash(k) % ent.length;
		
		if(ent[h] == null) {
			return null;
		} else if(k == ent[h].key) {
			Ent r = ent[h];
			
			size--;
			ent[h] = r.next;
			return r;
		} else {
			size--;
			return ent[h].removeEntry(k);
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#remove(java.lang.Object)
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public V removeElement(double k) {
		Ent r = _remove(k);
		
		return (r != null) ? (V)r.value : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToDoubleMap#size()
	 */
	public int size() {
		return size;
	}
	
	//
	private void writeObject(
			java.io.ObjectOutputStream s) throws IOException {
		s.defaultWriteObject();
		
		s.writeInt(ent.length);
		s.writeInt(size);
		for(PrimitiveEntryK<V> e : doubleKeyEntrySet()) {
			s.writeDouble (e.getKey());
			s.writeObject(e.getValue());
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
			double k = s.readDouble();
			V     v = (V)s.readObject();
			
			_putEntry(k, v);
		}
	}
	
}
