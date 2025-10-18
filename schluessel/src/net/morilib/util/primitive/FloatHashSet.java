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
package net.morilib.util.primitive;

import java.io.IOException;
import java.util.ConcurrentModificationException;
import java.util.NoSuchElementException;

import net.morilib.util.primitive.FloatCollection;
import net.morilib.util.primitive.iterator.FloatIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public class FloatHashSet extends AbstractFloatSet
implements java.io.Serializable {
	
	//
	private static final long serialVersionUID = 4823620077843167065L;
	private static final int MAXIMUM_CAPACITY = 1 << 30;
	
	//
	private static class Ent {
		
		//
		private float value;
		private Ent next;
		
		//
		private Ent(float value) {
			this.value = value;
		}
		
		public int hashCode() {
			return (int)value;
		}

		public boolean equals(Object obj) {
			if(obj instanceof Ent) {
				Ent e = (Ent)obj;
				
				return value == e.value;
			}
			return false;
		}

		public String toString() {
			return Float.toString(value);
		}
		
		//
		private Ent searchEntry(float k) {
			for(Ent f = this; f != null; f = f.next) {
				if(k == f.value) {
					return f;
				}
			}
			return null;
		}
		
		private Ent removeEntry(float k) {
			Ent g = this;
			
			for(Ent f = this.next; f != null; g = f, f = f.next) {
				if(k == f.value) {
					g.next = f.next;
					return f;
				}
			}
			return null;
		}
		
		//
		private boolean putEntry(float k) {
			Ent g = this;
			
			for(Ent f = this.next; f != null; g = f, f = f.next) {
				if(k == f.value) {
					return true;
				}
			}
			
			// add
			g.next = new Ent(k);
			return false;
		}
		
	}
	
	//
	private transient Ent[] ent;
	private transient int size;
	private final float loadFactor;
	private int threshold;
	private transient volatile int modCount = 0;
	
	public FloatHashSet(int initialCapacity, float loadFactor) {
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
	
	public FloatHashSet(int initialCapacity) {
		this(initialCapacity, 0.75f);
	}
	
	public FloatHashSet(float loadFactor) {
		this(16, loadFactor);
	}
	
	public FloatHashSet() {
		this(16, 0.75f);
	}

	/**
	 * @param abstractFloatSet
	 */
	public FloatHashSet(FloatCollection col) {
		this(16, 0.75f);
		addAllFloat(col);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#iterator()
	 */
	public FloatIterator floatIterator() {
		return new FloatIterator() {
			
			//
			private int     ptr = -1;
			private Ent     e   = _next();
			private Ent     remove = null;
			private int     exModCount = modCount;
			
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

			public float next() {
				remove = e;
				if(e == null) {
					throw new NoSuchElementException();
				} else if(exModCount != modCount) {
					throw new ConcurrentModificationException();
				}
				e = _next();
				return remove.value;
			}

			public void remove() {
				if(remove == null) {
					throw new IllegalStateException();
				}
				FloatHashSet.this.removeFloat(remove.value);
				exModCount = modCount;
				remove = null;
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#clear()
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
	protected int getHash(float k) {
		// %HASHCODE% float
		return Float.floatToIntBits(k);
	}
	
	//
	private Ent _get(float k) {
		int    h = getHash(k) % ent.length;
		
		return (ent[h] != null) ? ent[h].searchEntry(k) : null;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#inDomain(java.lang.Object)
	 */
	public boolean containsFloat(float k) {
		return _get(k) != null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size == 0;
	}
	
	//
	private void resize() {
		if(size > threshold && ent.length < MAXIMUM_CAPACITY) {
			int ns = ent.length;
			Ent[] ne;
			
			while(size > threshold) {
				ns <<= 1;
				threshold = (int)(ns * loadFactor + 1);
			}
			ne = new Ent[ns];
			
			for(FloatIterator bi = floatIterator(); bi.hasNext();) {
				float b = bi.next();
				int  h = getHash(b) % ns;
				
				if(ne[h] == null) {
					ne[h] = new Ent(b);
				} else {
					ne[h].putEntry(b);
				}
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#put(java.lang.Object, float)
	 */
	public boolean addFloat(float v) {
		int h = getHash(v) % ent.length;
		
		modCount++;
		if(ent[h] == null) {
			size++;
			resize();
			ent[h] = new Ent(v);
			return false;
		} else {
			boolean r = ent[h].putEntry(v);
			
			if(!r) {
				size++;
				resize();
			}
			return r;
		}
	}
	
	//
	private boolean _putEntry(float v) {
		int h = getHash(v) % ent.length;
		
		if(ent[h] == null) {
			ent[h] = new Ent(v);
			return true;
		} else {
			return ent[h].putEntry(v);
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#putAll(net.morilib.util.primitive.map.op.ToFloatMap)
	 */
	public boolean addAllFloat(FloatCollection c) {
		int nd = c.size();
		boolean r = false;
		
		modCount++;
		if(nd == 0) {
			return false;
		}
		size += nd;
		resize();
		
		for(FloatIterator i = c.floatIterator(); i.hasNext();) {
			r = _putEntry(i.next()) | r;
		}
		return r;
	}
	
	//
	private Ent _remove(float k) {
		int h = getHash(k) % ent.length;
		
		if(ent[h] == null) {
			return null;
		} else if(k == ent[h].value) {
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
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#remove(java.lang.Object)
	 */
	public boolean removeFloat(float k) {
		return _remove(k) != null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToFloatMap#size()
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
		for(FloatIterator i = floatIterator(); i.hasNext();) {
			s.writeFloat(i.next());
		}
	}
	
	//
	private void readObject(
			java.io.ObjectInputStream s
			) throws IOException, ClassNotFoundException {
		s.defaultReadObject();
		
		int cp = s.readInt();
		ent = new Ent[cp];
		
		size = s.readInt();
		for(int i = 0; i < size; i++) {
			_putEntry(s.readFloat());
		}
	}
	
}
