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

import net.morilib.util.primitive.IntegerCollection;
import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public class IntegerHashSet extends AbstractIntegerSet
implements java.io.Serializable {
	
	//
	private static final long serialVersionUID = 4823620077843167065L;
	private static final int MAXIMUM_CAPACITY = 1 << 30;
	
	//
	private static class Ent {
		
		//
		private int value;
		private Ent next;
		
		//
		private Ent(int value) {
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
			return Integer.toString(value);
		}
		
		//
		private Ent searchEntry(int k) {
			for(Ent f = this; f != null; f = f.next) {
				if(k == f.value) {
					return f;
				}
			}
			return null;
		}
		
		private Ent removeEntry(int k) {
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
		private boolean putEntry(int k) {
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
	
	public IntegerHashSet(int initialCapacity, float loadFactor) {
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
	
	public IntegerHashSet(int initialCapacity) {
		this(initialCapacity, 0.75f);
	}
	
	public IntegerHashSet(float loadFactor) {
		this(16, loadFactor);
	}
	
	public IntegerHashSet() {
		this(16, 0.75f);
	}

	/**
	 * @param abstractIntegerSet
	 */
	public IntegerHashSet(IntegerCollection col) {
		this(16, 0.75f);
		addAllInt(col);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#iterator()
	 */
	public IntegerIterator intIterator() {
		return new IntegerIterator() {
			
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

			public int next() {
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
				IntegerHashSet.this.removeInt(remove.value);
				exModCount = modCount;
				remove = null;
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#clear()
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
	protected int getHash(int k) {
		// %HASHCODE% int
		return (int)(k);
	}
	
	//
	private Ent _get(int k) {
		int    h = getHash(k) % ent.length;
		
		return (ent[h] != null) ? ent[h].searchEntry(k) : null;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#inDomain(java.lang.Object)
	 */
	public boolean containsInt(int k) {
		return _get(k) != null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#isEmpty()
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
			
			for(IntegerIterator bi = intIterator(); bi.hasNext();) {
				int b = bi.next();
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
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#put(java.lang.Object, int)
	 */
	public boolean addInt(int v) {
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
	private boolean _putEntry(int v) {
		int h = getHash(v) % ent.length;
		
		if(ent[h] == null) {
			ent[h] = new Ent(v);
			return true;
		} else {
			return ent[h].putEntry(v);
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#putAll(net.morilib.util.primitive.map.op.ToIntegerMap)
	 */
	public boolean addAllInt(IntegerCollection c) {
		int nd = c.size();
		boolean r = false;
		
		modCount++;
		if(nd == 0) {
			return false;
		}
		size += nd;
		resize();
		
		for(IntegerIterator i = c.intIterator(); i.hasNext();) {
			r = _putEntry(i.next()) | r;
		}
		return r;
	}
	
	//
	private Ent _remove(int k) {
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
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#remove(java.lang.Object)
	 */
	public boolean removeInt(int k) {
		return _remove(k) != null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToIntegerMap#size()
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
		for(IntegerIterator i = intIterator(); i.hasNext();) {
			s.writeInt(i.next());
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
			_putEntry(s.readInt());
		}
	}
	
}
