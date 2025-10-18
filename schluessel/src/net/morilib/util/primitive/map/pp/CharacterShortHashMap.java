/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Shortersion 2.0 (the "License");
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
public class CharacterShortHashMap
extends AbstractCharacterShortMap
implements java.io.Serializable {
	
	//
	private static final long serialVersionUID = -2524637876889987554L;
	private static final int MAXIMUM_CAPACITY = 1 << 30;
	private static final Object MARK_A = new Object();
	
	//
	private static class Ent implements PrimitiveEntry {
		
		//
		private char  key;
		private short value;
		private Ent next;
		
		//
		private Ent(char key, short value) {
			this.key   = key;
			this.value = value;
		}
		
		public int hashCode() {
			return (int)key;
		}

		public boolean equals(Object obj) {
			if(obj instanceof Ent) {
				Ent e = (Ent)obj;
				
				return key == e.key;
			}
			return false;
		}

		public String toString() {
			return Character.toString(key) + "=" + value;
		}
		
		//
		private Ent searchEntry(char k) {
			for(Ent f = this; f != null; f = f.next) {
				if(k == f.key) {
					return f;
				}
			}
			return null;
		}
		
		private Ent removeEntry(char k) {
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
		private Object putEntry(char k, Short v) {
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

		public char getKey() {
			return key;
		}

		public short getValue() {
			return value;
		}

		public short setValue(short v) {
			short r = value;
			
			value = v;
			return r;
		}
		
	}
	
	//
	private transient Ent[] ent;
	private transient int size;
	private final float loadFactor;
	private int threshold;
	private transient volatile int modCount = 0;
	private transient Set<PrimitiveEntry> entrySet = null;
	
	/**
	 * 
	 * @param initialCapacity
	 * @param loadFactor
	 */
	public CharacterShortHashMap(int initialCapacity, float loadFactor) {
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
	public CharacterShortHashMap(int initialCapacity) {
		this(initialCapacity, 0.75f);
	}
	
	/**
	 * 
	 * @param loadFactor
	 */
	public CharacterShortHashMap(float loadFactor) {
		this(16, loadFactor);
	}
	
	/**
	 * 
	 */
	public CharacterShortHashMap() {
		this(16, 0.75f);
	}

	/**
	 * @param abstractCharacterSet
	 */
	public CharacterShortHashMap(CharacterShortMap mp) {
		this(16, 0.75f);
		putAllElement(mp);
	}

	//
	private Iterator<PrimitiveEntry> entrySetIterator() {
		return new Iterator<PrimitiveEntry>() {
			
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

			public PrimitiveEntry next() {
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
	 * @see net.morilib.util.primitive.map.po.CharacterMap#charEntrySet()
	 */
	public Set<CharacterShortMap.PrimitiveEntry> primitiveEntrySet() {
		if(entrySet == null) {
			entrySet = new AbstractSet<PrimitiveEntry>() {

				public Iterator<PrimitiveEntry> iterator() {
					return entrySetIterator();
				}

				public int size() {
					return CharacterShortHashMap.this.size();
				}
				
			};
		}
		return entrySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#clear()
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
	protected int getHash(char k) {
		// %HASHCODE% char
		return (int)(k);
	}
	
	//
	private Ent _get(char k) {
		int    h = getHash(k) % ent.length;
		
		return (ent[h] != null) ? ent[h].searchEntry(k) : null;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#inDomain(java.lang.Object)
	 */
	public boolean containsKeyElement(char k) {
		return _get(k) != null;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.pp.CharacterShortMap#getElement(char)
	 */
	public short f(char k) {
		return _get(k).value;
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
			
			for(PrimitiveEntry e : primitiveEntrySet()) {
				char b = e.getKey();
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
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#put(java.lang.Object, char)
	 */
	public Short putElement(char k, short v) {
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
				return (Short)r;
			}
		}
	}
	
	//
	private boolean _putEntry(char k, Short v) {
		int h = getHash(k) % ent.length;
		
		if(ent[h] == null) {
			ent[h] = new Ent(k, v);
			return true;
		} else {
			return ent[h].putEntry(k, v) != MARK_A;
		}
	}
	
	//
	private Ent _remove(char k) {
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
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#remove(java.lang.Object)
	 */
	public Short removeElement(char k) {
		Ent r = _remove(k);
		
		return (r != null) ? (Short)r.value : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#size()
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
		for(PrimitiveEntry e : primitiveEntrySet()) {
			s.writeChar  (e.getKey());
			s.writeObject(e.getValue());
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
			char k = s.readChar();
			Short     v = (Short)s.readObject();
			
			_putEntry(k, v);
		}
	}
	
}
