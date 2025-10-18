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

import java.util.Collection;
import java.util.Iterator;

import net.morilib.util.primitive.iterator.CharacterIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractCharacterCollection
implements CharacterCollection {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#addAll(net.morilib.util.primitive.CharacterCollection)
	 */
	public boolean addAllChar(CharacterCollection a) {
		CharacterIterator i = a.charIterator();
		boolean r = false;
		
		if(a.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				r = addChar(i.next()) | r;
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#addAll(net.morilib.util.primitive.CharacterCollection[])
	 */
	public boolean addAllChar(CharacterCollection... as) {
		boolean r = false;
		
		for(CharacterCollection a : as) {
			r = addAllChar(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#addAll(java.util.Collection)
	 */
	public boolean addAllChar(Collection<? extends CharacterCollection> as) {
		boolean r = false;
		
		for(CharacterCollection a : as) {
			r = addAllChar(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#clear()
	 */
	public void clear() {
		CharacterIterator i = charIterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#contains(char)
	 */
	public boolean containsChar(char v) {
		CharacterIterator i = charIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#containsAll(net.morilib.util.primitive.CharacterCollection)
	 */
	public boolean containsAllChar(CharacterCollection a) {
		if(!isInfinite() && a.isInfinite()) {
			return false;
		} else {
			CharacterIterator i = a.charIterator();
			
			while(i.hasNext()) {
				if(!containsChar(i.next())) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#remove(char)
	 */
	public boolean removeChar(char v) {
		CharacterIterator i = charIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				i.remove();
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#removeAll(net.morilib.util.primitive.CharacterCollection)
	 */
	public boolean removeAllChar(CharacterCollection a) {
		CharacterIterator i = a.toSet().charIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			char b = i.next();
			
			if(containsChar(b)) {
				r = removeChar(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#retainAll(net.morilib.util.primitive.CharacterCollection)
	 */
	public boolean retainAllChar(CharacterCollection a) {
		CharacterIterator i = charIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			char b = i.next();
			
			if(!a.containsChar(b)) {
				r = removeChar(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#toArray()
	 */
	public char[] toCharArray() {
		CharacterIterator i = charIterator();
		char[]       r = new char[size()];
		
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#toArray(char[])
	 */
	public char[] toCharArray(char[] a) {
		if(a.length < size()) {
			return toCharArray();
		} else {
			CharacterIterator i = charIterator();
			
			for(int j = 0; i.hasNext(); j++) {
				a[j] = i.next();
			}
			return a;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#contains(java.lang.Object)
	 */
	public boolean contains(Object o) {
		if(o instanceof Character) {
			return containsChar(((Character)o).charValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#iterator()
	 */
	public Iterator<Character> iterator() {
		final CharacterIterator i = charIterator();
		
		return new Iterator<Character>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Character next() {
				return i.next();
			}

			public void remove() {
				i.remove();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#toArray()
	 */
	public Object[] toArray() {
		CharacterIterator i = charIterator();
		Character[]      r = new Character[size()];
		
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#toArray(T[])
	 */
	@SuppressWarnings("unchecked")
	public <T> T[] toArray(T[] a) {
		Character[] r;
		CharacterIterator i;
		
		if(!(a instanceof Character[])) {
			throw new ClassCastException();
		} else if(a.length < size()) {
			r = new Character[size()];
		} else {
			r = (Character[])a;
		}
		
		i = charIterator();
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return (T[])r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#add(java.lang.Object)
	 */
	public boolean add(Character e) {
		return addChar(e.charValue());
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		if(o instanceof Character) {
			return remove(((Character)o).charValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#containsAll(java.util.Collection)
	 */
	public boolean containsAll(Collection<?> c) {
		Iterator<?> i = c.iterator();
		
		while(i.hasNext()) {
			Object o = i.next();
			
			if(!(o instanceof Character)) {
				return false;
			} else if(!containsChar(((Character)o).charValue())) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Character> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		if(c.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				Object o = i.next();
				
				if(o instanceof Character) {
					r = addChar(((Character)o).charValue()) | r;
				}
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#removeAll(java.util.Collection)
	 */
	public boolean removeAll(Collection<?> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		while(i.hasNext()) {
			Object o = i.next();
			
			if(o instanceof Character) {
				char x = ((Character)o).charValue();
				if(containsChar(x)) {
					r = removeChar(x) | r;
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#retainAll(java.util.Collection)
	 */
	public boolean retainAll(Collection<?> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		while(i.hasNext()) {
			Object o = i.next();
			
			if(o instanceof Character) {
				char x = ((Character)o).charValue();
				if(!containsChar(x)) {
					r = removeChar(x) | r;
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#toSet()
	 */
	public CharacterSet toSet() {
		return CharacterCollections.unmodifiableSet(
				new CharacterHashSet(this));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#add(int)
	 */
	public boolean add(int v) {
		if(v < Character.MIN_VALUE || v > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return addChar((char)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#contains(int)
	 */
	public boolean contains(int v) {
		return (v >= Character.MIN_VALUE && v <= Character.MAX_VALUE &&
				containsChar((char)v));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#removeElement(int)
	 */
	public boolean removeElement(int v) {
		if(v < Character.MIN_VALUE || v > Character.MAX_VALUE) {
			return false;
		}
		return removeChar((char)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();
		CharacterIterator i = charIterator();
		String d = "";
		
		b.append("[");
		while(i.hasNext()) {
			b.append(d).append(i.next());
			d = ",";
		}
		b.append("]");
		return b.toString();
	}

}
