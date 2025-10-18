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

import java.util.AbstractList;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.RandomAccess;

import net.morilib.lang.Hashes;
import net.morilib.util.primitive.iterator.CharacterIterator;
import net.morilib.util.primitive.iterator.CharacterVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractCharacterVector extends AbstractCharacterCollection
implements CharacterVector, RandomAccess {

	//
//	private static final long serialVersionUID = 3332872309405682099L;

	/**
	 * 
	 */
	protected transient int modCount = 0;

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterList#addAll(int, net.morilib.util.primitive.CharacterCollection)
	 */
	public boolean addAllChar(int index, CharacterCollection a) {
		int i2 = index;

		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		modCount++;
		if(a.isEmpty()) {
			return false;
		} else {
			CharacterIterator i = a.charIterator();

			while(i.hasNext()) {
				addChar(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterList#first()
	 */
	public char first() {
		if(isEmpty()) {
			throw new NoSuchElementException();
		}
		return getChar(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterList#indexOf(char)
	 */
	public int indexOfChar(char v) {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				if(getChar(i) == v) {
					return i;
				}
			}
		} else {
			CharacterVectorIterator i = charVectorIterator();

			for(int j = 0; i.hasNext(); j++) {
				if(i.next() == v) {
					return j;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterList#rest()
	 */
	public CharacterList rest() {
		return subVector(1, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterList#rest(int)
	 */
	public CharacterList rest(int index) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		} else if(index == size()) {
			return CharacterCollections.EMPTY_VECTOR;
		}
		return subVector(index, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#add(char)
	 */
	public boolean addChar(char v) {
		modCount++;
		addChar(size(), v);
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#addAll(net.morilib.util.primitive.CharacterCollection)
	 */
	public boolean addAllChar(CharacterCollection a) {
		return addAllChar(size(), a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#addAll(net.morilib.util.primitive.CharacterCollection[])
	 */
	public boolean addAllChar(CharacterCollection... as) {
		int nsize = 0;
		for(CharacterCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(CharacterCollection a : as) {
				r = addAllChar(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#addAll(java.util.Iterator)
	 */
	public boolean addAllChar(Collection<? extends CharacterCollection> as) {
		int nsize = 0;
		for(CharacterCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(CharacterCollection a : as) {
				r = addAllChar(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#contains(char)
	 */
	public boolean containsChar(char v) {
		return indexOfChar(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#isInfinite()
	 */
	public boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllChar(char[] a) {
		return addAllChar(size(), new CharacterArrayVector(a));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllChar(int ptr, char[] a) {
		return addAllChar(ptr, new CharacterArrayVector(a));
	}

	//
	private class Itr implements CharacterIterator {

		//
		/*package*/ int ptr = 0;
		/*package*/ int ptrrem = -1;
		/*package*/ int exModCount = modCount;

		//
		/*package*/ void checks() {
			if(!hasNext()) {
				throw new NoSuchElementException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}
		}

		//
		/*package*/ void checks2() {
			if(ptrrem < 0) {
				throw new IllegalStateException();
			} else if(ptrrem >= size()) {
				throw new NoSuchElementException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}
		}

		public boolean hasNext() {
			return ptr < size();
		}

		public char next() {
			checks();
			ptrrem = ptr;
			return getChar(ptr++);
		}

		public void remove() {
			checks2();
			try {
				AbstractCharacterVector.this.removeAt(ptrrem);
				exModCount = modCount;
				ptr--;
				ptrrem = -1;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

	};

	//
	private class VItr extends Itr implements CharacterVectorIterator {

		public void addChar(char v) {
			if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				ptrrem = -1;
				AbstractCharacterVector.this.addChar(ptr++, v);
				exModCount = modCount;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		public boolean hasPrevious() {
			return ptr > 0;
		}

		public int nextIndex() {
			return ptr;
		}

		public char previous() {
			char res;

			if(!hasPrevious()) {
				throw new NoSuchElementException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				res = AbstractCharacterVector.this.getChar(ptr);
				ptrrem = ptr = ptr - 1;
				return res;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		public int previousIndex() {
			return ptr - 1;
		}

		public void setChar(char v) {
			if(!hasNext()) {
				throw new IllegalArgumentException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			} else if(ptrrem < 0) {
				throw new IllegalStateException();
			}

			try {
				AbstractCharacterVector.this.setChar(ptr, v);
				ptrrem = -1;
				exModCount = modCount;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.CharacterVectorIterator#add(int)
		 */
		public void add(int v) {
			if(v < Character.MIN_VALUE || v > Character.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			addChar((char)v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.CharacterVectorIterator#set(int)
		 */
		public void set(int v) {
			if(v < Character.MIN_VALUE || v > Character.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			setChar((char)v);
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#iterator()
	 */
	public CharacterIterator charIterator() {
		return new Itr();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#remove(char)
	 */
	public boolean removeChar(char v) {
		int ind = indexOfChar(v);

		modCount++;
		if(ind < 0) {
			return false;
		} else {
			removeAt(ind);
			return true;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.primitive.AbstractCharacterCollection#remove(java.lang.Object)
	 */
	public boolean remove(Object v) {
		int ind = indexOf(v);

		modCount++;
		if(ind < 0) {
			return false;
		} else {
			removeAt(ind);
			return true;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.primitive.AbstractCharacterCollection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Character> a) {
		modCount++;
		return addAll(size(), a);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.primitive.AbstractCharacterCollection#contains(java.lang.Object)
	 */
	public boolean contains(Object v) {
		return indexOf(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterVector#lastIndexOf(char)
	 */
	public int lastIndexOfChar(char v) {
		if(this instanceof RandomAccess) {
			for(int i = size() - 1; i >= 0; i--) {
				if(getChar(i) == v) {
					return i;
				}
			}
		} else {
			CharacterVectorIterator i = charVectorIterator();

			for(; i.hasNext(); i.next());
			for(int j = size() - 1; i.hasPrevious(); j--) {
				if(i.previous() == v) {
					return j;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterVector#vectorIterator()
	 */
	public CharacterVectorIterator charVectorIterator() {
		return charVectorIterator(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterVector#vectorIterator(int)
	 */
	public CharacterVectorIterator charVectorIterator(int index) {
		return new VItr();
	}

	//
	private static class SubV extends AbstractCharacterVector {

		//
		private CharacterVector vector;
		private int bindex, eindex;

		//
		private SubV(CharacterVector vector, int b, int e) {
			this.vector = vector;
			this.bindex = b;
			this.eindex = e;
		}

		public void addChar(int index, char v) {
			if(index < 0 || index > size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex++;
			vector.addChar(index + bindex, v);
		}

		public char getChar(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.getChar(index + bindex);
		}

		public char removeAt(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex--;
			return vector.removeAt(index + bindex);
		}

		public char setChar(int index, char v) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.setChar(index + bindex, v);
		}

		public int size() {
			return eindex - bindex;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterVector#subVector(int, int)
	 */
	public CharacterVector subVector(int start, int end) {
		if(start < 0 || start >= size()) {
			throw new IndexOutOfBoundsException();
		} else if(end < 0 || end > size()) {
			throw new IndexOutOfBoundsException();
		} else if(end < start) {
			throw new IndexOutOfBoundsException();
		}
		return new SubV(this, start, end);
	}

	/* (non-Javadoc)
	 * @see java.util.List#addAll(int, java.util.Collection)
	 */
	public boolean addAll(int index, Collection<? extends Character> c) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		int i2 = index;
		modCount++;
		if(c.isEmpty()) {
			return false;
		} else {
			Iterator<? extends Character> i = c.iterator();

			while(i.hasNext()) {
				add(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#get(int)
	 */
	public Character get(int index) {
		return getChar(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	public Character set(int index, Character element) {
		return setChar(index, element.charValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, Character element) {
		addChar(index, element.charValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public Character remove(int index) {
		return removeAt(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object o) {
		if(o instanceof Character) {
			return indexOfChar(((Character)o).charValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	public int lastIndexOf(Object o) {
		if(o instanceof Character) {
			return lastIndexOfChar(((Character)o).charValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator()
	 */
	public ListIterator<Character> listIterator() {
		final CharacterVectorIterator i = charVectorIterator();

		return new ListIterator<Character>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Character next() {
				return i.next();
			}

			public boolean hasPrevious() {
				return i.hasPrevious();
			}

			public Character previous() {
				return i.previous();
			}

			public int nextIndex() {
				return i.nextIndex();
			}

			public int previousIndex() {
				return i.previousIndex();
			}

			public void remove() {
				i.remove();
			}

			public void set(Character e) {
				i.setChar(e.charValue());
			}

			public void add(Character e) {
				i.addChar(e.charValue());
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator(int)
	 */
	public ListIterator<Character> listIterator(int index) {
		return listIterator(0);
	}

	/* (non-Javadoc)
	 * @see java.util.List#subList(int, int)
	 */
	public List<Character> subList(int fromIndex, int toIndex) {
		final CharacterVector v = subVector(fromIndex, toIndex);

		return new AbstractList<Character>() {

			public Character get(int index) {
				return v.get(index);
			}

			public int size() {
				return AbstractCharacterVector.this.size();
			}

			public Character set(int index, Character element) {
				return v.set(index, element);
			}

			public void add(int index, Character element) {
				v.add(index, element);
			}

			public Character remove(int index) {
				return v.remove(index);
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterList#add(int, int)
	 */
	public void add(int index, int v) {
		if(v < Character.MIN_VALUE || v > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		addChar(index, (char)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterList#set(int, int)
	 */
	public char set(int index, int v) {
		if(v < Character.MIN_VALUE || v > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return setChar(index, (char)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterList#indexOf(int)
	 */
	public int indexOf(int v) {
		if(v < Character.MIN_VALUE || v > Character.MAX_VALUE) {
			return -1;
		}
		return indexOfChar((char)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Hashes.sumHashCode(toCharArray());
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj == this) {
			return true;
		} else if(obj instanceof CharacterVector) {
			CharacterVector v = (CharacterVector)obj;

			if(size() != v.size()) {
				return false;
			}
			for(int i = 0; i < size(); i++) {
				if(getChar(i) != v.getChar(i)) {
					return false;
				}
			}
			return true;
		} else if(obj instanceof CharacterList) {
			CharacterIterator i = charIterator();
			CharacterIterator j = ((CharacterList)obj).charIterator();

			while(i.hasNext() && j.hasNext()) {
				if(i.next() != j.next()) {
					return false;
				}
			}
			return !i.hasNext() && !j.hasNext();
		} else {
			return false;
		}
	}

}
