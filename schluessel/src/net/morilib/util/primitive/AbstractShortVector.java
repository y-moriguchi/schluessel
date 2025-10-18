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
import net.morilib.util.primitive.iterator.ShortIterator;
import net.morilib.util.primitive.iterator.ShortVectorIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractShortVector extends AbstractShortCollection
implements ShortVector, RandomAccess {

	//
//	private static final long serialVersionUID = 3332872309405682099L;

	/**
	 * 
	 */
	protected transient int modCount = 0;

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortList#addAll(int, net.morilib.util.primitive.ShortCollection)
	 */
	public boolean addAllShort(int index, ShortCollection a) {
		int i2 = index;

		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		modCount++;
		if(a.isEmpty()) {
			return false;
		} else {
			ShortIterator i = a.shortIterator();

			while(i.hasNext()) {
				addShort(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortList#first()
	 */
	public short first() {
		if(isEmpty()) {
			throw new NoSuchElementException();
		}
		return getShort(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortList#indexOf(short)
	 */
	public int indexOfShort(short v) {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				if(getShort(i) == v) {
					return i;
				}
			}
		} else {
			ShortVectorIterator i = shortVectorIterator();

			for(int j = 0; i.hasNext(); j++) {
				if(i.next() == v) {
					return j;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortList#rest()
	 */
	public ShortList rest() {
		return subVector(1, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortList#rest(int)
	 */
	public ShortList rest(int index) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		} else if(index == size()) {
			return ShortCollections.EMPTY_VECTOR;
		}
		return subVector(index, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#add(short)
	 */
	public boolean addShort(short v) {
		modCount++;
		addShort(size(), v);
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#addAll(net.morilib.util.primitive.ShortCollection)
	 */
	public boolean addAllShort(ShortCollection a) {
		return addAllShort(size(), a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#addAll(net.morilib.util.primitive.ShortCollection[])
	 */
	public boolean addAllShort(ShortCollection... as) {
		int nsize = 0;
		for(ShortCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(ShortCollection a : as) {
				r = addAllShort(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#addAll(java.util.Iterator)
	 */
	public boolean addAllShort(Collection<? extends ShortCollection> as) {
		int nsize = 0;
		for(ShortCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(ShortCollection a : as) {
				r = addAllShort(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#contains(short)
	 */
	public boolean containsShort(short v) {
		return indexOfShort(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#isInfinite()
	 */
	public boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllShort(short[] a) {
		return addAllShort(size(), new ShortArrayVector(a));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllShort(int ptr, short[] a) {
		return addAllShort(ptr, new ShortArrayVector(a));
	}

	//
	private class Itr implements ShortIterator {

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

		public short next() {
			checks();
			ptrrem = ptr;
			return getShort(ptr++);
		}

		public void remove() {
			checks2();
			try {
				AbstractShortVector.this.removeAt(ptrrem);
				exModCount = modCount;
				ptr--;
				ptrrem = -1;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

	};

	//
	private class VItr extends Itr implements ShortVectorIterator {

		public void addShort(short v) {
			if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				ptrrem = -1;
				AbstractShortVector.this.addShort(ptr++, v);
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

		public short previous() {
			short res;

			if(!hasPrevious()) {
				throw new NoSuchElementException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				res = AbstractShortVector.this.getShort(ptr);
				ptrrem = ptr = ptr - 1;
				return res;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		public int previousIndex() {
			return ptr - 1;
		}

		public void setShort(short v) {
			if(!hasNext()) {
				throw new IllegalArgumentException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			} else if(ptrrem < 0) {
				throw new IllegalStateException();
			}

			try {
				AbstractShortVector.this.setShort(ptr, v);
				ptrrem = -1;
				exModCount = modCount;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.ShortVectorIterator#add(int)
		 */
		public void add(int v) {
			if(v < Short.MIN_VALUE || v > Short.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			addShort((short)v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.ShortVectorIterator#set(int)
		 */
		public void set(int v) {
			if(v < Short.MIN_VALUE || v > Short.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			setShort((short)v);
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#iterator()
	 */
	public ShortIterator shortIterator() {
		return new Itr();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#remove(short)
	 */
	public boolean removeShort(short v) {
		int ind = indexOfShort(v);

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
	 * @see net.morilib.util.primitive.AbstractShortCollection#remove(java.lang.Object)
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
	 * @see net.morilib.util.primitive.AbstractShortCollection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Short> a) {
		modCount++;
		return addAll(size(), a);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.primitive.AbstractShortCollection#contains(java.lang.Object)
	 */
	public boolean contains(Object v) {
		return indexOf(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortVector#lastIndexOf(short)
	 */
	public int lastIndexOfShort(short v) {
		if(this instanceof RandomAccess) {
			for(int i = size() - 1; i >= 0; i--) {
				if(getShort(i) == v) {
					return i;
				}
			}
		} else {
			ShortVectorIterator i = shortVectorIterator();

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
	 * @see net.morilib.util.primitive.ShortVector#vectorIterator()
	 */
	public ShortVectorIterator shortVectorIterator() {
		return shortVectorIterator(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortVector#vectorIterator(int)
	 */
	public ShortVectorIterator shortVectorIterator(int index) {
		return new VItr();
	}

	//
	private static class SubV extends AbstractShortVector {

		//
		private ShortVector vector;
		private int bindex, eindex;

		//
		private SubV(ShortVector vector, int b, int e) {
			this.vector = vector;
			this.bindex = b;
			this.eindex = e;
		}

		public void addShort(int index, short v) {
			if(index < 0 || index > size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex++;
			vector.addShort(index + bindex, v);
		}

		public short getShort(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.getShort(index + bindex);
		}

		public short removeAt(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex--;
			return vector.removeAt(index + bindex);
		}

		public short setShort(int index, short v) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.setShort(index + bindex, v);
		}

		public int size() {
			return eindex - bindex;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortVector#subVector(int, int)
	 */
	public ShortVector subVector(int start, int end) {
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
	public boolean addAll(int index, Collection<? extends Short> c) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		int i2 = index;
		modCount++;
		if(c.isEmpty()) {
			return false;
		} else {
			Iterator<? extends Short> i = c.iterator();

			while(i.hasNext()) {
				add(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#get(int)
	 */
	public Short get(int index) {
		return getShort(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	public Short set(int index, Short element) {
		return setShort(index, element.shortValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, Short element) {
		addShort(index, element.shortValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public Short remove(int index) {
		return removeAt(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object o) {
		if(o instanceof Short) {
			return indexOfShort(((Short)o).shortValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	public int lastIndexOf(Object o) {
		if(o instanceof Short) {
			return lastIndexOfShort(((Short)o).shortValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator()
	 */
	public ListIterator<Short> listIterator() {
		final ShortVectorIterator i = shortVectorIterator();

		return new ListIterator<Short>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Short next() {
				return i.next();
			}

			public boolean hasPrevious() {
				return i.hasPrevious();
			}

			public Short previous() {
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

			public void set(Short e) {
				i.setShort(e.shortValue());
			}

			public void add(Short e) {
				i.addShort(e.shortValue());
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator(int)
	 */
	public ListIterator<Short> listIterator(int index) {
		return listIterator(0);
	}

	/* (non-Javadoc)
	 * @see java.util.List#subList(int, int)
	 */
	public List<Short> subList(int fromIndex, int toIndex) {
		final ShortVector v = subVector(fromIndex, toIndex);

		return new AbstractList<Short>() {

			public Short get(int index) {
				return v.get(index);
			}

			public int size() {
				return AbstractShortVector.this.size();
			}

			public Short set(int index, Short element) {
				return v.set(index, element);
			}

			public void add(int index, Short element) {
				v.add(index, element);
			}

			public Short remove(int index) {
				return v.remove(index);
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortList#add(int, int)
	 */
	public void add(int index, int v) {
		if(v < Short.MIN_VALUE || v > Short.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		addShort(index, (short)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortList#set(int, int)
	 */
	public short set(int index, int v) {
		if(v < Short.MIN_VALUE || v > Short.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return setShort(index, (short)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortList#indexOf(int)
	 */
	public int indexOf(int v) {
		if(v < Short.MIN_VALUE || v > Short.MAX_VALUE) {
			return -1;
		}
		return indexOfShort((short)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Hashes.sumHashCode(toShortArray());
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj == this) {
			return true;
		} else if(obj instanceof ShortVector) {
			ShortVector v = (ShortVector)obj;

			if(size() != v.size()) {
				return false;
			}
			for(int i = 0; i < size(); i++) {
				if(getShort(i) != v.getShort(i)) {
					return false;
				}
			}
			return true;
		} else if(obj instanceof ShortList) {
			ShortIterator i = shortIterator();
			ShortIterator j = ((ShortList)obj).shortIterator();

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
