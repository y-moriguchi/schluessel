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
import net.morilib.util.primitive.iterator.LongIterator;
import net.morilib.util.primitive.iterator.LongVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractLongVector extends AbstractLongCollection
implements LongVector, RandomAccess {

	//
//	private static final long serialVersionUID = 3332872309405682099L;

	/**
	 * 
	 */
	protected transient int modCount = 0;

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongList#addAll(int, net.morilib.util.primitive.LongCollection)
	 */
	public boolean addAllLong(int index, LongCollection a) {
		int i2 = index;

		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		modCount++;
		if(a.isEmpty()) {
			return false;
		} else {
			LongIterator i = a.longIterator();

			while(i.hasNext()) {
				addLong(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongList#first()
	 */
	public long first() {
		if(isEmpty()) {
			throw new NoSuchElementException();
		}
		return getLong(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongList#indexOf(long)
	 */
	public int indexOfLong(long v) {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				if(getLong(i) == v) {
					return i;
				}
			}
		} else {
			LongVectorIterator i = longVectorIterator();

			for(int j = 0; i.hasNext(); j++) {
				if(i.next() == v) {
					return j;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongList#rest()
	 */
	public LongList rest() {
		return subVector(1, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongList#rest(int)
	 */
	public LongList rest(int index) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		} else if(index == size()) {
			return LongCollections.EMPTY_VECTOR;
		}
		return subVector(index, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#add(long)
	 */
	public boolean addLong(long v) {
		modCount++;
		addLong(size(), v);
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#addAll(net.morilib.util.primitive.LongCollection)
	 */
	public boolean addAllLong(LongCollection a) {
		return addAllLong(size(), a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#addAll(net.morilib.util.primitive.LongCollection[])
	 */
	public boolean addAllLong(LongCollection... as) {
		int nsize = 0;
		for(LongCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(LongCollection a : as) {
				r = addAllLong(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#addAll(java.util.Iterator)
	 */
	public boolean addAllLong(Collection<? extends LongCollection> as) {
		int nsize = 0;
		for(LongCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(LongCollection a : as) {
				r = addAllLong(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#contains(long)
	 */
	public boolean containsLong(long v) {
		return indexOfLong(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#isInfinite()
	 */
	public boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllLong(long[] a) {
		return addAllLong(size(), new LongArrayVector(a));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllLong(int ptr, long[] a) {
		return addAllLong(ptr, new LongArrayVector(a));
	}

	//
	private class Itr implements LongIterator {

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

		public long next() {
			checks();
			ptrrem = ptr;
			return getLong(ptr++);
		}

		public void remove() {
			checks2();
			try {
				AbstractLongVector.this.removeAt(ptrrem);
				exModCount = modCount;
				ptr--;
				ptrrem = -1;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

	};

	//
	private class VItr extends Itr implements LongVectorIterator {

		public void addLong(long v) {
			if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				ptrrem = -1;
				AbstractLongVector.this.addLong(ptr++, v);
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

		public long previous() {
			long res;

			if(!hasPrevious()) {
				throw new NoSuchElementException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				res = AbstractLongVector.this.getLong(ptr);
				ptrrem = ptr = ptr - 1;
				return res;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		public int previousIndex() {
			return ptr - 1;
		}

		public void setLong(long v) {
			if(!hasNext()) {
				throw new IllegalArgumentException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			} else if(ptrrem < 0) {
				throw new IllegalStateException();
			}

			try {
				AbstractLongVector.this.setLong(ptr, v);
				ptrrem = -1;
				exModCount = modCount;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.LongVectorIterator#add(int)
		 */
		public void add(int v) {
			if(v < Long.MIN_VALUE || v > Long.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			addLong((long)v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.LongVectorIterator#set(int)
		 */
		public void set(int v) {
			if(v < Long.MIN_VALUE || v > Long.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			setLong((long)v);
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#iterator()
	 */
	public LongIterator longIterator() {
		return new Itr();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#remove(long)
	 */
	public boolean removeLong(long v) {
		int ind = indexOfLong(v);

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
	 * @see net.morilib.util.primitive.AbstractLongCollection#remove(java.lang.Object)
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
	 * @see net.morilib.util.primitive.AbstractLongCollection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Long> a) {
		modCount++;
		return addAll(size(), a);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.primitive.AbstractLongCollection#contains(java.lang.Object)
	 */
	public boolean contains(Object v) {
		return indexOf(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongVector#lastIndexOf(long)
	 */
	public int lastIndexOfLong(long v) {
		if(this instanceof RandomAccess) {
			for(int i = size() - 1; i >= 0; i--) {
				if(getLong(i) == v) {
					return i;
				}
			}
		} else {
			LongVectorIterator i = longVectorIterator();

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
	 * @see net.morilib.util.primitive.LongVector#vectorIterator()
	 */
	public LongVectorIterator longVectorIterator() {
		return longVectorIterator(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongVector#vectorIterator(int)
	 */
	public LongVectorIterator longVectorIterator(int index) {
		return new VItr();
	}

	//
	private static class SubV extends AbstractLongVector {

		//
		private LongVector vector;
		private int bindex, eindex;

		//
		private SubV(LongVector vector, int b, int e) {
			this.vector = vector;
			this.bindex = b;
			this.eindex = e;
		}

		public void addLong(int index, long v) {
			if(index < 0 || index > size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex++;
			vector.addLong(index + bindex, v);
		}

		public long getLong(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.getLong(index + bindex);
		}

		public long removeAt(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex--;
			return vector.removeAt(index + bindex);
		}

		public long setLong(int index, long v) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.setLong(index + bindex, v);
		}

		public int size() {
			return eindex - bindex;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongVector#subVector(int, int)
	 */
	public LongVector subVector(int start, int end) {
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
	public boolean addAll(int index, Collection<? extends Long> c) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		int i2 = index;
		modCount++;
		if(c.isEmpty()) {
			return false;
		} else {
			Iterator<? extends Long> i = c.iterator();

			while(i.hasNext()) {
				add(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#get(int)
	 */
	public Long get(int index) {
		return getLong(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	public Long set(int index, Long element) {
		return setLong(index, element.longValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, Long element) {
		addLong(index, element.longValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public Long remove(int index) {
		return removeAt(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object o) {
		if(o instanceof Long) {
			return indexOfLong(((Long)o).longValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	public int lastIndexOf(Object o) {
		if(o instanceof Long) {
			return lastIndexOfLong(((Long)o).longValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator()
	 */
	public ListIterator<Long> listIterator() {
		final LongVectorIterator i = longVectorIterator();

		return new ListIterator<Long>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Long next() {
				return i.next();
			}

			public boolean hasPrevious() {
				return i.hasPrevious();
			}

			public Long previous() {
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

			public void set(Long e) {
				i.setLong(e.longValue());
			}

			public void add(Long e) {
				i.addLong(e.longValue());
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator(int)
	 */
	public ListIterator<Long> listIterator(int index) {
		return listIterator(0);
	}

	/* (non-Javadoc)
	 * @see java.util.List#subList(int, int)
	 */
	public List<Long> subList(int fromIndex, int toIndex) {
		final LongVector v = subVector(fromIndex, toIndex);

		return new AbstractList<Long>() {

			public Long get(int index) {
				return v.get(index);
			}

			public int size() {
				return AbstractLongVector.this.size();
			}

			public Long set(int index, Long element) {
				return v.set(index, element);
			}

			public void add(int index, Long element) {
				v.add(index, element);
			}

			public Long remove(int index) {
				return v.remove(index);
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongList#add(int, int)
	 */
	public void add(int index, int v) {
		if(v < Long.MIN_VALUE || v > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		addLong(index, (long)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongList#set(int, int)
	 */
	public long set(int index, int v) {
		if(v < Long.MIN_VALUE || v > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return setLong(index, (long)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongList#indexOf(int)
	 */
	public int indexOf(int v) {
		if(v < Long.MIN_VALUE || v > Long.MAX_VALUE) {
			return -1;
		}
		return indexOfLong((long)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Hashes.sumHashCode(toLongArray());
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj == this) {
			return true;
		} else if(obj instanceof LongVector) {
			LongVector v = (LongVector)obj;

			if(size() != v.size()) {
				return false;
			}
			for(int i = 0; i < size(); i++) {
				if(getLong(i) != v.getLong(i)) {
					return false;
				}
			}
			return true;
		} else if(obj instanceof LongList) {
			LongIterator i = longIterator();
			LongIterator j = ((LongList)obj).longIterator();

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
