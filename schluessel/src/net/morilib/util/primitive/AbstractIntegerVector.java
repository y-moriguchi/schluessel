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
import net.morilib.util.primitive.iterator.IntegerIterator;
import net.morilib.util.primitive.iterator.IntegerVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractIntegerVector extends AbstractIntegerCollection
implements IntegerVector, RandomAccess {

	//
//	private static final long serialVersionUID = 3332872309405682099L;

	/**
	 * 
	 */
	protected transient int modCount = 0;

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#addAll(int, net.morilib.util.primitive.IntegerCollection)
	 */
	public boolean addAllInt(int index, IntegerCollection a) {
		int i2 = index;

		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		modCount++;
		if(a.isEmpty()) {
			return false;
		} else {
			IntegerIterator i = a.intIterator();

			while(i.hasNext()) {
				addInt(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#first()
	 */
	public int first() {
		if(isEmpty()) {
			throw new NoSuchElementException();
		}
		return getInt(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#indexOf(int)
	 */
	public int indexOfInt(int v) {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				if(getInt(i) == v) {
					return i;
				}
			}
		} else {
			IntegerVectorIterator i = intVectorIterator();

			for(int j = 0; i.hasNext(); j++) {
				if(i.next() == v) {
					return j;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#rest()
	 */
	public IntegerList rest() {
		return subVector(1, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#rest(int)
	 */
	public IntegerList rest(int index) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		} else if(index == size()) {
			return IntegerCollections.EMPTY_VECTOR;
		}
		return subVector(index, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#add(int)
	 */
	public boolean addInt(int v) {
		modCount++;
		addInt(size(), v);
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#addAll(net.morilib.util.primitive.IntegerCollection)
	 */
	public boolean addAllInt(IntegerCollection a) {
		return addAllInt(size(), a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#addAll(net.morilib.util.primitive.IntegerCollection[])
	 */
	public boolean addAllInt(IntegerCollection... as) {
		int nsize = 0;
		for(IntegerCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(IntegerCollection a : as) {
				r = addAllInt(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#addAll(java.util.Iterator)
	 */
	public boolean addAllInt(Collection<? extends IntegerCollection> as) {
		int nsize = 0;
		for(IntegerCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(IntegerCollection a : as) {
				r = addAllInt(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#contains(int)
	 */
	public boolean containsInt(int v) {
		return indexOfInt(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#isInfinite()
	 */
	public boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllInt(int[] a) {
		return addAllInt(size(), new IntegerArrayVector(a));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllInt(int ptr, int[] a) {
		return addAllInt(ptr, new IntegerArrayVector(a));
	}

	//
	private class Itr implements IntegerIterator {

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

		public int next() {
			checks();
			ptrrem = ptr;
			return getInt(ptr++);
		}

		public void remove() {
			checks2();
			try {
				AbstractIntegerVector.this.removeAt(ptrrem);
				exModCount = modCount;
				ptr--;
				ptrrem = -1;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

	};

	//
	private class VItr extends Itr implements IntegerVectorIterator {

		public void addInt(int v) {
			if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				ptrrem = -1;
				AbstractIntegerVector.this.addInt(ptr++, v);
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

		public int previous() {
			int res;

			if(!hasPrevious()) {
				throw new NoSuchElementException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				res = AbstractIntegerVector.this.getInt(ptr);
				ptrrem = ptr = ptr - 1;
				return res;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		public int previousIndex() {
			return ptr - 1;
		}

		public void setInt(int v) {
			if(!hasNext()) {
				throw new IllegalArgumentException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			} else if(ptrrem < 0) {
				throw new IllegalStateException();
			}

			try {
				AbstractIntegerVector.this.setInt(ptr, v);
				ptrrem = -1;
				exModCount = modCount;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.IntegerVectorIterator#add(int)
		 */
		public void add(int v) {
			if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			addInt((int)v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.IntegerVectorIterator#set(int)
		 */
		public void set(int v) {
			if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			setInt((int)v);
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#iterator()
	 */
	public IntegerIterator intIterator() {
		return new Itr();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#remove(int)
	 */
	public boolean removeInt(int v) {
		int ind = indexOfInt(v);

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
	 * @see net.morilib.util.primitive.AbstractIntegerCollection#remove(java.lang.Object)
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
	 * @see net.morilib.util.primitive.AbstractIntegerCollection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Integer> a) {
		modCount++;
		return addAll(size(), a);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.primitive.AbstractIntegerCollection#contains(java.lang.Object)
	 */
	public boolean contains(Object v) {
		return indexOf(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerVector#lastIndexOf(int)
	 */
	public int lastIndexOfInt(int v) {
		if(this instanceof RandomAccess) {
			for(int i = size() - 1; i >= 0; i--) {
				if(getInt(i) == v) {
					return i;
				}
			}
		} else {
			IntegerVectorIterator i = intVectorIterator();

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
	 * @see net.morilib.util.primitive.IntegerVector#vectorIterator()
	 */
	public IntegerVectorIterator intVectorIterator() {
		return intVectorIterator(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerVector#vectorIterator(int)
	 */
	public IntegerVectorIterator intVectorIterator(int index) {
		return new VItr();
	}

	//
	private static class SubV extends AbstractIntegerVector {

		//
		private IntegerVector vector;
		private int bindex, eindex;

		//
		private SubV(IntegerVector vector, int b, int e) {
			this.vector = vector;
			this.bindex = b;
			this.eindex = e;
		}

		public void addInt(int index, int v) {
			if(index < 0 || index > size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex++;
			vector.addInt(index + bindex, v);
		}

		public int getInt(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.getInt(index + bindex);
		}

		public int removeAt(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex--;
			return vector.removeAt(index + bindex);
		}

		public int setInt(int index, int v) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.setInt(index + bindex, v);
		}

		public int size() {
			return eindex - bindex;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerVector#subVector(int, int)
	 */
	public IntegerVector subVector(int start, int end) {
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
	public boolean addAll(int index, Collection<? extends Integer> c) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		int i2 = index;
		modCount++;
		if(c.isEmpty()) {
			return false;
		} else {
			Iterator<? extends Integer> i = c.iterator();

			while(i.hasNext()) {
				add(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#get(int)
	 */
	public Integer get(int index) {
		return getInt(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	public Integer set(int index, Integer element) {
		return setInt(index, element.intValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, Integer element) {
		addInt(index, element.intValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public Integer remove(int index) {
		return removeAt(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object o) {
		if(o instanceof Integer) {
			return indexOfInt(((Integer)o).intValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	public int lastIndexOf(Object o) {
		if(o instanceof Integer) {
			return lastIndexOfInt(((Integer)o).intValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator()
	 */
	public ListIterator<Integer> listIterator() {
		final IntegerVectorIterator i = intVectorIterator();

		return new ListIterator<Integer>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Integer next() {
				return i.next();
			}

			public boolean hasPrevious() {
				return i.hasPrevious();
			}

			public Integer previous() {
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

			public void set(Integer e) {
				i.setInt(e.intValue());
			}

			public void add(Integer e) {
				i.addInt(e.intValue());
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator(int)
	 */
	public ListIterator<Integer> listIterator(int index) {
		return listIterator(0);
	}

	/* (non-Javadoc)
	 * @see java.util.List#subList(int, int)
	 */
	public List<Integer> subList(int fromIndex, int toIndex) {
		final IntegerVector v = subVector(fromIndex, toIndex);

		return new AbstractList<Integer>() {

			public Integer get(int index) {
				return v.get(index);
			}

			public int size() {
				return AbstractIntegerVector.this.size();
			}

			public Integer set(int index, Integer element) {
				return v.set(index, element);
			}

			public void add(int index, Integer element) {
				v.add(index, element);
			}

			public Integer remove(int index) {
				return v.remove(index);
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#add(int, int)
	 */
	public void add(int index, int v) {
		if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		addInt(index, (int)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#set(int, int)
	 */
	public int set(int index, int v) {
		if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return setInt(index, (int)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#indexOf(int)
	 */
	public int indexOf(int v) {
		if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
			return -1;
		}
		return indexOfInt((int)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Hashes.sumHashCode(toIntArray());
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj == this) {
			return true;
		} else if(obj instanceof IntegerVector) {
			IntegerVector v = (IntegerVector)obj;

			if(size() != v.size()) {
				return false;
			}
			for(int i = 0; i < size(); i++) {
				if(getInt(i) != v.getInt(i)) {
					return false;
				}
			}
			return true;
		} else if(obj instanceof IntegerList) {
			IntegerIterator i = intIterator();
			IntegerIterator j = ((IntegerList)obj).intIterator();

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
