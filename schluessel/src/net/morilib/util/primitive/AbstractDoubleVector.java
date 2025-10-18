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
import net.morilib.util.primitive.iterator.DoubleIterator;
import net.morilib.util.primitive.iterator.DoubleVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractDoubleVector extends AbstractDoubleCollection
implements DoubleVector, RandomAccess {

	//
//	private static final long serialVersionUID = 3332872309405682099L;

	/**
	 * 
	 */
	protected transient int modCount = 0;

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleList#addAll(int, net.morilib.util.primitive.DoubleCollection)
	 */
	public boolean addAllDouble(int index, DoubleCollection a) {
		int i2 = index;

		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		modCount++;
		if(a.isEmpty()) {
			return false;
		} else {
			DoubleIterator i = a.doubleIterator();

			while(i.hasNext()) {
				addDouble(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleList#first()
	 */
	public double first() {
		if(isEmpty()) {
			throw new NoSuchElementException();
		}
		return getDouble(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleList#indexOf(double)
	 */
	public int indexOfDouble(double v) {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				if(getDouble(i) == v) {
					return i;
				}
			}
		} else {
			DoubleVectorIterator i = doubleVectorIterator();

			for(int j = 0; i.hasNext(); j++) {
				if(i.next() == v) {
					return j;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleList#rest()
	 */
	public DoubleList rest() {
		return subVector(1, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleList#rest(int)
	 */
	public DoubleList rest(int index) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		} else if(index == size()) {
			return DoubleCollections.EMPTY_VECTOR;
		}
		return subVector(index, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#add(double)
	 */
	public boolean addDouble(double v) {
		modCount++;
		addDouble(size(), v);
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#addAll(net.morilib.util.primitive.DoubleCollection)
	 */
	public boolean addAllDouble(DoubleCollection a) {
		return addAllDouble(size(), a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#addAll(net.morilib.util.primitive.DoubleCollection[])
	 */
	public boolean addAllDouble(DoubleCollection... as) {
		int nsize = 0;
		for(DoubleCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(DoubleCollection a : as) {
				r = addAllDouble(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#addAll(java.util.Iterator)
	 */
	public boolean addAllDouble(Collection<? extends DoubleCollection> as) {
		int nsize = 0;
		for(DoubleCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(DoubleCollection a : as) {
				r = addAllDouble(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#contains(double)
	 */
	public boolean containsDouble(double v) {
		return indexOfDouble(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#isInfinite()
	 */
	public boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllDouble(double[] a) {
		return addAllDouble(size(), new DoubleArrayVector(a));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllDouble(int ptr, double[] a) {
		return addAllDouble(ptr, new DoubleArrayVector(a));
	}

	//
	private class Itr implements DoubleIterator {

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

		public double next() {
			checks();
			ptrrem = ptr;
			return getDouble(ptr++);
		}

		public void remove() {
			checks2();
			try {
				AbstractDoubleVector.this.removeAt(ptrrem);
				exModCount = modCount;
				ptr--;
				ptrrem = -1;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

	};

	//
	private class VItr extends Itr implements DoubleVectorIterator {

		public void addDouble(double v) {
			if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				ptrrem = -1;
				AbstractDoubleVector.this.addDouble(ptr++, v);
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

		public double previous() {
			double res;

			if(!hasPrevious()) {
				throw new NoSuchElementException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				res = AbstractDoubleVector.this.getDouble(ptr);
				ptrrem = ptr = ptr - 1;
				return res;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		public int previousIndex() {
			return ptr - 1;
		}

		public void setDouble(double v) {
			if(!hasNext()) {
				throw new IllegalArgumentException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			} else if(ptrrem < 0) {
				throw new IllegalStateException();
			}

			try {
				AbstractDoubleVector.this.setDouble(ptr, v);
				ptrrem = -1;
				exModCount = modCount;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.DoubleVectorIterator#add(int)
		 */
		public void add(int v) {
			if(v < Double.MIN_VALUE || v > Double.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			addDouble((double)v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.DoubleVectorIterator#set(int)
		 */
		public void set(int v) {
			if(v < Double.MIN_VALUE || v > Double.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			setDouble((double)v);
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#iterator()
	 */
	public DoubleIterator doubleIterator() {
		return new Itr();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#remove(double)
	 */
	public boolean removeDouble(double v) {
		int ind = indexOfDouble(v);

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
	 * @see net.morilib.util.primitive.AbstractDoubleCollection#remove(java.lang.Object)
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
	 * @see net.morilib.util.primitive.AbstractDoubleCollection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Double> a) {
		modCount++;
		return addAll(size(), a);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.primitive.AbstractDoubleCollection#contains(java.lang.Object)
	 */
	public boolean contains(Object v) {
		return indexOf(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleVector#lastIndexOf(double)
	 */
	public int lastIndexOfDouble(double v) {
		if(this instanceof RandomAccess) {
			for(int i = size() - 1; i >= 0; i--) {
				if(getDouble(i) == v) {
					return i;
				}
			}
		} else {
			DoubleVectorIterator i = doubleVectorIterator();

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
	 * @see net.morilib.util.primitive.DoubleVector#vectorIterator()
	 */
	public DoubleVectorIterator doubleVectorIterator() {
		return doubleVectorIterator(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleVector#vectorIterator(int)
	 */
	public DoubleVectorIterator doubleVectorIterator(int index) {
		return new VItr();
	}

	//
	private static class SubV extends AbstractDoubleVector {

		//
		private DoubleVector vector;
		private int bindex, eindex;

		//
		private SubV(DoubleVector vector, int b, int e) {
			this.vector = vector;
			this.bindex = b;
			this.eindex = e;
		}

		public void addDouble(int index, double v) {
			if(index < 0 || index > size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex++;
			vector.addDouble(index + bindex, v);
		}

		public double getDouble(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.getDouble(index + bindex);
		}

		public double removeAt(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex--;
			return vector.removeAt(index + bindex);
		}

		public double setDouble(int index, double v) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.setDouble(index + bindex, v);
		}

		public int size() {
			return eindex - bindex;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleVector#subVector(int, int)
	 */
	public DoubleVector subVector(int start, int end) {
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
	public boolean addAll(int index, Collection<? extends Double> c) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		int i2 = index;
		modCount++;
		if(c.isEmpty()) {
			return false;
		} else {
			Iterator<? extends Double> i = c.iterator();

			while(i.hasNext()) {
				add(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#get(int)
	 */
	public Double get(int index) {
		return getDouble(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	public Double set(int index, Double element) {
		return setDouble(index, element.doubleValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, Double element) {
		addDouble(index, element.doubleValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public Double remove(int index) {
		return removeAt(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object o) {
		if(o instanceof Double) {
			return indexOfDouble(((Double)o).doubleValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	public int lastIndexOf(Object o) {
		if(o instanceof Double) {
			return lastIndexOfDouble(((Double)o).doubleValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator()
	 */
	public ListIterator<Double> listIterator() {
		final DoubleVectorIterator i = doubleVectorIterator();

		return new ListIterator<Double>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Double next() {
				return i.next();
			}

			public boolean hasPrevious() {
				return i.hasPrevious();
			}

			public Double previous() {
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

			public void set(Double e) {
				i.setDouble(e.doubleValue());
			}

			public void add(Double e) {
				i.addDouble(e.doubleValue());
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator(int)
	 */
	public ListIterator<Double> listIterator(int index) {
		return listIterator(0);
	}

	/* (non-Javadoc)
	 * @see java.util.List#subList(int, int)
	 */
	public List<Double> subList(int fromIndex, int toIndex) {
		final DoubleVector v = subVector(fromIndex, toIndex);

		return new AbstractList<Double>() {

			public Double get(int index) {
				return v.get(index);
			}

			public int size() {
				return AbstractDoubleVector.this.size();
			}

			public Double set(int index, Double element) {
				return v.set(index, element);
			}

			public void add(int index, Double element) {
				v.add(index, element);
			}

			public Double remove(int index) {
				return v.remove(index);
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleList#add(int, int)
	 */
	public void add(int index, int v) {
		if(v < Double.MIN_VALUE || v > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		addDouble(index, (double)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleList#set(int, int)
	 */
	public double set(int index, int v) {
		if(v < Double.MIN_VALUE || v > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return setDouble(index, (double)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleList#indexOf(int)
	 */
	public int indexOf(int v) {
		if(v < Double.MIN_VALUE || v > Double.MAX_VALUE) {
			return -1;
		}
		return indexOfDouble((double)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Hashes.sumHashCode(toDoubleArray());
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj == this) {
			return true;
		} else if(obj instanceof DoubleVector) {
			DoubleVector v = (DoubleVector)obj;

			if(size() != v.size()) {
				return false;
			}
			for(int i = 0; i < size(); i++) {
				if(getDouble(i) != v.getDouble(i)) {
					return false;
				}
			}
			return true;
		} else if(obj instanceof DoubleList) {
			DoubleIterator i = doubleIterator();
			DoubleIterator j = ((DoubleList)obj).doubleIterator();

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
