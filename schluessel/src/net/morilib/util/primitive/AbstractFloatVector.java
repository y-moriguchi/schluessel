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
import net.morilib.util.primitive.iterator.FloatIterator;
import net.morilib.util.primitive.iterator.FloatVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractFloatVector extends AbstractFloatCollection
implements FloatVector, RandomAccess {

	//
//	private static final long serialVersionUID = 3332872309405682099L;

	/**
	 * 
	 */
	protected transient int modCount = 0;

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatList#addAll(int, net.morilib.util.primitive.FloatCollection)
	 */
	public boolean addAllFloat(int index, FloatCollection a) {
		int i2 = index;

		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		modCount++;
		if(a.isEmpty()) {
			return false;
		} else {
			FloatIterator i = a.floatIterator();

			while(i.hasNext()) {
				addFloat(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatList#first()
	 */
	public float first() {
		if(isEmpty()) {
			throw new NoSuchElementException();
		}
		return getFloat(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatList#indexOf(float)
	 */
	public int indexOfFloat(float v) {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				if(getFloat(i) == v) {
					return i;
				}
			}
		} else {
			FloatVectorIterator i = floatVectorIterator();

			for(int j = 0; i.hasNext(); j++) {
				if(i.next() == v) {
					return j;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatList#rest()
	 */
	public FloatList rest() {
		return subVector(1, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatList#rest(int)
	 */
	public FloatList rest(int index) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		} else if(index == size()) {
			return FloatCollections.EMPTY_VECTOR;
		}
		return subVector(index, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#add(float)
	 */
	public boolean addFloat(float v) {
		modCount++;
		addFloat(size(), v);
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#addAll(net.morilib.util.primitive.FloatCollection)
	 */
	public boolean addAllFloat(FloatCollection a) {
		return addAllFloat(size(), a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#addAll(net.morilib.util.primitive.FloatCollection[])
	 */
	public boolean addAllFloat(FloatCollection... as) {
		int nsize = 0;
		for(FloatCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(FloatCollection a : as) {
				r = addAllFloat(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#addAll(java.util.Iterator)
	 */
	public boolean addAllFloat(Collection<? extends FloatCollection> as) {
		int nsize = 0;
		for(FloatCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(FloatCollection a : as) {
				r = addAllFloat(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#contains(float)
	 */
	public boolean containsFloat(float v) {
		return indexOfFloat(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#isInfinite()
	 */
	public boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllFloat(float[] a) {
		return addAllFloat(size(), new FloatArrayVector(a));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllFloat(int ptr, float[] a) {
		return addAllFloat(ptr, new FloatArrayVector(a));
	}

	//
	private class Itr implements FloatIterator {

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

		public float next() {
			checks();
			ptrrem = ptr;
			return getFloat(ptr++);
		}

		public void remove() {
			checks2();
			try {
				AbstractFloatVector.this.removeAt(ptrrem);
				exModCount = modCount;
				ptr--;
				ptrrem = -1;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

	};

	//
	private class VItr extends Itr implements FloatVectorIterator {

		public void addFloat(float v) {
			if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				ptrrem = -1;
				AbstractFloatVector.this.addFloat(ptr++, v);
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

		public float previous() {
			float res;

			if(!hasPrevious()) {
				throw new NoSuchElementException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				res = AbstractFloatVector.this.getFloat(ptr);
				ptrrem = ptr = ptr - 1;
				return res;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		public int previousIndex() {
			return ptr - 1;
		}

		public void setFloat(float v) {
			if(!hasNext()) {
				throw new IllegalArgumentException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			} else if(ptrrem < 0) {
				throw new IllegalStateException();
			}

			try {
				AbstractFloatVector.this.setFloat(ptr, v);
				ptrrem = -1;
				exModCount = modCount;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.FloatVectorIterator#add(int)
		 */
		public void add(int v) {
			if(v < Float.MIN_VALUE || v > Float.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			addFloat((float)v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.FloatVectorIterator#set(int)
		 */
		public void set(int v) {
			if(v < Float.MIN_VALUE || v > Float.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			setFloat((float)v);
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#iterator()
	 */
	public FloatIterator floatIterator() {
		return new Itr();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#remove(float)
	 */
	public boolean removeFloat(float v) {
		int ind = indexOfFloat(v);

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
	 * @see net.morilib.util.primitive.AbstractFloatCollection#remove(java.lang.Object)
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
	 * @see net.morilib.util.primitive.AbstractFloatCollection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Float> a) {
		modCount++;
		return addAll(size(), a);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.primitive.AbstractFloatCollection#contains(java.lang.Object)
	 */
	public boolean contains(Object v) {
		return indexOf(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatVector#lastIndexOf(float)
	 */
	public int lastIndexOfFloat(float v) {
		if(this instanceof RandomAccess) {
			for(int i = size() - 1; i >= 0; i--) {
				if(getFloat(i) == v) {
					return i;
				}
			}
		} else {
			FloatVectorIterator i = floatVectorIterator();

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
	 * @see net.morilib.util.primitive.FloatVector#vectorIterator()
	 */
	public FloatVectorIterator floatVectorIterator() {
		return floatVectorIterator(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatVector#vectorIterator(int)
	 */
	public FloatVectorIterator floatVectorIterator(int index) {
		return new VItr();
	}

	//
	private static class SubV extends AbstractFloatVector {

		//
		private FloatVector vector;
		private int bindex, eindex;

		//
		private SubV(FloatVector vector, int b, int e) {
			this.vector = vector;
			this.bindex = b;
			this.eindex = e;
		}

		public void addFloat(int index, float v) {
			if(index < 0 || index > size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex++;
			vector.addFloat(index + bindex, v);
		}

		public float getFloat(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.getFloat(index + bindex);
		}

		public float removeAt(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex--;
			return vector.removeAt(index + bindex);
		}

		public float setFloat(int index, float v) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.setFloat(index + bindex, v);
		}

		public int size() {
			return eindex - bindex;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatVector#subVector(int, int)
	 */
	public FloatVector subVector(int start, int end) {
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
	public boolean addAll(int index, Collection<? extends Float> c) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		int i2 = index;
		modCount++;
		if(c.isEmpty()) {
			return false;
		} else {
			Iterator<? extends Float> i = c.iterator();

			while(i.hasNext()) {
				add(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#get(int)
	 */
	public Float get(int index) {
		return getFloat(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	public Float set(int index, Float element) {
		return setFloat(index, element.floatValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, Float element) {
		addFloat(index, element.floatValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public Float remove(int index) {
		return removeAt(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object o) {
		if(o instanceof Float) {
			return indexOfFloat(((Float)o).floatValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	public int lastIndexOf(Object o) {
		if(o instanceof Float) {
			return lastIndexOfFloat(((Float)o).floatValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator()
	 */
	public ListIterator<Float> listIterator() {
		final FloatVectorIterator i = floatVectorIterator();

		return new ListIterator<Float>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Float next() {
				return i.next();
			}

			public boolean hasPrevious() {
				return i.hasPrevious();
			}

			public Float previous() {
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

			public void set(Float e) {
				i.setFloat(e.floatValue());
			}

			public void add(Float e) {
				i.addFloat(e.floatValue());
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator(int)
	 */
	public ListIterator<Float> listIterator(int index) {
		return listIterator(0);
	}

	/* (non-Javadoc)
	 * @see java.util.List#subList(int, int)
	 */
	public List<Float> subList(int fromIndex, int toIndex) {
		final FloatVector v = subVector(fromIndex, toIndex);

		return new AbstractList<Float>() {

			public Float get(int index) {
				return v.get(index);
			}

			public int size() {
				return AbstractFloatVector.this.size();
			}

			public Float set(int index, Float element) {
				return v.set(index, element);
			}

			public void add(int index, Float element) {
				v.add(index, element);
			}

			public Float remove(int index) {
				return v.remove(index);
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatList#add(int, int)
	 */
	public void add(int index, int v) {
		if(v < Float.MIN_VALUE || v > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		addFloat(index, (float)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatList#set(int, int)
	 */
	public float set(int index, int v) {
		if(v < Float.MIN_VALUE || v > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return setFloat(index, (float)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatList#indexOf(int)
	 */
	public int indexOf(int v) {
		if(v < Float.MIN_VALUE || v > Float.MAX_VALUE) {
			return -1;
		}
		return indexOfFloat((float)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Hashes.sumHashCode(toFloatArray());
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj == this) {
			return true;
		} else if(obj instanceof FloatVector) {
			FloatVector v = (FloatVector)obj;

			if(size() != v.size()) {
				return false;
			}
			for(int i = 0; i < size(); i++) {
				if(getFloat(i) != v.getFloat(i)) {
					return false;
				}
			}
			return true;
		} else if(obj instanceof FloatList) {
			FloatIterator i = floatIterator();
			FloatIterator j = ((FloatList)obj).floatIterator();

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
