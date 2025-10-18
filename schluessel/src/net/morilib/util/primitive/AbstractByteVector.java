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
import net.morilib.util.primitive.iterator.ByteIterator;
import net.morilib.util.primitive.iterator.ByteVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractByteVector extends AbstractByteCollection
implements ByteVector, RandomAccess {

	//
//	private static final long serialVersionUID = 3332872309405682099L;

	/**
	 * 
	 */
	protected transient int modCount = 0;

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#addAll(int, net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllByte(int index, ByteCollection a) {
		int i2 = index;

		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		modCount++;
		if(a.isEmpty()) {
			return false;
		} else {
			ByteIterator i = a.byteIterator();

			while(i.hasNext()) {
				addByte(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#first()
	 */
	public byte first() {
		if(isEmpty()) {
			throw new NoSuchElementException();
		}
		return getByte(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#indexOf(byte)
	 */
	public int indexOfByte(byte v) {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				if(getByte(i) == v) {
					return i;
				}
			}
		} else {
			ByteVectorIterator i = byteVectorIterator();

			for(int j = 0; i.hasNext(); j++) {
				if(i.next() == v) {
					return j;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#rest()
	 */
	public ByteList rest() {
		return subVector(1, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#rest(int)
	 */
	public ByteList rest(int index) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		} else if(index == size()) {
			return ByteCollections.EMPTY_VECTOR;
		}
		return subVector(index, size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#add(byte)
	 */
	public boolean addByte(byte v) {
		modCount++;
		addByte(size(), v);
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllByte(ByteCollection a) {
		return addAllByte(size(), a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection[])
	 */
	public boolean addAllByte(ByteCollection... as) {
		int nsize = 0;
		for(ByteCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(ByteCollection a : as) {
				r = addAllByte(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(java.util.Iterator)
	 */
	public boolean addAllByte(Collection<? extends ByteCollection> as) {
		int nsize = 0;
		for(ByteCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			boolean r = false;

			modCount++;
			for(ByteCollection a : as) {
				r = addAllByte(a) | r;
			}
			return r;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllByte(byte[] a) {
		return addAllByte(size(), new ByteArrayVector(a));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllByte(int ptr, byte[] a) {
		return addAllByte(ptr, new ByteArrayVector(a));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#contains(byte)
	 */
	public boolean containsByte(byte v) {
		return indexOfByte(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#isInfinite()
	 */
	public boolean isInfinite() {
		return false;
	}

	//
	private class Itr implements ByteIterator {

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

		public byte next() {
			checks();
			ptrrem = ptr;
			return getByte(ptr++);
		}

		public void remove() {
			checks2();
			try {
				AbstractByteVector.this.removeAt(ptrrem);
				exModCount = modCount;
				ptr--;
				ptrrem = -1;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

	};

	//
	private class VItr extends Itr implements ByteVectorIterator {

		public void addByte(byte v) {
			if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				ptrrem = -1;
				AbstractByteVector.this.addByte(ptr++, v);
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

		public byte previous() {
			byte res;

			if(!hasPrevious()) {
				throw new NoSuchElementException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}

			try {
				res = AbstractByteVector.this.getByte(ptr);
				ptrrem = ptr = ptr - 1;
				return res;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		public int previousIndex() {
			return ptr - 1;
		}

		public void setByte(byte v) {
			if(!hasNext()) {
				throw new IllegalArgumentException();
			} else if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			} else if(ptrrem < 0) {
				throw new IllegalStateException();
			}

			try {
				AbstractByteVector.this.setByte(ptr, v);
				ptrrem = -1;
				exModCount = modCount;
			} catch(IndexOutOfBoundsException e) {
				throw new ConcurrentModificationException();
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.ByteVectorIterator#add(int)
		 */
		public void add(int v) {
			if(v < Byte.MIN_VALUE || v > Byte.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			addByte((byte)v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.iterator.ByteVectorIterator#set(int)
		 */
		public void set(int v) {
			if(v < Byte.MIN_VALUE || v > Byte.MAX_VALUE) {
				throw new IllegalArgumentException();
			}
			setByte((byte)v);
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#iterator()
	 */
	public ByteIterator byteIterator() {
		return new Itr();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#remove(byte)
	 */
	public boolean removeByte(byte v) {
		int ind = indexOfByte(v);

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
	 * @see net.morilib.util.primitive.AbstractByteCollection#remove(java.lang.Object)
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
	 * @see net.morilib.util.primitive.AbstractByteCollection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Byte> a) {
		modCount++;
		return addAll(size(), a);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.primitive.AbstractByteCollection#contains(java.lang.Object)
	 */
	public boolean contains(Object v) {
		return indexOf(v) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteVector#lastIndexOf(byte)
	 */
	public int lastIndexOfByte(byte v) {
		if(this instanceof RandomAccess) {
			for(int i = size() - 1; i >= 0; i--) {
				if(getByte(i) == v) {
					return i;
				}
			}
		} else {
			ByteVectorIterator i = byteVectorIterator();

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
	 * @see net.morilib.util.primitive.ByteVector#vectorIterator()
	 */
	public ByteVectorIterator byteVectorIterator() {
		return byteVectorIterator(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteVector#vectorIterator(int)
	 */
	public ByteVectorIterator byteVectorIterator(int index) {
		return new VItr();
	}

	//
	private static class SubV extends AbstractByteVector {

		//
		private ByteVector vector;
		private int bindex, eindex;

		//
		private SubV(ByteVector vector, int b, int e) {
			this.vector = vector;
			this.bindex = b;
			this.eindex = e;
		}

		public void addByte(int index, byte v) {
			if(index < 0 || index > size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex++;
			vector.addByte(index + bindex, v);
		}

		public byte getByte(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.getByte(index + bindex);
		}

		public byte removeAt(int index) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			eindex--;
			return vector.removeAt(index + bindex);
		}

		public byte setByte(int index, byte v) {
			if(index < 0 || index >= size()) {
				throw new IndexOutOfBoundsException();
			}
			return vector.setByte(index + bindex, v);
		}

		public int size() {
			return eindex - bindex;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteVector#subVector(int, int)
	 */
	public ByteVector subVector(int start, int end) {
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
	public boolean addAll(int index, Collection<? extends Byte> c) {
		if(index > size() || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		int i2 = index;
		modCount++;
		if(c.isEmpty()) {
			return false;
		} else {
			Iterator<? extends Byte> i = c.iterator();

			while(i.hasNext()) {
				add(i2++, i.next());
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#get(int)
	 */
	public Byte get(int index) {
		return getByte(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	public Byte set(int index, Byte element) {
		return setByte(index, element.byteValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, Byte element) {
		addByte(index, element.byteValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public Byte remove(int index) {
		return removeAt(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object o) {
		if(o instanceof Byte) {
			return indexOfByte(((Byte)o).byteValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	public int lastIndexOf(Object o) {
		if(o instanceof Byte) {
			return lastIndexOfByte(((Byte)o).byteValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator()
	 */
	public ListIterator<Byte> listIterator() {
		final ByteVectorIterator i = byteVectorIterator();

		return new ListIterator<Byte>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Byte next() {
				return i.next();
			}

			public boolean hasPrevious() {
				return i.hasPrevious();
			}

			public Byte previous() {
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

			public void set(Byte e) {
				i.setByte(e.byteValue());
			}

			public void add(Byte e) {
				i.addByte(e.byteValue());
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator(int)
	 */
	public ListIterator<Byte> listIterator(int index) {
		return listIterator(0);
	}

	/* (non-Javadoc)
	 * @see java.util.List#subList(int, int)
	 */
	public List<Byte> subList(int fromIndex, int toIndex) {
		final ByteVector v = subVector(fromIndex, toIndex);

		return new AbstractList<Byte>() {

			public Byte get(int index) {
				return v.get(index);
			}

			public int size() {
				return AbstractByteVector.this.size();
			}

			public Byte set(int index, Byte element) {
				return v.set(index, element);
			}

			public void add(int index, Byte element) {
				v.add(index, element);
			}

			public Byte remove(int index) {
				return v.remove(index);
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#add(int, int)
	 */
	public void add(int index, int v) {
		if(v < Byte.MIN_VALUE || v > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		addByte(index, (byte)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#set(int, int)
	 */
	public byte set(int index, int v) {
		if(v < Byte.MIN_VALUE || v > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return setByte(index, (byte)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#indexOf(int)
	 */
	public int indexOf(int v) {
		if(v < Byte.MIN_VALUE || v > Byte.MAX_VALUE) {
			return -1;
		}
		return indexOfByte((byte)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Hashes.sumHashCode(toByteArray());
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj == this) {
			return true;
		} else if(obj instanceof ByteVector) {
			ByteVector v = (ByteVector)obj;

			if(size() != v.size()) {
				return false;
			}
			for(int i = 0; i < size(); i++) {
				if(getByte(i) != v.getByte(i)) {
					return false;
				}
			}
			return true;
		} else if(obj instanceof ByteList) {
			ByteIterator i = byteIterator();
			ByteIterator j = ((ByteList)obj).byteIterator();

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
