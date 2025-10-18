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
import java.util.Collection;
import java.util.RandomAccess;

import net.morilib.lang.Hashes;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public class IntegerArrayVector extends AbstractIntegerVector
implements RandomAccess, java.io.Serializable {

	//
	private static final long serialVersionUID = 3332872309405682099L;

	//
	private int[] array;
	private transient int size;

	public IntegerArrayVector(int initialCapacity) {
		super();
		if(initialCapacity < 0) {
			throw new IllegalArgumentException();
		}
		array = new int[initialCapacity];
	}

	public IntegerArrayVector() {
		this(10);
	}

	public IntegerArrayVector(int[] bytes) {
		array = new int[bytes.length];
		size  = array.length;
		System.arraycopy(bytes, 0, array, 0, bytes.length);
	}

	public IntegerArrayVector(IntegerCollection a) {
		array = a.toIntArray();
		size  = array.length;
	}

	public IntegerArrayVector(IntegerCollection... as) {
		size = 0;
		for(IntegerCollection a : as) {
			size  += a.size();
		}

		int s2 = 0;
		array = new int[size];
		for(IntegerCollection a : as) {
			int[] b = a.toIntArray();

			System.arraycopy(b, 0, array, s2, b.length);
			s2 += a.size();
		}
	}

	public IntegerArrayVector(
			Collection<? extends IntegerCollection> as) {
		size = 0;
		for(IntegerCollection a : as) {
			size  += a.size();
		}

		int s2 = 0;
		array = new int[size];
		for(IntegerCollection a : as) {
			int[] b = a.toIntArray();

			System.arraycopy(b, 0, array, s2, b.length);
			s2 += a.size();
		}
	}

	/**
	 * 
	 * @param nsize
	 */
	public void ensureCapacity(int nsize) {
		if(nsize > array.length) {
			int ns = nsize;
			int[] b;

			while(ns < array.length) {
				ns = (array.length < 1288490186) ?
						(array.length / 3 * 5) + 1 : Integer.MAX_VALUE;
			}
			b = new int[ns];
			System.arraycopy(array, 0, b, 0, size);
			array = b;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#add(int, int)
	 */
	public void addInt(int index, int v) {
		if(index > size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		ensureCapacity(size + 1);
		modCount++;
		if(index < size) {
			System.arraycopy(
					array, index, array, index + 1, size - index);
		}
		array[index] = v;
		size++;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#addAll(int, net.morilib.util.primitive.IntegerCollection)
	 */
	public boolean addAllInt(int index, IntegerCollection a) {
		if(index > size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		modCount++;
		if(a.isEmpty()) {
			return false;
		} else {
			int[] b = a.toIntArray();

			ensureCapacity(size + b.length);
			System.arraycopy(
					array, index, array, index + b.length,
					size - index);
			System.arraycopy(b, 0, array, index, b.length);
			size += b.length;
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#get(int)
	 */
	public int getInt(int index) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		return array[index];
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#remove(int)
	 */
	public int removeAt(int index) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		int res = array[index];
		modCount++;
		if(index < size - 1) {
			System.arraycopy(
					array, index + 1, array, index,
					size - index - 1);
		}
		size--;
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerList#set(int, int)
	 */
	public int setInt(int index, int v) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		int res = array[index];
		modCount++;
		array[index] = v;
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#addAll(net.morilib.util.primitive.IntegerCollection)
	 */
	public boolean addAllInt(IntegerCollection a) {
		int[] b = a.toIntArray();

		modCount++;
		ensureCapacity(size + b.length);
		System.arraycopy(b, 0, array, size, b.length);
		size += b.length;
		return true;
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
			int s2 = size;

			modCount++;
			ensureCapacity(size + nsize);
			for(IntegerCollection a : as) {
				int[] b = a.toIntArray();

				System.arraycopy(b, 0, array, s2, b.length);
				s2 += a.size();
			}
			size += nsize;
			return true;
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
			int s2 = size;

			modCount++;
			ensureCapacity(size + nsize);
			for(IntegerCollection a : as) {
				int[] b = a.toIntArray();

				System.arraycopy(b, 0, array, s2, b.length);
				s2 += a.size();
			}
			size += nsize;
			return true;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllInt(int[] b) {
		modCount++;
		ensureCapacity(size + b.length);
		System.arraycopy(b, 0, array, size, b.length);
		size += b.length;
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#addAll(int, net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllInt(int index, int[] b) {
		if(index > size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		modCount++;
		if(b.length == 0) {
			return false;
		} else {
			ensureCapacity(size + b.length);
			System.arraycopy(
					array, index, array, index + b.length,
					size - index);
			System.arraycopy(b, 0, array, index, b.length);
			size += b.length;
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#clear()
	 */
	public void clear() {
		modCount++;
		size = 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#size()
	 */
	public int size() {
		return size;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#toArray()
	 */
	public int[] toIntArray() {
		int[] b = new int[size];

		System.arraycopy(array, 0, b, 0, size);
		return b;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#toArray(int[])
	 */
	public int[] toIntArray(int[] a) {
		if(a.length < size) {
			return toIntArray();
		}
		System.arraycopy(array, 0, a, 0, size);
		return a;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Hashes.sumHashCode(array);
	}

	//
	private void writeObject(
			java.io.ObjectOutputStream s) throws IOException {
		s.defaultWriteObject();
	}

	//
	private void readObject(
			java.io.ObjectInputStream s
			) throws IOException, ClassNotFoundException {
		s.defaultReadObject();
		size = array.length;
	}

}
