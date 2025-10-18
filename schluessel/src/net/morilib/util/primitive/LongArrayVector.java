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
public class LongArrayVector extends AbstractLongVector
implements RandomAccess, java.io.Serializable {

	//
	private static final long serialVersionUID = 3332872309405682099L;

	//
	private long[] array;
	private transient int size;

	public LongArrayVector(int initialCapacity) {
		super();
		if(initialCapacity < 0) {
			throw new IllegalArgumentException();
		}
		array = new long[initialCapacity];
	}

	public LongArrayVector() {
		this(10);
	}

	public LongArrayVector(long[] bytes) {
		array = new long[bytes.length];
		size  = array.length;
		System.arraycopy(bytes, 0, array, 0, bytes.length);
	}

	public LongArrayVector(LongCollection a) {
		array = a.toLongArray();
		size  = array.length;
	}

	public LongArrayVector(LongCollection... as) {
		size = 0;
		for(LongCollection a : as) {
			size  += a.size();
		}

		int s2 = 0;
		array = new long[size];
		for(LongCollection a : as) {
			long[] b = a.toLongArray();

			System.arraycopy(b, 0, array, s2, b.length);
			s2 += a.size();
		}
	}

	public LongArrayVector(
			Collection<? extends LongCollection> as) {
		size = 0;
		for(LongCollection a : as) {
			size  += a.size();
		}

		int s2 = 0;
		array = new long[size];
		for(LongCollection a : as) {
			long[] b = a.toLongArray();

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
			long[] b;

			while(ns < array.length) {
				ns = (array.length < 1288490186) ?
						(array.length / 3 * 5) + 1 : Integer.MAX_VALUE;
			}
			b = new long[ns];
			System.arraycopy(array, 0, b, 0, size);
			array = b;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongList#add(int, long)
	 */
	public void addLong(int index, long v) {
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
	 * @see net.morilib.util.primitive.LongList#addAll(int, net.morilib.util.primitive.LongCollection)
	 */
	public boolean addAllLong(int index, LongCollection a) {
		if(index > size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		modCount++;
		if(a.isEmpty()) {
			return false;
		} else {
			long[] b = a.toLongArray();

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
	 * @see net.morilib.util.primitive.LongList#get(int)
	 */
	public long getLong(int index) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		return array[index];
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongList#remove(int)
	 */
	public long removeAt(int index) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		long res = array[index];
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
	 * @see net.morilib.util.primitive.LongList#set(int, long)
	 */
	public long setLong(int index, long v) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		long res = array[index];
		modCount++;
		array[index] = v;
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#addAll(net.morilib.util.primitive.LongCollection)
	 */
	public boolean addAllLong(LongCollection a) {
		long[] b = a.toLongArray();

		modCount++;
		ensureCapacity(size + b.length);
		System.arraycopy(b, 0, array, size, b.length);
		size += b.length;
		return true;
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
			int s2 = size;

			modCount++;
			ensureCapacity(size + nsize);
			for(LongCollection a : as) {
				long[] b = a.toLongArray();

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
	 * @see net.morilib.util.primitive.LongCollection#addAll(java.util.Iterator)
	 */
	public boolean addAllLong(Collection<? extends LongCollection> as) {
		int nsize = 0;
		for(LongCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			int s2 = size;

			modCount++;
			ensureCapacity(size + nsize);
			for(LongCollection a : as) {
				long[] b = a.toLongArray();

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
	public boolean addAllLong(long[] b) {
		modCount++;
		ensureCapacity(size + b.length);
		System.arraycopy(b, 0, array, size, b.length);
		size += b.length;
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#addAll(int, net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllLong(int index, long[] b) {
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
	 * @see net.morilib.util.primitive.LongCollection#clear()
	 */
	public void clear() {
		modCount++;
		size = 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#size()
	 */
	public int size() {
		return size;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#toArray()
	 */
	public long[] toLongArray() {
		long[] b = new long[size];

		System.arraycopy(array, 0, b, 0, size);
		return b;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#toArray(long[])
	 */
	public long[] toLongArray(long[] a) {
		if(a.length < size) {
			return toLongArray();
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
