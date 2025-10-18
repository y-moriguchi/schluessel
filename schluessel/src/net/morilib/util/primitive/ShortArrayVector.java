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
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public class ShortArrayVector extends AbstractShortVector
implements RandomAccess, java.io.Serializable {

	//
	private static final long serialVersionUID = 3332872309405682099L;

	//
	private short[] array;
	private transient int size;


	public ShortArrayVector(int initialCapacity) {
		super();
		if(initialCapacity < 0) {
			throw new IllegalArgumentException();
		}
		array = new short[initialCapacity];
	}


	public ShortArrayVector() {
		this(10);
	}


	public ShortArrayVector(short[] bytes) {
		array = new short[bytes.length];
		size  = array.length;
		System.arraycopy(bytes, 0, array, 0, bytes.length);
	}


	public ShortArrayVector(ShortCollection a) {
		array = a.toShortArray();
		size  = array.length;
	}


	public ShortArrayVector(ShortCollection... as) {
		size = 0;
		for(ShortCollection a : as) {
			size  += a.size();
		}

		int s2 = 0;
		array = new short[size];
		for(ShortCollection a : as) {
			short[] b = a.toShortArray();

			System.arraycopy(b, 0, array, s2, b.length);
			s2 += a.size();
		}
	}


	public ShortArrayVector(
			Collection<? extends ShortCollection> as) {
		size = 0;
		for(ShortCollection a : as) {
			size  += a.size();
		}

		int s2 = 0;
		array = new short[size];
		for(ShortCollection a : as) {
			short[] b = a.toShortArray();

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
			short[] b;

			while(ns < array.length) {
				ns = (array.length < 1288490186) ?
						(array.length / 3 * 5) + 1 : Integer.MAX_VALUE;
			}
			b = new short[ns];
			System.arraycopy(array, 0, b, 0, size);
			array = b;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortList#add(int, short)
	 */
	public void addShort(int index, short v) {
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
	 * @see net.morilib.util.primitive.ShortList#addAll(int, net.morilib.util.primitive.ShortCollection)
	 */
	public boolean addAllShort(int index, ShortCollection a) {
		if(index > size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		modCount++;
		if(a.isEmpty()) {
			return false;
		} else {
			short[] b = a.toShortArray();

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
	 * @see net.morilib.util.primitive.ShortList#get(int)
	 */
	public short getShort(int index) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}
		return array[index];
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortList#remove(int)
	 */
	public short removeAt(int index) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		short res = array[index];
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
	 * @see net.morilib.util.primitive.ShortList#set(int, short)
	 */
	public short setShort(int index, short v) {
		if(index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}

		short res = array[index];
		modCount++;
		array[index] = v;
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#addAll(net.morilib.util.primitive.ShortCollection)
	 */
	public boolean addAllShort(ShortCollection a) {
		short[] b = a.toShortArray();

		modCount++;
		ensureCapacity(size + b.length);
		System.arraycopy(b, 0, array, size, b.length);
		size += b.length;
		return true;
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
			int s2 = size;

			modCount++;
			ensureCapacity(size + nsize);
			for(ShortCollection a : as) {
				short[] b = a.toShortArray();

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
	 * @see net.morilib.util.primitive.ShortCollection#addAll(java.util.Iterator)
	 */
	public boolean addAllShort(Collection<? extends ShortCollection> as) {
		int nsize = 0;
		for(ShortCollection a : as) {
			nsize  += a.size();
		}

		if(nsize > 0) {
			int s2 = size;

			modCount++;
			ensureCapacity(size + nsize);
			for(ShortCollection a : as) {
				short[] b = a.toShortArray();

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
	public boolean addAllShort(short[] b) {
		modCount++;
		ensureCapacity(size + b.length);
		System.arraycopy(b, 0, array, size, b.length);
		size += b.length;
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteList#addAll(int, net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllShort(int index, short[] b) {
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
	 * @see net.morilib.util.primitive.ShortCollection#clear()
	 */
	public void clear() {
		modCount++;
		size = 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#size()
	 */
	public int size() {
		return size;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#toArray()
	 */
	public short[] toShortArray() {
		short[] b = new short[size];

		System.arraycopy(array, 0, b, 0, size);
		return b;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#toArray(short[])
	 */
	public short[] toShortArray(short[] a) {
		if(a.length < size) {
			return toShortArray();
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
