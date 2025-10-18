/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.util.uvector;

import java.util.Arrays;

import net.morilib.lang.Hashes;
import net.morilib.util.Arrays2;
import net.morilib.util.Bytes;

/**
 * An array of long values, or a sequence of long values.
 * <p>IntArray can have signed or unsigned long.
 * Range of signed long is from -2^63 to 2^63 - 1,
 * and range of unsigned long is from 0 to 2^64 - 1.
 * 
 * <p>longの配列、またはlongの並びです。
 * <p>配列の値として符号付き、または符号なしのlongを
 * 格納することができます。
 * 符号付きlongは-2^63〜2^63 - 1まで、
 * 符号なしlongは0〜2^64 - 1までの値を
 * 格納することができます。
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/11
 */
public class LongArray extends AbstractUniformArray
implements java.io.Serializable {

	private long[] array;
	private TypeAttr signed;


	private LongArray(long[] arr, TypeAttr attr) {
		if(!attr.isExact()) {
			throw new IllegalArgumentException(attr.toString());
		} else if(arr == null) {
			throw new NullPointerException();
		}

		array = new long[arr.length];
		signed = attr;
		System.arraycopy(arr, 0, array, 0, arr.length);
	}

	/**
	 * Creates a copy of the given array.
	 * <p>与えられた配列のコピーを作成します。
	 * 
	 * @param a an array to be copied
	 */
	public LongArray(LongArray a) {
		array = Arrays2.copy(a.array);
		signed = a.signed;
	}

	/**
	 * Constructs an array of signed long
	 * from the given array of long[].
	 * <p>long[]型の配列から符号付きのLongArrayを生成します。
	 * 
	 * @param arr an array to be copied
	 * @return a signed LongArray
	 */
	public static LongArray newArray(long[] arr) {
		return new LongArray(arr, TypeAttr.SIGNED_INT);
	}

	/**
	 * Constructs an array of unsigned long
	 * from the given array of long[].
	 * <p>long[]型の配列から符号なしのLongArrayを生成します。
	 * 
	 * @param arr an array to be copied
	 * @return a unsigned LongArray
	 */
	public static LongArray newuArray(long[] arr) {
		return new LongArray(arr, TypeAttr.UNSIGNED_INT);
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public static LongArray malloc(int size) {
		return new LongArray(new long[size], TypeAttr.SIGNED_INT);
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public static LongArray mallocu(int size) {
		return new LongArray(new long[size], TypeAttr.UNSIGNED_INT);
	}

	/**
	 * Gets the i'th value of this array as a signed byte.
	 * <p>i番目の値を符号付きのbyteとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#getByte(int)
	 */
	public byte getByte(int i) {
		short r = getShort(i);

		return (byte)r;
	}

	/**
	 * Gets the i'th value of this array as a double value.
	 * <p>i番目の値をdoubleとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#getDouble(int)
	 */
	public double getDouble(int i) {
		if(signed.isSigned()) {
			return getLong(i);
		} else {
			return Bytes.ulongToDouble(getLong(i));
		}
	}

	/**
	 * Gets the i'th value of this array as a float value.
	 * <p>i番目の値をfloatとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#getFloat(int)
	 */
	public float getFloat(int i) {
		if(signed.isSigned()) {
			return getLong(i);
		} else {
			return Bytes.ulongToFloat(getLong(i));
		}
	}

	/**
	 * Gets the i'th value of this array as a signed int.
	 * <p>i番目の値を符号付きのintとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#getInt(int)
	 */
	public int getInt(int i) {
		return (int)getLong(i);
	}

	/**
	 * Gets the i'th value of this array as a signed long.
	 * <p>i番目の値を符号付きのlongとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#getLong(int)
	 */
	public long getLong(int i) {
		if(i < 0 || i >= array.length) {
			throw new IndexOutOfBoundsException(i + "");
		}
		return array[i];
	}

	/**
	 * Gets the i'th value of this array as a signed short.
	 * <p>i番目の値を符号付きのshortとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#getShort(int)
	 */
	public short getShort(int i) {
		return (short)getLong(i);
	}

	/**
	 * Sets the given signed byte to the i'th index.
	 * <p>符号付きのbyteをi番目の値としてセットします。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setByte(int, byte)
	 */
	public void setByte(int i, byte x) {
		setLong(i, signed.isSigned() ? x : Bytes.ubyteToLong(x));
	}

	/**
	 * Sets the given double value to the i'th index,
	 * the given value is converted to the long value.
	 * <p>符号付きのdoubleをi番目の値としてセットします。
	 * セットする値は符号も含めlong型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setDouble(int, double)
	 */
	public void setDouble(int i, double x) {
		setLong(i,
				signed.isSigned() ? (long)x : Bytes.doubleTouLong(x));
	}

	/**
	 * Sets the given float value to the i'th index,
	 * the given value is converted to the long value.
	 * <p>符号付きのfloatをi番目の値としてセットします。
	 * セットする値は符号も含めlong型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setFloat(int, float)
	 */
	public void setFloat(int i, float x) {
		setDouble(i, x);
	}

	/**
	 * Sets the given signed int to the i'th index,
	 * the given value is converted to the long value.
	 * <p>符号付きのintをi番目の値としてセットします。
	 * セットする値は符号も含めlong型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setInt(int, int)
	 */
	public void setInt(int i, int x) {
		setLong(i, signed.isSigned() ? x : Bytes.uintToLong(x));
	}

	/**
	 * Sets the given signed long to the i'th index.
	 * <p>符号付きのlongをi番目の値としてセットします。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setLong(int, long)
	 */
	public void setLong(int i, long x) {
		if(i < 0 || i >= array.length) {
			throw new IndexOutOfBoundsException(i + "");
		}
		array[i] = x;
	}

	/**
	 * Sets the given signed short to the i'th index,
	 * the given value is converted to the long value.
	 * <p>符号付きのshortをi番目の値としてセットします。
	 * セットする値は符号も含めlong型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setShort(int, short)
	 */
	public void setShort(int i, short x) {
		setLong(i, signed.isSigned() ? x : Bytes.ushortToLong(x));
	}

	/**
	 * Returns the size of this array.
	 * <p>この配列のサイズを返します。
	 * 
	 * @return the size of this array
	 * @see net.morilib.util.uvector.UniformArray#size()
	 */
	public int size() {
		return array.length;
	}

	/**
	 * Translates this array to a byte[] array on given endianness.
	 * <p>この配列を与えられたエンディアンでbyte[]に変換します。
	 * 
	 * @see net.morilib.util.uvector.UniformArray#toByteArray(net.morilib.util.uvector.Endianness)
	 */
	public byte[] toByteArray(Endianness e) {
		byte[] res = new byte[array.length << 3];

		for(int i = 0; i < array.length; i++) {
			e.writeLong(res, i << 3, array[i]);
		}
		return res;
	}

	/**
	 * translates this array to a double[] array.
	 * <p>この配列をdouble[]配列として取得します。
	 */
	public long[] toArray() {
		return Arrays2.copy(array);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return Hashes.sumHashCode(array);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LongArray) {
			return Arrays.equals(array, ((LongArray)obj).array);
		}
		return false;
	}

}
