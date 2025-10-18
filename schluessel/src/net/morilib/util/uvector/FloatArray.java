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

/**
 * An array of float values.
 * 
 * <p>floatの配列です。
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/11
 */
public class FloatArray extends AbstractUniformArray
implements java.io.Serializable {

	//
	private float[] array;

	//
	private FloatArray(float[] arr) {
		if(arr == null) {
			throw new NullPointerException();
		}

		array = new float[arr.length];
		System.arraycopy(arr, 0, array, 0, arr.length);
	}

	/**
	 * Creates a copy of the given array.
	 * <p>与えられた配列のコピーを作成します。
	 * 
	 * @param a an array to be copied
	 */
	public FloatArray(FloatArray a) {
		array = Arrays2.copy(a.array);
	}

	/**
	 * Constructs an array of float value
	 * from the given array of float[].
	 * <p>float[]型の配列から符号付きのFloatArrayを生成します。
	 * 
	 * @param arr an array to be copied
	 * @return a FloatArray
	 */
	public static FloatArray newArray(float[] arr) {
		return new FloatArray(arr);
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public static FloatArray malloc(int size) {
		return new FloatArray(new float[size]);
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
		return (byte)getFloat(i);
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
		return getFloat(i);
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
		if(i < 0 || i >= array.length) {
			throw new IndexOutOfBoundsException(i + "");
		}
		return array[i];
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
		return (int)getFloat(i);
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
		return (long)getFloat(i);
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
		return (short)getFloat(i);
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
		setFloat(i, x);
	}

	/**
	 * Sets the given double value to the i'th index,
	 * the given value is converted to the float value.
	 * <p>符号付きのfloatをi番目の値としてセットします。
	 * セットする値は符号も含めfloat型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setDouble(int, double)
	 */
	public void setDouble(int i, double x) {
		setFloat(i, (float)x);
	}

	/**
	 * Sets the given float value to the i'th index.
	 * <p>符号付きのfloatをi番目の値としてセットします。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setFloat(int, float)
	 */
	public void setFloat(int i, float x) {
		if(i < 0 || i >= array.length) {
			throw new IndexOutOfBoundsException(i + "");
		}
		array[i] = x;
	}

	/**
	 * Sets the given signed int to the i'th index,
	 * the given value is converted to the float value.
	 * <p>符号付きのintをi番目の値としてセットします。
	 * セットする値は符号も含めfloat型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setInt(int, int)
	 */
	public void setInt(int i, int x) {
		setFloat(i, x);
	}

	/**
	 * Sets the given signed long to the i'th index,
	 * the given value is converted to the float value.
	 * <p>符号付きのlongをi番目の値としてセットします。
	 * セットする値は符号も含めfloat型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setLong(int, long)
	 */
	public void setLong(int i, long x) {
		setFloat(i, x);
	}

	/**
	 * Sets the given signed short to the i'th index,
	 * the given value is converted to the float value.
	 * <p>符号付きのshortをi番目の値としてセットします。
	 * セットする値は符号も含めfloat型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setShort(int, short)
	 */
	public void setShort(int i, short x) {
		setFloat(i, x);
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
		byte[] res = new byte[array.length << 2];

		for(int i = 0; i < array.length; i++) {
			e.writeInt(res, i << 2, Float.floatToIntBits(array[i]));
		}
		return res;
	}

	/**
	 * translates this array to a float[] array.
	 * <p>この配列をfloat[]配列として取得します。
	 */
	public float[] toArray() {
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
		if(obj instanceof FloatArray) {
			return Arrays.equals(array, ((FloatArray)obj).array);
		}
		return false;
	}

}
