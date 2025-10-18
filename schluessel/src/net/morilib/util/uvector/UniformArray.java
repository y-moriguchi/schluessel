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

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public interface UniformArray {

	//
	/*package*/ enum TypeAttr implements java.io.Serializable {
		SIGNED_INT, UNSIGNED_INT, FLOAT;

		public boolean isExact() {
			return this != FLOAT;
		}

		public boolean isSigned() {
			return this != UNSIGNED_INT;
		}

	};

	/**
	 * Returns the size of this array.
	 * <p>この配列のサイズを返します。
	 * 
	 * @return the size of this array
	 */
	public int size();

	/**
	 * Gets the i'th value of this array as a signed byte.
	 * <p>i番目の値を符号付きのbyteとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public byte getByte(int i);

	/**
	 * Gets the i'th value of this array as a signed short.
	 * <p>i番目の値を符号付きのshortとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public short getShort(int i);

	/**
	 * Gets the i'th value of this array as a signed int.
	 * <p>i番目の値を符号付きのintとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public int getInt(int i);

	/**
	 * Gets the i'th value of this array as a signed long.
	 * <p>i番目の値を符号付きのlongとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public long getLong(int i);

	/**
	 * Gets the i'th value of this array as a float value.
	 * <p>i番目の値をfloatとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public float getFloat(int i);

	/**
	 * Gets the i'th value of this array as a double value.
	 * <p>i番目の値をdoubleとして取得します。
	 * 
	 * @param i an index
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public double getDouble(int i);

	/**
	 * Sets the given signed byte to the i'th index.
	 * <p>符号付きのbyteをi番目の値としてセットします。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void setByte(int i, byte x);

	/**
	 * Sets the given signed short to the i'th index.
	 * <p>符号付きのshortをi番目の値としてセットします。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void setShort(int i, short x);

	/**
	 * Sets the given signed int to the i'th index.
	 * <p>符号付きのintをi番目の値としてセットします。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void setInt(int i, int x);

	/**
	 * Sets the given signed long to the i'th index.
	 * <p>符号付きのlongをi番目の値としてセットします。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void setLong(int i, long x);

	/**
	 * Sets the given signed float to the i'th index.
	 * <p>符号付きのfloatをi番目の値としてセットします。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void setFloat(int i, float x);

	/**
	 * Sets the given signed double to the i'th index.
	 * <p>符号付きのdoubleをi番目の値としてセットします。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void setDouble(int i, double x);

	/**
	 * Translates this array to a byte[] array on given endianness.
	 * <p>この配列を与えられたエンディアンでbyte[]に変換します。
	 */
	public byte[] toByteArray(Endianness e);

}
