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
 * An array of bytes, or a sequence of bytes.
 * <p>ByteArray can have signed or unsigned bytes.
 * Range of signed byte is from -128 to 127,
 * and range of unsigned byte is from 0 to 255.
 * 
 * <p>バイトの配列、またはバイトの並びです。
 * <p>配列の値として符号付き、または符号なしのbyteを
 * 格納することができます。
 * 符号付きbyteは-128〜127まで、符号なしbyteは0〜255までの値を
 * 格納することができます。
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/11
 */
public class ByteArray extends AbstractUniformArray
implements ByteMemory, java.io.Serializable {

	//
	private byte[] array;
	private TypeAttr signed;

	//
	private ByteArray(byte[] arr, TypeAttr attr) {
		if(!attr.isExact()) {
			throw new IllegalArgumentException(attr.toString());
		} else if(arr == null) {
			throw new NullPointerException();
		}

		array = Arrays2.copy(arr);
		signed = attr;
		//System.arraycopy(arr, 0, array, 0, arr.length);
	}

	/**
	 * Creates a copy of the given array.
	 * <p>与えられた配列のコピーを作成します。
	 * 
	 * @param a an array to be copied
	 */
	public ByteArray(ByteArray a) {
		array = Arrays2.copy(a.array);
		signed = a.signed;
	}

	/**
	 * Constructs an array of signed byte
	 * from the given array of byte[].
	 * <p>byte[]型の配列から符号付きのByteArrayを生成します。
	 * 
	 * @param arr an array to be copied
	 * @return a signed ByteArray
	 */
	public static ByteArray newArray(byte[] arr) {
		return new ByteArray(arr, TypeAttr.SIGNED_INT);
	}

	/**
	 * Constructs an array of unsigned byte
	 * from the given array of byte[].
	 * <p>byte[]型の配列から符号なしのByteArrayを生成します。
	 * 
	 * @param arr an array to be copied
	 * @return a unsigned ByteArray
	 */
	public static ByteArray newuArray(byte[] arr) {
		return new ByteArray(arr, TypeAttr.UNSIGNED_INT);
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public static ByteArray malloc(int size) {
		return new ByteArray(new byte[size], TypeAttr.SIGNED_INT);
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public static ByteArray mallocu(int size) {
		return new ByteArray(new byte[size], TypeAttr.UNSIGNED_INT);
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
		if(i < 0 || i >= array.length) {
			throw new IndexOutOfBoundsException(i + "");
		}
		return array[i];
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
		return getInt(i);
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
		return getInt(i);
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
		byte r = getByte(i);
		int s;

		s = signed.isSigned() ? r : Bytes.ubyteToInt(r);
		return s;
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
		return getInt(i);
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
		return (short)getInt(i);
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
		if(i < 0 || i >= array.length) {
			throw new IndexOutOfBoundsException(i + "");
		}
		array[i] = x;
	}

	/**
	 * Sets the given double value to the i'th index,
	 * the given value is converted to the byte value.
	 * <p>符号付きのdoubleをi番目の値としてセットします。
	 * セットする値は符号も含めbyte型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setDouble(int, double)
	 */
	public void setDouble(int i, double x) {
		setInt(i, (int)x);
	}

	/**
	 * Sets the given float value to the i'th index,
	 * the given value is converted to the byte value.
	 * <p>符号付きのfloatをi番目の値としてセットします。
	 * セットする値は符号も含めbyte型に変換されます。
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
	 * the given value is converted to the byte value.
	 * <p>符号付きのintをi番目の値としてセットします。
	 * セットする値は符号も含めbyte型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setInt(int, int)
	 */
	public void setInt(int i, int x) {
		setByte(i, (byte)x);
	}

	/**
	 * Sets the given signed long to the i'th index,
	 * the given value is converted to the byte value.
	 * <p>符号付きのlongをi番目の値としてセットします。
	 * セットする値は符号も含めbyte型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setLong(int, long)
	 */
	public void setLong(int i, long x) {
		setByte(i, (byte)x);
	}

	/**
	 * Sets the given signed short to the i'th index,
	 * the given value is converted to the byte value.
	 * <p>符号付きのshortをi番目の値としてセットします。
	 * セットする値は符号も含めbyte型に変換されます。
	 * 
	 * @param i an index
	 * @param x a value to be set
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.UniformArray#setShort(int, short)
	 */
	public void setShort(int i, short x) {
		setByte(i, (byte)x);
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
	 * Reads the i'th value of this memory.
	 * <p>このbyte記憶のi番目の値を読み取ります。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readByte(int)
	 */
	public byte readByte(int i) {
		return getByte(i);
	}

	/**
	 * Reads the 8 bytes from the given index
	 * and translates it to the double value
	 * according to the given endianness.
	 * <p>このbyte記憶のi番目からi+7番目の値を読み取り、
	 * 与えられたエンディアンに従ってdouble型に変換します。
	 * 
	 * @param i an address
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readDouble(int, net.morilib.util.uvector.Endianness)
	 */
	public double readDouble(int i, Endianness e) {
		return Double.longBitsToDouble(readLong(i, e));
	}

	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the float value
	 * according to the given endianness.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * 与えられたエンディアンに従ってfloat型に変換します。
	 * 
	 * @param i an address
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readFloat(int, net.morilib.util.uvector.Endianness)
	 */
	public float readFloat(int i, Endianness e) {
		return Float.intBitsToFloat(readInt(i, e));
	}

	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the int value
	 * according to the given endianness.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * 与えられたエンディアンに従ってint型に変換します。
	 * 
	 * @param i an address
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readInt(int, net.morilib.util.uvector.Endianness)
	 */
	public int readInt(int i, Endianness e) {
		return e.readInt(array, i);
	}

	/**
	 * Reads the 8 bytes from the given index
	 * and translates it to the long value
	 * according to the given endianness.
	 * <p>このbyte記憶のi番目からi+7番目の値を読み取り、
	 * 与えられたエンディアンに従ってlong型に変換します。
	 * 
	 * @param i an address
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readLong(int, net.morilib.util.uvector.Endianness)
	 */
	public long readLong(int i, Endianness e) {
		return e.readLong(array, i);
	}

	/**
	 * Reads the 2 bytes from the given index
	 * and translates it to the short value
	 * according to the given endianness.
	 * <p>このbyte記憶のi番目からi+1番目の値を読み取り、
	 * 与えられたエンディアンに従ってshort型に変換します。
	 * 
	 * @param i an address
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readShort(int, net.morilib.util.uvector.Endianness)
	 */
	public short readShort(int i, Endianness e) {
		return (short)readuShort(i, e);
	}

	/**
	 * Write a byte to the i'th index.
	 * <p>このbyte記憶のi番目にbyte値を書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeByte(int, byte)
	 */
	public void writeByte(int i, byte x) {
		setByte(i, x);
	}

	/**
	 * Write 8-bytes code translated from the given double value
	 * from the i'th index on the given endianness.
	 * <p>このbyte記憶にdoubleの値を変換した8バイトのコードを
	 * 指定したエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeDouble(int, double, net.morilib.util.uvector.Endianness)
	 */
	public void writeDouble(int i, double x, Endianness e) {
		writeLong(i, Double.doubleToLongBits(x), e);
	}

	/**
	 * Write 4-bytes code translated from the given float value
	 * from the i'th index on the given endianness.
	 * <p>このbyte記憶にfloatの値を変換した4バイトのコードを
	 * 指定したエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeFloat(int, float, net.morilib.util.uvector.Endianness)
	 */
	public void writeFloat(int i, float x, Endianness e) {
		writeInt(i, Float.floatToIntBits(x), e);
	}

	/**
	 * Write 4-bytes code translated from the given int value
	 * from the i'th index on the given endianness.
	 * <p>このbyte記憶にintの値を変換した4バイトのコードを
	 * 指定したエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeInt(int, int, net.morilib.util.uvector.Endianness)
	 */
	public void writeInt(int i, int x, Endianness e) {
		e.writeInt(array, i, x);
	}

	/**
	 * Write 8-bytes code translated from the given long value
	 * from the i'th index on the given endianness.
	 * <p>このbyte記憶にlongの値を変換した8バイトのコードを
	 * 指定したエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeLong(int, long, net.morilib.util.uvector.Endianness)
	 */
	public void writeLong(int i, long x, Endianness e) {
		e.writeLong(array, i, x);
	}

	/**
	 * Write 2-bytes code translated from the given short value
	 * from the i'th index on the given endianness.
	 * <p>このbyte記憶にshortの値を変換した2バイトのコードを
	 * 指定したエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeShort(int, short, net.morilib.util.uvector.Endianness)
	 */
	public void writeShort(int i, short x, Endianness e) {
		e.writeShort(array, i, x);
	}

	/**
	 * Reads a unsigned byte from i'th index.
	 * <p>このbyte記憶のi番目から符号なし整数を読み込みます。
	 * 
	 * @param i an address to read
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readuByte(int)
	 */
	public int readuByte(int i) {
		return Bytes.ubyteToInt(readByte(i));
	}

	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the unsigned int value
	 * according to the given endianness.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * 与えられたエンディアンに従って符号なしint型に変換します。
	 * 
	 * @param i an address
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readuInt(int, net.morilib.util.uvector.Endianness)
	 */
	public long readuInt(int i, Endianness e) {
		return e.readuInt(array, i);
	}

	/**
	 * Reads the 2 bytes from the given index
	 * and translates it to the unsigned short value
	 * according to the given endianness.
	 * <p>このbyte記憶のi番目からi+1番目の値を読み取り、
	 * 与えられたエンディアンに従って符号なしshort型に変換します。
	 * 
	 * @param i an address
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readuShort(int, net.morilib.util.uvector.Endianness)
	 */
	public int readuShort(int i, Endianness e) {
		return e.readuShort(array, i);
	}

	/**
	 * Write a unsigned byte to the i'th index.
	 * <p>このbyte記憶のi番目に符号なしbyte値を書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeuByte(int, int)
	 */
	public void writeuByte(int i, int x) {
		writeByte(i, (byte)x);
	}

	/**
	 * Write 4-bytes code translated from the given unsigned int value
	 * from the i'th index on the given endianness.
	 * <p>このbyte記憶に符号なしintの値を変換した4バイトのコードを
	 * 指定したエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeuInt(int, long, net.morilib.util.uvector.Endianness)
	 */
	public void writeuInt(int i, long x, Endianness e) {
		e.writeuInt(array, i, x);
	}

	/**
	 * Write 2-bytes code translated from the given unsigned short value
	 * from the i'th index on the given endianness.
	 * <p>このbyte記憶に符号なしshortの値を変換した4バイトのコードを
	 * 指定したエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @param e endianness
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeuInt(int, long, net.morilib.util.uvector.Endianness)
	 */
	public void writeuShort(int i, int x, Endianness e) {
		e.writeuShort(array, i, x);
	}

	/**
	 * Reads the 8 bytes from the given index
	 * and translates it to the double value on big-endian.
	 * <p>このbyte記憶のi番目からi+7番目の値を読み取り、
	 * ビッグエンディアンに従ってdouble型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readDoubleB(int)
	 */
	public double readDoubleB(int i) {
		return readDouble(i, Endianness.BIG);
	}

	/**
	 * Reads the 8 bytes from the given index
	 * and translates it to the double value on little-endian.
	 * <p>このbyte記憶のi番目からi+7番目の値を読み取り、
	 * リトルエンディアンに従ってdouble型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readDoubleL(int)
	 */
	public double readDoubleL(int i) {
		return readDouble(i, Endianness.LITTLE);
	}

	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the float value on big-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * ビッグエンディアンに従ってfloat型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readFloatB(int)
	 */
	public float readFloatB(int i) {
		return readFloat(i, Endianness.BIG);
	}

	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the double value on little-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * リトルエンディアンに従ってdouble型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readDoubleL(int)
	 */
	public float readFloatL(int i) {
		return readFloat(i, Endianness.LITTLE);
	}

	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the int value on big-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * ビッグエンディアンに従ってint型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readIntB(int)
	 */
	public int readIntB(int i) {
		return readInt(i, Endianness.BIG);
	}

	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the int value on little-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * リトルエンディアンに従ってint型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readIntL(int)
	 */
	public int readIntL(int i) {
		return readInt(i, Endianness.LITTLE);
	}

	/**
	 * Reads the 8 bytes from the given index
	 * and translates it to the long value on big-endian.
	 * <p>このbyte記憶のi番目からi+7番目の値を読み取り、
	 * ビッグエンディアンに従ってlong型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readLongB(int)
	 */
	public long readLongB(int i) {
		return readLong(i, Endianness.BIG);
	}

	/**
	 * Reads the 8 bytes from the given index
	 * and translates it to the long value on little-endian.
	 * <p>このbyte記憶のi番目からi+7番目の値を読み取り、
	 * リトルエンディアンに従ってlong型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readLongL(int)
	 */
	public long readLongL(int i) {
		return readLong(i, Endianness.LITTLE);
	}

	/**
	 * Reads the 2 bytes from the given index
	 * and translates it to the short value on big-endian.
	 * <p>このbyte記憶のi番目からi+1番目の値を読み取り、
	 * ビッグエンディアンに従ってshort型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readShortB(int)
	 */
	public short readShortB(int i) {
		return readShort(i, Endianness.BIG);
	}

	/**
	 * Reads the 2 bytes from the given index
	 * and translates it to the short value on little-endian.
	 * <p>このbyte記憶のi番目からi+1番目の値を読み取り、
	 * リトルエンディアンに従ってshort型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readShortL(int)
	 */
	public short readShortL(int i) {
		return readShort(i, Endianness.LITTLE);
	}

	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the unsigned int value on big-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * ビッグエンディアンに従って符号なしint型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readuIntB(int)
	 */
	public long readuIntB(int i) {
		return readuInt(i, Endianness.BIG);
	}

	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the unsigned int value on little-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * リトルエンディアンに従って符号なしint型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readuIntL(int)
	 */
	public long readuIntL(int i) {
		return readuInt(i, Endianness.LITTLE);
	}

	/**
	 * Reads the 2 bytes from the given index
	 * and translates it to the unsigned short value on big-endian.
	 * <p>このbyte記憶のi番目からi+1番目の値を読み取り、
	 * ビッグエンディアンに従って符号なしshort型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readuShortB(int)
	 */
	public int readuShortB(int i) {
		return readuShort(i, Endianness.BIG);
	}

	/**
	 * Reads the 2 bytes from the given index
	 * and translates it to the unsigned short value on little-endian.
	 * <p>このbyte記憶のi番目からi+1番目の値を読み取り、
	 * リトルエンディアンに従って符号なしshort型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#readuShortL(int)
	 */
	public int readuShortL(int i) {
		return readuShort(i, Endianness.LITTLE);
	}

	/**
	 * Write 8-bytes code translated from the given double value
	 * from the i'th index on big-endian.
	 * <p>このbyte記憶にdoubleの値を変換した8バイトのコードを
	 * ビッグエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeDoubleB(int, double)
	 */
	public void writeDoubleB(int i, double x) {
		writeDouble(i, x, Endianness.BIG);
	}

	/**
	 * Write 8-bytes code translated from the given double value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶にdoubleの値を変換した8バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeDoubleL(int, double)
	 */
	public void writeDoubleL(int i, double x) {
		writeDouble(i, x, Endianness.LITTLE);
	}

	/**
	 * Write 4-bytes code translated from the given float value
	 * from the i'th index on big-endian.
	 * <p>このbyte記憶にfloatの値を変換した4バイトのコードを
	 * ビッグエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeFloatB(int, float)
	 */
	public void writeFloatB(int i, float x) {
		writeFloat(i, x, Endianness.BIG);
	}

	/**
	 * Write 4-bytes code translated from the given float value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶にfloatの値を変換した4バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeFloatL(int, float)
	 */
	public void writeFloatL(int i, float x) {
		writeFloat(i, x, Endianness.LITTLE);
	}

	/**
	 * Write 4-bytes code translated from the given int value
	 * from the i'th index on big-endian.
	 * <p>このbyte記憶にintの値を変換した4バイトのコードを
	 * ビッグエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeIntB(int, int)
	 */
	public void writeIntB(int i, int x) {
		writeInt(i, x, Endianness.BIG);
	}

	/**
	 * Write 4-bytes code translated from the given int value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶にintの値を変換した4バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeIntL(int, int)
	 */
	public void writeIntL(int i, int x) {
		writeInt(i, x, Endianness.LITTLE);
	}

	/**
	 * Write 8-bytes code translated from the given long value
	 * from the i'th index on big-endian.
	 * <p>このbyte記憶にlongの値を変換した8バイトのコードを
	 * ビッグエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeLongB(int, long)
	 */
	public void writeLongB(int i, long x) {
		writeLong(i, x, Endianness.BIG);
	}

	/**
	 * Write 8-bytes code translated from the given long value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶にlongの値を変換した8バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeLongL(int, long)
	 */
	public void writeLongL(int i, long x) {
		writeLong(i, x, Endianness.LITTLE);
	}

	/**
	 * Write 2-bytes code translated from the given short value
	 * from the i'th index on big-endian.
	 * <p>このbyte記憶にshortの値を変換した2バイトのコードを
	 * ビッグエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeShortB(int, short)
	 */
	public void writeShortB(int i, short x) {
		writeShort(i, x, Endianness.BIG);
	}

	/**
	 * Write 2-bytes code translated from the given short value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶にshortの値を変換した2バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeShortL(int, short)
	 */
	public void writeShortL(int i, short x) {
		writeShort(i, x, Endianness.LITTLE);
	}

	/**
	 * Write 4-bytes code translated from the given unsigned int value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶に符号なしintの値を変換した4バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeuIntB(int, long)
	 */
	public void writeuIntB(int i, long x) {
		writeuInt(i, x, Endianness.BIG);
	}

	/**
	 * Write 4-bytes code translated from the given unsigned int value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶に符号なしintの値を変換した4バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeuIntL(int, long)
	 */
	public void writeuIntL(int i, long x) {
		writeuInt(i, x, Endianness.LITTLE);
	}

	/**
	 * Write 2-bytes code translated from the given unsigned short value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶に符号なしshortの値を変換した2バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeuShortB(int, int)
	 */
	public void writeuShortB(int i, int x) {
		writeuShort(i, x, Endianness.BIG);
	}

	/**
	 * Write 2-bytes code translated from the given unsigned short value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶に符号なしshortの値を変換した2バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 * @see net.morilib.util.uvector.ByteMemory#writeuShortL(int, int)
	 */
	public void writeuShortL(int i, int x) {
		writeuShort(i, x, Endianness.LITTLE);
	}

	/**
	 * Translates this array to a byte[] array.
	 * <p>このbyte記憶をbyte[]に変換します。
	 * 
	 * @see net.morilib.util.uvector.ByteMemory#toByteArray()
	 */
	public byte[] toByteArray() {
		return Arrays2.copy(array);
	}

	/**
	 * Translates this array to a byte[] array on given endianness.
	 * <p>このbyte記憶を与えられたエンディアンでbyte[]に変換します。
	 * 
	 * @see net.morilib.util.uvector.UniformArray#toByteArray(net.morilib.util.uvector.Endianness)
	 */
	public byte[] toByteArray(Endianness e) {
		return toByteArray();
	}

	/**
	 * @param sd
	 * @param s
	 * @param ss
	 * @param l
	 */
	public void arraycopy(int sd, ByteArray s, int ss, int l) {
		System.arraycopy(s.array, ss, array, sd, l);
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
		if(obj instanceof ByteArray) {
			return Arrays.equals(array, ((ByteArray)obj).array);
		}
		return false;
	}

}
