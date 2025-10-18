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
public abstract class AbstractByteMemory implements ByteMemory {
	
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

}
