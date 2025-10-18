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

public interface ByteMemory {
	
	/**
	 * Reads the i'th value of this memory.
	 * <p>このbyte記憶のi番目の値を読み取ります。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public byte readByte(int i);
	
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
	 */
	public short readShort(int i, Endianness e);
	
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
	 */
	public int readInt(int i, Endianness e);
	
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
	 */
	public long readLong(int i, Endianness e);
	
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
	 */
	public float readFloat(int i, Endianness e);
	
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
	 */
	public double readDouble(int i, Endianness e);
	
	/**
	 * Write a byte to the i'th index.
	 * <p>このbyte記憶のi番目にbyte値を書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeByte(int i, byte x);
	
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
	 */
	public void writeShort(int i, short x, Endianness e);
	
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
	 */
	public void writeInt(int i, int x, Endianness e);
	
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
	 */
	public void writeLong(int i, long x, Endianness e);
	
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
	 */
	public void writeFloat(int i, float x, Endianness e);
	
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
	 */
	public void writeDouble(int i, double x, Endianness e);
	
	/**
	 * Reads a unsigned byte from i'th index.
	 * <p>このbyte記憶のi番目から符号なし整数を読み込みます。
	 * 
	 * @param i an address to read
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public int readuByte(int i);
	
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
	 */
	public int readuShort(int i, Endianness e);
	
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
	 */
	public long readuInt(int i, Endianness e);
	
	/**
	 * Write a unsigned byte to the i'th index.
	 * <p>このbyte記憶のi番目に符号なしbyte値を書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeuByte(int i, int x);
	
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
	 */
	public void writeuShort(int i, int x, Endianness e);
	
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
	 */
	public void writeuInt(int i, long x, Endianness e);
	
	/**
	 * Reads the 2 bytes from the given index
	 * and translates it to the short value on little-endian.
	 * <p>このbyte記憶のi番目からi+1番目の値を読み取り、
	 * リトルエンディアンに従ってshort型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public short readShortL(int i);
	
	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the int value on little-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * リトルエンディアンに従ってint型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public int readIntL(int i);
	
	/**
	 * Reads the 8 bytes from the given index
	 * and translates it to the long value on little-endian.
	 * <p>このbyte記憶のi番目からi+7番目の値を読み取り、
	 * リトルエンディアンに従ってlong型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public long readLongL(int i);
	
	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the double value on little-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * リトルエンディアンに従ってdouble型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public float readFloatL(int i);
	
	/**
	 * Reads the 8 bytes from the given index
	 * and translates it to the double value on little-endian.
	 * <p>このbyte記憶のi番目からi+7番目の値を読み取り、
	 * リトルエンディアンに従ってdouble型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public double readDoubleL(int i);
	
	/**
	 * Write 2-bytes code translated from the given short value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶にshortの値を変換した2バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeShortL(int i, short x);
	
	/**
	 * Write 4-bytes code translated from the given int value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶にintの値を変換した4バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeIntL(int i, int x);
	
	/**
	 * Write 8-bytes code translated from the given long value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶にlongの値を変換した8バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeLongL(int i, long x);
	
	/**
	 * Write 4-bytes code translated from the given float value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶にfloatの値を変換した4バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeFloatL(int i, float x);
	
	/**
	 * Write 8-bytes code translated from the given double value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶にdoubleの値を変換した8バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeDoubleL(int i, double x);
	
	/**
	 * Reads the 2 bytes from the given index
	 * and translates it to the unsigned short value on little-endian.
	 * <p>このbyte記憶のi番目からi+1番目の値を読み取り、
	 * リトルエンディアンに従って符号なしshort型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public int readuShortL(int i);
	
	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the unsigned int value on little-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * リトルエンディアンに従って符号なしint型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public long readuIntL(int i);
	
	/**
	 * Write 2-bytes code translated from the given unsigned short value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶に符号なしshortの値を変換した2バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeuShortL(int i, int x);
	
	/**
	 * Write 4-bytes code translated from the given unsigned int value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶に符号なしintの値を変換した4バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeuIntL(int i, long x);
	
	/**
	 * Reads the 2 bytes from the given index
	 * and translates it to the short value on big-endian.
	 * <p>このbyte記憶のi番目からi+1番目の値を読み取り、
	 * ビッグエンディアンに従ってshort型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public short readShortB(int i);
	
	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the int value on big-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * ビッグエンディアンに従ってint型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public int readIntB(int i);
	
	/**
	 * Reads the 8 bytes from the given index
	 * and translates it to the long value on big-endian.
	 * <p>このbyte記憶のi番目からi+7番目の値を読み取り、
	 * ビッグエンディアンに従ってlong型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public long readLongB(int i);
	
	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the float value on big-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * ビッグエンディアンに従ってfloat型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public float readFloatB(int i);
	
	/**
	 * Reads the 8 bytes from the given index
	 * and translates it to the double value on big-endian.
	 * <p>このbyte記憶のi番目からi+7番目の値を読み取り、
	 * ビッグエンディアンに従ってdouble型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public double readDoubleB(int i);
	
	/**
	 * Write 2-bytes code translated from the given short value
	 * from the i'th index on big-endian.
	 * <p>このbyte記憶にshortの値を変換した2バイトのコードを
	 * ビッグエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeShortB(int i, short x);
	
	/**
	 * Write 4-bytes code translated from the given int value
	 * from the i'th index on big-endian.
	 * <p>このbyte記憶にintの値を変換した4バイトのコードを
	 * ビッグエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeIntB(int i, int x);
	
	/**
	 * Write 8-bytes code translated from the given long value
	 * from the i'th index on big-endian.
	 * <p>このbyte記憶にlongの値を変換した8バイトのコードを
	 * ビッグエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeLongB(int i, long x);
	
	/**
	 * Write 4-bytes code translated from the given float value
	 * from the i'th index on big-endian.
	 * <p>このbyte記憶にfloatの値を変換した4バイトのコードを
	 * ビッグエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeFloatB(int i, float x);
	
	/**
	 * Write 8-bytes code translated from the given double value
	 * from the i'th index on big-endian.
	 * <p>このbyte記憶にdoubleの値を変換した8バイトのコードを
	 * ビッグエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeDoubleB(int i, double x);
	
	/**
	 * Reads the 2 bytes from the given index
	 * and translates it to the unsigned short value on big-endian.
	 * <p>このbyte記憶のi番目からi+1番目の値を読み取り、
	 * ビッグエンディアンに従って符号なしshort型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public int readuShortB(int i);
	
	/**
	 * Reads the 4 bytes from the given index
	 * and translates it to the unsigned int value on big-endian.
	 * <p>このbyte記憶のi番目からi+3番目の値を読み取り、
	 * ビッグエンディアンに従って符号なしint型に変換します。
	 * 
	 * @param i an address
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public long readuIntB(int i);
	
	/**
	 * Write 2-bytes code translated from the given unsigned short value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶に符号なしshortの値を変換した2バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeuShortB(int i, int x);
	
	/**
	 * Write 4-bytes code translated from the given unsigned int value
	 * from the i'th index on little-endian.
	 * <p>このbyte記憶に符号なしintの値を変換した4バイトのコードを
	 * リトルエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param i an address to write
	 * @param x value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeuIntB(int i, long x);
	
	/**
	 * Translates this array to a byte[] array.
	 * <p>このbyte記憶をbyte[]に変換します。
	 */
	public byte[] toByteArray();
	
	
	public int size();
	
}
