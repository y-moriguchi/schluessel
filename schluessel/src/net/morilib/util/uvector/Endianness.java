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

import net.morilib.math.ArrayIntSymmetricGroup;
import net.morilib.math.IntPermutation;
import net.morilib.util.Bytes;

/**
 * Endianness, and reads/writes a byte[] array
 * on the endiannness.
 * <p>エンディアンを表現するオブジェクトである。
 * byte[]型の配列からそのオブジェクトが表現するエンディアンに
 * 従って読み込み/書き込みする機能も備える。
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/11
 */
public final class Endianness {
	
	/**
	 * Indicates &quot;big-endian&quot;.
	 * 「ビッグエンディアン」を表現する。
	 */
	public static final Endianness BIG = new Endianness(
			"BIG",
			ArrayIntSymmetricGroup.getInstance(2).getIdentity(),
			ArrayIntSymmetricGroup.getInstance(4).getIdentity(),
			ArrayIntSymmetricGroup.getInstance(8).getIdentity());
	
	/**
	 * Indicates &quot;little-endian&quot;.
	 * 「リトルエンディアン」を表現する。
	 */
	public static final Endianness LITTLE = Endianness.getInstance(
			"LITTLE",
			new int[] { 1, 0 },
			new int[] { 3, 2, 1, 0 },
			new int[] { 7, 6, 5, 4, 3, 2, 1, 0 });
		
	
	private String code;
	private IntPermutation p2, p4, p8;
	
	
	private Endianness(
			String code,
			IntPermutation p2,
			IntPermutation p4,
			IntPermutation p8) {
		this.code = code;
		this.p2   = p2;
		this.p4   = p4;
		this.p8   = p8;
	}
	
	private static int[] mapinc(int[] src) {
		if(src == null) {
			throw new NullPointerException();
		}
		
		int res[] = new int[src.length];
		int bit   = 0;
		
		for(int i = 0; i < src.length; i++) {
			if(src[i] < 0 || src[i] >= src.length) {
				throw new IndexOutOfBoundsException(src[i] + "");
			}
			res[i]  = src[i] + 1;
			bit    |= 1 << i;
		}
		
		if(bit != (1 << src.length) - 1) {
			throw new IllegalArgumentException(
					"map of endianness bit is not unique");
		}
		return res;
	}
	
	/**
	 * Creates a endianness which has given order.
	 * <p>引数で与えられた順序をもつ新しいエンディアンを定義する。
	 * <p>第1引数はエンディアンの名称を指定する。
	 * <p>第2引数から第4引数はそれぞれshort, int, long型の順序を
	 * int[]型で指定する。順序は0から始まる重複のない整数の列とする。
	 * 例として、リトルエンディアンは、
	 * <pre>
	 * Endianness.getInstance(
	 *   "LITTLE",
	 *   new int[] { 1, 0 },
	 *   new int[] { 3, 2, 1, 0 },
	 *   new int[] { 7, 6, 5, 4, 3, 2, 1, 0 }
	 * );
	 * </pre>
	 * と定義される。
	 * 
	 * @param code name of endianness
	 * @param end2 byte-order of short(2-bytes)
	 * @param end4 byte-order of int(4-bytes)
	 * @param end8 byte-order of long(8-bytes)
	 */
	public static Endianness getInstance(
			String code, int[] end2, int[] end4, int[] end8) {
		if(end2.length != 2) {
			throw new IllegalArgumentException(
					"short endian must have 2 integers");
		} else if(end4.length != 4) {
			throw new IllegalArgumentException(
					"int endian must have 4 integers");
		} else if(end8.length != 8) {
			throw new IllegalArgumentException(
					"long endian must have 8 integers");
		}
		
		int[] p2 = mapinc(end2);
		int[] p4 = mapinc(end4);
		int[] p8 = mapinc(end8);
		
		return new Endianness(
				code,
				ArrayIntSymmetricGroup.getInstance(2).newElement(p2),
				ArrayIntSymmetricGroup.getInstance(4).newElement(p4),
				ArrayIntSymmetricGroup.getInstance(8).newElement(p8));
	}
	
	/**
	 * Reads the 2 bytes from the given array
	 * and translates it to the short value on this endianness.
	 * <p>与えられたbyte[]配列のi番目からi+1番目の値を読み取り、
	 * オブジェクトが示すエンディアンに従ってshort型に変換します。
	 * 
	 * @param src an array to be read
	 * @param offset offset
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public short readShort(byte[] src, int offset) {
		short r;
		int i = offset;
		
		if(i < 0 || i >= src.length - 1) {
			throw new IndexOutOfBoundsException(i + "");
		}
		
		r = (short)(
				Bytes.ubyteToInt(src[i + p2.get(1) - 1]) << 8 |
				Bytes.ubyteToInt(src[i + p2.get(2) - 1]));
		return r;
	}
	
	/**
	 * Reads the 4 bytes from the given array
	 * and translates it to the int value on this endianness.
	 * <p>与えられたbyte[]配列のi番目からi+3番目の値を読み取り、
	 * オブジェクトが示すエンディアンに従ってint型に変換します。
	 * 
	 * @param src an array to be read
	 * @param offset offset
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public int readInt(byte[] src, int offset) {
		int r;
		int i = offset;
		
		if(i < 0 || i >= src.length - 3) {
			throw new IndexOutOfBoundsException(i + "");
		}
		
		r = (int)(
				Bytes.ubyteToInt(src[i + p4.get(1) - 1]) << 24 |
				Bytes.ubyteToInt(src[i + p4.get(2) - 1]) << 16 |
				Bytes.ubyteToInt(src[i + p4.get(3) - 1]) << 8  |
				Bytes.ubyteToInt(src[i + p4.get(4) - 1]));
		return r;
	}
	
	/**
	 * Reads the 8 bytes from the given array
	 * and translates it to the long value on this endianness.
	 * <p>与えられたbyte[]配列のi番目からi+7番目の値を読み取り、
	 * オブジェクトが示すエンディアンに従ってlong型に変換します。
	 * 
	 * @param src an array to be read
	 * @param offset offset
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public long readLong(byte[] src, int offset) {
		long r;
		int i = offset;
		
		if(i < 0 || i >= src.length - 7) {
			throw new IndexOutOfBoundsException(i + "");
		}
		
		r = (long)(
				Bytes.ubyteToLong(src[i + p8.get(1) - 1]) << 56 |
				Bytes.ubyteToLong(src[i + p8.get(2) - 1]) << 48 |
				Bytes.ubyteToLong(src[i + p8.get(3) - 1]) << 40 |
				Bytes.ubyteToLong(src[i + p8.get(4) - 1]) << 32 |
				Bytes.ubyteToLong(src[i + p8.get(5) - 1]) << 24 |
				Bytes.ubyteToLong(src[i + p8.get(6) - 1]) << 16 |
				Bytes.ubyteToLong(src[i + p8.get(7) - 1]) << 8  |
				Bytes.ubyteToLong(src[i + p8.get(8) - 1]));
		return r;
	}
	
	/**
	 * Reads the 2 bytes from the given array
	 * and translates it to the unsigned short value on this endianness.
	 * <p>与えられたbyte[]配列のi番目からi+1番目の値を読み取り、
	 * オブジェクトが示すエンディアンに従って符号なしshort型に変換します。
	 * 
	 * @param src an array to be read
	 * @param offset offset
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public int readuShort(byte[] src, int offset) {
		int r;
		int i = offset;
		
		if(i < 0 || i >= src.length - 1) {
			throw new IndexOutOfBoundsException(i + "");
		}
		
		r = (int)(
				Bytes.ubyteToInt(src[i + p2.get(1) - 1]) << 8 |
				Bytes.ubyteToInt(src[i + p2.get(2) - 1]));
		return r;
	}
	
	/**
	 * Reads the 4 bytes from the given array
	 * and translates it to the unsigned int value on this endianness.
	 * <p>与えられたbyte[]配列のi番目からi+3番目の値を読み取り、
	 * オブジェクトが示すエンディアンに従って符号なしint型に変換します。
	 * 
	 * @param src an array to be read
	 * @param offset offset
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public long readuInt(byte[] src, int offset) {
		long r;
		int i = offset;
		
		if(i < 0 || i >= src.length - 3) {
			throw new IndexOutOfBoundsException(i + "");
		}
		
		r = (long)(
				Bytes.ubyteToLong(src[i + p4.get(1) - 1]) << 24 |
				Bytes.ubyteToLong(src[i + p4.get(2) - 1]) << 16 |
				Bytes.ubyteToLong(src[i + p4.get(3) - 1]) << 8  |
				Bytes.ubyteToLong(src[i + p4.get(4) - 1]));
		return r;
	}
	
	/**
	 * Write 2-bytes code translated from the given short value
	 * from the given byte[] array on this endianness.
	 * <p>このbyte記憶にshortの値を変換した2バイトのコードを
	 * オブジェクトが示すエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param src an array to be written
	 * @param offset offset
	 * @param value value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeShort(byte[] src, int offset, short value) {
		int i = offset;
		
		if(i < 0 || i >= src.length - 1) {
			throw new IndexOutOfBoundsException(i + "");
		}
		
		src[i + p2.get(1) - 1] = (byte)(value >>> 8);
		src[i + p2.get(2) - 1] = (byte)(value);
	}
	
	/**
	 * Write 4-bytes code translated from the given int value
	 * from the given byte[] array on this endianness.
	 * <p>このbyte記憶にintの値を変換した4バイトのコードを
	 * オブジェクトが示すエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param src an array to be written
	 * @param offset offset
	 * @param value value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeInt(byte[] src, int offset, int value) {
		int i = offset;
		
		if(i < 0 || i >= src.length - 3) {
			throw new IndexOutOfBoundsException(i + "");
		}
		
		src[i + p4.get(1) - 1] = (byte)(value >>> 24);
		src[i + p4.get(2) - 1] = (byte)(value >>> 16);
		src[i + p4.get(3) - 1] = (byte)(value >>> 8);
		src[i + p4.get(4) - 1] = (byte)(value);
	}
	
	/**
	 * Write 8-bytes code translated from the given long value
	 * from the given byte[] array on this endianness.
	 * <p>このbyte記憶にlongの値を変換した8バイトのコードを
	 * オブジェクトが示すエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param src an array to be written
	 * @param offset offset
	 * @param value value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeLong(byte[] src, int offset, long value) {
		int i = offset;
		
		if(i < 0 || i >= src.length - 7) {
			throw new IndexOutOfBoundsException(i + "");
		}
		
		src[i + p8.get(1) - 1] = (byte)(value >>> 56);
		src[i + p8.get(2) - 1] = (byte)(value >>> 48);
		src[i + p8.get(3) - 1] = (byte)(value >>> 40);
		src[i + p8.get(4) - 1] = (byte)(value >>> 32);
		src[i + p8.get(5) - 1] = (byte)(value >>> 24);
		src[i + p8.get(6) - 1] = (byte)(value >>> 16);
		src[i + p8.get(7) - 1] = (byte)(value >>> 8);
		src[i + p8.get(8) - 1] = (byte)(value);
	}
	
	/**
	 * Write 2-bytes code translated from the given unsigned short value
	 * from the given byte[] array on this endianness.
	 * <p>このbyte記憶に符号なしshortの値を変換した2バイトのコードを
	 * オブジェクトが示すエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param src an array to be written
	 * @param offset offset
	 * @param value value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeuShort(byte[] src, int offset, int value) {
		int i = offset;
		
		if(i < 0 || i >= src.length - 1) {
			throw new IndexOutOfBoundsException(i + "");
		}
		
		src[i + p2.get(1) - 1] = (byte)(value >>> 8);
		src[i + p2.get(2) - 1] = (byte)(value);
	}
	
	/**
	 * Write 4-bytes code translated from the given unsigned int value
	 * from the given byte[] array on this endianness.
	 * <p>このbyte記憶に符号なしintの値を変換した2バイトのコードを
	 * オブジェクトが示すエンディアンに従ってi番目から書き込みます。
	 * 
	 * @param src an array to be written
	 * @param offset offset
	 * @param value value to be written
	 * @throws IndexOutOfBoundsException if the index is out of bounds
	 */
	public void writeuInt(byte[] src, int offset, long value) {
		int i = offset;
		
		if(i < 0 || i >= src.length - 3) {
			throw new IndexOutOfBoundsException(i + "");
		}
		
		src[i + p4.get(1) - 1] = (byte)(value >>> 24);
		src[i + p4.get(2) - 1] = (byte)(value >>> 16);
		src[i + p4.get(3) - 1] = (byte)(value >>> 8);
		src[i + p4.get(4) - 1] = (byte)(value);
	}
	
	
	public String toString() {
		return code;
	}
	
}
