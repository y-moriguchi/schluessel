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
package net.morilib.util;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * An utility class to treat bytes.
 * <p>バイトを扱うユーティリティクラスです。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public final class Bytes {
	
	private Bytes() {
		// do nothing
	}
	
	/**
	 * translate the given byte to unsigned short,
	 * the byte is treated as unsigned.
	 * <p>byteを符号なしと見なしてshort型に変換します。
	 * 
	 * @param r an unsigned byte
	 * @return unsigned short
	 */
	public static short ubyteToShort(byte r) {
		return (short)ubyteToInt(r);
	}
	
	/**
	 * translate the given byte to unsigned int,
	 * the byte is treated as unsigned.
	 * <p>byteを符号なしと見なしてint型に変換します。
	 * 
	 * @param r an unsigned byte
	 * @return unsigned int
	 */
	public static int ubyteToInt(byte r) {
		return (r < 0) ? (0x80 | (r & 0x7f)) : r;
	}
	
	/**
	 * translate the given short to unsigned int,
	 * the short is treated as unsigned.
	 * <p>shortを符号なしと見なしてint型に変換します。
	 * 
	 * @param r an unsigned short
	 * @return unsigned int
	 */
	public static int ushortToInt(short r) {
		return (r < 0) ? (0x8000 | (r & 0x7fff)) : r;
	}
	
	/**
	 * translate the given byte to unsigned long,
	 * the byte is treated as unsigned.
	 * <p>byteを符号なしと見なしてlong型に変換します。
	 * 
	 * @param r an unsigned byte
	 * @return unsigned long
	 */
	public static long ubyteToLong(byte r) {
		return ubyteToInt(r);
	}
	
	/**
	 * translate the given short to unsigned long,
	 * the short is treated as unsigned.
	 * <p>shortを符号なしと見なしてlong型に変換します。
	 * 
	 * @param r an unsigned short
	 * @return unsigned long
	 */
	public static long ushortToLong(short r) {
		return ushortToInt(r);
	}
	
	/**
	 * translate the given int to unsigned long,
	 * the int is treated as unsigned.
	 * <p>intを符号なしと見なしてlong型に変換します。
	 * 
	 * @param r an unsigned int
	 * @return unsigned long
	 */
	public static long uintToLong(int r) {
		return (r < 0) ? (0x80000000l | (r & 0x7fffffffl)) : r;
	}
	
	/**
	 * translate the given long to BigInteger,
	 * the long is treated as unsigned.
	 * <p>longを符号なしと見なしてBigInteger型に変換します。
	 * 
	 * @param r an unsigned long
	 * @return BigInteger
	 */
	public static BigInteger ulongToBigInteger(long r) {
		byte[] b = new byte[8];
		
		b[0] = (byte)(r >>> 56);
		b[1] = (byte)(r >>> 48);
		b[2] = (byte)(r >>> 40);
		b[3] = (byte)(r >>> 32);
		b[4] = (byte)(r >>> 24);
		b[5] = (byte)(r >>> 16);
		b[6] = (byte)(r >>> 8);
		b[7] = (byte)(r);
		return new BigInteger(1, b);
	}
	
	/**
	 * translate the given long to float,
	 * the long is treated as unsigned.
	 * <p>longを符号なしと見なしてfloat型に変換します。
	 * 
	 * @param r an unsigned long
	 * @return float
	 */
	public static float ulongToFloat(long r) {
		return ulongToBigInteger(r).floatValue();
	}
	
	/**
	 * translate the given long to double,
	 * the long is treated as unsigned.
	 * <p>longを符号なしと見なしてdouble型に変換します。
	 * 
	 * @param r an unsigned long
	 * @return double
	 */
	public static double ulongToDouble(long r) {
		return ulongToBigInteger(r).doubleValue();
	}
	
	/**
	 * translate the given float to unsigned long.
	 * <p>floatを符号なしlongに変換します。
	 * 
	 * @param r an float
	 * @return unsigned long
	 */
	public static long floatTouLong(float r) {
		return new BigDecimal(r).longValue();
	}
	
	/**
	 * translate the given double to unsigned long.
	 * <p>doubleを符号なしlongに変換します。
	 * 
	 * @param r an double
	 * @return unsigned long
	 */
	public static long doubleTouLong(double r) {
		return new BigDecimal(r).longValue();
	}
	
}
