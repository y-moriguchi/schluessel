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
package net.morilib.util;

import java.math.BigInteger;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public final class Endianness2 {

	//
	private boolean bigEndian;

	/**
	 * 
	 */
	public static final Endianness2 BIG = new Endianness2(true);

	/**
	 * 
	 */
	public static final Endianness2 LITTLE = new Endianness2(false);

	//
	private Endianness2(boolean b) {
		bigEndian = b;
	}

	//
	private int indexTo(int i, int len) {
		return bigEndian ? i : (len - i - 1);
	}

	//
	private int indexTo2(int i, int len) {
		return !bigEndian ? i : (len - i - 1);
	}

	/**
	 * 
	 * @param b
	 * @param off
	 * @param len
	 * @param val
	 */
	public void write(byte[] b, int off, int len, long val) {
		long v2 = val;

		if(len <= 0 || len > 8) {
			throw new IllegalArgumentException();
		}

		for(int i = 0; i < len; i++) {
			b[off + indexTo2(i, len)] = (byte)(v2 & 0xff);
			v2 >>= 8;
		}
	}

	/**
	 * 
	 * @param b
	 * @param off
	 * @param len
	 * @param val
	 */
	public void write(byte[] b, int off, int len, BigInteger val) {
		BigInteger v2 = val;

		if(len <= 0)  throw new IllegalArgumentException();
		for(int i = 0; i < len; i++) {
//			b[off + indexTo2(i, len)] = (byte)(v2 & 0xff);
//			v2 >>= 8;
			b[off + indexTo2(i, len)] = v2.byteValue();
			v2 = v2.shiftRight(8);
		}
	}

	/**
	 * 
	 * @param b
	 * @param off
	 * @param len
	 * @return
	 */
	public long readu(byte[] b, int off, int len) {
		long val = 0;

		if(len <= 0 || len > 8) {
			throw new IllegalArgumentException();
		}

		for(int i = 0; i < len; i++) {
			val <<= 8;
			val |= (long)b[off + indexTo(i, len)] & 0xffl;
		}
		return val;
	}

	/**
	 * 
	 * @param b
	 * @param off
	 * @param len
	 * @return
	 */
	public BigInteger readuBig(byte[] b, int off, int len) {
		BigInteger val = BigInteger.ZERO;

		if(len <= 0)  throw new IllegalArgumentException();
		for(int i = 0; i < len; i++) {
//			val <<= 8;
//			val |= (long)b[off + indexTo(i, len)] & 0xffl;
			val = val.shiftLeft(8);
			val = val.or(BigInteger.valueOf(
					(int)b[off + indexTo(i, len)] & 0xff));
		}
		return val;
	}

	/**
	 * 
	 * @param b
	 * @param off
	 * @param len
	 * @return
	 */
	public long read(byte[] b, int off, int len) {
		long val = readu(b, off, len);

		if((val & (1l << ((len << 3) - 1))) != 0) {
			for(int i = (len << 3); i < 64; i += 8) {
				val |= 0xffl << i;
			}
		}
		return val;
	}

	/**
	 * 
	 * @param b
	 * @param off
	 * @param len
	 * @return
	 */
	public BigInteger readBig(byte[] b, int off, int len) {
		BigInteger val = BigInteger.ZERO;
		byte w;
		int v;

		if(len <= 0)  throw new IllegalArgumentException();
		for(int i = 0; i < len; i++) {
			v = (int)(w = b[off + indexTo(i, len)]) & 0xff;
			if(i == 0) {
				val = BigInteger.valueOf(w);
			} else {
				val = val.shiftLeft(8);
				val = val.or(BigInteger.valueOf(v));
			}
		}
		return val;
	}

}
