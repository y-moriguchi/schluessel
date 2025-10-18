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
package net.morilib.math.series.decimal;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Iterator;

import net.morilib.util.primitive.ByteArrayVector;
import net.morilib.util.primitive.ByteVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public class RationalDecimalSeries implements Iterator<BigDecimal> {

	//
	private StringBuilder buf = new StringBuilder();
	private byte[] hou0, jit0;
	private final byte[] zero;
	private transient byte[] tmp0, tmp1;
	private int h0;
	private BigDecimal prev;

	/**
	 * 
	 * @param a
	 * @param b
	 */
	public RationalDecimalSeries(BigInteger a, BigInteger b) {
		hou0 = split(b);
		jit0 = ext(split(a.mod(b)), hou0.length);
		zero = new byte[jit0.length];
		tmp0 = new byte[jit0.length];
		tmp1 = new byte[jit0.length];
		buf.append(a.divide(b) + ".");

		Arrays.fill(zero, (byte)0);
		h0 = toInt(hou0[0]);
		if(hou0.length > 1) {
			h0 = (h0 * 10) + toInt(hou0[1]);
		} else {
			h0 = h0 * 10;
		}
	}

	//
	private static int toInt(byte b) {
		return (b < 0) ? (b & 0x7f) + 128 : b;
	}

	//
	private static boolean mult(byte[] r, byte[] b, int v, int s) {
		int carry = 0;

		for(int i = r.length - 1; i >= 0; i--) {
			int x;
			int k = i - (r.length - b.length) + s;

			x = ((k >= 0 && k < b.length) ?
					toInt(b[k]) : 0) * v + carry;
			carry = (x / 10);
			r[i] = (byte)(x % 10);
		}
		return carry == 0;
	}

	//
	private static boolean sub0(byte[] r, byte[] b, byte[] c) {
		int carry = 0, x;

		for(int i = b.length - 1; i >= 0; i--) {
			x = toInt(b[i]) - toInt(c[i]) + carry;
			carry = (x > 0) ? x / 10 : (x - 9) / 10;
			r[i] = (byte)((x >= 0) ? (x % 10) : (x + 10) % 10);
		}
		return carry == 0;
	}

	//
	private static byte[] split(BigInteger a) {
		ByteVector b = new ByteArrayVector();
		BigInteger[] c;
		byte[] r;

		while(a.signum() > 0) {
			c = a.divideAndRemainder(BigInteger.TEN);
			b.add(c[1].byteValue());
			a = c[0];
		}

		r = new byte[b.size()];
		for(int i = 0; i < b.size(); i++) {
			r[i] = b.get(b.size() - i - 1);
		}
		return r;
	}

	//
	private static byte[] ext(byte[] a, int ext) {
		byte[] r = new byte[ext + 1];

		Arrays.fill(r, (byte)0);
		System.arraycopy(a, 0, r, ext - a.length, a.length);
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		return true;
	}

	//
	BigDecimal _next() {
		int j, j0;

		if(Arrays.equals(jit0, zero)) {
			return prev;
		} else {
			j0 = toInt(jit0[0]) + 1;
			j0 = j0 * 10 + (jit0.length > 1 ? toInt(jit0[1]) : 0);
			j0 = j0 * 10;
			for(j = j0 / h0;; j--) {
				if(mult(tmp0, hou0, j, 0) && sub0(tmp1, jit0, tmp0)) {
					break;
				}
			}

			buf.append((char)(j + '0'));
			System.arraycopy(tmp1, 1, jit0, 0, tmp1.length - 1);
			jit0[tmp1.length - 1] = 0;
			return prev = new BigDecimal(buf.toString());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	public BigDecimal next() {
		if(Arrays.equals(jit0, zero)) {
			return prev;
		} else {
			_next();
			return _next();
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}

}
