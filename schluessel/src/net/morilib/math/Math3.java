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
package net.morilib.math;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/31
 */
public final class Math3 {

	//
	private Math3() {}

	//
	private static final int BASE = 10;
	private static final BigInteger BBASE = BigInteger.TEN;
	private static final BigInteger BBASE2 =
		BigInteger.valueOf(100);

	//
	private static int toInt(byte b) {
		return (b < 0) ? (b & 0x7f) + 128 : b;
	}

	//
	private static BigInteger[] split(BigInteger a, int n) {
		List<BigInteger> b = new ArrayList<BigInteger>();
		BigInteger[] c, r;
		BigInteger nbase = BBASE.pow(n);

		while(a.signum() > 0) {
			c = a.divideAndRemainder(nbase);
			b.add(c[1]);
			a = c[0];
		}

		r = new BigInteger[b.size()];
		for(int i = 0; i < b.size(); i++) {
			r[i] = b.get(b.size() - i - 1);
		}
		return r;
	}

	//
	static BigInteger nrt(BigInteger q, int n) {
		BigInteger[] a;
		BigInteger r = BigInteger.ZERO;
		BigInteger t = BigInteger.ZERO;
		BigInteger nbase = BBASE.pow(n);
		BigInteger x;
		int i = 0, b;

		a = split(q, n);
		for(; i < a.length; i++) {
			b = 0;
			t = t.multiply(nbase).add(a[i]);
			for(; b < BASE; b++) {
				x = r.add(BigInteger.valueOf(b)).pow(n);
				if(x.compareTo(t) > 0) {
					break;
				}
			}
			r = r.add(BigInteger.valueOf(b - 1)).multiply(BBASE);
		}
		return r.divide(BBASE);
	}

	/**
	 * 
	 * @param q
	 * @param n
	 * @param digits
	 * @return
	 */
	public static BigDecimal nrtDecimal(BigInteger q, int n,
			int digits) {
		BigInteger q1 = q.multiply(BBASE.pow(n * digits)), q2;

		q2 = nrt(q1, n);
		return new BigDecimal(q2, digits);
	}

	/**
	 * 
	 * @param q
	 * @param n
	 * @param digits
	 * @return
	 */
	public static BigDecimal sqrtDecimal(BigDecimal q, int n,
			int digits) {
		BigDecimal q1 = q.multiply(
				new BigDecimal(BBASE.pow(2 * digits)));
		BigInteger q2;

		q2 = nrt(q1.toBigInteger(), n);
		return new BigDecimal(q2, digits);
	}

	//
	private static byte[] toDecimal(BigInteger b) {
		byte[] t = new byte[b.toString().length()];
		BigInteger x = b, y;
		int i = 0;

		while((y = x.divide(BBASE2)).signum() > 0) {
			t[i++] = x.mod(BBASE2).byteValue();
			x = y;
		}
		t[i++] = x.byteValue();

		byte[] r = new byte[i];
		for(int j = 0; j < i; j++) {
			r[i - j - 1] = t[j];
		}
		return r;
	}

	//
	static BigInteger sqrt(BigInteger q) {
		byte[] a;
		BigInteger t, s, v, r;
		int i = 0, x = 0;

		t = s = r = BigInteger.ZERO;
		a = toDecimal(q);
		for(; a[i] == 0; i++);
		for(; i < a.length; i++) {
			x = 0;
			if(s.signum() == 0) {
				t = BigInteger.valueOf(toInt(a[i]));
				for(x = 0; x < BASE; x++) {
					if(x * x > t.intValue())  break;
				}
				s = BigInteger.valueOf(--x);
			} else {
				v = s;
				t = t.multiply(BBASE2).add(BigInteger.valueOf(
						toInt(a[i])));

				do {
					s = v.multiply(BBASE).add(BigInteger.valueOf(x));
				} while(s.multiply(
						BigInteger.valueOf(x++)).compareTo(t) <= 0);
				x -= 2;
				s = v.multiply(BBASE).add(BigInteger.valueOf(x));
			}
			t = t.subtract(s.multiply(BigInteger.valueOf(x)));
			s = s.add(BigInteger.valueOf(x));
			r = r.multiply(BBASE).add(BigInteger.valueOf(x));
		}
		return r;
	}

	/**
	 * 
	 * @param q
	 * @param digits
	 * @return
	 */
	public static BigDecimal sqrtDecimal(BigInteger q, int digits) {
		BigInteger q1 = q.multiply(BBASE.pow(2 * digits)), q2;

		q2 = sqrt(q1);
		return new BigDecimal(q2, digits);
	}

	/**
	 * 
	 * @param q
	 * @param digits
	 * @return
	 */
	public static BigDecimal sqrtDecimal(BigDecimal q, int digits) {
		BigDecimal q1 = q.multiply(
				new BigDecimal(BBASE.pow(2 * digits)));
		BigInteger q2;

		q2 = sqrt(q1.toBigInteger());
		return new BigDecimal(q2, digits);
	}

}
