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

import java.lang.ref.WeakReference;
import java.math.BigInteger;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

import net.morilib.math.special.GammaFunctions;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/11/21
 */
public final class Math2 {

	//
	private static class T1 {

		private byte[] arr;

		private T1(byte[] arr) {
			this.arr = new byte[arr.length];
			System.arraycopy(arr, 0, this.arr, 0, arr.length);
		}

		public int hashCode() {
			return Arrays.hashCode(arr);
		}

		public boolean equals(Object o) {
			return (o instanceof T1 &&
					Arrays.equals(arr, ((T1)o).arr));
		}
	}

	//
	private static final int BASE = 16;
	private static final int LOG2BASE = 4;
	private static final BigInteger LIMIT_NRT =
		BigInteger.valueOf(Integer.MAX_VALUE);
	private static WeakReference<short[]> shortPrimes = null;

	//
	private static Map<Integer, BigInteger> facmemo =
		new WeakHashMap<Integer, BigInteger>();
	private static Map<Long, BigInteger> cmbmemo =
		new WeakHashMap<Long, BigInteger>();

	/**
	 * 
	 */
	public static final double EULER_CONSTANT =
		0.57721566490153286060651209008240243;

	/**
	 * 
	 */
	public static final double GOLDEN_RATIO =
		1.618033988749894848204586834365;

	/**
	 * 
	 */
	public static final int HARDY_RAMANUJAN_NUMBER = 1729;

	//
	private Math2() {}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static final int minus1ToThe(int n) {
		return ((n & 1) == 0) ? 1 : -1;
	}

	/**
	 * 
	 * @param alpha
	 * @return
	 */
	public static double decimalPart(double x) {
		double r = Math.IEEEremainder(x, 1.0);

		return (r < 0) ? 1 + r : r;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isInteger(double x) {
		return decimalPart(x) == 0.0;
	}

	//
	private static int toInt(byte b) {
		return (b < 0) ? (b & 0x7f) + 128 : b;
	}

	/**
	 * 
	 * @param q
	 * @return
	 */
	public static BigInteger[] sqrtExact2(BigInteger q) {
		byte[] a;
		int i = 0;
		BigInteger s = BigInteger.ZERO;
		BigInteger t = BigInteger.ZERO;
		BigInteger r = BigInteger.ZERO;

		if(q.signum() == 0) {
			return new BigInteger[] {
					BigInteger.ZERO, BigInteger.ZERO
			};
		} else if(q.signum() < 0) {
			throw new IllegalArgumentException();
		}

		a = q.toByteArray();
		while(a[i] == 0);
		for(; i < a.length; i++) {
			int x = 0;

			if(s.signum() == 0) {
				t = BigInteger.valueOf(toInt(a[i]));
				for(; x * x <= t.intValue(); x++);
				x--;
				s = BigInteger.valueOf(x);
			} else {
				BigInteger s0 = s;

				t = t.shiftLeft(LOG2BASE * 2).add(BigInteger.valueOf(
						toInt(a[i])));
				do {
					s = s0.shiftLeft(LOG2BASE).add(
							BigInteger.valueOf(x));
				} while(s.multiply(
						BigInteger.valueOf(x++)).compareTo(t) <= 0);
				x -= 2;
				s = s0.shiftLeft(LOG2BASE).add(
						BigInteger.valueOf(x));
			}
			t = t.subtract(s.multiply(BigInteger.valueOf(x)));
			s = s.add(BigInteger.valueOf(x));
			r = r.shiftLeft(LOG2BASE).add(BigInteger.valueOf(x));
		}
//		return (t.signum() == 0) ? r : r.negate();
		return new BigInteger[] { r, t };
	}

	/**
	 * 
	 * @param q
	 * @return
	 */
	public static BigInteger sqrtExact(BigInteger q) {
		BigInteger[] r2;

		r2 = sqrtExact2(q);
		return (r2[1].signum() == 0) ? r2[0] : r2[0].negate();
	}
	//
	private static BigInteger[] split(BigInteger b, int n) {
		byte[] c = b.toByteArray();
		byte[] d;
		BigInteger[] r;
		BigInteger   x;
		int i, l = 0;

		for(; c[l] == 0; l++);
		d = new byte[(c.length - l) * 2];
		for(int k = l; k < c.length; k++) {
			d[(k - l) * 2]     = (byte)(toInt(c[k]) >> 4);
			d[(k - l) * 2 + 1] = (byte)(toInt(c[k]) & 0xf);
		}

		r = new BigInteger[(d.length + n - 1) / n];
		i = d.length % n;
		if(i > 0) {
			x = BigInteger.ZERO;
			for(int k = 0; k < i; k++) {
				x = x.shiftLeft(LOG2BASE).add(BigInteger.valueOf(
						toInt(d[k])));
			}
			r[0] = x;
		}

		for(; i < d.length; i += n) {
			x = BigInteger.ZERO;
			for(int k = 0; k < n; k++) {
				x = x.shiftLeft(LOG2BASE).add(BigInteger.valueOf(
						toInt(d[i + k])));
			}
			r[(i + n - 1) / n] = x;
		}
		return r;
	}

	/**
	 * 
	 * @param q
	 * @param n
	 * @return
	 */
	public static BigInteger nrtExact(BigInteger q, int n) {
		BigInteger[] a;
		BigInteger   r = BigInteger.ZERO;
		BigInteger   t = BigInteger.ZERO;
		int i = 0;

		if(q.signum() == 0) {
			return BigInteger.ZERO;
		} else if(q.signum() < 0) {
			throw new IllegalArgumentException();
		} else if(n < 0) {
			throw new IllegalArgumentException();
		}

		a = split(q, n);
		for(; i < a.length; i++) {
			BigInteger x;
			int b = 0;

			t = t.shiftLeft(LOG2BASE * n).add(a[i]);
			r = r.shiftLeft(LOG2BASE);
			for(; b < BASE; b++) {
				x = r.add(BigInteger.valueOf(b)).pow(n);
				if(x.compareTo(t) > 0) {
					break;
				}
			}
			r = r.add(BigInteger.valueOf(b - 1));
		}
		return (t.subtract(q).signum() == 0) ? r : r.negate();
	}

	/**
	 * 
	 * @param q
	 * @param n
	 * @return
	 */
	public static BigInteger nrtExact(BigInteger q, BigInteger n) {
		if(n.signum() < 0 || n.compareTo(LIMIT_NRT) > 0) {
			throw new IllegalArgumentException();
		}
		return nrtExact(q, n.intValue());
	}

	/**
	 * 
	 * @param x
	 * @param b
	 * @return
	 */
	public static BigInteger integerLog(BigInteger x, BigInteger b) {
		BigInteger r = BigInteger.valueOf(-1);
		BigInteger s;
		BigInteger y = x;

		if(x.signum() <= 0) {
			throw new IllegalArgumentException();
		}

		do {
			y = y.divide(b);
			s = y.remainder(b);
			r = r.add(BigInteger.ONE);
		} while(y.signum() > 0);
		return (s.signum() == 0) ? r : r.negate();
	}

	/**
	 * 
	 * @param b1
	 * @param b2
	 * @return
	 */
	public static BigInteger pow(BigInteger b1, BigInteger b2) {
		byte[] a = b2.toByteArray();
		int i = 0;
		BigInteger r = BigInteger.ONE;

		if(b2.signum() < 0) {
			throw new IllegalArgumentException();
		} else if(b2.signum() == 0) {
			return BigInteger.ONE;
		} else if(b1.signum() == 0) {
			return BigInteger.ZERO;
		} else if(b1.equals(BigInteger.ONE)) {
			return BigInteger.ONE;
		} else if(b1.equals(BigInteger.valueOf(-1))) {
			int p = b2.remainder(BigInteger.valueOf(2)).intValue();

			return BigInteger.valueOf((p == 0) ? 1 : -1);
		}

		for(; a[i] == 0; i++);
		for(; i < a.length; i++) {
			r = r.pow(256);
			r = r.multiply(b1.pow(toInt(a[i])));
		}
		return r;
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static int gcd(int a, int b){
		int r = Math.abs(a), t = Math.abs(b);

		if(a == 0 && b == 0) {
			return 0;
		} else {
			for(int c = r; t > 0; r = t, t = c % t);
			return r;
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static long gcd(long a, long b){
		long r = Math.abs(a), t = Math.abs(b);

		if(a == 0 && b == 0) {
			return 0;
		} else {
			for(long c = r; t > 0; r = t, t = c % t);
			return r;
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static int lcm(short a, short b) {
		return (a == 0 || b == 0) ? 0 : a * b / gcd(a, b);
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static long lcm(int a, int b) {
		long r = a, t = b;

		return (r == 0 || t == 0) ? 0 : r * t / gcd(r, t);
	}

	//
	private static boolean mult(byte[] r, byte[] b, int v, int s,
			int radix) {
		int carry = 0;

		for(int i = r.length - 1; i >= 0; i--) {
			int x;
			int k = i - (r.length - b.length) + s;

			x = ((k >= 0 && k < b.length) ?
					toInt(b[k]) : 0) * v + carry;
			carry = (x / radix);
			r[i] = (byte)(x % radix);
		}
		return carry == 0;
	}

	//
	private static boolean sub0(byte[] r, byte[] b, byte[] c,
			int radix) {
		int carry = 0, x;

		for(int i = b.length - 1; i >= 0; i--) {
			x = toInt(b[i]) - toInt(c[i]) + carry;
			carry = (x > 0) ? x / radix : (x - radix + 1) / radix;
			r[i] = (byte)((x >= 0) ?
					(x % radix) : (x + radix) % radix);
		}
		return carry == 0;
	}

	//
	private static byte[] split2(BigInteger a, int radix) {
		StringBuilder b = new StringBuilder();
		BigInteger[] c;
		byte[] r;

		while(a.signum() > 0) {
			c = a.divideAndRemainder(BigInteger.valueOf(radix));
			b.append((char)(c[1].byteValue() + '0'));
			a = c[0];
		}

		r = new byte[b.length()];
		for(int i = 0; i < b.length(); i++) {
			r[i] = (byte)(b.charAt(b.length() - i - 1) - '0');
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

	//
	private static String _toDecimalString(
			BigInteger a, BigInteger b, int radix) {
		StringBuilder buf  = new StringBuilder();
		StringBuilder sho0 = new StringBuilder();
		byte[] hou0 = split2(b, radix);
		byte[] jit0 = ext(split2(a, radix), hou0.length);
		byte[] zero = new byte[jit0.length];
		byte[] tmp0 = new byte[jit0.length];
		byte[] tmp1 = new byte[jit0.length];
		Set<T1> mem = new LinkedHashSet<T1>();
		Iterator<T1> itr;
		int j, j0 = 0, h0, sez = 0;
		T1 m0;

		Arrays.fill(zero, (byte)0);
		h0 = toInt(hou0[0]);
		if(hou0.length > 1) {
			h0 = (h0 * radix) + toInt(hou0[1]);
		} else {
			h0 = h0 * radix;
		}

//		out: for(int i = 0;; i++) {
		out: while(true) {
			j0 = toInt(jit0[0]) + 1;
			j0 = j0 * radix + (jit0.length > 1 ? toInt(jit0[1]) : 0);
			j0 = j0 * radix;
			for(j = j0 / h0;; j--) {
				if(mult(tmp0, hou0, j, 0, radix) &&
						sub0(tmp1, jit0, tmp0, radix)) {
					break;
				}
			}

			sho0.append((char)(j < 10 ? j + '0' : j + 'A' - 10));
			System.arraycopy(tmp1, 1, jit0, 0, tmp1.length - 1);
			jit0[tmp1.length - 1] = 0;
			if(Arrays.equals(jit0, zero)) {
				sez = -1;
				break;
			} else if(mem.contains(m0 = new T1(jit0))) {
				itr = mem.iterator();
				for(sez = 0; sez < mem.size(); sez++) {
					if(itr.next().equals(m0)) {
						break out;
					}
				}
			} else {
				mem.add(m0);
			}
		}

		if(sez < 0) {
			buf = sho0;
		} else {
			buf.append(sho0.substring(0, sez));
			buf.append("{");
			buf.append(sho0.substring(sez, sho0.length() - 1));
			buf.append("}");
		}
		return buf.toString();
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static String toDecimalString(BigInteger a, BigInteger b,
			int radix) {
		BigInteger[] rr = a.divideAndRemainder(b);

		if(radix < 2 || radix > 36) {
			throw new IllegalArgumentException();
		} else if(rr[1].signum() == 0) {
			return rr[0].toString();
		}
		return rr[0] + "." + _toDecimalString(rr[1].abs(), b, radix);
	}

	//
	private static void genShortPrimes() {
		Set<Integer> primes;
		short[] a;
		int c;

		if(shortPrimes == null || shortPrimes.get() == null) {
			synchronized(Math2.class) {
				primes = new LinkedHashSet<Integer>();
				primes.add(2);
				outer: for(int i = 3; i < 65536; i += 2) {
					for(Integer p : primes) {
						if(i % p == 0) {
							continue outer;
						}
					}
					primes.add(i);
				}

				a = new short[primes.size()];
				c = 0;
				for(Integer i : primes) {
					a[c++] = i.shortValue();
				}
				shortPrimes = new WeakReference<short[]>(a);
			}
		}
	}

	/**
	 * 
	 * @param args
	 * @return
	 */
	public static List<Integer> getShortPrimes() {
		genShortPrimes();
		return new AbstractList<Integer>() {

			@Override
			public Integer get(int index) {
				return Integer.valueOf(
						shortPrimes.get()[index] & 0xffff);
			}

			@Override
			public int size() {
				return shortPrimes.get().length;
			}

		};
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int[] factorize(int x) {
		int[] r = new int[33], s;
		int p, a = x, j = 0;

		genShortPrimes();
		if(x < 0) {
			r[j++] = -1;
			x = -x;
		} else if(x == 0) {
			return new int[] { 0 };
		}

		for(int i = 0; a != 0 && i < shortPrimes.get().length; i++) {
			p = (int)shortPrimes.get()[i] & 0xffff;
			while(a % p == 0) {
				r[j++] = p;
				a = a / p;
			}
		}

		s = new int[j];
		System.arraycopy(r, 0, s, 0, j);
		return s;
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static BigInteger factorial(int n) {
		BigInteger r;

		if(n < 0) {
			return null;  // undefined
		} else if(n == 0 || n == 1) {
			return BigInteger.ONE;
		} else if(n == 2) {
			return BigInteger.valueOf(n);
		} else if((r = facmemo.get(Integer.valueOf(n))) == null) {
			r = BigInteger.ONE;
			for(int i = 2; i <= n; i++) {
				r = r.multiply(BigInteger.valueOf(i));
			}
			facmemo.put(Integer.valueOf(n), r);
		}
		return r;
	}

	/**
	 * approximate by Stirling's formula
	 * 
	 * @param n
	 * @return
	 */
	public static double approximateFactorial(int n) {
		if(n < 0) {
			return Double.NaN;
		} else if(n < 11) {
			return factorial(n).doubleValue();
		} else {
			return (Math.sqrt(2 * Math.PI * n) *
					Math.pow(n / Math.E, (double)n));
		}
	}

	/**
	 * approximate by Stirling's formula
	 * 
	 * @param n
	 * @return
	 */
	public static double lnApproximateFactorial(int n) {
		if(n < 0) {
			return Double.NaN;
		} else if(n < 30) {
			return Math.log(factorial(n).doubleValue());
		} else {
			return n * (Math.log(n) - 1);
		}
	}

	/**
	 * 
	 * @param n
	 * @param k
	 * @return
	 */
	public static BigInteger binomialCoefficient(int n, int k) {
		BigInteger r;
		long x;

		if(n < 0 || k < 0 || n < k) {
			return null;
		} else if(k == 0 || k == n) {
			return BigInteger.ONE;
		} else if(k == 1 || k == n - 1) {
			return BigInteger.valueOf(n);
		} else if((r = facmemo.get(Long.valueOf(
				x = ((long)n << 32 | (long)k)))) == null) {
			r = BigInteger.ONE;
			for(int i = Math.max(k, n - k) + 1; i <= n; i++) {
				r = r.multiply(BigInteger.valueOf(i));
			}
			cmbmemo.put(x, r);
		}
		return r;
	}

	/**
	 * 
	 * @param a
	 * @param k
	 * @return
	 */
	public static double binomialCoefficient(double a, int k) {
		// (alpha k) = Gamma(alpha+1) / Gamma(k+1)Gamma(alpha-k+1)
		return (GammaFunctions.gamma(a + 1) /
				GammaFunctions.gamma(k + 1) /
				GammaFunctions.gamma(a - k + 1));
	}

	/**
	 * 
	 * @param perm
	 * @return
	 */
	public static int permutationParity(int[] perm) {
		int t, sgn = 1;

		for(int i = 0; i < perm.length; i++) {
			while(perm[i] != i) {
				t = perm[i];
				if(perm[i] == perm[t])  return 0;
				perm[i] = perm[t];
				perm[t] = t;
				sgn = -sgn;
			}
		}
		return sgn;
	}

	/**
	 * 
	 * @param numer
	 * @param denom
	 * @return
	 */
	public static BigInteger[] toContinuedFraction(
			BigInteger numer, BigInteger denom) {
		List<BigInteger> l = new ArrayList<BigInteger>();
		BigInteger[] x;
		BigInteger n = numer, d = denom;

		while(d.signum() != 0) {
			x = n.divideAndRemainder(d);
			l.add(x[0]);
			n = d;  d = x[1];
		}
		return l.toArray(new BigInteger[0]);
	}

}
