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
package net.morilib.lang.number;

import net.morilib.lang.algebra.Multipliable;
import net.morilib.lang.algebra.Subtractable;
import net.morilib.util.BitUtils;
import net.morilib.util.IntMath;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class Q32FractionalPart
implements Subtractable<Q32FractionalPart>,
Multipliable<Q32FractionalPart> {

	/**
	 * 
	 */
	public static Q32FractionalPart ZERO = new Q32FractionalPart(0);

	//
	private int q32;

	//
	private Q32FractionalPart(int numer, int denom) {
		q32 = 0;
		if(numer != 0) {
			int n = numer;
			int d = BitUtils.getMsb(denom);

			for(int i = 0; i < d; i++) {
				if((n & (1 << (d - i - 1))) != 0) {
					q32 |= 1 << (31 - i);
				}
			}
		}
	}

	//
	private Q32FractionalPart(int q32) {
		this.q32 = q32;
	}

	/**
	 * 
	 * @param numer
	 * @param denom
	 * @return
	 */
	public static Q32FractionalPart valueOf(int numer, int denom) {
		int n2 = numer % denom;

		if(denom == 0) {
			throw new ArithmeticException("divide by zero");
		} else if(numer == 0) {
			return ZERO;
		} else if(BitUtils.countBit(denom) > 1) {
			throw new IllegalArgumentException("not 2^n");
		} else {
			int dd = IntMath.gcd(numer, denom);

			return new Q32FractionalPart(n2 / dd, denom / dd);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public Q32FractionalPart add(Q32FractionalPart x) {
		return new Q32FractionalPart(q32 + x.q32);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public Q32FractionalPart multiply(int n) {
		return new Q32FractionalPart(q32 * n);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public Q32FractionalPart multiply(Q32FractionalPart x) {
		return new Q32FractionalPart((q32 >> 16) * (x.q32 >> 16));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public Q32FractionalPart power(int n) {
		if(n < 0) {
			throw new IllegalArgumentException(Integer.toString(n));
		} else if(n == 0) {
			return ZERO;
		} else if(n == 1) {
			return this;
		} else {
			Q32FractionalPart q = this;

			for(int i = 1; i < n; i++) {
				q.multiply(this);
			}
			return q;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public Q32FractionalPart subtract(Q32FractionalPart x) {
		return new Q32FractionalPart(q32 - x.q32);
	}

	/**
	 * 
	 * @return
	 */
	public double doubleValue() {
		int lsb;
		double res = 0, p = 0.5;

		if(q32 == 0) {
			return 0.0;
		} else {
			lsb = BitUtils.getLsb(q32) - 1;
			for(int i = 31; i >= lsb; i--) {
				if((q32 & (1 << i)) != 0) {
					res += p;
				}
				p *= 0.5;
			}
			return res;
		}
	}

	/**
	 * 
	 * @return
	 */
	public long getDenominator() {
		if(q32 == 0) {
			return 1;
		} else {
			return 1 << (33 - BitUtils.getLsb(q32));
		}
	}

	/**
	 * 
	 * @return
	 */
	public long getNumerator() {
		if(q32 == 0) {
			return 0;
		} else {
			int lsb = BitUtils.getLsb(q32) - 1;
			long res = (long)q32;

			if(res < 0) {
				res = (-res) & 0x80000000l; 
			}
			return res >> lsb;
		}
	}

	/**
	 * 
	 * @return
	 */
	public boolean isZero() {
		return q32 == 0;
	}

	/**
	 * 
	 * @return
	 */
	public Q32FractionalPart addHalf() {
		return new Q32FractionalPart(-q32);
	}

}
