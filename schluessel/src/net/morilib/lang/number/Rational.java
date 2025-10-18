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
package net.morilib.lang.number;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import net.morilib.lang.DoubleUtils;
import net.morilib.lang.algebra.QuotientAndRemainder;
import net.morilib.util.BitUtils;
import net.morilib.util.IntMath;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public final class Rational extends AbstractNumerical<Rational>
implements NumericalFieldElement<Rational> {

	//
	private Integer2 numer;
	private Integer2 denom;

	//
	private Rational(Integer2 num, Integer2 den) {
		numer = num;
		denom = den;
	}

	/**
	 * 
	 */
	public static final Rational ZERO =
		new Rational(Integer2.ZERO, Integer2.ONE);

	/**
	 * 
	 */
	public static final Rational ONE =
		new Rational(Integer2.ONE, Integer2.ONE);

	/**
	 * 
	 * @param num
	 * @param den
	 * @return
	 */
	public static final Rational valueOf(
			Integer2 num, Integer2 den) {
		Integer2 n = num;
		Integer2 d = den;
		int nsig = num.signum();
		int dsig = den.signum();

		if(dsig == 0) {
			throw new ArithmeticException("denominator is zero");
		} else if(nsig == 0) {
			return ZERO;
		} else if(dsig < 0) {
			n = n.negate();
			d = d.negate();
		}

		Integer2 gcd =
			Integer2.valueOf(n.toBigInteger().gcd(d.toBigInteger()));
		n = n.divide(gcd);
		d = d.divide(gcd);

		return new Rational(n, d);
	}

	/**
	 * 
	 * @param num
	 * @param den
	 * @return
	 */
	public static final Rational valueOf(int num, int den) {
		int n1   = num;
		int d1   = den;

		if(d1 == 0) {
			throw new ArithmeticException("denominator is zero");
		} else if(n1 == 0) {
			return ZERO;
		} else if(d1 < 0) {
			n1 = -n1;
			d1 = -d1;
		}

		int gcd = IntMath.gcd(n1, d1);
		n1  = n1 / gcd;
		d1  = d1 / gcd;

		return new Rational(
				Integer2.valueOf(n1), Integer2.valueOf(d1));
	}

	/**
	 * 
	 * @param num
	 * @param den
	 * @return
	 */
	public static final Rational valueOf(int num) {
		return valueOf(num, 1);
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static Rational valueOf(BigDecimal v) {
		BigInteger n = v.unscaledValue();

		if(v.scale() > 0) {
			BigInteger d = BigInteger.TEN.pow(v.scale());

			return valueOf(
					Integer2.valueOf(n),
					Integer2.valueOf(d));
		} else if(v.scale() < 0) {
			BigInteger d = BigInteger.TEN.pow(-v.scale());

			return valueOf(
					Integer2.valueOf(n.multiply(d)),
					Integer2.ONE);
		} else {
			return valueOf(Integer2.valueOf(n), Integer2.ONE);
		}
	}

	/**
	 * 
	 * @param val
	 * @return
	 */
	public static Rational valueOf(double val) {
		if(Double.isInfinite(val) || Double.isNaN(val)) {
			throw new ArithmeticException(
					"Infinity and NaN is not supported");
		}

//		BigDecimal v = BigDecimal.valueOf(val);
//		return valueOf(v);
		int  e = DoubleUtils.getExponent(val);
		long f = DoubleUtils.getExponent(val);
		Integer2 n, d;

		if(val == 0.0) {
			return ZERO;
		} else if(f == 0) {   // 2^n
			if(e >= 0) {
				d = Integer2.ONE;
				n = Integer2.valueOf(2).power(e);
			} else {
				d = Integer2.valueOf(2).power(-e);
				n = Integer2.ONE;
			}
		} else if(e >= DoubleUtils.MIN_NORMALIZED_EXPONENT) {
			int m = BitUtils.getLsb(f) - 1;
			int t = DoubleUtils.FRACTION_BITS - m;

			f = f | (1 << DoubleUtils.FRACTION_BITS);
			if(e - t >= 0) {
				d = Integer2.ONE;
				n = Integer2.valueOf(2).power(e - t);
				n = n.multiply(Integer2.valueOf(f >> m));
			} else {
				d = Integer2.valueOf(2).power(t - e);
				n = Integer2.valueOf(f >> m);
			}
		} else {  // unnormalized
			d = Integer2.valueOf(2).power(DoubleUtils.MIN_EXPONENT);
			n = Integer2.valueOf(f);
		}
		return valueOf(n, d);
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public static Rational valueOf(BigInteger b) {
		return new Rational(Integer2.valueOf(b), Integer2.ONE);
	}

	/**
	 * 
	 * @param r
	 * @return
	 */
	public static BigDecimal toBigDecimal(Rational r) {
		BigDecimal n = new BigDecimal(r.numer.toBigInteger());
		BigDecimal d = new BigDecimal(r.denom.toBigInteger());

		return n.divide(d);
	}

	/**
	 * 
	 * @param r
	 * @param mc
	 * @return
	 */
	public static BigDecimal toBigDecimal(Rational r, MathContext mc) {
		BigDecimal n = new BigDecimal(r.numer.toBigInteger());
		BigDecimal d = new BigDecimal(r.denom.toBigInteger());

		return n.divide(d, mc);
	}

	/**
	 * 
	 * @param r1
	 * @param r2
	 * @return
	 */
	public static Rational rationalize(Rational r1, Rational r2) {
		Rational e, x, y;
		Integer2 xn, xd, yn, yd;
		Integer2 a, b, c, d;

		e = (r2.compareTo(ZERO) < 0) ? r2.negate() : r2;
		x = r1.add(e);
		y = r1.subtract(e);
		if(x.equals(y)) {
			return x;
		}
		xn = x.numer;  xd = x.denom;
		yn = y.numer;  yd = y.denom;

		a = d = Integer2.ONE;
		b = c = Integer2.ZERO;
		while(true) {
			QuotientAndRemainder<Integer2> qx, qy;
			Integer2 xq;

			qx = xn.divideAndRemainder(xd);
			qy = yn.divideAndRemainder(yd);
			xq = qx.getQuotient();
			if(qx.getRemainder().isZero()) {
				return valueOf(
						a.multiply(xq).add(b),
						c.multiply(xq).add(d));
			} else if(xq.compareTo(qy.getQuotient()) < 0) {
				return valueOf(
						a.multiply(xq.add(Integer2.ONE)).add(b),
						c.multiply(xq.add(Integer2.ONE)).add(d));
			} else if(xq.compareTo(qy.getQuotient()) > 0) {
				throw new RuntimeException("implementation error");
			} else {
				Integer2 aa = a, cc = c;

				a = a.multiply(xq).add(b);  b = aa;
				c = c.multiply(xq).add(d);  d = cc;
				xn = yd;  yn = xd;
				xd = qy.getRemainder();  yd = qx.getRemainder();
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	public Integer2 getNumerator() {
		return numer;
	}

	/**
	 * 
	 * @return
	 */
	public Integer2 getDenominator() {
		return denom;
	}

	/**
	 * 
	 * @return
	 */
	public Integer2 getIntegerPart() {
		return numer.divide(denom);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public Rational add(Rational n) {
		Integer2 nd = denom.multiply(n.denom);
		Integer2 nn = numer.multiply(n.denom).add(
				n.numer.multiply(denom));

		return Rational.valueOf(nn, nd);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public Rational divide(Rational n) {
		Integer2 nd = denom.multiply(n.numer);
		Integer2 nn = numer.multiply(n.denom);

		return Rational.valueOf(nn, nd);
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public boolean isEqualTo(Rational n) {
		return (numer.equals(n.numer) && denom.equals(n.denom));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public Rational multiply(Rational n) {
		Integer2 nd = denom.multiply(n.denom);
		Integer2 nn = numer.multiply(n.numer);

		return Rational.valueOf(nn, nd);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public Rational subtract(Rational n) {
		Integer2 nd = denom.multiply(n.denom);
		Integer2 nn = numer.multiply(n.denom).subtract(
				n.numer.multiply(denom));

		return Rational.valueOf(nn, nd);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public Rational negate() {
		return new Rational(numer.negate(), denom);
	}

	/**
	 * 
	 * @return
	 */
	public int signum() {
		return numer.signum();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return numer.equals(Integer2.ONE) && denom.equals(Integer2.ONE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return isEqualTo(ZERO);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public Rational multiply(int n) {
		Integer2 nn = numer.multiply(n);

		return Rational.valueOf(nn, denom);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public Rational power(int n) {
		Integer2 nd = denom.power(n);
		Integer2 nn = numer.power(n);

		return Rational.valueOf(nn, nd);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public Rational invert() {
		return Rational.valueOf(denom, numer);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#doubleValue()
	 */
	public double doubleValue() {
		return numer.doubleValue() / denom.doubleValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#floatValue()
	 */
	public float floatValue() {
		return numer.floatValue() / denom.floatValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInt()
	 */
	public int castInt() {
		return castInteger2().toInt();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castLong()
	 */
	public long castLong() {
		return castInteger2().toLong();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInteger2()
	 */
	public Integer2 castInteger2() {
		return numer.divide(denom);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Floor()
	 */
	public Integer2 getInteger2Floor() {
		QuotientAndRemainder<Integer2> r0 =
			numer.divideAndRemainder(denom);

		if(r0.getRemainder().equals(Integer2.ZERO)) {
			return r0.getQuotient();
		} else if((numer.signum() > 0) ^ (denom.signum() > 0)) {
			return r0.getQuotient();
		} else {
			return r0.getQuotient().subtract(Integer2.ONE);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intFloor()
	 */
	public int intFloor() {
		return getInteger2Floor().toInt();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longFloor()
	 */
	public long longFloor() {
		return getInteger2Floor().toLong();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Ceil()
	 */
	public Integer2 getInteger2Ceil() {
		QuotientAndRemainder<Integer2> r0 =
			numer.divideAndRemainder(denom);

		if(r0.getRemainder().equals(Integer2.ZERO)) {
			return r0.getQuotient();
		} else if((numer.signum() > 0) ^ (denom.signum() > 0)) {
			return r0.getQuotient().add(Integer2.ONE);
		} else {
			return r0.getQuotient();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intCeil()
	 */
	public int intCeil() {
		return getInteger2Ceil().toInt();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longCeil()
	 */
	public long longCeil() {
		return getInteger2Ceil().toLong();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#isInteger()
	 */
	public boolean isInteger() {
		return denom.equals(Integer2.ONE);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Rational o) {
		Integer2 c = numer.multiply(o.denom);
		Integer2 d = o.numer.multiply(denom);

		return c.compareTo(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getUniverse()
	 */
	public NumericalField<Rational> getUniverse() {
		return RationalField.getInstance();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getRational()
	 */
	public Rational getRational() {
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object x) {
		if(x instanceof Rational) {
			Rational n = (Rational)x;

			return (numer.equals(n.numer) && denom.equals(n.denom));
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int l = 17;

		l = 37 * l + numer.hashCode();
		l = 37 * l + denom.hashCode();
		return l;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		if(numer.isZero()) {
			return "0";
		} else if(denom.isUnit()) {
			return numer.toString();
		} else {
			return numer.toString() + "/" + denom.toString();
		}
	}

}
