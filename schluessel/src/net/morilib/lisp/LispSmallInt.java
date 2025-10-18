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
package net.morilib.lisp;

import java.math.BigDecimal;
import java.math.BigInteger;

import net.morilib.lang.Decimal32;
import net.morilib.lang.Decimal64;
import net.morilib.lang.number.Integer2;
import net.morilib.lang.number.Rational;
import net.morilib.lisp.math.InvalidOrdinalNumberException;
import net.morilib.lisp.math.LispOrdinalNumber;
import net.morilib.util.Inclementor;
import net.morilib.util.IntMath;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class LispSmallInt extends LispInteger
implements JavaObjective, java.io.Serializable {

	//
	private static final long serialVersionUID = -2512252765408329066L;

	//
	private int value;

	//
	/*package*/ LispSmallInt(int value) {
		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	public LispNumber add(LispNumber x) {
		if(x instanceof LispSmallInt) {
			long vr1 = value;
			long vr2 = x.getExactSmallInt();

			return LispInteger.valueOf(vr1 + vr2);
		} else if(x instanceof LispBigInt) {
			BigInteger vb = BigInteger.valueOf(value);

			return LispInteger.valueOf(vb.add(x.getBigInteger()));
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger vb = BigInteger.valueOf(value);
			BigInteger nd = r.getDenominator();
			BigInteger nn = vb.multiply(
					r.getDenominator()).add(r.getNumerator());

			return LispRational.newRational(nn, nd);
		} else {
			return super.add(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#div(net.morilib.lisp.LispNumber)
	 */
	public LispNumber div(LispNumber x) {
		if(x instanceof LispSmallInt) {
			return LispRational.newRational(
					value, x.getExactSmallInt());
		} else if(x instanceof LispBigInt) {
			BigInteger vb = BigInteger.valueOf(value);

			return LispRational.newRational(vb, x.getBigInteger());
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger vb = BigInteger.valueOf(value);
			BigInteger nd = r.getNumerator();
			BigInteger nn = vb.multiply(r.getDenominator());

			return LispRational.newRational(nn, nd);
		} else {
			return super.div(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#mul(net.morilib.lisp.LispNumber)
	 */
	public LispNumber mul(LispNumber x) {
		if(x instanceof LispSmallInt) {
			long vr1 = value;
			long vr2 = x.getExactSmallInt();

			return LispInteger.valueOf(vr1 * vr2);
		} else if(x instanceof LispBigInt) {
			BigInteger vb = BigInteger.valueOf(value);

			return LispInteger.valueOf(vb.multiply(x.getBigInteger()));
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger vb = BigInteger.valueOf(value);
			BigInteger nd = r.getDenominator();
			BigInteger nn = vb.multiply(r.getNumerator());

			return LispRational.newRational(nn, nd);
		} else {
			return super.mul(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#sub(net.morilib.lisp.LispNumber)
	 */
	public LispNumber sub(LispNumber x) {
		if(x instanceof LispSmallInt) {
			long vr1 = value;
			long vr2 = x.getExactSmallInt();

			return LispInteger.valueOf(vr1 - vr2);
		} else if(x instanceof LispBigInt) {
			BigInteger vb = BigInteger.valueOf(value);

			return LispInteger.valueOf(vb.subtract(x.getBigInteger()));
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger vb = BigInteger.valueOf(value);
			BigInteger nd = r.getDenominator();
			BigInteger nn = vb.multiply(
					r.getDenominator()).subtract(r.getNumerator());

			return LispRational.newRational(nn, nd);
		} else {
			return super.sub(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#uminus()
	 */
	public LispInteger uminus() {
		return new LispSmallInt(-value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#signum()
	 */
	public int signum() {
		return IntMath.signum(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isEqualTo(net.morilib.lisp.LispNumber)
	 */
	public boolean isEqualTo(LispNumber x) {
		if(x instanceof LispSmallInt) {
			return value == x.getExactSmallInt();
		} else if(x instanceof LispBigInt) {
			return false;
		} else if(x instanceof LispRational) {
			return false;
		} else if(x instanceof LispDouble) {
			return value == ((LispDouble)x).doubleValue();
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isLessThan(net.morilib.lisp.LispReal)
	 */
	public boolean isLessThan(LispReal x) {
		if(x instanceof LispSmallInt) {
			return value < x.getExactSmallInt();
		} else if(x instanceof LispBigInt) {
			BigInteger vb = BigInteger.valueOf(value);

			return vb.compareTo(x.getBigInteger()) < 0;
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger vb = BigInteger.valueOf(value);
			BigInteger n1 = vb.multiply(r.getDenominator());
			BigInteger n2 = r.getNumerator();

			return n1.compareTo(n2) < 0;
		} else if(x instanceof LispDouble) {
			return value < x.getRealDouble();
		} else if(x instanceof LispAlternatingSeriesNumber) {
			return ((LispAlternatingSeriesNumber)x).value.compareTo(
					getRational()) > 0;
		}
		throw new IllegalArgumentException(x.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isMoreThan(net.morilib.lisp.LispReal)
	 */
	public boolean isMoreThan(LispReal x) {
		if(x instanceof LispSmallInt) {
			return value > x.getExactSmallInt();
		} else if(x instanceof LispBigInt) {
			BigInteger vb = BigInteger.valueOf(value);

			return vb.compareTo(x.getBigInteger()) > 0;
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger vb = BigInteger.valueOf(value);
			BigInteger n1 = vb.multiply(r.getDenominator());
			BigInteger n2 = r.getNumerator();

			return n1.compareTo(n2) > 0;
		} else if(x instanceof LispDouble) {
			return value > x.getRealDouble();
		} else if(x instanceof LispAlternatingSeriesNumber) {
			return ((LispAlternatingSeriesNumber)x).value.compareTo(
					getRational()) < 0;
		}
		throw new IllegalArgumentException(x.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#toInexact()
	 */
	public LispReal toInexact() {
		return new LispDouble(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	public String print() {
		return Integer.toString(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	public String getResult() {
		return Integer.toString(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int)
	 */
	public LispString toLispString(int radix) {
		if(radix < 2 || radix > 36) {
			throw new IndexOutOfBoundsException("radix is out of range");
		}

		return new LispString(Integer.toString(value, radix));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isOne()
	 */
	public boolean isOne() {
		return value == 1;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigInteger()
	 */
	@Override
	public BigInteger getBigInteger() {
		return BigInteger.valueOf(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getInt()
	 */
	@Override
	public int getInt() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getLong()
	 */
	public long getLong() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigDecimal()
	 */
	public BigDecimal getBigDecimal() {
		return new BigDecimal(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getExactSmallInt()
	 */
	@Override
	public int getExactSmallInt() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDouble()
	 */
	@Override
	public double getRealDouble() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDecimal64()
	 */
	@Override
	public long getRealDecimal64() {
		return Decimal64.toDecimal(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDecimal32()
	 */
	@Override
	public int getRealDecimal32() {
		return Decimal32.toDecimal(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#equalIncliment(net.morilib.util.Inclementor)
	 */
	public boolean equalIncliment(Inclementor<?> i) {
		if(i instanceof LispSmallInt) {
			return value == ((LispSmallInt)i).value;
		} else {
			return i.equalInt(value);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#equalInt(int)
	 */
	public boolean equalInt(int i) {
		return value == i;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#equalInt(java.math.BigInteger)
	 */
	public boolean equalInt(BigInteger i) {
		return i.equals(BigInteger.valueOf(value));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#suc()
	 */
	public Inclementor<LispInteger> suc() {
		return suc(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#suc(int)
	 */
	public Inclementor<LispInteger> suc(int step) {
		long r = (long)value + (long)step;

		return LispInteger.valueOf(r);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#toInt()
	 */
	public int toInt() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#toRational()
	 */
	@Override
	public Rational toRational() {
		return Rational.valueOf(value, 1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return value == 1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public LispReal invert() {
		return LispRational.newRational(1, value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInt()
	 */
	public int castInt() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castLong()
	 */
	public long castLong() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInteger2()
	 */
	public Integer2 castInteger2() {
		return Integer2.valueOf(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intFloor()
	 */
	public int intFloor() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longFloor()
	 */
	public long longFloor() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Floor()
	 */
	public Integer2 getInteger2Floor() {
		return Integer2.valueOf(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intCeil()
	 */
	public int intCeil() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longCeil()
	 */
	public long longCeil() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Ceil()
	 */
	public Integer2 getInteger2Ceil() {
		return Integer2.valueOf(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getRational()
	 */
	public Rational getRational() {
		return toRational();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#floatValue()
	 */
	public float floatValue() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#doubleValue()
	 */
	public double doubleValue() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toObject()
	 */
	public Object toObject() {
		return Integer.valueOf(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.LispOrdinalNumber#inclement()
	 */
	public LispOrdinalNumber inclement() {
		if(value < 0) {
			throw new InvalidOrdinalNumberException();
		} else if(value == Integer.MAX_VALUE) {
			return LispInteger.valueOf((long)Integer.MAX_VALUE + 1l);
		} else {
			return LispInteger.valueOf(value + 1);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#remainder(net.morilib.lisp.LispReal)
	 */
	@Override
	public LispReal remainder(LispReal r) {
		BigInteger n, d;

		if(!r.isExact()) {
			return new LispDouble(Math.IEEEremainder(
					(double)value, r.doubleValue()));
		} else if(r instanceof LispSmallInt) {
			return LispInteger.valueOf(
					value % ((LispSmallInt)r).value);
		} else if(r instanceof LispBigInt) {
			n = BigInteger.valueOf(value);
			n = n.remainder(r.getBigInteger());
			return LispInteger.valueOf(n);
		} else if(r instanceof LispRational) {
			d = r.getDenominator();
			n = BigInteger.valueOf(value).multiply(d);
			n = n.remainder(r.getNumerator());
			return LispRational.newRational(n, d);
		} else {
			return new LispDouble(Math.IEEEremainder(
					(double)value, r.doubleValue()));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object x) {
		if(x instanceof LispSmallInt) {
			return value == ((LispSmallInt)x).value;
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int l = 17;

		l = 37 * l + value;
		return l;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return Integer.toString(value);
	}

}
