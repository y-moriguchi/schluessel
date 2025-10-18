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

import java.math.BigInteger;

import net.morilib.lang.algebra.AlgebricInteger;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public abstract class Integer2 extends AbstractNumerical<Integer2>
implements AlgebricInteger<Integer2> {

	//
	private static final BigInteger _I_MAX =
		BigInteger.valueOf(Integer.MAX_VALUE);
	private static final BigInteger _I_MIN =
		BigInteger.valueOf(Integer.MIN_VALUE);
//	private static final BigDecimal _D_MAX =
//		new BigDecimal(Double.MAX_VALUE);
//	private static final BigDecimal _D_MIN =
//		new BigDecimal(Double.MIN_VALUE);
//	private static final BigDecimal _F_MAX =
//		new BigDecimal(Float.MAX_VALUE);
//	private static final BigDecimal _F_MIN =
//		new BigDecimal(Float.MIN_VALUE);

	/**
	 * 
	 */
	public static final Integer2 ZERO = Integer2.valueOf(0);

	/**
	 * 
	 */
	public static final Integer2 ONE  = Integer2.valueOf(1);

	//
	/*package*/ Integer2() { }

	/**
	 * 
	 * @param val
	 * @return
	 */
	public static Integer2 valueOf(BigInteger val) {
		if(val.compareTo(_I_MAX) > 0 || val.compareTo(_I_MIN) < 0) {
			return new BigInt(val);
		} else {
			return new SmallInt(val.intValue());
		}
	}

	/**
	 * 
	 * @param val
	 * @return
	 */
	public static Integer2 valueOf(int val) {
		return new SmallInt(val);
	}

	/**
	 * 
	 * @param val
	 * @return
	 */
	public static Integer2 valueOf(long val) {
		if(val > Integer.MAX_VALUE || val < Integer.MIN_VALUE) {
			return new BigInt(BigInteger.valueOf(val));
		} else {
			return new SmallInt((int)val);
		}
	}

	/**
	 * 
	 * @return
	 */
	public byte toByte() {
		return (byte)toInt();
	}

	/**
	 * 
	 * @return
	 */
	public short toShort() {
		return (short)toInt();
	}

	/**
	 * 
	 * @return
	 */
	public abstract int toInt();

	/**
	 * 
	 * @return
	 */
	public abstract long toLong();

	/**
	 * 
	 * @return
	 */
	public abstract BigInteger toBigInteger();

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castByte()
	 */
	@Override
	public byte castByte() {
		return toByte();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castShort()
	 */
	@Override
	public short castShort() {
		return toShort();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInt()
	 */
	public int castInt() {
		return toInt();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castLong()
	 */
	public long castLong() {
		return toLong();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInteger2()
	 */
	public Integer2 castInteger2() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#intFloor()
	 */
	public int intFloor() {
		return toInt();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#longFloor()
	 */
	public long longFloor() {
		return toLong();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#intCeil()
	 */
	public int intCeil() {
		return toInt();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#longCeil()
	 */
	public long longCeil() {
		return toLong();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isInteger()
	 */
	public final boolean isInteger() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getUniverse()
	 */
	public NumericalRing<Integer2> getUniverse() {
		return Integer2Ring.getInstance();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Floor()
	 */
	public Integer2 getInteger2Floor() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Ceil()
	 */
	public Integer2 getInteger2Ceil() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getRational()
	 */
	public Rational getRational() {
		return Rational.valueOf(this, Integer2.ONE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isDoubleValue()
	 */
//	public boolean isDoubleValue() {
//		BigDecimal d = new BigDecimal(getBigInteger());
//		
//		return d.compareTo(_D_MAX) <= 0 && d.compareTo(_D_MIN) >= 0;
//	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isFloatValue()
	 */
//	public boolean isFloatValue() {
//		BigDecimal d = new BigDecimal(getBigInteger());
//		
//		return d.compareTo(_F_MAX) <= 0 && d.compareTo(_F_MIN) >= 0;
//	}

}
