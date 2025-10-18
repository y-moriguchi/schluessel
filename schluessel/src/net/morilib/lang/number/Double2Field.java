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

import java.math.BigDecimal;

import net.morilib.lang.algebra.AbstractField;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public class Double2Field extends AbstractField<Double2>
implements NumericalField<Double2> {

	//
	private static final Double2Field INSTANCE = new Double2Field();

	//
	private Double2Field() { }

	/**
	 * 
	 * @return
	 */
	public static Double2Field getInstance() {
		return INSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRing#getUnit()
	 */
	public Double2 getUnit() {
		return Double2.ONE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Ring#getZero()
	 */
	public Double2 getZero() {
		return Double2.ZERO;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(byte)
	 */
	public Double2 valueOf(byte v) {
		return Double2.valueOf(v);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(short)
	 */
	public Double2 valueOf(short v) {
		return Double2.valueOf(v);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(int)
	 */
	public Double2 valueOf(int v) {
		return Double2.valueOf(v);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(long)
	 */
	public Double2 valueOf(long v) {
		return Double2.valueOf(v);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(net.morilib.lang.number.Integer2)
	 */
	public Double2 valueOf(Integer2 v) {
		return Double2.valueOf(v.doubleValue());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalUniverse#valueOf(float)
	 */
	public Double2 valueOf(float v) {
		return Double2.valueOf(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalUniverse#valueOf(double)
	 */
	public Double2 valueOf(double v) {
		return Double2.valueOf(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalUniverse#valueOf(java.math.BigDecimal)
	 */
	public Double2 valueOf(BigDecimal v) {
		return Double2.valueOf(v.doubleValue());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(net.morilib.lang.number.Rational)
	 */
	public Double2 valueOf(Rational v) {
		return Double2.valueOf(v.doubleValue());
	}

}
