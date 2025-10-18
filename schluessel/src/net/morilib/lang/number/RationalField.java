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
 * @author MORIGUCHI, Yuichiro 2009
 */
public class RationalField extends AbstractField<Rational>
implements NumericalField<Rational> {

	//
	private static final RationalField INSTANCE = new RationalField();

	//
	private RationalField() { }

	/**
	 * 
	 * @return
	 */
	public static RationalField getInstance() {
		return INSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRing#getUnit()
	 */
	public Rational getUnit() {
		return Rational.ONE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Ring#getZero()
	 */
	public Rational getZero() {
		return Rational.ZERO;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(byte)
	 */
	public Rational valueOf(byte v) {
		return Rational.valueOf(v, 1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(short)
	 */
	public Rational valueOf(short v) {
		return Rational.valueOf(v, 1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(int)
	 */
	public Rational valueOf(int v) {
		return Rational.valueOf(v, 1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(long)
	 */
	public Rational valueOf(long v) {
		return Rational.valueOf(Integer2.valueOf(v), Integer2.ONE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalUniverse#valueOf(float)
	 */
	public Rational valueOf(float v) {
		return Rational.valueOf(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalUniverse#valueOf(double)
	 */
	public Rational valueOf(double v) {
		return Rational.valueOf(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalUniverse#valueOf(java.math.BigDecimal)
	 */
	public Rational valueOf(BigDecimal v) {
		return Rational.valueOf(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(net.morilib.lang.number.Integer2)
	 */
	public Rational valueOf(Integer2 v) {
		return Rational.valueOf(v, Integer2.ONE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalField#valueOf(net.morilib.lang.number.Rational)
	 */
	public Rational valueOf(Rational v) {
		return v;
	}

}
