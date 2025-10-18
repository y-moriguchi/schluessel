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
package net.morilib.math.series;

import net.morilib.lang.number.Rational;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public class Exp2FractionSeries extends ContinuedFractionSeries {

	//
	private static final Rational TWO   = Rational.valueOf(2);
	private static final Rational FIVE  = Rational.valueOf(5);
	private static final Rational SEVEN = Rational.valueOf(7);

	/**
	 * 
	 */
	public Exp2FractionSeries() {
		init();  next();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.constants.ContinuedFractionSeries#getA1()
	 */
	@Override
	protected Rational getA1() {
		return TWO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.constants.ContinuedFractionSeries#getAn(int)
	 */
	@Override
	protected Rational getAn(int n) {
		return Rational.ONE;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.constants.ContinuedFractionSeries#getB0()
	 */
	@Override
	protected Rational getB0() {
		return SEVEN;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.constants.ContinuedFractionSeries#getB1()
	 */
	@Override
	protected Rational getB1() {
		return FIVE;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.constants.ContinuedFractionSeries#getBn(int)
	 */
	@Override
	protected Rational getBn(int n) {
		return Rational.valueOf(2 * n + 5);
	}

}
