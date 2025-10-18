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
package net.morilib.math.constants;

import java.util.Iterator;

import net.morilib.lang.number.Rational;
import net.morilib.math.series.Exp2FractionSeries;
import net.morilib.math.series.ExpFractionSeries;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public class ExpNumber extends AlternatingSeriesNumber {

	//
	private static final Rational TWO = Rational.valueOf(2);
	private static final Iterable<Rational>
	EXP2 = new Iterable<Rational>() {

		public Iterator<Rational> iterator() {
			return new Exp2FractionSeries();
		}

	};

	//
	private Rational val;

	/**
	 * @param iterable
	 * @param scale
	 */
	public ExpNumber(Rational x, Rational scale, Rational shift) {
		super(_iter(x), scale, shift);
		val = x;
	}

	//
	private static Iterable<Rational> _iter(Rational x) {
		final Rational x0 = x.divide(TWO);

		if(x.equals(TWO)) {
			return EXP2;
		} else {
			return new Iterable<Rational>() {

				public Iterator<Rational> iterator() {
					return new ExpFractionSeries(x0);
				}

			};
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		String ss, sh;

		ss = val.isUnit() ? "e" : "e^" + val;
		sh = shift.isZero() ? "" : (shift.signum() > 0 ?
				"+" + shift : "" + shift);
		return scale.isUnit() ? ss + sh : scale + ss + sh;
	}

}
