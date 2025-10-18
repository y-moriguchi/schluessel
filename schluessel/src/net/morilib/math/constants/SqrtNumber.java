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

import java.math.BigInteger;
import java.util.Iterator;
import java.util.Locale;

import net.morilib.lang.number.Rational;
import net.morilib.math.series.SqrtSeries;
import net.morilib.math.series.decimal.ConstantSeries;
import net.morilib.math.series.decimal.RationalDecimalSeries;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public class SqrtNumber extends AlternatingSeriesNumber {

	//
	private Rational val;

	/**
	 * @param iterable
	 * @param scale
	 */
	public SqrtNumber(Rational x, Rational scale, Rational shift) {
		super(_iter(x), scale, shift);
		val = x;
	}

	//
	private static Iterable<Rational> _iter(Rational x) {
		final BigInteger n = x.getNumerator().toBigInteger();
		final BigInteger d = x.getDenominator().toBigInteger();
		final BigInteger[] r = n.divideAndRemainder(d);

		if(r[1].equals(BigInteger.ZERO)) {
			return new Iterable<Rational>() {

				public Iterator<Rational> iterator() {
					return new SqrtSeries(new ConstantSeries(r[0]));
				}

			};
		} else {
			return new Iterable<Rational>() {

				public Iterator<Rational> iterator() {
					return new SqrtSeries(
							new RationalDecimalSeries(n, d));
				}

			};
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		String st, en, sh;

		if(Locale.getDefault().getLanguage().equals("ja")) {
			st = "√";  en = "";
		} else {
			st = "sqrt(";  en = ")";
		}

		sh = shift.isZero() ? "" : (shift.signum() > 0 ?
				"+" + shift : "" + shift);
		if(scale.isUnit()) {
			return st + val + en + sh;
		} else {
			return scale + st + val + en + sh;
		}
	}

}
