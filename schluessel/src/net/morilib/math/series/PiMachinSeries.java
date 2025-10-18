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

import java.util.Iterator;

import net.morilib.lang.number.Rational;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/26
 */
public class PiMachinSeries implements Iterator<Rational> {

	/**
	 * 
	 */
	public static final Iterable<Rational>
	FACTORY = new Iterable<Rational>() {

		public Iterator<Rational> iterator() {
			return new PiMachinSeries();
		}

	};

	//
	private Rational FIVETOTHE2 = Rational.valueOf(5 * 5);
	private Rational _239TOTHE2 = Rational.valueOf(239 * 239);
	private Rational FOUR = Rational.valueOf(4);
	private Rational _16  = Rational.valueOf(16);

	//
	private int m5, m239;
	private Rational over5, over239;
	private Rational sumover5, sumover239;

	/**
	 * 
	 */
	public PiMachinSeries() {
		over5   = Rational.valueOf(5).invert();
		over239 = Rational.valueOf(239).invert();
		sumover5   = over5;
		sumover239 = over239;
		m5 = m239 = 1;
//		nextover5();  nextover5();
	}

	//
	private void nextover5() {
		over5 = over5.divide(FIVETOTHE2).negate();
		sumover5 = sumover5.add(over5.divide(
				Rational.valueOf(2 * m5++ + 1)));
	}

	//
	private void nextover239() {
		over239 = over239.divide(_239TOTHE2).negate();
		sumover239 = sumover239.add(over239.divide(
				Rational.valueOf(2 * m239++ + 1)));
	}

	public boolean hasNext() {
		return true;
	}

	public Rational next() {
		Rational r = sumover5.multiply(_16).subtract(
				sumover239.multiply(FOUR));

		nextover5();  //nextover5();  nextover5();
		nextover239();
		return r;
	}

	public void remove() {
		throw new UnsupportedOperationException();
	}

}
