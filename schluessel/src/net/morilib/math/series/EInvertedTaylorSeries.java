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
public class EInvertedTaylorSeries implements Iterator<Rational> {

	/**
	 * 
	 */
	public static final Iterable<Rational>
	FACTORY = new Iterable<Rational>() {

		public Iterator<Rational> iterator() {
			return new EInvertedTaylorSeries();
		}

	};

	//
	private static final Rational STB = Rational.valueOf(3);

	//
	private Rational fact = Rational.ONE;
	private Rational inv  = Rational.ONE;
	private int k = 1;

	public boolean hasNext() {
		return true;
	}

	public Rational next() {
		Rational r = inv;

		fact = fact.divide(Rational.valueOf(k++)).negate();
		inv  = inv.add(fact);
		return (r.signum() == 0) ? STB : r.invert();
	}

	public void remove() {
		throw new UnsupportedOperationException();
	}

}
