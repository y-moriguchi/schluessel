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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Iterator;

import net.morilib.lang.number.Rational;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/02
 */
public class ChampernowneConstant extends AlternatingSeriesNumber {

	//
	static final Iterable<Rational>
	FACTORY = new Iterable<Rational>() {

		public Iterator<Rational> iterator() {
			return new Iterator<Rational>() {

				private long cnt = 9;
				private StringBuilder buf =
					new StringBuilder("0.12345678");

				public boolean hasNext() {
					return true;
				}

				public Rational next() {
					BigDecimal r;

					r = new BigDecimal(buf.append(cnt++).toString());
					if(cnt % 2 == 1) {
						r = r.add(new BigDecimal(
								BigInteger.ONE, buf.length() - 2));
					}
					return Rational.valueOf(r);
				}

				public void remove() {
					throw new UnsupportedOperationException();
				}

			};
		}

	};

	/**
	 * 
	 */
	public static final ChampernowneConstant INSTANCE =
		new ChampernowneConstant(FACTORY, Rational.ONE, Rational.ZERO);

	/**
	 * @param iterable
	 * @param scale
	 */
	public ChampernowneConstant(Iterable<Rational> iterable,
			Rational scale, Rational shift) {
		super(iterable, scale, shift);
	}

	/**
	 * @param scale
	 */
	public ChampernowneConstant(Rational scale, Rational shift) {
		super(FACTORY, scale, shift);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return scale.isUnit() ?
				"champernowne-constant" :
					scale.toString() + "champernowne-constant";
	}

}
