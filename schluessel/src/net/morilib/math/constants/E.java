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

import net.morilib.lang.number.Rational;
import net.morilib.math.series.EInvertedTaylorSeries;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/26
 */
public class E extends AlternatingSeriesNumber {

	/**
	 * 
	 */
	public static final E E = new E(Rational.ONE, Rational.ZERO);

	/**
	 * 
	 * @param scale
	 * @param shift
	 */
	public E(Rational scale, Rational shift) {
		super(EInvertedTaylorSeries.FACTORY, scale, shift);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return scale.hashCode() + 88;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		return ((o instanceof E) && scale.equals(((E)o).scale));
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return scale.isUnit() ? "e" : scale.toString() + "e";
	}

}
