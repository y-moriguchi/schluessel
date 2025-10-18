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
import net.morilib.math.series.PiMachinSeries;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/26
 */
public class Pi extends AlternatingSeriesNumber {

	/**
	 * 
	 */
	public static final Pi PI = new Pi(Rational.ONE, Rational.ZERO);

	/**
	 * 
	 */
	public static final Pi ONE_DEGREE =
		new Pi(Rational.valueOf(180).invert(), Rational.ZERO);

	/**
	 * 
	 * @param scale
	 */
	public Pi(Rational scale, Rational shift) {
		super(PiMachinSeries.FACTORY, scale, shift);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return scale.hashCode() + 72;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		return ((o instanceof Pi) && scale.equals(((Pi)o).scale));
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		String sh;

		sh = shift.isZero() ? "" : (shift.signum() > 0 ?
				"+" + shift : "" + shift);
		return scale.isUnit() ? "pi" + sh : scale + "pi" + sh;
	}

}
