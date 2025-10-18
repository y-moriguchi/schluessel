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
package net.morilib.lang.number.cut;

import net.morilib.lang.number.IEEE754Double;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/25
 */
public final class DoubleSchnitt implements NumberSchnitt<Double> {
	
	//
	private double point;
	private int isupper;
	
	//
	private DoubleSchnitt(double point, int isupper) {
		if(Double.isInfinite(point)) {
			throw new IllegalArgumentException(
					"Infinity is not allowed");
		} else if(Double.isNaN(point)) {
			throw new IllegalArgumentException(
					"NaN is not allowed");
		}
		this.point   = point;
		this.isupper = isupper;
	}
	
	/**
	 * 
	 * @param point
	 * @return
	 */
	public static DoubleSchnitt cutUpper(double point) {
		if(point <= Double.MIN_VALUE) {
			throw new IllegalArgumentException(
					"cannot constract a cut");
		}
		return new DoubleSchnitt(point, 1);
	}
	
	/**
	 * 
	 * @param point
	 * @return
	 */
	public static DoubleSchnitt cutLower(double point) {
		if(point >= Double.MAX_VALUE) {
			throw new IllegalArgumentException(
					"cannot constract a cut");
		}
		return new DoubleSchnitt(point, -1);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.Schnitt#belongs(java.lang.Object)
	 */
	@Override
	public int belongs(Double t) {
		return belongs(t.doubleValue());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.schnitt.NumberSchnitt#belongs(byte)
	 */
	@Override
	public int belongs(byte t) {
		return belongs((double)t);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.schnitt.NumberSchnitt#belongs(short)
	 */
	@Override
	public int belongs(short t) {
		return belongs((double)t);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.schnitt.NumberSchnitt#belongs(int)
	 */
	@Override
	public int belongs(int t) {
		return belongs((double)t);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.schnitt.NumberSchnitt#belongs(long)
	 */
	@Override
	public int belongs(long t) {
		return belongs((double)t);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.schnitt.NumberSchnitt#belongs(float)
	 */
	@Override
	public int belongs(float t) {
		return belongs((double)t);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.schnitt.NumberSchnitt#belongs(double)
	 */
	@Override
	public int belongs(double t) {
		return (t < point) ? -1 : ((t > point) ? 1 : isupper);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.Schnitt#gratestLowerBound()
	 */
	@Override
	public Double gratestLowerBound() {
		return doubleGratestLowerBound();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.Schnitt#leastUpperBound()
	 */
	@Override
	public Double leastUpperBound() {
		return doubleLeastLowerBound();
	}
	
	/**
	 * 
	 * @return
	 */
	public double doubleGratestLowerBound() {
		return (isupper < 0) ? point : IEEE754Double.inclement(point);
	}
	
	/**
	 * 
	 * @return
	 */
	public double doubleLeastLowerBound() {
		return (isupper > 0) ? point : IEEE754Double.declement(point);
	}

}
