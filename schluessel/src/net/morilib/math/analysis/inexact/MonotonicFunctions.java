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
package net.morilib.math.analysis.inexact;

import net.morilib.lang.transform.DoubleTransform;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/16
 */
public final class MonotonicFunctions {

	//
	private static final double MAX_SOLUTION =  1e140;
	private static final double MIN_SOLUTION = -1e140;

	//
	private MonotonicFunctions() {}

	/**
	 * 
	 * @param f
	 * @param x
	 * @param a
	 * @param b
	 * @return
	 */
	public static double findRootByBisectionMethod(DoubleTransform f,
			double x, double a, double b) {
		double p, q;
		double ya, yb, t;

		p = (a > Double.NEGATIVE_INFINITY) ? a : MIN_SOLUTION;
		q = (b < Double.POSITIVE_INFINITY) ? b : MAX_SOLUTION;
		ya = f.f(p) - x;
		yb = f.f(q) - x;
		if(ya == 0) {
			return p;
		} else if(yb == 0) {
			return q;
		} else if(Math.signum(ya) * Math.signum(yb) > 0) {
			return Double.NaN;
		} else {
			q = (p + q) / 2;
		}

		while(Math.abs(p - q) > Math.max(Math.ulp(p), Math.ulp(q))) {
			ya = f.f(p) - x;
			yb = f.f(q) - x;
			if(Double.isNaN(ya))  ya = -Double.MAX_VALUE;
			if(Double.isNaN(yb))  yb =  Double.MAX_VALUE;

			if(ya == 0) {
				return p;
			} else if(yb == 0) {
				return q;
			} else if(Math.signum(ya) * Math.signum(yb) > 0) {
				t = p;
				p = q;
				q = q + (q - t);
			} else {
				q = (p + q) / 2;
			}
		}
		return p;
	}

}
