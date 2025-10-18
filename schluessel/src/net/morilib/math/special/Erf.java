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
package net.morilib.math.special;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/15
 */
public class Erf {

	//
	private static final double SQRTPI = Math.sqrt(Math.PI);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static double erf(double x) {
//		double r, s, p;

		if(Double.isNaN(x)) {
			return x;
		} else if(Double.isInfinite(x)) {
			return x > 0 ? 1.0 : -1.0;
		} else if(x == 0) {
			return 0;
		} else if(x > 0) {
//			p = 1.0;
//			r = x;
//			s = Double.MAX_VALUE;
//			for(int n = 1; r != s; n++) {
//				s = r;
//				p = -(x * x / n) * p;
//				r = r + p * x / (2 * n + 1);
//			}
//			return r * 2 / SQRTPI;
			return Gamma2.incompleteGammaLower(0.5, x * x) / SQRTPI;
		} else {
			return -erf(-x);
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static double erfc(double x) {
		return 1.0 - erf(x);
	}

}
