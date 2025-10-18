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

import net.morilib.math.analysis.inexact.DoubleContinuedFractionFunction;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/17
 */
public class BetaFunction {

	//
	static class IBeta extends DoubleContinuedFractionFunction {

		//
		private double a, b;

		//
		private IBeta(double a, double b) {
			this.a = a;
			this.b = b;
		}

		@Override
		protected double getA1(double x) {
			return 1.0;
		}

		@Override
		protected double getAn(int n, double x) {
			double r;
			int k = (n - 1) / 2;

			if(n % 2 > 0) {  // 2k
				r = k / (a + 2 * k - 1) * (b - k) / (a + 2 * k) * x; 
			} else {  // 2k + 1
				r = -(a + k) / (a + 2 * k);
				r = r * (a + b + k) / (a + 2 * k + 1) * x;
			}
			return r;
		}

		@Override
		protected double getB0(double x) {
			return 0;
		}

		@Override
		protected double getB1(double x) {
			return 1;
		}

		@Override
		protected double getBn(int n, double x) {
			return 1;
		}

	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static double logBeta(double a, double b) {
		return (Gamma2.lnGamma(a) + Gamma2.lnGamma(b) -
				Gamma2.lnGamma(a + b));
	}

	/**
	 * Reference:
	 * <ol>
	 * <li><a href="http://functions.wolfram.com/06.21.10.0001.01">
	 * http://functions.wolfram.com/06.21.10.0001.01
	 * </a>
	 * </ol>
	 * 
	 * @param x
	 * @param a
	 * @param b
	 * @return
	 */
	public static double I(double x, double a, double b) {
		double k;

		if(Double.isNaN(x) || Double.isNaN(a) || Double.isNaN(b)) {
			return Double.NaN;
		} else if(x <= 0 || x > 1 || a <= 0 || b <= 0) {
			return Double.NaN;
		} else if(x == 1) {
			return 1.0;
		} else {
			k = a * Math.log(x) + b * Math.log(1 - x);
			k = k - Math.log(a) - logBeta(a, b);
			return Math.exp(k) * new IBeta(a, b).f(x);
		}
	}

}
