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

import net.morilib.lang.transform.DoubleTransform;
import net.morilib.math.analysis.inexact.DoubleContinuedFractionFunction;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/16
 */
public final class Gamma2 {

	//
	static class IGammaL extends DoubleContinuedFractionFunction {

		//
		private double s;

		//
		private IGammaL(double s) {
			this.s = s;
		}

		@Override
		protected double getA1(double x) {
			return 1.0;
		}

		@Override
		protected double getAn(int n, double x) {
			return (n % 2 == 0 ? -s - ((n - 1) / 2) : (n / 2)) * x;
		}

		@Override
		protected double getB0(double x) {
			return 0;
		}

		@Override
		protected double getB1(double x) {
			return s;
		}

		@Override
		protected double getBn(int n, double x) {
			return s + n - 1;
		}

	}

	//
	static class IGammaU extends DoubleContinuedFractionFunction {

		//
		private double s;

		//
		private IGammaU(double s) {
			this.s = s;
		}

		@Override
		protected double getA1(double x) {
			return 1.0;
		}

		@Override
		protected double getAn(int n, double x) {
			return (n % 2 > 0) ? (n - 1) / 2 : n / 2 - s;
		}

		@Override
		protected double getB0(double x) {
			return 0;
		}

		@Override
		protected double getB1(double x) {
			return x;
		}

		@Override
		protected double getBn(int n, double x) {
			return (n % 2 > 0) ? x : 1;
		}

	}

	//
	private static final double LOG_SQRT_2PI =
		Math.log(2 * Math.PI) / 2;

	//
	private static final double[] COEFFS = new double[] {
		0.99999999999999709182,
		57.156235665862923517,
		-59.597960355475491248,
		14.136097974741747174,
		-0.49191381609762019978,
		.33994649984811888699e-4,
		.46523628927048575665e-4,
		-.98374475304879564677e-4,
		.15808870322491248884e-3,
		-.21026444172410488319e-3,
		.21743961811521264320e-3,
		-.16431810653676389022e-3,
		.84418223983852743293e-4,
		-.26190838401581408670e-4,
		.36899182659531622704e-5,
	};

	//
	private Gamma2() {}

	/**
	 * Reference:
	 * <ul>
	 * <li><a href="http://my.fit.edu/~gabdo/gamma.txt">
	 * http://my.fit.edu/~gabdo/gamma.txt
	 * </a></li>
	 * </ul>
	 * 
	 * @param x
	 * @return
	 */
	public static double lnGamma(double x) {
		double g = 607.0 / 128.0;
		double r = 0, s = 0, t;

		if(x > 0) {
			for(int k = COEFFS.length - 1; k > 0; k--) {
				s += COEFFS[k] / (x + k);
			}
			s += COEFFS[0];
			t  = x + g + 0.5;
			r  = (x + 0.5) * Math.log(t);
			r -= t;
			r += LOG_SQRT_2PI;
			r += Math.log(s / x);
			return r;
		} else {
			return Double.NaN;
		}
	}

	/**
	 * 
	 * @param s
	 * @param x
	 * @return
	 */
	public static double incompleteGammaLower(double s, double x) {
		return Math.exp(lnIncompleteGammaLower(s, x));
	}

	/**
	 * 
	 * @param s
	 * @param x
	 * @return
	 */
	public static double lnIncompleteGammaLower(double s, double x) {
		double t = new HypergeometricFunction(1, 1 + s).f(x);

		return Math.log(t) + s * Math.log(x) - x - Math.log(s);
	}

	/**
	 * 
	 * @param s
	 * @param x
	 * @return
	 */
	public static double incompleteGammaUpper(double s, double x) {
		return Math.exp(lnIncompleteGammaUpper(s, x));
	}

	/**
	 * 
	 * @param s
	 * @param x
	 * @return
	 */
	public static double lnIncompleteGammaUpper(double s, double x) {
		return (Math.log(new IGammaU(s).f(x)) +
				s * Math.log(x) - x);
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static DoubleTransform incompleteGammaLowerFunction(
			final double s) {
		final DoubleTransform f = new HypergeometricFunction(1, 1 + s);

		return new DoubleTransform() {

			public double f(double x) {
				return Math.exp(
						Math.log(f.f(x)) + s * Math.log(x) -
						x - Math.log(s));
			}

		};
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static DoubleTransform lnIncompleteGammaLowerFunction(
			final double s) {
		final DoubleTransform f = new HypergeometricFunction(1, 1 + s);

		return new DoubleTransform() {

			public double f(double x) {
				return (Math.log(f.f(x)) + s * Math.log(x) -
						x - Math.log(s));
			}

		};
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static DoubleTransform incompleteGammaUpperFunction(
			final double s) {
		final DoubleTransform f0 = new IGammaU(s);

		return new DoubleTransform() {

			public double f(double x) {
				return Math.exp(
						Math.log(f0.f(x)) + s * Math.log(x) - x);
			}

		};
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static DoubleTransform lnIncompleteGammaUpperFunction(
			final double s) {
		final DoubleTransform f0 = new IGammaU(s);

		return new DoubleTransform() {

			public double f(double x) {
				return (Math.log(f0.f(x)) + s * Math.log(x) - x);
			}

		};
	}

}
