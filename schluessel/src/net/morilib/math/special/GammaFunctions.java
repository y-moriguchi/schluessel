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

import net.morilib.lang.number.complex.ComplexDouble;
import net.morilib.lang.number.complex.ComplexDoubleTransform;
import net.morilib.lang.number.complex.ComplexMath;
import net.morilib.lang.number.complex.RectanglarComplexDouble;
import net.morilib.lang.number.complex.RectanglarComplexDoubleRegister;
import net.morilib.lang.transform.DoubleTransform;
import net.morilib.math.Math2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/11/21
 */
public class GammaFunctions {

	//
	static double byInfiniteProduct(double x) {
		double r = 1, rold = Double.MAX_VALUE;
		double w = x - 1;
		int p = 1;

		while((Math.abs(r - rold) /
				Math.abs(r + rold)) > 0.00000000000001) {
			rold  = r;
			r    *= (1 + w / p) * Math.exp(-w / p);
			if(p++ >= 100) {
				break;
			}
//			System.out.println(w + ": " + r);
		}
		return 1 / r / Math.exp(Math2.EULER_CONSTANT * w);
	}

	//
	static double logGamma(double x) {
		double r = 0, rold = Double.MAX_VALUE;
		double w = x - 1;
		int n = 2;

		while(Math.abs(r - rold) > 0.00000000000001) {
			rold  = r;
			r    += (ZetaFunctions.RIEMANN_ZETA_SEQUENCE.f(n) / n *
					Math.pow(w, n) * (((n & 1) == 0) ? 1 : -1));
			n++;
//			System.out.println(x + ":" + r);
		}
		return r - Math2.EULER_CONSTANT * w;
	}

	//
	static double logGamma2(double x) {
		double r = 0, rold = Double.MAX_VALUE;
		double w = x - 2;
		int n = 2;

		while(Math.abs(r - rold) > Double.MIN_VALUE) {
			rold  = r;
			r    += ((ZetaFunctions.RIEMANN_ZETA_SEQUENCE.f(n) - 1)
					/ n * Math.pow(w, n) * (((n & 1) == 0) ? 1 : -1));
			n++;
//			System.out.println(x + ":" + r);
		}
		return r + (1 - Math2.EULER_CONSTANT) * w;
	}

	//
	static ComplexDouble logGamma(ComplexDouble z) {
		ComplexDouble w = z.subtract(1);
		ComplexDouble rold = ComplexDouble.REAL_POSITIVE_INFINITY;
		RectanglarComplexDoubleRegister r, w2;
		int n = 2;

		r  = new RectanglarComplexDoubleRegister(0, 0);
		w2 = new RectanglarComplexDoubleRegister(w);
		while(rold.subtract(r.toComplex()).abs() > 0.00000000000001) {
			double zetan = ZetaFunctions.RIEMANN_ZETA_SEQUENCE.f(n);

			rold  = r.toComplex();
			w2.multiply(w);
			if((n & 1) == 0) {
				r.add(w2.toComplex().multiply(zetan / n));
			} else {
				r.subtract(w2.toComplex().multiply(zetan / n));
			}
			n++;
//			System.out.println(x + ":" + r);
		}
		return r.subtract(
				w.multiply(Math2.EULER_CONSTANT)).toComplex();
	}

	//
	static ComplexDouble logGamma2(ComplexDouble z) {
		ComplexDouble w = z.subtract(2);
		ComplexDouble rold = ComplexDouble.REAL_POSITIVE_INFINITY;
		RectanglarComplexDoubleRegister r, w2;
		int n = 2;

		r  = new RectanglarComplexDoubleRegister(0, 0);
		w2 = new RectanglarComplexDoubleRegister(w);
		while(rold.subtract(r.toComplex()).abs() > 0.00000000000001) {
			double zetan = ZetaFunctions.RIEMANN_ZETA_SEQUENCE.f(n);

			rold  = r.toComplex();
			w2.multiply(w);
			if((n & 1) == 0) {
				r.add(w2.toComplex().multiply((zetan - 1) / n));
			} else {
				r.subtract(w2.toComplex().multiply((zetan - 1) / n));
			}
			n++;
//			System.out.println(z + ":" + r.toComplex());
		}
		return r.add(
				w.multiply(1 - Math2.EULER_CONSTANT)).toComplex();
	}

	//
	static double byAsymptoticExpansion(double x) {
		double r = 0;
		double w = x - 1;

		r  = 1;
		r += (1.0 / 12) / w;
		r += (1.0 / 288) / w / w;
		r += (139.0 / 51840) / w / w / w;
		return (r * Math.exp(-w + w * Math.log(w)) *
				Math.sqrt(2 * Math.PI * w));
	}

	//
	static ComplexDouble byAsymptoticExpansion(ComplexDouble z) {
		RectanglarComplexDoubleRegister r;
		ComplexDouble w = z.add(-1);

		r = new RectanglarComplexDoubleRegister(1, 0);
		r.add(w.power(-1).multiply(1.0 / 12));
		r.add(w.power(-2).multiply(1.0 / 288));
		r.add(w.power(-3).multiply(139.0 / 51840));
		r.multiply(ComplexMath.exp(
				w.multiply(ComplexMath.log(w)).subtract(w)));
		r.multiply(ComplexMath.sqrt(w.multiply(2 * Math.PI)));
		return r.toComplex();
	}

	static double gammaBetween01(double x) {
		if(x <= 0.001) {
			return (Math.PI /
					Math.exp(logGamma(1 - x)) /
					Math.sin(Math.PI * x));
		} else if(x == 1.0 || x == 2.0) {
			return 1.0;
		} else if(x <= 0.5) {
			return Math.exp(logGamma(x));
		} else {
			return (Math.PI /
					Math.exp(logGamma(1 - x)) /
					Math.sin(Math.PI * x));
		}
	}

	//
	static double byIntegral(double x) {
		double h = 0.001;
		double r = 0;
		int n = 0;

		r = 0.0;
		while(n * h < 10) {
			double t0 = h * n;
			double t1 = h * (n + 1);

			r   += 4 * Math.exp(-t0) * Math.pow(t0, x - 1) * h / 3;
			r   += 2 * Math.exp(-t1) * Math.pow(t1, x - 1) * h / 3;
			n   += 2;
		}
		r += Math.exp(-h * n) * Math.pow(h * n, x - 1) * h / 3;
		return r;
	}

	//
	static class _G implements DoubleTransform {

		public double f(double x) {
			if(x < 0) {
				double n = Math.floor(x);
				double w = Math.IEEEremainder(x, 1.0);
				double r;

				if(w == 0) {
					return Double.NaN;
				} else if(w < 0) {
					w = 1.0 + w;
				}
				r = f(w);
				for(int i = -1; i >= n - 0.0001; i--) {
					r = r / (i + w);
				}
				return r;
			} else if(x == 0.0) {
				return Double.NaN;
			} else if(x <= 0.5) {
				return (Math.PI /
						Math.exp(logGamma(1 - x)) /
						Math.sin(Math.PI * x));
			} else if(x == 1.0) {
				return 1.0;
			} else if(x <  1.5) {
				return Math.exp(logGamma2(x + 1)) / x;
			} else if(x <= 2.5) {
				return Math.exp(logGamma2(x));
			} else if(x <= 30.0) {
				int n = (int)Math.floor(x);
				double w = Math.IEEEremainder(x, 1.0);
				double r;

				if(w == 0.0) {
					r = 1.0;
					for(int i = 1; i < n; i++) {
						r = r * i;
					}
				} else {
					if(w < 0) {
						w = 1.0 + w;
					}
					r = Math.exp(logGamma2(w + 1));
					for(int i = 1; i < n; i++) {
						r = r * (i + w);
					}
				}
				return r;
			} else {
				return byAsymptoticExpansion(x);
			}
		}

	}

	//
	static class _GC implements ComplexDoubleTransform {

		public ComplexDouble f(ComplexDouble z) {
			if(z.isReal()) {
				return RectanglarComplexDouble.realValueOf(
						GAMMA.f(z.realPart()));
			} else if(z.realPart() < 0) {
				double x = z.realPart();
				double n = Math.floor(x);
				double rm = Math2.decimalPart(z.realPart());
				RectanglarComplexDoubleRegister r;
				ComplexDouble w;

				w = RectanglarComplexDouble.valueOf(
						rm, z.imagPart());
				r = new RectanglarComplexDoubleRegister(f(w));
				for(int i = -1; i >= n - 0.0001; i--) {
					r.divide(w.add(i));
				}
				return r.toComplex();
			} else if(z.isZero()) {
				return ComplexDouble.NaN;
			} else if(z.abs() <= 0.5) {
				RectanglarComplexDoubleRegister rg;

				rg = new RectanglarComplexDoubleRegister(Math.PI, 0);
				rg.divide(ComplexMath.exp(logGamma(
						z.subtract(1).negate())));
				rg.divide(ComplexMath.sin(z.multiply(Math.PI)));
				return rg.toComplex();
			} else if(z.isUnit()) {
				return RectanglarComplexDouble.realValueOf(1.0);
			} else if(z.abs() <  1.5) {
				return ComplexMath.exp(logGamma2(z.add(1))).divide(z);
			} else if(z.abs() <= 2.5) {
				return ComplexMath.exp(logGamma2(z));
			} else if(z.realPart() > 2.5 && z.realPart() <= 30.0) {
				int n = (int)Math.floor(z.realPart());
				double rm = Math2.decimalPart(z.realPart());
				RectanglarComplexDoubleRegister r;
				ComplexDouble w;

				w = RectanglarComplexDouble.valueOf(
						rm, z.imagPart());
				r = new RectanglarComplexDoubleRegister(
						f(w.add(1)));
				for(int i = 1; i < n; i++) {
					r.multiply(w.add(i));
				}
				return r.toComplex();
			} else if(z.abs() <= 30) {
				return ComplexMath.exp(logGamma2(z));
			} else {
				return byAsymptoticExpansion(z);
			}
		}

	}

	/**
	 * 
	 */
	public static final DoubleTransform GAMMA = new _G();

	/**
	 * 
	 */
	public static final ComplexDoubleTransform GAMMAC = new _GC();

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static double gamma(double x) {
//		return GAMMA.f(x);
		return Math.exp(Gamma2.lnGamma(x));
	}

	/**
	 * 
	 * @param z
	 * @return
	 */
	public static ComplexDouble gamma(ComplexDouble z) {
		return GAMMAC.f(z);
	}

}
