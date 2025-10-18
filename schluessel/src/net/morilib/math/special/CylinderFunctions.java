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
 * @author MORIGUCHI, Yuichiro 2010/11/20
 */
public class CylinderFunctions {
	
	//
	static double gosa(ComplexDouble r, ComplexDouble rold) {
		if(Double.isInfinite(rold.realPart())) {
			return Double.POSITIVE_INFINITY;
		}
		return r.subtract(rold).abs() / r.add(rold).abs();
	}
	
	//
	static double JbyIntegral(double x, int n) {
		int n0 = (((x > n) ? (int)x : n) & 0xfffffffe) * 1000;
		double h = Math.PI / n0;
		double r = 0;
		
		r = 1.0 * h / 3;
		for(int i = 1; i < n0 - 1; i += 2) {
			double xi0 = h * i;
			double xi1 = h * (i + 1);
			
			r += 4 * Math.cos(
					n * xi0 - x * Math.sin(xi0)) * h / 3;
			r += 2 * Math.cos(
					n * xi1 - x * Math.sin(xi1)) * h / 3;
		}
		r += Math.cos(n * Math.PI) * h / 3;
		return r / Math.PI;
	}
	
	//
	static double bySeriesExpansion(double x, double nu, boolean isI) {
		double r = 0, rold = Double.MIN_VALUE;
		double mf = 1, kf = 1, df;
		
		kf = GammaFunctions.GAMMA.f(nu + 1);
		df = Math.pow(x / 2, nu);
		r  = df / kf;
		
		int m = 1;
		while(Math.abs(r - rold) > 0.0000001) {
			double d;
			
			rold = r;
			mf   = mf * m;
			kf   = kf * (m + nu);
			df   = df * x * x / 4;
			d    = df / mf / kf;
			if(isI) {
				r    = r + d;
			} else {
				r    = r + (((m & 1) == 0) ? d : -d);
			}
			m++;
		}
		return r;
	}
	
	//
	static ComplexDouble bySeriesExpansion(
			ComplexDouble z, double nu, boolean isI) {
		RectanglarComplexDoubleRegister r;
		ComplexDouble rold = ComplexDouble.REAL_POSITIVE_INFINITY;
		double mf = 1, kf;
		RectanglarComplexDoubleRegister df;
		ComplexDouble z2 = z.divide(2);
		
		kf = GammaFunctions.GAMMA.f(nu + 1);
		df = new RectanglarComplexDoubleRegister(
				ComplexMath.expt(z2, nu));
		r  = new RectanglarComplexDoubleRegister(
				df.toComplex().divide(kf));
		
		int m = 1;
		while(r.toComplex().subtract(rold).abs() > 0.0000001) {
			ComplexDouble d;
			
			rold = r.toComplex();
			mf   = mf * m;
			kf   = kf * (m + nu);
			df.multiply(z2).multiply(z2);
			d    = df.toComplex().divide(mf).divide(kf);
			if(isI) {
				r.add(d);
			} else if((m & 1) == 0) {
				r.add(d);
			} else {
				r.subtract(d);
			}
			m++;
		}
		return r.toComplex();
	}
	
	//
	static double nu_m2(double nu, int m) {
		double r = 1.0;
		
		for(int i = 1; i <= m; i++) {
			r *= ((2 * m - 1) - 4 * nu * nu) / 4 / m;
		}
		return r;
	}
	
	//
	static ComplexDouble JbyAsymptoticExpansion(
			ComplexDouble z, double nu) {
		ComplexDouble w;
		
		w = ComplexMath.sqrt(z.invert().multiply(2 / Math.PI));
		return w.multiply(ComplexMath.cos(
				z.subtract(Math.PI * nu / 2 + Math.PI / 4)));
//		double nu2 = (nu > 0) ? nu : -nu;
//		double aold = Double.POSITIVE_INFINITY;
//		ComplexDouble w1, p1, res;
//		RectanglarComplexDoubleRegister r, zs;
//		int m;
//		
//		//
//		p1 = z.subtract(Math.PI * nu / 2 + Math.PI / 4);
//		w1 = ComplexMath.sqrt(z.invert().multiply(2 / Math.PI));
//		w1 = w1.multiply(ComplexMath.cos(p1));
//		
//		//
//		m = 1;
//		r  = new RectanglarComplexDoubleRegister(w1);
//		zs = new RectanglarComplexDoubleRegister(1, 0);
//		while(m < 4) {
//			zs.divide(z).divide(z).divide(4);
//			zs.multiply((4 * m - 1) * (4 * m - 1) - 4 * nu2 * nu2);
//			zs.divide(4).divide(m);
//			if(zs.toComplex().abs() > aold) {
//				break;
//			}
//			r.add(zs.toComplex());
//			aold = zs.toComplex().abs();
//			m++;
//		}
//		res = r.toComplex();
//		return res;
	}
	
	//
	static ComplexDouble YbyAsymptoticExpansion(
			ComplexDouble z, double nu) {
		ComplexDouble w;
		
		w = ComplexMath.sqrt(z.invert().multiply(2 / Math.PI));
		return w.multiply(ComplexMath.sin(
				z.subtract(Math.PI * nu / 2 + Math.PI / 4)));
	}
	
	//
	static double J0to2(double x, double nu) {
		if(x < 30) {
			return bySeriesExpansion(x, nu, false);
		} else {
			return (Math.sqrt(2 / Math.PI / x) *
					Math.cos(x - Math.PI * nu / 2 - Math.PI / 4));
		}
	}
	
	//
	static ComplexDouble J0to2(ComplexDouble z, double nu) {
		if(z.abs() < 30) {
			return bySeriesExpansion(z, nu, false);
		} else {
			return JbyAsymptoticExpansion(z, nu);
		}
	}
	
	//
	static final double G_LOG2 = Math2.EULER_CONSTANT - Math.log(2);
	
	//
	static double subu(double r, double x, int n) {
		double t, rold, s;
		int m = 1;
		
		s = 0.0;  t = 1.0;
		for(int i = 1; i <= n; i++) {
			 s  += 1.0 / i;
			 t  *= (x / 2) / i;
		}
		rold = Double.MAX_VALUE;
		r += G_LOG2 * J(x, n);
		r -= t * s;
		while(Math.abs(rold - r) > 0.000000000001) {
			rold = r;
			s += 1.0 / (n + m);
			t *= (x * x / 4) / m / (n + m);
			if((m & 1) == 0) {
				r -= t * s;
			} else {
				r += t * s;
			}
			m++;
//			System.out.println(r);
		}
		return r;
	}
	
	//
	static ComplexDouble subu(
			ComplexDouble r, ComplexDouble z, int n) {
		RectanglarComplexDoubleRegister res, t;
		ComplexDouble rold, w = z.divide(2);
		double s;
		int m = 1;
		
		s = 0.0;
		t = new RectanglarComplexDoubleRegister(1, 0);
		for(int i = 1; i <= n; i++) {
			 s  += 1.0 / i;
			 t.multiply(w).divide(i);
		}
		rold = ComplexDouble.REAL_POSITIVE_INFINITY;
		res  = new RectanglarComplexDoubleRegister(
				r.add(J(z, n).multiply(G_LOG2)));
		res.subtract(t.toComplex().multiply(s));
		while(rold.subtract(
				res.toComplex()).abs() > 0.000000000001) {
			rold = res.toComplex();
			s += 1.0 / (n + m);
			t.multiply(w).multiply(w).divide(m * (n + m));
			if((m & 1) == 0) {
				res.subtract(t.toComplex().multiply(s));
			} else {
				res.add(t.toComplex().multiply(s));
			}
			m++;
		}
		return res.toComplex();
	}
	
	//
	static double Y0to2(double x, int n) {
		if(x >= 30) {
			return (Math.sqrt(2 / Math.PI / x) *
					Math.sin(x - Math.PI * n / 2 - Math.PI / 4));
		} else if(n == 1) {
			return subu(J(x, 1) * Math.log(x) -
					J(x, 0) * 1/x, x, 1) * 2 / Math.PI;
		} else {
			return subu(J(x, 0) * Math.log(x), x, 0) * 2 / Math.PI;
		}
	}
	
	//
	static ComplexDouble Y0to2(ComplexDouble z, int n) {
		if(z.abs() >= 30) {
			return YbyAsymptoticExpansion(z, n);
		} else if(n == 1) {
			return subu(J(z, 1).multiply(ComplexMath.log(z)).subtract(
					J(z, 0).multiply(z.invert())),
					z, 1).multiply(2 / Math.PI);
		} else {
			return subu(J(z, 0).multiply(
					ComplexMath.log(z)), z, 0).multiply(2 / Math.PI);
		}
	}
	
	//
	static double recurrenceJY(
			double x, double nu, double x0, double x1) {
		int    n  = (int)Math.floor(nu);
		double w  = Math2.decimalPart(nu);
		double j0 = 0;
		double j1 = x0;
		double r  = x1;
		
		for(int i = 2; i <= n; i++) {
			j0 = j1;  j1 = r;
			r = j1 * (2 * (i - 1 + w)) / x - j0;
		}
		return r;
	}
	
	//
	static ComplexDouble recurrenceJY(
			ComplexDouble z, double nu,
			ComplexDouble z0,
			ComplexDouble z1) {
		int    n  = (int)Math.floor(nu);
		double w  = Math2.decimalPart(nu);
		ComplexDouble j0 = null;
		ComplexDouble j1 = z0;
		RectanglarComplexDoubleRegister r;
		
		r = new RectanglarComplexDoubleRegister(z1);
		for(int i = 2; i <= n; i++) {
			j0 = j1;  j1 = r.toComplex();
			r.multiply(2 * (i - 1 + w)).divide(z).subtract(j0);
		}
		return r.toComplex();
	}
	
	/**
	 * 
	 */
	public static final DoubleTransform J0 = getJ(0);
	
	/**
	 * 
	 */
	public static final DoubleTransform J1 = getJ(1);
	
	//
	static class _Jn implements DoubleTransform {
		
		//
		private double nu;
		
		_Jn(double nu) {
			this.nu = nu;
		}

		public double f(double x) {
			if(x < 0) {
				return Math2.isInteger(nu) ?
						Math2.minus1ToThe((int)nu) * f(-x) :
							Double.NaN;
			} else if(x == 0) {
				return (nu == 0) ? 1 : 0;
			} else if(x < Math.sqrt(nu + 1)) {
				return bySeriesExpansion(x, nu, false);
			} else if(nu < 0 &&
					Math.IEEEremainder(nu, 1.0) == 0.0) {
				return (Math2.minus1ToThe((int)nu) *
						getJ(-nu).f(x));
			} else if(nu >= 0.0 && nu < 2.0) {
				return J0to2(x, nu);
			} else if(nu >= 2.0) {
				double w  = Math2.decimalPart(nu);
				
				return recurrenceJY(
						x, nu, J0to2(x, w), J0to2(x, w + 1));
			} else {
				int    n  = (int)Math.floor(nu);
				double w  = Math2.decimalPart(nu);
				double j2 = 0;
				double j1 = J0to2(x, w + 1);
				double r  = J0to2(x, w);
				
				for(int i = 0; i > n; i--) {
					j2 = j1;  j1 = r;
					r = j1 * (2 * (i + w)) / x - j2;
				}
				return r;
			}
		}
		
	}
	
	//
	static class _JnC implements ComplexDoubleTransform {
		
		//
		private double nu;
		
		_JnC(double nu) {
			this.nu = nu;
		}

		public ComplexDouble f(ComplexDouble z) {
			if(z.isZero()) {
				return (nu == 0) ?
						ComplexDouble.ONE : ComplexDouble.ZERO;
			} else if(z.isReal()) {
				if(z.realPart() > 0) {
					return RectanglarComplexDouble.realValueOf(
							getJ(nu).f(z.realPart()));
				} else if(Math2.isInteger(nu)) {
					return RectanglarComplexDouble.realValueOf(
							getJ(nu).f(z.realPart()));
				}
			} else if(Math.abs(z.angle())  > 1.37 &&
					Math.abs(z.angle()) < 1.77) {
				return getIC(nu).f(z.divide(ComplexDouble.I)).multiply(
						ComplexMath.expt(ComplexDouble.I, nu));
			}
			
			if(z.abs() < Math.sqrt(nu + 1)) {
				return bySeriesExpansion(z, nu, false);
			} else if(nu < 0 &&
					Math.IEEEremainder(nu, 1.0) == 0.0) {
				return getJC(-nu).f(z).multiply(
						Math2.minus1ToThe((int)nu));
			} else if(nu >= 0.0 && nu < 2.0) {
				return J0to2(z, nu);
			} else if(nu >= 2.0) {
				double w  = Math2.decimalPart(nu);
				
				return recurrenceJY(
						z, nu, J0to2(z, w), J0to2(z, w + 1));
			} else {
				int    n  = (int)Math.floor(nu);
				double w  = Math2.decimalPart(nu);
				ComplexDouble j2 = null;
				ComplexDouble j1 = J0to2(z, w + 1);
				RectanglarComplexDoubleRegister r;
				
				r = new RectanglarComplexDoubleRegister(
						J0to2(z, w));
				for(int i = 0; i > n; i--) {
					j2 = j1;  j1 = r.toComplex();
					r.multiply(2 * (i + w)).divide(z).subtract(j2);
				}
				return r.toComplex();
			}
		}
		
	}
	
	//
	static class _JnD implements DoubleTransform {
		
		//
		private double nu;
		
		_JnD(double nu) {
			this.nu = nu;
		}

		public double f(double x) {
			return ((getJ(nu - 1).f(x) - getJ(nu + 1).f(x)) / 2);
		}
		
	}
	
	//
	static class _JnDC implements ComplexDoubleTransform {
		
		//
		private double nu;
		
		_JnDC(double nu) {
			this.nu = nu;
		}

		public ComplexDouble f(ComplexDouble z) {
			if(z.isReal() && z.realPart() > 0) {
				return RectanglarComplexDouble.realValueOf(
						getDxJ(nu).f(z.realPart()));
			} else {
				RectanglarComplexDoubleRegister r;
				
				r = new RectanglarComplexDoubleRegister(0, 0);
				r.add(getJC(nu - 1).f(z));
				r.subtract(getJC(nu + 1).f(z));
				r.divide(2);
				return r.toComplex();
			}
		}
		
	}
	
	//
	static class _Yn implements DoubleTransform {
		
		//
		private double nu;
		
		_Yn(double nu) {
			this.nu = nu;
		}

		public double f(double x) {
			if(Math2.isInteger(nu)) {
				if(nu >= 0 && nu <= 1) {
					return Y0to2(x, (int)nu);
				} else if(nu >= 2) {
					return recurrenceJY(
							x, nu, Y0to2(x, 0), Y0to2(x, 1));
				} else {
					return (Math2.minus1ToThe((int)nu) *
							getY(-nu).f(x));
				}
			} else {
				return ((getJ(nu).f(x) * Math.cos(nu * Math.PI) -
						getJ(-nu).f(x)) / Math.sin(nu * Math.PI));
			}
		}
		
	}
	
	//
	static class _YnC implements ComplexDoubleTransform {
		
		//
		private double nu;
		
		_YnC(double nu) {
			this.nu = nu;
		}

		public ComplexDouble f(ComplexDouble z) {
			if(z.isReal() && z.realPart() > 0) {
				return RectanglarComplexDouble.realValueOf(
						getY(nu).f(z.realPart()));
			} else if(Math2.isInteger(nu)) {
				if(nu >= 0 && nu <= 1) {
					return Y0to2(z, (int)nu);
				} else if(nu >= 2) {
					return recurrenceJY(
							z, nu, Y0to2(z, 0), Y0to2(z, 1));
				} else {
					return getYC(-nu).f(z).multiply(
							Math2.minus1ToThe((int)nu));
				}
			} else {
				RectanglarComplexDoubleRegister r;
				
				r = new RectanglarComplexDoubleRegister(0, 0);
				r.add(getJC(nu).f(z));
				r.multiply(Math.cos(nu * Math.PI));
				r.subtract(getJC(-nu).f(z));
				r.divide(Math.sin(nu * Math.PI));
				return r.toComplex();
			}
		}
		
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static DoubleTransform getJ(double nu) {
		return new _Jn(nu);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static ComplexDoubleTransform getJC(double nu) {
		return new _JnC(nu);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static DoubleTransform getDxJ(double nu) {
		return new _JnD(nu);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static ComplexDoubleTransform getDzJC(double nu) {
		return new _JnDC(nu);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static DoubleTransform getY(double nu) {
		return new _Yn(nu);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static ComplexDoubleTransform getYC(double nu) {
		return new _YnC(nu);
	}
	
	/**
	 * 
	 * @param x
	 * @param nu
	 * @return
	 */
	public static double J(double x, double nu) {
		return getJ(nu).f(x);
	}
	
	/**
	 * 
	 * @param x
	 * @param nu
	 * @return
	 */
	public static ComplexDouble J(ComplexDouble z, double nu) {
		return getJC(nu).f(z);
	}
	
	/**
	 * 
	 * @param x
	 * @param nu
	 * @return
	 */
	public static double Y(double x, double nu) {
		return getY(nu).f(x);
	}
	
	/**
	 * 
	 * @param x
	 * @param nu
	 * @return
	 */
	public static ComplexDouble Y(ComplexDouble z, double nu) {
		return getYC(nu).f(z);
	}
	
	/**
	 * 
	 */
	public static final DoubleTransform I0 = getI(0);
	
	/**
	 * 
	 */
	public static final DoubleTransform I1 = getI(1);
	
	//
	static double IbyAsymptoticExpansion(double x, double nu) {
		double w1;
		
		//
		w1 = Math.exp(x) / (Math.sqrt(x * (2 * Math.PI)));
		return w1;
	}
	
	//
	static ComplexDouble IbyAsymptoticExpansion(
			ComplexDouble z, double nu) {
		ComplexDouble w1;
		
		//
		w1 = ComplexMath.exp(z).divide(
				ComplexMath.sqrt(z.multiply(2 * Math.PI)));
		return w1;
	}
	
	//
	static double I0to2(double x, double nu) {
		if(x <= 90) {
			return bySeriesExpansion(x, nu, true);
		} else {
			return IbyAsymptoticExpansion(x, nu);
//			return (Math.exp(x) / Math.sqrt(2 * Math.PI * x));
		}
	}
	
	//
	static ComplexDouble I0to2(ComplexDouble z, double nu) {
		if(z.abs() <= 90) {
			return bySeriesExpansion(z, nu, true);
		} else {
			return IbyAsymptoticExpansion(z, nu);
		}
	}

	static int pos(int n) {
		return (n == 0) ? 1 : n;
	}
	
	static double _kninit(double x, int n) {
		double r, p;

		if(n == 0) {
			return 0;
		} else {
			p = 2 / x;
			for(int i = 1; i <= n - 1; i++) {
				p *= i / (x / 2);
			}

			r = 0;
			for(int m = 0; m <= n - 1; m++) {
				r += ((m & 1) == 0) ? p : -p;
				p *= 1 / pos(n - 1 - m) / pos(m) * x * x / 4;
			}
			return r / 2;
		}
	}
	
	//
	static double _kn(double x, int n) {
		double t, rold, s1, s2, r;
		int m = 1;

		s1 = 0.0;  s2 = 0.0; t = 1.0;
		for(int i = 1; i <= n; i++) {
			s2 += 1.0 / i;
			t  *= (x / 2) / i;
		}
		rold = Double.MAX_VALUE;
		r = 0;
		while(Math.abs(rold - r) > 0.000000000001) {
			double q;
			
			rold = r;
			q   = Math.log(x / 2) - s1 / 2 - s2 / 2;
			q  += Math2.EULER_CONSTANT;
			r  += t * q;
			t  *= x * x / 4 / m / (n + m);
			s1 += 1.0 / m;
			s2 += 1.0 / (n + m);
			m++;
//			System.out.println(_kninit(x, n) + ":" + r);
		}
		return _kninit(x, n) + (((n & 1) == 0) ? -r : r);
	}
	
	//
	static double _K0to2(double x, double nu) {
		double w;
		
		if(Math2.isInteger(nu)) {
			if(x > 40) {
				w = Math.sqrt(Math.PI / 2 / x) * Math.exp(-x);
				return w * (1 + (4 * nu * nu - 1) / 4 / 2 / x);
			} else {
				return _kn(x, (int)nu);
			}
		} else {
			if(x > 5) {
				w = Math.sqrt(Math.PI / 2 / x) * Math.exp(-x);
				return w * (1 + (4 * nu * nu - 1) / 4 / 2 / x);
			} else {
				w = getI(-nu).f(x) - getI(nu).f(x);
				return w / Math.sin(nu * Math.PI) * Math.PI / 2;
			}
		}
	}
	
	static ComplexDouble _kninit(ComplexDouble z, int n) {
		RectanglarComplexDoubleRegister r, p;

		if(n == 0) {
			return ComplexDouble.ZERO;
		} else {
			p = new RectanglarComplexDoubleRegister(
					z.invert().multiply(2));
			for(int i = 1; i <= n - 1; i++) {
				p.multiply(i / 2).divide(z);
			}

			r = new RectanglarComplexDoubleRegister(0, 0);
			for(int m = 0; m <= n - 1; m++) {
				if((m & 1) == 0) {
					r.add(p.toComplex());
				} else {
					r.subtract(p.toComplex());
				}
				p.multiply(z.power(2).divide(
						pos(n - 1 - m) * pos(m) * 4));
			}
			return r.divide(2).toComplex();
		}
	}
	
	//
	static ComplexDouble _kn(ComplexDouble z, int n) {
		RectanglarComplexDoubleRegister t, r;
		ComplexDouble rold;
		double s1, s2;
		int m = 1;

		s1 = 0.0;  s2 = 0.0;
		t  = new RectanglarComplexDoubleRegister(1, 0);
		for(int i = 1; i <= n; i++) {
			s2 += 1.0 / i;
			t.multiply(z).divide(2 / i);
		}
		rold = ComplexDouble.REAL_POSITIVE_INFINITY;
		r  = new RectanglarComplexDoubleRegister(0, 0);
		while(rold.subtract(r.toComplex()).abs() > 0.000000000001) {
			ComplexDouble q;
			
			rold = r.toComplex();
			q   = ComplexMath.log(z.divide(2));
			q   = q.subtract(s1 / 2).subtract(s2 / 2);
			q   = q.add(Math2.EULER_CONSTANT);
			r.add(t.toComplex().multiply(q));
			t.multiply(z.power(2).divide(4).divide(m * (n + m)));
			s1 += 1.0 / m;
			s2 += 1.0 / (n + m);
			m++;
//			System.out.println(_kninit(x, n) + ":" + r);
		}
		
		if((n & 1) == 0) {
			return _kninit(z, n).subtract(r.toComplex());
		} else {
			return _kninit(z, n).add(r.toComplex());
		}
	}
	
	//
	static ComplexDouble _K0to2(ComplexDouble z, double nu) {
		ComplexDouble w;
		
		if(Math2.isInteger(nu)) {
			if(z.abs() > 40) {
				w = ComplexMath.sqrt(z.invert().multiply(Math.PI / 2));
				w = w.multiply(ComplexMath.exp(z.negate()));
				return w.add(w.multiply(z.invert().multiply(
						(4 * nu * nu - 1) / 4 / 2)));
			} else {
				return _kn(z, (int)nu);
			}
		} else {
			if(z.abs() > 5) {
				w = ComplexMath.sqrt(z.invert().multiply(Math.PI / 2));
				w = w.multiply(ComplexMath.exp(z.negate()));
				return w.add(w.multiply(z.invert().multiply(
						(4 * nu * nu - 1) / 4 / 2)));
			} else {
				w = getIC(-nu).f(z).subtract(getIC(nu).f(z));
				return w.multiply(
						Math.PI / 2 / Math.sin(nu * Math.PI));
			}
		}
	}
	
	//
	static class _In implements DoubleTransform {
		
		//
		private double nu;
		
		_In(double nu) {
			this.nu = nu;
		}

		public double f(double x) {
			if(x < 0) {
				return Math2.isInteger(nu) ?
						Math2.minus1ToThe((int)nu) * f(-x) :
							Double.NaN;
			} else if(x == 0) {
				return (nu == 0) ? 1 : 0;
			} else if(nu < 0 &&
					Math.IEEEremainder(nu, 1.0) == 0.0) {
				return (getI(-nu).f(x));
			} else {
				return I0to2(x, nu);
			}
		}
		
	}
	
	//
	static class _InC implements ComplexDoubleTransform {
		
		//
		private double nu;
		
		_InC(double nu) {
			this.nu = nu;
		}

		public ComplexDouble f(ComplexDouble z) {
			if(z.isZero()) {
				return (nu == 0) ?
						ComplexDouble.ONE : ComplexDouble.ZERO;
			} else if(z.isReal()) {
				if(z.realPart() > 0) {
					return RectanglarComplexDouble.realValueOf(
							getI(nu).f(z.realPart()));
				} else if(Math2.isInteger(nu)) {
					return RectanglarComplexDouble.realValueOf(
							getI(nu).f(z.realPart()));
				}
			} else if(Math.abs(z.angle())  > 0.2 &&
					Math.abs(z.angle()) < 2.94) {
				return getJC(nu).f(ComplexDouble.I.multiply(
						z)).multiply(ComplexMath.expt(
								ComplexDouble.I, -nu));
			}
			
			if(nu < 0 &&
					Math.IEEEremainder(nu, 1.0) == 0.0) {
				return (getIC(-nu).f(z));
			} else {
				return I0to2(z, nu);
			}
		}
		
	}
	
	//
	static class _Kn implements DoubleTransform {
		
		//
		private double nu;
		
		_Kn(double nu) {
			this.nu = nu;
		}
		
		public double f(double x) {
			if(nu < 0) {
				return getK(-nu).f(x);
			} else if(nu >= 2) {
				double k0, k1, r, w;
				int n;
				
				w  = Math2.decimalPart(nu);
				n  = (int)Math.floor(nu);
				k1 = _K0to2(x, 0);  r = _K0to2(x, 1);
				for(int i = 2; i <= n; i++) {
					k0 = k1;  k1 = r;
					r  = k0 + 2 * (i - 1 + w) / x * k1;
				}
				return r;
			} else {
				return _K0to2(x, nu);
			}
		}
		
	}
	
	//
	static class _KnC implements ComplexDoubleTransform {
		
		//
		private double nu;
		
		_KnC(double nu) {
			this.nu = nu;
		}
		
		public ComplexDouble f(ComplexDouble z) {
			if(nu < 0) {
				return getKC(-nu).f(z);
			} else if(nu >= 2) {
				ComplexDouble k0, k1, r;
				double w;
				int n;
				
				w  = Math2.decimalPart(nu);
				n  = (int)Math.floor(nu);
				k1 = _K0to2(z, 0);  r = _K0to2(z, 1);
				for(int i = 2; i <= n; i++) {
					k0 = k1;  k1 = r;
					r  = k0.add(
							k1.multiply(2 * (i - 1 + w)).divide(z));
				}
				return r;
			} else {
				return _K0to2(z, nu);
			}
		}
		
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static DoubleTransform getI(double nu) {
		return new _In(nu);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static ComplexDoubleTransform getIC(double nu) {
		return new _InC(nu);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static DoubleTransform getK(double nu) {
		return new _Kn(nu);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static ComplexDoubleTransform getKC(double nu) {
		return new _KnC(nu);
	}
	
	/**
	 * 
	 * @param x
	 * @param nu
	 * @return
	 */
	public static double I(double x, double nu) {
		return getI(nu).f(x);
	}
	
	/**
	 * 
	 * @param x
	 * @param nu
	 * @return
	 */
	public static ComplexDouble I(ComplexDouble z, double nu) {
		return getIC(nu).f(z);
	}
	
	/**
	 * 
	 * @param x
	 * @param nu
	 * @return
	 */
	public static double K(double x, double nu) {
		return getK(nu).f(x);
	}
	
	/**
	 * 
	 * @param x
	 * @param nu
	 * @return
	 */
	public static ComplexDouble K(ComplexDouble z, double nu) {
		return getKC(nu).f(z);
	}
	
}
