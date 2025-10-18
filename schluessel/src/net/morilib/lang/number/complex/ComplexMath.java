/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lang.number.complex;

public final class ComplexMath {
	
	private static final ComplexDouble TWO_I =
		RectanglarComplexDouble.valueOf(0.0, 2.0);

	private static final ComplexDouble N_INF_PI_HALF_I =
		RectanglarComplexDouble.valueOf(
				Double.NEGATIVE_INFINITY, -Math.PI / 2);
	
	private static final Double P_ZERO = Double.valueOf( 0.0);
	
	private static final Double M_ZERO = Double.valueOf(-0.0);
	
	private ComplexMath() {
		// do nothing
	}

	private static ComplexDouble exptC(
			ComplexDouble nm1, ComplexDouble nm2) {
		double a1 = nm1.realPart();
		double b1 = nm1.imagPart();
		double r1 = Math.hypot(a1, b1);
		double t1 = Math.atan2(b1, a1);
		double a2 = nm2.realPart();
		double b2 = nm2.imagPart();
		double rr = a2 * Math.log(r1) - t1 * b2;
		double tr = b2 * Math.log(r1) + t1 * a2;
		
		return RectanglarComplexDouble.valueOf(
				Math.exp(rr) * Math.cos(tr),
				Math.exp(rr) * Math.sin(tr));
	}

	public static boolean isPlusZero(double r) {
		return (r == 0.0) && P_ZERO.equals(Double.valueOf(r));
	}

	public static boolean isMinusZero(double r) {
		return (r == 0.0) && M_ZERO.equals(Double.valueOf(r));
	}
	
	public static ComplexDouble expt(
			ComplexDouble nm1, ComplexDouble nm2) {
		if(nm1.isNaN() || nm2.isNaN()) {
			return ComplexDouble.NaN;
		} else if(nm2.isZero()) {
			return ComplexDouble.ONE;
		} else if(nm1.isZero()) {
			return ComplexDouble.ZERO;
		} else if(nm1.isUnit()) {
			return ComplexDouble.ONE;
		} else if(nm2.isUnit()) {
			return nm1;
		} else if(!nm2.isReal()) {
			return exptC(nm1, nm2);
		} else if(!nm1.isReal()) {
			return ComplexMath.expt(nm1, nm2.realPart());
		} else if(!nm2.isInteger()) {
			if(nm1.realPart() > 0) {
				return RectanglarComplexDouble.realValueOf(Math.pow(
						nm1.realPart(), nm2.realPart()));
			} else {
				return ComplexMath.expt(nm1, nm2.realPart());
			}
		} else {
			return RectanglarComplexDouble.realValueOf(Math.pow(
					nm1.realPart(), nm2.realPart()));
		}
	}

	public static ComplexDouble expt(ComplexDouble nm1, double r) {
		double a1 = nm1.realPart();
		double b1 = nm1.imagPart();
		
		if(nm1.isNaN() || Double.isNaN(r)) {
			return ComplexDouble.NaN;
		} else if(b1 == 0.0 && a1 == 1.0) {
			return ComplexDouble.ONE;
		} else if(r == Double.POSITIVE_INFINITY) {
			double r1 = Math.hypot(a1, b1);
			
			if(r1 < 1.0 && r1 > -1.0) {
				return ComplexDouble.ZERO;
			} else if(b1 == 0.0 && a1 > 1.0) {
				return ComplexDouble.REAL_POSITIVE_INFINITY;
			} else {
				return ComplexDouble.NaN;
			}
		} else if(r == Double.NEGATIVE_INFINITY) {
			double r1 = Math.hypot(a1, b1);
			
			if(r1 > 1.0 || r1 < -1.0) {
				return ComplexDouble.ZERO;
			} else if(b1 == 0.0 && a1 < 1.0 && a1 > 0.0) {
				return ComplexDouble.REAL_POSITIVE_INFINITY;
			} else {
				return ComplexDouble.NaN;
			}
		} else if(b1 == 0.0 && a1 > 0.0) {
			return RectanglarComplexDouble.realValueOf(
					Math.pow(a1, r));
		} else {
			double r1 = Math.hypot(a1, b1);
			double t1 = Math.atan2(b1, a1);
			double a2 = r;
			double rr = a2 * Math.log(r1);
			double tr = t1 * a2;
			
			return RectanglarComplexDouble.valueOf(
					Math.exp(rr) * Math.cos(tr),
					Math.exp(rr) * Math.sin(tr));
		}
	}

	public static ComplexDouble sqrt(ComplexDouble n) {
		if(n.isReal()) {
			if(n.realPart() == 0.0) {
				return RectanglarComplexDouble.realValueOf(0.0);
			} else if(n.realPart() > 0.0) {
				return RectanglarComplexDouble.realValueOf(
						Math.sqrt(n.realPart()));
			} else {
				return RectanglarComplexDouble.valueOf(
						0.0, Math.sqrt(-n.realPart()));
			}
		} else {
			return ComplexMath.expt(n, 0.5);
		}
	}

	public static ComplexDouble exp(ComplexDouble n) {
		if(n.isReal()) {
			return RectanglarComplexDouble.realValueOf(
					Math.exp(n.realPart()));
		} else {
			double a = n.realPart();
			double b = n.imagPart();
			
			return RectanglarComplexDouble.valueOf(
					Math.exp(a) * Math.cos(b),
					Math.exp(a) * Math.sin(b));
		}
	}

	public static ComplexDouble log(ComplexDouble n) {
		if(n.realPart() == 0.0 && isMinusZero(n.imagPart())) {
			return N_INF_PI_HALF_I;
		} else if(n.isReal() && Math.signum(n.realPart()) > 0) {
			return RectanglarComplexDouble.realValueOf(
					Math.log(n.realPart()));
		} else {
			double a = n.realPart();
			double b = n.imagPart();
			double r = Math.hypot(a, b);
			double t = Math.atan2(b, a);
			
			return RectanglarComplexDouble.valueOf(Math.log(r), t);
		}
	}

	public static ComplexDouble sin(ComplexDouble n) {
		if(n.isReal()) {
			return RectanglarComplexDouble.realValueOf(
					Math.sin(n.realPart()));
		} else if(n.realPart() == 0.0) {
			double y = n.imagPart();
			double b = Math.sinh(y);
			
			return RectanglarComplexDouble.valueOf(0.0, b);
		} else {
			double x = n.realPart();
			double y = n.imagPart();
			double a = Math.sin(x) * Math.cosh(y);
			double b = Math.cos(x) * Math.sinh(y);
			
			return RectanglarComplexDouble.valueOf(a, b);
		}
	}

	public static ComplexDouble cos(ComplexDouble n) {
		if(n.isReal()) {
			return RectanglarComplexDouble.realValueOf(
					Math.cos(n.realPart()));
		} else if(n.realPart() == 0.0) {
			double y = n.imagPart();
			double a = Math.cosh(y);
			
			return RectanglarComplexDouble.valueOf(a, 0.0);
		} else {
			double x = n.realPart();
			double y = n.imagPart();
			double a = Math.cos(x) * Math.cosh(y);
			double b = -(Math.sin(x) * Math.sinh(y));
			
			return RectanglarComplexDouble.valueOf(a, b);
		}
	}

	public static ComplexDouble tan(ComplexDouble n) {
		if(n.isReal()) {
			return RectanglarComplexDouble.realValueOf(
					Math.tan(n.realPart()));
		} else if(n.realPart() == 0.0) {
			double y = n.imagPart();
			
			return RectanglarComplexDouble.valueOf(0.0, Math.tanh(y));
		} else if(n.imagPart() == Double.POSITIVE_INFINITY) {
			double x = n.realPart();
			ComplexDouble nm, dn;
			
			nm = RectanglarComplexDouble.valueOf(Math.tan(x), 1);
			dn = RectanglarComplexDouble.valueOf(1, Math.tan(x));
			return nm.divide(dn);
		} else if(n.imagPart() == Double.NEGATIVE_INFINITY) {
			double x = n.realPart();
			ComplexDouble nm, dn;
			
			nm = RectanglarComplexDouble.valueOf(Math.tan(x), -1);
			dn = RectanglarComplexDouble.valueOf(1, -Math.tan(x));
			return nm.divide(dn);
		} else {
			return sin(n).divide(cos(n));
		}
	}

	public static ComplexDouble asin(ComplexDouble n) {
		double x = n.realPart();
		double y = n.imagPart();
		
		if(n.isReal() && x >= -1.0 && x <= 1.0) {
			return RectanglarComplexDouble.realValueOf(
					Math.asin(n.realPart()));
		} else if(Double.isInfinite(x) && Double.isInfinite(y)) {
			return ComplexDouble.NaN;
		} else if(x == Double.POSITIVE_INFINITY) {
			if(y <= 0) {
				x = -x;
			}
			return RectanglarComplexDouble.valueOf(Math.PI / 2, x);
		} else if(x == Double.NEGATIVE_INFINITY) {
			if(y >= 0) {
				x = -x;
			}
			return RectanglarComplexDouble.valueOf(-Math.PI / 2, x);
		} else if(y == Double.POSITIVE_INFINITY) {
			return RectanglarComplexDouble.valueOf(0, y);
		} else if(y == Double.NEGATIVE_INFINITY) {
			return RectanglarComplexDouble.valueOf(0, y);
		} else {
			ComplexDouble z1 =
				ComplexDouble.I.multiply(n);
			ComplexDouble z2 =
				sqrt(ComplexDouble.ONE.subtract(n.multiply(n)));
			ComplexDouble z3 = log(z1.add(z2));
			
			//return RectanglarComplexDouble.MINUS_I.multiply(z3);
			return RectanglarComplexDouble.valueOf(
					z3.imagPart(), -z3.realPart());
		}
	}

	public static ComplexDouble acos(ComplexDouble n) {
		double x = n.realPart();
		
		if(n.isReal() && x >= -1.0 && x <= 1.0) {
			return RectanglarComplexDouble.realValueOf(
					Math.acos(n.realPart()));
		} else {
			ComplexDouble pi2 =
				RectanglarComplexDouble.realValueOf(Math.PI / 2);
			
			return pi2.subtract(asin(n));
		}
	}

	public static ComplexDouble atan(ComplexDouble n) {
		if(n.isReal()) {
			return RectanglarComplexDouble.realValueOf(
					Math.atan(n.realPart()));
		} else if(Double.isInfinite(n.imagPart())) {
			double im = n.imagPart();
			
			if(Double.isInfinite(n.realPart())) {
				return ComplexDouble.NaN;
			} else if(im > 0) {
				return RectanglarComplexDouble.realValueOf(
						Math.PI / 2);
			} else {
				return RectanglarComplexDouble.realValueOf(
						-Math.PI / 2);
			}
		} else {
			ComplexDouble z1 = ComplexDouble.ONE.add(
					RectanglarComplexDouble.I.multiply(n));
			ComplexDouble z2 = ComplexDouble.ONE.subtract(
					RectanglarComplexDouble.I.multiply(n));
			ComplexDouble z3 = log(z1).subtract(log(z2));
			
			return z3.divide(TWO_I);
		}
	}
	
}
