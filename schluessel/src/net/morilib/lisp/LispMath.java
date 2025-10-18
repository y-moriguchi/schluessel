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
package net.morilib.lisp;

import java.math.BigDecimal;
import java.math.BigInteger;

import net.morilib.lang.number.Rational;
import net.morilib.lisp.math.constants.LispExp;
import net.morilib.lisp.math.constants.LispSqrt;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.math.Math2;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class LispMath {

	//
	private static final LispComplex I =
		LispComplex.newComplex(0.0, 1.0);
	private static final LispComplex TWO_I =
		LispComplex.newComplex(0.0, 2.0);
	private static final LispComplex N_INF_PI_HALF_I =
		LispComplex.newComplex(
				Double.NEGATIVE_INFINITY,
				-Math.PI / 2);
	private static final Double P_ZERO = Double.valueOf( 0.0);
	private static final Double M_ZERO = Double.valueOf(-0.0);
	private static final BigInteger LIMIT_NRT =
		BigInteger.valueOf(256);
	private static final BigInteger LIMIT_INT =
		BigInteger.valueOf(Integer.MAX_VALUE);
	private static final BigInteger DEG_ALL = BigInteger.valueOf(360);

	/**
	 * 
	 */
	public static final LispDouble INEXACT_PI =
		new LispDouble(Math.PI);

	/**
	 * 
	 */
	public static final LispDouble INEXACT_RAD_PER_DEG =
		new LispDouble(Math.PI / 180.0);

	//
	private LispMath() {
		// do nothing
	}

	//
	private static LispComplex exptC(LispComplex nm1, LispComplex nm2) {
		double a1 = nm1.getRealDouble();
		double b1 = nm1.getImagDouble();
		double r1 = Math.hypot(a1, b1);
		double t1 = Math.atan2(b1, a1);
		double a2 = nm2.getRealDouble();
		double b2 = nm2.getImagDouble();
		double rr = a2 * Math.log(r1) - t1 * b2;
		double tr = b2 * Math.log(r1) + t1 * a2;

		return LispComplex.newComplex(
				Math.exp(rr) * Math.cos(tr),
				Math.exp(rr) * Math.sin(tr));
	}

	/**
	 * 
	 * @param r
	 * @return
	 */
	public static boolean isPlusZero(double r) {
		return (r == 0.0) && P_ZERO.equals(Double.valueOf(r));
	}

	/**
	 * 
	 * @param r
	 * @return
	 */
	public static boolean isMinusZero(double r) {
		return (r == 0.0) && M_ZERO.equals(Double.valueOf(r));
	}

	/**
	 * 
	 * @param nm1
	 * @param nm2
	 * @return
	 */
	public static LispComplex expt(LispComplex nm1, LispComplex nm2) {
		if(nm1.isNaN() || nm2.isNaN()) {
			return LispDouble.NaN;
		} else if(nm2.isZero()) {
			if(nm1.isExact() && nm2.isExact()) {
				return LispInteger.ONE;
			} else {
				return LispDouble.ONE;
			}
		} else if(nm1.isZero()) {
			if(nm1.isExact() && nm2.isExact()) {
				return LispInteger.ZERO;
			} else {
				return LispDouble.ZERO;
			}
		} else if(nm1.isOne()) {
			if(nm1.isExact() && nm2.isExact()) {
				return LispInteger.ONE;
			} else {
				return LispDouble.ONE;
			}
		} else if(nm2.isOne()) {
			if(nm1.isExact() && nm2.isExact()) {
				return nm1;
			} else {
				return nm1.toInexact();
			}
		} else if(!nm2.isReal()) {
			return exptC(nm1, nm2);
		} else if(!nm1.isReal()) {
			return LispMath.expt(nm1, nm2.getRealDouble());
		} else if(!nm1.isExact() || !nm1.isRational() ||
				!nm2.isExact() || !nm2.isRational()) {
			if(nm1.getRealDouble() > 0) {
				return new LispDouble(Math.pow(
						nm1.getRealDouble(), nm2.getRealDouble()));
			} else {
				return LispMath.expt(nm1, nm2.getRealDouble());
			}
		} else if(!nm2.isInteger()) {
			BigInteger n1, d1, n2, d2, rn, rd;

			if(((LispReal)nm1).signum() < 0) {
				return LispMath.expt(nm1, nm2.getRealDouble());
			} else if(((LispReal)nm2).signum() < 0) {
				n1 = nm1.getDenominator();
				d1 = nm1.getNumerator();
				n2 = ((LispReal)nm2).negate().getNumerator();
				d2 = ((LispReal)nm2).negate().getDenominator();
			} else {
				n1 = nm1.getNumerator();
				d1 = nm1.getDenominator();
				n2 = nm2.getNumerator();
				d2 = nm2.getDenominator();
			}

			if(d2.compareTo(LIMIT_NRT) > 0 ||
					n2.compareTo(LIMIT_INT) > 0) {
				return new LispDouble(Math.pow(
						nm1.getRealDouble(), nm2.getRealDouble()));
			} else if((rn = Math2.nrtExact(n1, d2)).signum() < 0 ||
					(rd = Math2.nrtExact(d1, d2)).signum() < 0) {
				return new LispDouble(Math.pow(
						nm1.getRealDouble(), nm2.getRealDouble()));
			} else {
				return LispRational.newRational(
						rn.pow(n2.intValue()), rd.pow(n2.intValue()));
			}
		} else if(nm1.isInteger()) {
			LispReal rm2 = nm2.getReal();
			BigInteger b = rm2.getBigInteger();
			Integer b2 = LispUtils.toIntExact(b);
			BigInteger a = nm1.getReal().getBigInteger();

			if(b2 == null) {
				return new LispDouble(Math.pow(
						nm1.getRealDouble(), nm2.getRealDouble()));
			}

			if(b.signum() > 0) {
				return LispInteger.valueOf(a.pow(b2.intValue()));
			} else {
				return LispRational.newRational(
						BigInteger.ONE,
						a.pow(-b2.intValue()));
			}
		} else if(nm1.isRational()) {
			LispReal rm2 = nm2.getReal();
			BigInteger b = rm2.getBigInteger();
			Integer b2 = LispUtils.toIntExact(b);
			BigInteger an = nm1.getReal().getNumerator();
			BigInteger ad = nm1.getReal().getDenominator();

			if(b2 == null) {
				return new LispDouble(Math.pow(
						nm1.getRealDouble(), nm2.getRealDouble()));
			}

			if(b.signum() > 0) {
				return LispRational.newRational(
						an.pow(b2.intValue()),
						ad.pow(b2.intValue()));
			} else {
				return LispRational.newRational(
						ad.pow(-b2.intValue()),
						an.pow(-b2.intValue()));
			}
		}
		return exptC(nm1, nm2);
	}

	/**
	 * 
	 * @param nm1
	 * @param r
	 * @return
	 */
	public static LispComplex expt(LispComplex nm1, double r) {
		double a1 = nm1.getRealDouble();
		double b1 = nm1.getImagDouble();

		if(nm1.isNaN() || Double.isNaN(r)) {
			return LispDouble.NaN;
		} else if(b1 == 0.0 && a1 == 1.0) {
			return LispDouble.ONE;
		} else if(r == Double.POSITIVE_INFINITY) {
			double r1 = Math.hypot(a1, b1);

			if(r1 < 1.0 && r1 > -1.0) {
				return LispDouble.ZERO;
			} else if(b1 == 0.0 && a1 > 1.0) {
				return LispDouble.POSITIVE_INFINITY;
			} else {
				return LispDouble.NaN;
			}
		} else if(r == Double.NEGATIVE_INFINITY) {
			double r1 = Math.hypot(a1, b1);

			if(r1 > 1.0 || r1 < -1.0) {
				return LispDouble.ZERO;
			} else if(b1 == 0.0 && a1 < 1.0 && a1 > 0.0) {
				return LispDouble.POSITIVE_INFINITY;
			} else {
				return LispDouble.NaN;
			}
		} else if(b1 == 0.0 && a1 > 0.0) {
			return new LispDouble(Math.pow(a1, r));
		} else {
			double r1 = Math.hypot(a1, b1);
			double t1 = Math.atan2(b1, a1);
			double a2 = r;
			double rr = a2 * Math.log(r1);
			double tr = t1 * a2;

			return LispComplex.newComplex(
					Math.exp(rr) * Math.cos(tr),
					Math.exp(rr) * Math.sin(tr));
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex sqrt(LispComplex n) {
		BigInteger b, c;
		Rational r;
		LispReal x;

		if(n.isZero()) {
			return LispInteger.ZERO;
		} else if(n.isOne()) {
			return LispInteger.ONE;
		} else if(n.isExact() && n.isInteger()) {
			b = Math2.sqrtExact(n.getBigInteger().abs());
			if(b.signum() > 0) {
				x = LispInteger.valueOf(b);
			} else {
				x = (LispReal)LispSqrt.valueOf(
						n.getBigInteger().abs(),
						Rational.ONE, Rational.ZERO);
			}

			if(n.getBigInteger().signum() > 0) {
				return x;
			} else {
				return LispComplex.newComplex(LispInteger.ZERO, x);
			}
		} else if(n.isExact() && n.isRational()) {
			r = ((LispExactReal)n).getRational();
			b = Math2.sqrtExact(r.getNumerator().toBigInteger().abs());
			c = Math2.sqrtExact(
					r.getDenominator().toBigInteger().abs());
			if(b.signum() > 0 && c.signum() > 0) {
				x = LispRational.newRational(b, c);
			} else {
				x = (LispReal)LispSqrt.valueOf(
						(r.signum() > 0 ? r : r.negate()),
						Rational.ONE, Rational.ZERO);
			}

			if(r.signum() > 0) {
				return x;
			} else {
				return LispComplex.newComplex(LispInteger.ZERO, x);
			}
		} else if(n.isReal()) {
			if(n.getRealDouble() == 0.0) {
				return new LispDouble(0.0);
			} else if(n.getRealDouble() > 0.0) {
				return new LispDouble(Math.sqrt(n.getRealDouble()));
			} else {
				return LispComplex.newComplex(
						0.0, Math.sqrt(-n.getRealDouble()));
			}
		} else {
			return LispMath.expt(n, 0.5);
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex exp(LispComplex n) {
		if(n.isRational() && n.isExact()) {
			return LispExp.valueOf(((LispExactReal)n).toRational(),
					Rational.ONE, Rational.ZERO);
		} else if(n.isReal()) {
			return new LispDouble(Math.exp(n.getRealDouble()));
		} else {
			double a = n.getRealDouble();
			double b = n.getImagDouble();

			return LispComplex.newComplex(
					Math.exp(a) * Math.cos(b),
					Math.exp(a) * Math.sin(b));
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex log(LispComplex n) {
		LispExp e;

		if(n instanceof LispExp &&
				(e = (LispExp)n).getScale().isUnit()) {
			return LispRational.valueOf(e.getX());
		} else if(n.getReal().isZero() &&
				isMinusZero(n.getImagDouble())) {
			return N_INF_PI_HALF_I;
		} else if(n.isReal() && n.getReal().signum() > 0) {
			return new LispDouble(Math.log(n.getRealDouble()));
		} else {
			double a = n.getRealDouble();
			double b = n.getImagDouble();
			double r = Math.hypot(a, b);
			double t = Math.atan2(b, a);

			return LispComplex.newComplex(Math.log(r), t);
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex sin(LispComplex n) {
		if(n.isReal()) {
			return new LispDouble(Math.sin(n.getRealDouble()));
		} else if(n.getReal().isZero()) {
			double y = n.getImagDouble();
			double b = Math.sinh(y);

			return LispComplex.newComplex(0.0, b);
		} else {
			double x = n.getRealDouble();
			double y = n.getImagDouble();
			double a = Math.sin(x) * Math.cosh(y);
			double b = Math.cos(x) * Math.sinh(y);

			return LispComplex.newComplex(a, b);
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex cos(LispComplex n) {
		if(n.isReal()) {
			return new LispDouble(Math.cos(n.getRealDouble()));
		} else if(n.getReal().isZero()) {
			double y = n.getImagDouble();
			double a = Math.cosh(y);

			return LispComplex.newComplex(a, 0.0);
		} else {
			double x = n.getRealDouble();
			double y = n.getImagDouble();
			double a = Math.cos(x) * Math.cosh(y);
			double b = -(Math.sin(x) * Math.sinh(y));

			return LispComplex.newComplex(a, b);
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex tan(LispComplex n) {
		if(n.isReal()) {
			return new LispDouble(Math.tan(n.getRealDouble()));
		} else if(n.getReal().isZero()) {
			double y = n.getImagDouble();

			return LispComplex.newComplex(0.0, Math.tanh(y));
		} else if(n.getImagDouble() == Double.POSITIVE_INFINITY) {
			double x = n.getRealDouble();
			LispComplex nm, dn;

			nm = LispComplex.newComplex(Math.tan(x), 1);
			dn = LispComplex.newComplex(1, Math.tan(x));
			return (LispComplex)nm.div(dn);
		} else if(n.getImagDouble() == Double.NEGATIVE_INFINITY) {
			double x = n.getRealDouble();
			LispComplex nm, dn;

			nm = LispComplex.newComplex(Math.tan(x), -1);
			dn = LispComplex.newComplex(1, -Math.tan(x));
			return (LispComplex)nm.div(dn);
		} else {
			return (LispComplex)sin(n).div(cos(n));
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex sinDeg(LispComplex n) {
		if(n.isExact() && n.isInteger()) {
			int b, s;

			b = n.getBigInteger().remainder(DEG_ALL).intValue();
			switch(b / 90) {
			case  0:  s = 1;   break;
			case  1:  s = 1;   b = 180 - b;  break;
			case  2:  s = -1;  b = b - 180;  break;
			case  3:  s = -1;  b = 360 - b;  break;
			default:  throw new RuntimeException();
			}

			switch(b) {
			case 0:
				return LispInteger.ZERO;
			case 30:
				return LispRational.newRational(s, 2);
			case 45:
				break;
			case 60:
				break;
			case 90:
				return LispInteger.valueOf(s);
			}
		}
		return sin((LispComplex)n.mul(INEXACT_RAD_PER_DEG));
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex cosDeg(LispComplex n) {
		if(n.isExact() && n.isInteger()) {
			int b, s;

			b = n.getBigInteger().remainder(DEG_ALL).intValue();
			switch(b / 90) {
			case  0:  s = 1;   break;
			case  1:  s = -1;  b = 180 - b;  break;
			case  2:  s = -1;  b = b - 180;  break;
			case  3:  s = 1;   b = 360 - b;  break;
			default:  throw new RuntimeException();
			}

			switch(b) {
			case 0:
				return LispInteger.valueOf(s);
			case 30:
				break;
			case 45:
				break;
			case 60:
				return LispRational.newRational(s, 2);
			case 90:
				return LispInteger.ZERO;
			}
		}
		return cos((LispComplex)n.mul(INEXACT_RAD_PER_DEG));
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex tanDeg(LispComplex n) {
		if(n.isExact() && n.isInteger()) {
			int b, s;

			b = n.getBigInteger().remainder(DEG_ALL).intValue();
			switch(b / 90) {
			case  0:  s = 1;   break;
			case  1:  s = -1;  b = 180 - b;  break;
			case  2:  s = -1;  b = b - 180;  break;
			case  3:  s = 1;   b = 360 - b;  break;
			default:  throw new RuntimeException();
			}

			switch(b) {
			case 0:
				return LispInteger.ZERO;
			case 30:
				break;
			case 45:
				return LispInteger.valueOf(s);
			case 60:
				break;
			case 90:
				break;
			}
		}
		return tan((LispComplex)n.mul(INEXACT_RAD_PER_DEG));
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex asin(LispComplex n) {
		double x = n.getRealDouble();
		double y = n.getImagDouble();

		if(n.isReal() && x >= -1.0 && x <= 1.0) {
			return new LispDouble(Math.asin(n.getRealDouble()));
		} else if(Double.isInfinite(x) && Double.isInfinite(y)) {
			return LispDouble.NaN;
		} else if(x == Double.POSITIVE_INFINITY) {
			if(y <= 0) {
				x = -x;
			}
			return LispComplex.newComplex(Math.PI / 2, x);
		} else if(x == Double.NEGATIVE_INFINITY) {
			if(y >= 0) {
				x = -x;
			}
			return LispComplex.newComplex(-Math.PI / 2, x);
		} else if(y == Double.POSITIVE_INFINITY) {
			return LispComplex.newComplex(0, y);
		} else if(y == Double.NEGATIVE_INFINITY) {
			return LispComplex.newComplex(0, y);
		} else {
			LispComplex z1 = (LispComplex)I.mul(n);
			LispComplex z2 = sqrt((LispComplex)LispDouble.ONE.sub(
					n.mul(n)));
			LispComplex z3 = log((LispComplex)z1.add(z2));

			//return LispComplex.MINUS_I.mul(z3);
			return LispComplex.newComplex(
					z3.getImagDouble(), -z3.getRealDouble());
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex acos(LispComplex n) {
		double x = n.getRealDouble();

		if(n.isReal() && x >= -1.0 && x <= 1.0) {
			return new LispDouble(Math.acos(n.getRealDouble()));
		} else {
			LispComplex pi2 = new LispDouble(Math.PI / 2);

			return (LispComplex)pi2.sub(asin(n));
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex atan(LispComplex n) {
		if(n.isReal()) {
			return new LispDouble(Math.atan(n.getRealDouble()));
		} else if(Double.isInfinite(n.getImagDouble())) {
			double im = n.getImagDouble();

			if(Double.isInfinite(n.getRealDouble())) {
				return LispDouble.NaN;
			} else if(im > 0) {
				return new LispDouble(Math.PI / 2);
			} else {
				return new LispDouble(-Math.PI / 2);
			}
		} else if(n.getRealDouble() == 0.0 &&
				n.getImagDouble() == 1.0) {
			return LispComplex.newComplex(0, Double.POSITIVE_INFINITY);
		} else if(n.getRealDouble() == 0.0 &&
				n.getImagDouble() == -1.0) {
			return LispComplex.newComplex(0, Double.NEGATIVE_INFINITY);
		} else {
			LispComplex z1 = (LispComplex)LispDouble.ONE.add(
					I.mul(n));
			LispComplex z2 = (LispComplex)LispDouble.ONE.sub(
					I.mul(n));
			LispComplex z3 = (LispComplex)log(z1).sub(log(z2));

			return (LispComplex)z3.div(TWO_I);
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex asinDeg(LispComplex n) {
		if(n.isExact() && n.isInteger()) {
			Integer x = SubrUtils.toIntExact(n.getBigInteger());

			if(x == null) {
				// do nothing
			} else if(x.intValue() == 0) {
				return LispInteger.ZERO;
			} else if(x.intValue() == 1) {
				return LispInteger.valueOf(90);
			} else if(x.intValue() == -1) {
				return LispInteger.valueOf(-90);
			}
		} else if(n.isExact() && n.isRational()) {
			Integer x = SubrUtils.toIntExact(n.getNumerator());
			Integer d = SubrUtils.toIntExact(n.getDenominator());

			if(x == null || d == null) {
				// do nothing
			} else if(x.intValue() == 1  && d.intValue() == 2) {
				return LispInteger.valueOf(30);
			} else if(x.intValue() == -1 && d.intValue() == 2) {
				return LispInteger.valueOf(-30);
			}
		}
		return asin((LispComplex)n.div(INEXACT_RAD_PER_DEG));
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex acosDeg(LispComplex n) {
		if(n.isExact() && n.isInteger()) {
			Integer x = SubrUtils.toIntExact(n.getBigInteger());

			if(x == null) {
				// do nothing
			} else if(x.intValue() == 0) {
				return LispInteger.valueOf(90);
			} else if(x.intValue() == 1) {
				return LispInteger.ZERO;
			} else if(x.intValue() == -1) {
				return LispInteger.valueOf(180);
			}
		} else if(n.isExact() && n.isRational()) {
			Integer x = SubrUtils.toIntExact(n.getNumerator());
			Integer d = SubrUtils.toIntExact(n.getDenominator());

			if(x == null || d == null) {
				// do nothing
			} else if(x.intValue() == 1  && d.intValue() == 2) {
				return LispInteger.valueOf(60);
			} else if(x.intValue() == -1 && d.intValue() == 2) {
				return LispInteger.valueOf(120);
			}
		}
		return acos((LispComplex)n.div(INEXACT_RAD_PER_DEG));
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static LispComplex atanDeg(LispComplex n) {
		if(n.isExact() && n.isInteger()) {
			Integer x = SubrUtils.toIntExact(n.getBigInteger());

			if(x == null) {
				// do nothing
			} else if(x.intValue() == 0) {
				return LispInteger.ZERO;
			} else if(x.intValue() == 1) {
				return LispInteger.valueOf(45);
			} else if(x.intValue() == -1) {
				return LispInteger.valueOf(-45);
			}
		}
		return atan((LispComplex)n.div(INEXACT_RAD_PER_DEG));
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static LispReal realLog(LispReal x, LispReal y) {
		if(x.signum() < 0 || y.signum() <= 0) {
			throw new IllegalArgumentException();
		} else if(x.signum() == 0) {
			return LispDouble.NEGATIVE_INFINITY;
		} else if(x.isExact() && x.isInteger() &&
				y.isExact() && y.isInteger()) {
			return LispInteger.valueOf(Math2.integerLog(
					x.getBigInteger(), y.getBigInteger()));
		} else {
			return new LispDouble(Math.log(
					x.doubleValue()) / y.doubleValue());
		}
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static LispReal quo(LispReal x, LispReal y) {
		if(x.isExact() && x.isRational() &&
				y.isExact() && y.isRational()) {
			LispReal z = x.divide(y);

			return LispInteger.valueOf(z.getNumerator().divide(
					z.getDenominator()));
		} else {
			BigDecimal z = BigDecimal.valueOf(
					x.getRealDouble() / y.getRealDouble());

			return new LispDouble(z.toBigInteger().doubleValue());
		}
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static LispReal rem(LispReal x, LispReal y) {
		if(x.isExact() && x.isRational() &&
				y.isExact() && y.isRational()) {
			LispReal z = x.divide(y), w;
			BigInteger n;

			n = z.getNumerator().divide(z.getDenominator());
			w = x.subtract(y.multiply(LispInteger.valueOf(n)));
			return w;
		} else {
			double x1 = x.getRealDouble();
			double x2 = y.getRealDouble();
			double x3 = x1 / x2;

			x3 = (x3 > 0) ? Math.floor(x3) : Math.ceil(x3);
			return new LispDouble(x1 - x2 * x3);
		}
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static LispReal mod(LispReal x, LispReal y) {
		if(x.isExact() && x.isRational() &&
				y.isExact() && y.isRational()) {
			LispReal z = x.divide(y), w;
			BigInteger n;

			n = z.getNumerator().divide(z.getDenominator());
			if(n.signum() < 0) {
				n = n.subtract(BigInteger.ONE);
			}
			w = x.subtract(y.multiply(LispInteger.valueOf(n)));
			return w;
		} else {
			double x1 = x.getRealDouble();
			double x2 = y.getRealDouble();

			return new LispDouble(x1 - x2 * Math.floor(x1 / x2));
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @param c
	 * @return
	 */
	public static LispComplex[] solveQuadratic(LispReal a, LispReal b,
			LispReal c) {
		LispReal D = b.multiply(b).subtract(a.multiply(c).multiply(4));
		LispReal dr, db, da = a.multiply(2);
		int sd;

		if((sd = D.signum()) > 0) {
			dr = sqrt(D).getReal();
			return new LispComplex[] {
					b.negate().add(dr).divide(da),
					b.negate().subtract(dr).divide(da)
			};
		} else if(sd == 0) {
			return new LispComplex[] {
					b.negate().divide(da)
			};
		} else {
			db = b.negate().divide(da);
			dr = sqrt(D.negate()).getReal().divide(da);
			return new LispComplex[] {
					LispComplex.newComplex(db, dr),
					LispComplex.newComplex(db, dr.negate())
			};
		}
	}

}
