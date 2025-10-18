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
package net.morilib.lang.number.complex;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class RectanglarComplexDouble extends ComplexDouble
implements java.io.Serializable {

	//
	private static final long serialVersionUID = 266450396005644630L;

	/**
	 * 
	 */
	public static final RectanglarComplexDouble ONE =
		new RectanglarComplexDouble(1, 0);

	/**
	 * 
	 */
	public static final RectanglarComplexDouble I =
		new RectanglarComplexDouble(0, 1);

	//
	private double real, imag;

	//
	private RectanglarComplexDouble(double real, double imag) {
		this.real = real;
		this.imag = imag;
	}

	/**
	 * 
	 * @param z
	 */
	public RectanglarComplexDouble(ComplexDouble z) {
		this.real = z.realPart();
		this.imag = z.imagPart();
	}

	/**
	 * 
	 * @param real
	 * @param imag
	 * @return
	 */
	public static ComplexDouble valueOf(double real, double imag) {
		if(real == 0.0 && imag == 0.0) {
			return ZERO;
		} else if(Double.isInfinite(real) && Double.isInfinite(imag)) {
			return INFINITY;
		} else if(Double.isNaN(real) || Double.isNaN(imag)) {
			return NaN;
		}
		return new RectanglarComplexDouble(real, imag);
	}

	/**
	 * 
	 * @param real
	 * @return
	 */
	public static ComplexDouble realValueOf(double real) {
		if(real == 0.0) {
			return ZERO;
		} else if(Double.isNaN(real)) {
			return NaN;
		}
		return new RectanglarComplexDouble(real, 0);
	}

	/**
	 * 
	 * @param imag
	 * @return
	 */
	public static ComplexDouble imagValueOf(double imag) {
		if(imag == 0.0) {
			return ZERO;
		} else if(Double.isNaN(imag)) {
			return NaN;
		}
		return new RectanglarComplexDouble(0, imag);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public ComplexDouble negate() {
		return valueOf(-real, -imag);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public ComplexDouble subtract(ComplexDouble x) {
		return valueOf(real - x.realPart(), imag - x.imagPart());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public ComplexDouble add(ComplexDouble x) {
		return valueOf(real + x.realPart(), imag + x.imagPart());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public ComplexDouble multiply(int n) {
		return valueOf(real * n, imag * n);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public ComplexDouble multiply(ComplexDouble x) {
		return valueOf(
				real * x.realPart() - imag * x.imagPart(),
				real * x.imagPart() + imag * x.realPart());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public ComplexDouble power(int n) {
		if(isZero()) {
			if(n >= 1) {
				return ZERO;
			} else {
				throw new ArithmeticException(toString());
			}
		} else if(isUnit()) {
			return this;
		} else if(n == 0) {
			return ONE;
		} else if(n == 1) {
			return this;
		} else if(n == -1) {
			return invert();
		} else if(n > 1) {
			ComplexDouble res = this;

			for(int i = 1; i < n; i++) {
				res = res.multiply(this);
			}
			return res;
		} else {
			ComplexDouble res = invert();

			for(int i = 1; i < n; i++) {
				res = res.divide(this);
			}
			return res;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public ComplexDouble invert() {
		double c, d, n;

		c = real;  d = imag;
		n = c * c + d * d;
		return valueOf(c / n, -d / n);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public ComplexDouble divide(ComplexDouble x) {
		double a, b, c, d, n;

		a = real;  b = imag;
		c = x.realPart();  d = x.imagPart();
		n = c * c + d * d;
		return valueOf((a * c + b * d) / n, (b * c - a * d) / n);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#realPart()
	 */
	@Override
	public double realPart() {
		return real;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#imagPart()
	 */
	@Override
	public double imagPart() {
		return imag;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#abs()
	 */
	@Override
	public double abs() {
		return Math.sqrt(real * real + imag * imag);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#angle()
	 */
	@Override
	public double angle() {
		return Math.atan2(imag, real);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#rotate(double)
	 */
	@Override
	public ComplexDouble rotate(double rad) {
		double n = abs();

		return valueOf(
				n * Math.cos(angle() + rad),
				n * Math.sin(angle() + rad));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#multiply(double)
	 */
	public ComplexDouble multiply(double x) {
		if(x == 0.0) {
			return ZERO;
		}
		return valueOf(real * x, imag * x);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#divide(double)
	 */
	public ComplexDouble divide(double x) {
		if(x == 0.0) {
			return INFINITY;
		}
		return valueOf(real / x, imag / x);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#add(double)
	 */
	public ComplexDouble add(double x) {
		return valueOf(real + x, imag);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#subtract(double)
	 */
	public ComplexDouble subtract(double x) {
		return valueOf(real - x, imag);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#isNaN()
	 */
	@Override
	public boolean isNaN() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ComplexDouble#isInfinity()
	 */
	@Override
	public boolean isInfinity() {
		return false;
	}

}
