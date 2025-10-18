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

import net.morilib.lang.Hashes;
import net.morilib.lang.algebra.FieldElement;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class ComplexDouble
implements FieldElement<ComplexDouble> {

	/**
	 * 
	 */
	public static final ComplexDouble NaN = new ComplexDouble() {

		public ComplexDouble negate() {
			return this;
		}

		public ComplexDouble subtract(ComplexDouble x) {
			return this;
		}

		public ComplexDouble add(ComplexDouble x) {
			return this;
		}

		public ComplexDouble multiply(int n) {
			return this;
		}

		public ComplexDouble multiply(ComplexDouble x) {
			return this;
		}

		public ComplexDouble power(int n) {
			return this;
		}

		public ComplexDouble invert() {
			return this;
		}

		public ComplexDouble divide(ComplexDouble x) {
			return this;
		}

		@Override
		public double realPart() {
			return Double.NaN;
		}

		@Override
		public double imagPart() {
			return Double.NaN;
		}

		@Override
		public double abs() {
			return Double.NaN;
		}

		@Override
		public double angle() {
			return Double.NaN;
		}

		@Override
		public ComplexDouble rotate(double rad) {
			return this;
		}

		@Override
		public boolean isNaN() {
			return true;
		}

		@Override
		public boolean isInfinity() {
			return false;
		}

		public String toString() {
			return "NaN";
		}

	};

	/**
	 * 
	 */
	public static final ComplexDouble ONE =
		RectanglarComplexDouble.valueOf(1, 0);

	/**
	 * 
	 */
	public static final ComplexDouble I =
		RectanglarComplexDouble.valueOf(0, 1);

	/**
	 * 
	 */
	public static final ComplexDouble REAL_POSITIVE_INFINITY =
		RectanglarComplexDouble.valueOf(Double.POSITIVE_INFINITY, 0);

	/**
	 * 
	 */
	public static final ComplexDouble REAL_NEGATIVE_INFINITY =
		RectanglarComplexDouble.valueOf(Double.NEGATIVE_INFINITY, 0);

	/**
	 * 
	 */
	public static final ComplexDouble ZERO = new ComplexDouble() {

		public ComplexDouble negate() {
			return this;
		}

		public ComplexDouble subtract(ComplexDouble x) {
			return x.negate();
		}

		public ComplexDouble add(ComplexDouble x) {
			return this;
		}

		public ComplexDouble multiply(int n) {
			return this;
		}

		public ComplexDouble multiply(ComplexDouble x) {
			return this;
		}

		public ComplexDouble power(int n) {
			return (n <= 0) ? NaN : this;
		}

		public ComplexDouble invert() {
			return INFINITY;
		}

		public ComplexDouble divide(ComplexDouble x) {
			return this;
		}

		@Override
		public double realPart() {
			return 0;
		}

		@Override
		public double imagPart() {
			return 0;
		}

		@Override
		public double abs() {
			return 0;
		}

		@Override
		public double angle() {
			return 0;
		}

		@Override
		public ComplexDouble rotate(double rad) {
			return this;
		}

		@Override
		public boolean isNaN() {
			return false;
		}

		@Override
		public boolean isInfinity() {
			return false;
		}

		@Override
		public boolean isReal() {
			return true;
		}

		@Override
		public boolean isInteger() {
			return true;
		}

	};

	/**
	 * 
	 */
	public static final ComplexDouble INFINITY = new ComplexDouble() {

		public ComplexDouble negate() {
			return this;
		}

		public ComplexDouble subtract(ComplexDouble x) {
			return x.isInfinity() ? NaN : this;
		}

		public ComplexDouble add(ComplexDouble x) {
			return x.isInfinity() ? INFINITY : this;
		}

		public ComplexDouble multiply(int n) {
			return this;
		}

		public ComplexDouble multiply(ComplexDouble x) {
			return x.isInfinity() ? INFINITY : this;
		}

		public ComplexDouble power(int n) {
			return this;
		}

		public ComplexDouble invert() {
			return ZERO;
		}

		public ComplexDouble divide(ComplexDouble x) {
			return x.isInfinity() ? NaN : this;
		}

		@Override
		public double realPart() {
			return Double.NaN;
		}

		@Override
		public double imagPart() {
			return Double.NaN;
		}

		@Override
		public double abs() {
			return Double.POSITIVE_INFINITY;
		}

		@Override
		public double angle() {
			return Double.NaN;
		}

		@Override
		public ComplexDouble rotate(double rad) {
			return this;
		}

		@Override
		public boolean isNaN() {
			return false;
		}

		@Override
		public boolean isInfinity() {
			return true;
		}

		@Override
		public boolean isReal() {
			return false;
		}

		@Override
		public boolean isInteger() {
			return false;
		}

	};

	/**
	 * 
	 * @param base
	 * @return
	 */
	public final double getPart(ImaginaryPart base) {
		if(base == ComplexImaginaryPart.I) {
			return imagPart();
		} else if(base == ComplexImaginaryPart.ONE) {
			return realPart();
		} else {
			throw new IllegalArgumentException(base.toString());
		}
	}

	/**
	 * 
	 * @return
	 */
	public abstract double realPart();

	/**
	 * 
	 * @return
	 */
	public abstract double imagPart();

	/**
	 * 
	 * @return
	 */
	public abstract double abs();

	/**
	 * 
	 * @return
	 */
	public abstract double angle();

	/**
	 * 
	 * @param rad
	 * @return
	 */
	public abstract ComplexDouble rotate(double rad);

	/**
	 * 
	 * @return
	 */
	public abstract boolean isNaN();

	/**
	 * 
	 * @return
	 */
	public abstract boolean isInfinity();

	/**
	 * 
	 * @param x
	 * @return
	 */
	public ComplexDouble multiply(double x) {
		if(x == 0.0) {
			return ZERO;
		}
		return multiply(RectanglarComplexDouble.realValueOf(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public ComplexDouble divide(double x) {
		if(x == 0.0) {
			return INFINITY;
		}
		return divide(RectanglarComplexDouble.realValueOf(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public ComplexDouble add(double x) {
		return add(RectanglarComplexDouble.realValueOf(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public ComplexDouble subtract(double x) {
		return subtract(RectanglarComplexDouble.realValueOf(x));
	}

	/**
	 * 
	 * @return
	 */
	public boolean isReal() {
		return imagPart() == 0.0;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isInteger() {
		return isReal() && Math.IEEEremainder(realPart(), 1.0) == 0.0;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return realPart() == 1.0 && imagPart() == 0.0;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return realPart() == 0.0 && imagPart() == 0.0;
	}

	/**
	 * 
	 * @param deg
	 * @return
	 */
	public ComplexDouble rotateDegree(int deg) {
		return rotate(deg * Math.PI / 180);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public final boolean equals(Object o) {
		if(o instanceof ComplexDouble) {
			ComplexDouble c = (ComplexDouble)o;

			return (realPart() == c.realPart() &&
					imagPart() == c.imagPart());
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public final int hashCode() {
		if(isInfinity()) {
			return Integer.MAX_VALUE;
		}
		return (Hashes.hashCode(realPart()) +
				Hashes.hashCode(imagPart()) * Hashes.A);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();

		if(isZero()) {
			return "0";
		} else {
			if(realPart() != 0.0) {
				b.append(realPart());
			}

			if(imagPart() > 0) {
				b.append("+").append(imagPart()).append("i");
			} else if(imagPart() < 0) {
				b.append(imagPart()).append("i");
			}
			return b.toString();
		}
	}

}
