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
 * @author MORIGUCHI, Yuichiro 2010/11/23
 */
public class RectanglarComplexDoubleRegister {
	
	/**
	 * 
	 */
	public double real;
	
	/**
	 * 
	 */
	public double imag;
	
	/**
	 * 
	 * @param real
	 * @param imag
	 */
	public RectanglarComplexDoubleRegister(double real, double imag) {
		this.real = real;
		this.imag = imag;
	}
	
	/**
	 * 
	 * @param z
	 */
	public RectanglarComplexDoubleRegister(ComplexDouble z) {
		this.real = z.realPart();
		this.imag = z.imagPart();
	}

	/**
	 * 
	 * @return
	 */
	public boolean isReal() {
		return imag == 0.0;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isInteger() {
		return isReal() && Math.IEEEremainder(real, 1.0) == 0.0;
	}
	
	
	public boolean isUnit() {
		return real == 1.0 && imag == 0.0;
	}


	public boolean isZero() {
		return real == 0.0 && imag == 0.0;
	}
	
	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public RectanglarComplexDoubleRegister negate() {
		real = -real;
		imag = -imag;
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public RectanglarComplexDoubleRegister subtract(ComplexDouble x) {
		real = real - x.realPart();
		imag = imag - x.imagPart();
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public RectanglarComplexDoubleRegister add(ComplexDouble x) {
		real = real + x.realPart();
		imag = imag + x.imagPart();
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public RectanglarComplexDoubleRegister multiply(int n) {
		real = real * n;
		imag = imag * n;
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public RectanglarComplexDoubleRegister multiply(ComplexDouble x) {
		double r = real, i = imag;
		
		real = r * x.realPart() - i * x.imagPart();
		imag = r * x.imagPart() + i * x.realPart();
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public RectanglarComplexDoubleRegister power(int n) {
		if(isZero()) {
			if(n >= 1) {
				real = imag = 0;
			} else {
				throw new ArithmeticException(toString());
			}
		} else if(isUnit()) {
			return this;
		} else if(n == 0) {
			real = 1;  imag = 0;
		} else if(n == 1) {
			return this;
		} else if(n == -1) {
			return invert();
		} else if(n > 1) {
			ComplexDouble res =
				RectanglarComplexDouble.valueOf(real, imag);
			
			for(int i = 1; i < n; i++) {
				multiply(res);
			}
		} else {
			ComplexDouble res =
				RectanglarComplexDouble.valueOf(real, imag);
			
			real = 1;  imag = 0;
			for(int i = 0; i < n; i++) {
				divide(res);
			}
		}
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public RectanglarComplexDoubleRegister invert() {
		double c, d, n;
		
		c = real;  d = imag;
		n = c * c + d * d;
		real = c / n;
		imag = -d / n;
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public RectanglarComplexDoubleRegister divide(ComplexDouble x) {
		double a, b, c, d, n;
		
		a = real;  b = imag;
		c = x.realPart();  d = x.imagPart();
		n = c * c + d * d;
		real = (a * c + b * d) / n;
		imag = (b * c - a * d) / n;
		return this;
	}
	
	/**
	 * 
	 * @param x
	 * @return
	 */
	public RectanglarComplexDoubleRegister multiply(double x) {
		if(x == 0.0) {
			real = imag = 0;
		} else {
			real = real * x;
			imag = imag * x;
		}
		return this;
	}
	
	/**
	 * 
	 * @param x
	 * @return
	 */
	public RectanglarComplexDoubleRegister divide(double x) {
		if(x == 0.0) {
			real = imag = Double.POSITIVE_INFINITY;
		} else {
			real = real / x;
			imag = imag / x;
		}
		return this;
	}
	
	/**
	 * 
	 * @param x
	 * @return
	 */
	public RectanglarComplexDoubleRegister add(double x) {
		real = real + x;
		return this;
	}
	
	/**
	 * 
	 * @param x
	 * @return
	 */
	public RectanglarComplexDoubleRegister subtract(double x) {
		real = real - x;
		return this;
	}
	
	/**
	 * 
	 * @return
	 */
	public ComplexDouble toComplex() {
		return RectanglarComplexDouble.valueOf(real, imag);
	}
	
}
