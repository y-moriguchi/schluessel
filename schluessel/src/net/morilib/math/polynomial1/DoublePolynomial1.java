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
package net.morilib.math.polynomial1;

import java.util.Arrays;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/07
 */
public class DoublePolynomial1 {

	/**
	 * 
	 */
	public static final DoublePolynomial1 ZERO = new DoublePolynomial1(
			new double[] { 0.0 });

	//
	double[] coefficients;

	/**
	 * 
	 * @param ds
	 */
	public DoublePolynomial1(double... ds) {
		int d = degree(ds);
		
		coefficients = new double[d + 1];
		for(int i = 0; i <= d; i++) {
			coefficients[i] = ds[i];
		}
	}

	//
	private int degree(double[] coeff) {
		for(int i = coeff.length - 1; i >= 0; i--) {
			if(coeff[i] != 0) {
				return i;
			}
		}
		return -1;
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public double coefficient(int n) {
		if(n < 0) {
			throw new IndexOutOfBoundsException();
		} else if(n >= coefficients.length) {
			return 0.0;
		}
		return coefficients[n];
	}

	/**
	 * 
	 * @return
	 */
	public int degree() {
		return coefficients.length - 1;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isZero() {
		return (coefficients.length == 0 ||
				(coefficients.length == 1 && coefficients[0] == 0));
	}

	/**
	 * 
	 * @return
	 */
	public DoublePolynomial1 uminus() {
		DoublePolynomial1 r = new DoublePolynomial1(coefficients);
		
		for(int i = 0; i <= r.degree(); i++) {
			r.coefficients[i] = -r.coefficients[i];
		}
		return r;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double substitute(double x) {
		double r = 0;

//		for(int i = 0; i <= degree(); i++) {
//			r = r + (coefficients[i] * Math.pow(x, i));
//		}
		for(int i = degree(); i >= 0 ; i--) {
			r = r * x + coefficients[i];
		}
		return r;
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public DoublePolynomial1 div(double a) {
		DoublePolynomial1 r = new DoublePolynomial1(coefficients);

		for(int i = 0; i <= r.degree(); i++) {
			r.coefficients[i] = r.coefficients[i] / a;
		}
		return r;
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public DoublePolynomial1[] divAndRemainder(DoublePolynomial1 b) {
		double[] d, r;
		double x;

		if(degree() < b.degree()) {
			return new DoublePolynomial1[] { ZERO, this };
		} else if(b.degree() == 0) {
			return new DoublePolynomial1[] {
					div(b.coefficient(0)), ZERO };
		} else {
			d = new double[degree() - b.degree() + 1];
			r = new double[degree() + 1];
			System.arraycopy(coefficients, 0, r, 0, r.length);
			for(int i = degree() - b.degree(); i >= 0; i--) {
				x = r[i + b.degree()] / b.coefficient(b.degree());
				for(int j = i + b.degree(); j >= i; j--) {
					r[j] = r[j] - (b.coefficient(j - i) * x);
				}
				d[i] = x;
			}
		}
		return new DoublePolynomial1[] {
				new DoublePolynomial1(d), new DoublePolynomial1(r)
		};
	}

	/**
	 * 
	 * @return
	 */
	public DoublePolynomial1 differenciate() {
		if(coefficients.length > 0) {
			double[] r = new double[degree()];
		
			for(int i = 1; i <= r.length; i++) {
				r[i - 1] = coefficients[i] * i;
			}
			return new DoublePolynomial1(r);
		} else {
			return ZERO;
		}
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public DoublePolynomial1 multiply(DoublePolynomial1 b) {
		double[] r = new double[degree() + b.degree() + 1];

		Arrays.fill(r, 0);
		for(int i = 0; i <= degree(); i++) {
			for(int j = 0; j <= b.degree(); j++) {
				r[i + j] =
					r[i + j] + coefficient(i) * b.coefficient(j);
			}
		}
		return new DoublePolynomial1(r);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return Arrays.toString(coefficients);
	}

}
