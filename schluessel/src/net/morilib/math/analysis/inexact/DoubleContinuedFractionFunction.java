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
package net.morilib.math.analysis.inexact;

import net.morilib.lang.transform.DoubleTransform;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/16
 */
public abstract class DoubleContinuedFractionFunction
implements DoubleTransform {

	// ref: http://www.tac.tsukuba.ac.jp/~yaoki/conf.pdf
	/**
	 * 
	 */
	protected double d  = 0;

	/**
	 * 
	 */
	protected double dh = 1;

	/**
	 * 
	 */
	protected double h  = 1;

	/**
	 * 
	 */
	protected int n;

	/**
	 * 
	 * @return
	 */
	protected abstract double getB0(double x);

	/**
	 * 
	 * @return
	 */
	protected abstract double getA1(double x);

	/**
	 * 
	 * @return
	 */
	protected abstract double getB1(double x);

	/**
	 * 
	 * @param n
	 * @return
	 */
	protected abstract double getAn(int n, double x);

	/**
	 * 
	 * @param n
	 * @return
	 */
	protected abstract double getBn(int n, double x);

	/**
	 * 
	 * @return
	 */
	protected double init(double x) {
		double b0, a1, b1;

		b0 = getB0(x);  a1 = getA1(x);  b1 = getB1(x);
		this.d  = 1.0 / b1;
		this.dh = a1 / b1;
		this.h  = b0 + dh;
		this.n  = 2;
		return h;
	}

	/**
	 * 
	 * @return
	 */
	protected double next(double x) {
		double a, b;

		a  = getAn(n, x);  b = getBn(n, x);
		d  = 1.0 / (b + a * d);
		dh = (b * d - 1.0) * dh;
		h  = h + dh;
		n++;
		return h;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.transform.DoubleTransform#f(double)
	 */
	public double f(double x) {
		double r, s;

		r = init(x);
		s = Double.NEGATIVE_INFINITY;
//		for(int i = 0; !Double.isNaN(r) && r != s; i++) {
		while(!Double.isNaN(r) && r != s) {
//					if(i >= MAX_ITERATE) {
//						throw new ArithmeticException();
//					}
			s = r;
			if(Double.isNaN(r = next(x)))  return Double.NaN;
		}
		return r;
	}

}
