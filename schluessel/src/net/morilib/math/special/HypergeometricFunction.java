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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/16
 */
public class HypergeometricFunction implements DoubleTransform {

	//
	private double[] a, b;

	/**
	 * 
	 * @param a
	 * @param b
	 */
	public HypergeometricFunction(double a, double b) {
		this.a = new double[1];
		this.b = new double[1];
		this.a[0] = a;  this.b[0] = b;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.transform.DoubleTransform#f(double)
	 */
	public double f(double x) {
		double r, s, t;
		int k;

		r = t = 1.0;  s = Double.NaN;
		for(int n = 0; Double.isNaN(s) || r != s; n++) {
			s = r;
			for(k = 0; k < a.length && k < b.length; k++) {
				t = t * (a[k] + n) / (b[k] + n);
			}
			for(; k < a.length; k++)  t = t * (a[k] + n);
			for(; k < b.length; k++)  t = t / (b[k] + n);
			t = t / (n + 1) * x;
			r = r + t;
			if(Double.isNaN(r))  break;
		}
		return r;
	}	

}
