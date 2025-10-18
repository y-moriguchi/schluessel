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
public class GaussianHypergeometricFunction
implements DoubleTransform {

	//
	private double a, b, c;

	/* (non-Javadoc)
	 * @see net.morilib.lang.transform.DoubleTransform#f(double)
	 */
	public double f(double x) {
		double r, s, t;

		r = t = 1.0;  s = Double.NaN;
		for(int n = 0; !Double.isNaN(s) && r != s; n++) {
			s = r;
			t = t * (a + n) / (c + n) * (b + n) / (n + 1) * x;
			r = r + t;
			if(Double.isNaN(r))  break;
		}
		return r;
	}	
	
}
