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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/07
 */
public final class Polynomial1Utils {

	//
	private static final DoublePolynomial1 X =
		new DoublePolynomial1(0, -1);
	private static final double ALPHA = 0.72;

	//
	private Polynomial1Utils() {}

	/**
	 * 
	 * @param p1
	 * @param epsilon
	 * @param iterate
	 * @return
	 */
	public static double[][] solveByBairstow1(
			DoublePolynomial1 p0,
			double epsilon, int iterate) {
		double p = 0, q, r, s, D, dp, dq;
		double a11, a12, a21, a22, b1, b2;
		DoublePolynomial1[] q0;
		DoublePolynomial1 g, p1, p2, q1, q2, q3;
		double[][] res = new double[p0.degree()][2];
		int i = 0;

		if(p0.degree() == 0) {
			return null;
		} else {
			p1 = p0.div(p0.coefficient(p0.degree()));
		}

		while(true) {
			p = p1.coefficient(1);
			q = p1.coefficient(0);
			g = null;
			switch(p1.degree()) {
			case 0:  return res;
			case 1:
				res[i][0] = -p1.coefficient(0);
				res[i][1] = 0;
				return res;
			default:
				// step 1
				for(double x : p1.coefficients) {
					p = p + x / p1.coefficients.length;
				}
				q = p + ALPHA * p;

				// step2
				for(int c = 0; true; c++) {
					p2 = new DoublePolynomial1(q, p, 1);
					q0 = p1.divAndRemainder(p2);
					g  = q0[0];
					q1 = q0[1];
					s  = q1.coefficient(0);
					r  = q1.coefficient(1);

					// step3
					q2 = g.multiply(X).divAndRemainder(p2)[1];
					q3 = g.uminus().divAndRemainder(p2)[1];
					a11 = q2.coefficient(1);
					a12 = q3.coefficient(1);
					a21 = q2.coefficient(0);
					a22 = q3.coefficient(0);
					b1  = -r;
					b2  = -s;

					// step4
					if((D = a11 * a22 - a12 * a21) == 0) {
						q = q + ALPHA * q;
						continue;
					}
					dp = ( a22 * b1 - a12 * b2) / D;
					dq = (-a21 * b1 + a11 * b2) / D;

					// step5, step6
					if(Math.abs(dp / p) < epsilon &&
							Math.abs(dq / p) < epsilon) {
						break;
					} else if(c > iterate) {
						return null;
					}
					p = p + ALPHA * dp;
					q = q + ALPHA * dq;
				}
				// go next
			case 2:
				// step7: solve x^2+p*x+q=0
				if((D = p * p - 4 * q) < 0) {
					res[i][0] = res[i + 1][0] = -p / 2;
					res[i][1] = Math.sqrt(-D) / 2;
					res[i + 1][1] = -res[i][1];
				} else {
					res[i][1] = res[i + 1][1] = 0;
					res[i][0]     = (-p + Math.sqrt(D)) / 2;
					res[i + 1][0] = (-p - Math.sqrt(D)) / 2;
				}
				if(p1.degree() == 2)  return res;
				p1 = g;
				i += 2;
			}
		}
	}

}
