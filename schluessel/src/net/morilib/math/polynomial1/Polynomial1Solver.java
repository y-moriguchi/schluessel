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

import java.util.ArrayList;
import java.util.List;

import net.morilib.util.primitive.DoubleArrayVector;
import net.morilib.util.primitive.DoubleVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/07
 */
public class Polynomial1Solver {

	//
	private List<DoublePolynomial1> strumlist;

	/**
	 * 
	 * @param p
	 */
	public Polynomial1Solver(DoublePolynomial1 p) {
		DoublePolynomial1[] r;
		DoublePolynomial1 t1, t2;

		strumlist = new ArrayList<DoublePolynomial1>();
		strumlist.add(t1 = p);
		strumlist.add(t2 = p.differenciate());
		while(!(r = t1.divAndRemainder(t2))[1].isZero()) {
			t1 = t2;
			strumlist.add(t2 = r[1].uminus());
		}
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public int changeSign(double a) {
		double w = Double.NaN, v;
		int cnt = 0;

		for(DoublePolynomial1 pp : strumlist) {
			v = pp.substitute(a);
			if(!Double.isNaN(w) && w * v < 0)  cnt++;
			w = (v != 0) ? v : w;
		}
		return cnt;
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @param p
	 * @return
	 */
	public int howManyRoots(double a, double b) {
		int w, v;

		w = changeSign(a);
		v = changeSign(b);
		return w - v;
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public double solve(double a, double b) {
		double t;
		int rts;

		if(howManyRoots(a, b) == 0)  return Double.NaN;
		while(Math.abs(b - a) > Math.max(Math.ulp(a), Math.ulp(b))) {
			rts = howManyRoots(a, b);
			if(rts > 0) {
				a = (a + b) / 2;
			} else {
				t = a;
				a = a - (b - a);
				b = t;
			}
		}
		return b;
	}

	/**
	 * 
	 * @param p
	 * @return
	 */
	public static double[] solveAll(DoublePolynomial1 p) {
		DoublePolynomial1 q = p;
		Polynomial1Solver slv;
		DoubleVector l = new DoubleArrayVector();
		double w;

		while(q.degree() > 1) {
			slv = new Polynomial1Solver(q);
			w = slv.solve(-Double.MAX_VALUE, Double.MAX_VALUE);
			if(Double.isNaN(w)) {
				return l.toDoubleArray();
			}
			l.add(w);
			q = q.divAndRemainder(new DoublePolynomial1(
					-(double)((float)w), 1))[0];
		}
		l.add(-q.coefficient(0) / q.coefficient(1));
		return l.toDoubleArray();
	}

	/**
	 * 
	 * @param p
	 * @return
	 */
	public static double[] solveAll(double[] p) {
		return solveAll(new DoublePolynomial1(p));
	}

}
