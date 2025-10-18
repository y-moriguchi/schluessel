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
package net.morilib.math.functions;

import net.morilib.lang.algebra.FieldElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/03
 */
public final class Polynomial1s {

	//
	private Polynomial1s() { }
	
	/**
	 * calculates the G.C.D. of the given values.
	 * <p>与えられた数の最大公約数を得る。
	 * 
	 * @param v1 value to be calculated 
	 * @param v2 another value to be calculated
	 * @return G.C.D. of the given values
	 */
	public static<C extends FieldElement<C>>
	Polynomial1Coefficients<C> gcd(
			Polynomial1Coefficients<C> v1,
			Polynomial1Coefficients<C> v2) {
		Polynomial1Coefficients<C> x = v1;
		Polynomial1Coefficients<C> y = v2;
		
		if(x.isZero() && y.isZero()) {
			return x;
		} else {
			while(!y.isZero()) {
				Polynomial1Coefficients<C> t =
					x.remainder(y);
				
				x = y;
				y = t;
			}
			return x;
		}
	}

}
