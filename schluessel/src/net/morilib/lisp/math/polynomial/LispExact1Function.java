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
package net.morilib.lisp.math.polynomial;

import net.morilib.lang.algebra.FieldElement;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.math.LispNumberField;
import net.morilib.math.functions.ListPolynomial1Coefficients;
import net.morilib.math.functions.RationalFunction1Coefficients;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/03
 */
public abstract class LispExact1Function extends Datum
implements FieldElement<LispExact1Function> {

	//
	/*package*/ Symbol symbol;
	
	//
	/*package*/ LispExact1Function(Symbol symbol) {
		this.symbol = symbol;
	}
	
	/**
	 * 
	 * @param p
	 * @return
	 */
	public static LispRationalFunction1 toRational(LispPolynomial1 p) {
		RationalFunction1Coefficients<LispNumberField> r1;
		ListPolynomial1Coefficients<LispNumberField> l1;
		
		l1 = new ListPolynomial1Coefficients<LispNumberField>(
				LispNumberField.ONE);
		r1 = RationalFunction1Coefficients.valueOf(p.getPoly(), l1);
		return new LispRationalFunction1(r1, p.symbol);
	}
	
}
