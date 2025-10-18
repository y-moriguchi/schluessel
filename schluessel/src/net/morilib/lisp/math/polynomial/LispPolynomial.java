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

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.math.LispNumberField;
import net.morilib.math.functions.Polynomial;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/03
 */
public class LispPolynomial extends Datum {

	/**
	 * 
	 */
	public static final LispPolynomial ZERO =
		new LispPolynomial(LispInteger.ZERO);
	
	/**
	 * 
	 */
	public static final LispPolynomial ONE =
		new LispPolynomial(LispInteger.ONE);
	
	//
	private Polynomial<String, LispNumberField> poly;

	/**
	 * 
	 * @param sym
	 * @param num
	 */
	public LispPolynomial(Symbol sym, LispNumber num) {
		poly = new Polynomial<String, LispNumberField>(
				new LispNumberField(num), sym.getName());
	}

	/**
	 * 
	 * @param num
	 */
	public LispPolynomial(LispNumber num) {
		poly = new Polynomial<String, LispNumberField>(
				new LispNumberField(num));
	}
	
	/**
	 * 
	 * @param poly
	 */
	public LispPolynomial(Polynomial<String, LispNumberField> poly) {
		this.poly = poly;
	}
	
	/**
	 * @return the poly
	 */
	public Polynomial<String, LispNumberField> getPoly() {
		return poly;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return poly.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispPolynomial) {
			return poly.equals(((LispPolynomial)obj).poly);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return poly.toString();
	}
	
}
