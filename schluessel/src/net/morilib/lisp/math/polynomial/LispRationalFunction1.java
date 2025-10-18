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

import net.morilib.lang.Hashes;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.math.LispNumberField;
import net.morilib.math.functions.RationalFunction1Coefficients;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/03
 */
public class LispRationalFunction1 extends LispExact1Function {

	//
	private RationalFunction1Coefficients<LispNumberField> rational;
	private Symbol symbol;
	
	/**
	 * 
	 * @param poly
	 * @param symbol
	 */
	public LispRationalFunction1(
			RationalFunction1Coefficients<LispNumberField> rational,
			Symbol symbol) {
		super(symbol);
		this.rational   = rational;
	}

	/**
	 * @return the poly
	 */
	public RationalFunction1Coefficients<LispNumberField>
	getRationalFunction() {
		return rational;
	}

	/**
	 * @return the symbol
	 */
	public Symbol getSymbol() {
		return symbol;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return rational.isUnit();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return rational.isZero();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public LispExact1Function negate() {
		return new LispRationalFunction1(
				rational.negate(), symbol);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public LispExact1Function subtract(LispExact1Function x) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public LispExact1Function add(LispExact1Function x) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public LispExact1Function multiply(int n) {
		return new LispRationalFunction1(
				rational.multiply(n), symbol);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public LispExact1Function multiply(LispExact1Function x) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public LispExact1Function power(int n) {
		return new LispRationalFunction1(
				rational.power(n), symbol);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public LispExact1Function invert() {
		if(rational.getNumerator().isUnit()) {
			return new LispPolynomial1(
					rational.getDenominator(), symbol);
		} else {
			return new LispRationalFunction1(
					rational.invert(), symbol);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public LispExact1Function divide(LispExact1Function x) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int r = Hashes.INIT;
		
		r = Hashes.A * (r + symbol.hashCode());
		r = Hashes.A * (r + rational.hashCode());
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispRationalFunction1) {
			LispRationalFunction1 p = (LispRationalFunction1)obj;
			
			return (symbol.equals(p.symbol) &&
					rational.equals(p.rational));
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return rational.toString(symbol.getName());
	}
	
}
