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
import net.morilib.math.functions.ListPolynomial1Coefficients;
import net.morilib.math.functions.Polynomial1Coefficients;
import net.morilib.math.functions.RationalFunction1Coefficients;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/03
 */
public class LispPolynomial1 extends LispExact1Function {

	//
	private Polynomial1Coefficients<LispNumberField> poly;
	
	/**
	 * 
	 * @param poly
	 * @param symbol
	 */
	public LispPolynomial1(
			Polynomial1Coefficients<LispNumberField> poly,
			Symbol symbol) {
		super(symbol);
		this.poly   = poly;
	}
	
	/**
	 * 
	 * @param poly
	 * @param symbol
	 */
	public LispPolynomial1(
			LispNumberField number,
			Symbol symbol) {
		super(symbol);
		this.poly =
			new ListPolynomial1Coefficients<LispNumberField>(number);
	}
	
	/**
	 * @return the poly
	 */
	public Polynomial1Coefficients<LispNumberField> getPoly() {
		return poly;
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
		return poly.isUnit();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return poly.isZero();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public LispExact1Function negate() {
		return new LispPolynomial1(poly.negate(), symbol);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public LispExact1Function subtract(LispExact1Function x) {
		if(!symbol.equals(x.symbol)) {
			throw new IllegalArgumentException();
		} else if(x instanceof LispPolynomial1) {
			return new LispPolynomial1(
					poly.subtract(((LispPolynomial1)x).poly),
					symbol);
		} else if(x instanceof LispRationalFunction1) {
			return LispExact1Function.toRational(this).subtract(x);
		} else {
			throw new IllegalArgumentException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public LispExact1Function add(LispExact1Function x) {
		if(!symbol.equals(x.symbol)) {
			throw new IllegalArgumentException();
		} else if(x instanceof LispPolynomial1) {
			return new LispPolynomial1(
					poly.add(((LispPolynomial1)x).poly),
					symbol);
		} else if(x instanceof LispRationalFunction1) {
			return LispExact1Function.toRational(this).add(x);
		} else {
			throw new IllegalArgumentException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public LispExact1Function multiply(int n) {
		return new LispPolynomial1(poly.multiply(n), symbol);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public LispExact1Function multiply(LispExact1Function x) {
		if(!symbol.equals(x.symbol)) {
			throw new IllegalArgumentException();
		} else if(x instanceof LispPolynomial1) {
			return new LispPolynomial1(
					poly.multiply(((LispPolynomial1)x).poly),
					symbol);
		} else if(x instanceof LispRationalFunction1) {
			return LispExact1Function.toRational(this).multiply(x);
		} else {
			throw new IllegalArgumentException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public LispExact1Function power(int n) {
		return new LispPolynomial1(poly.power(n), symbol);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public LispExact1Function invert() {
		RationalFunction1Coefficients<LispNumberField> r1;
		ListPolynomial1Coefficients<LispNumberField> l1;
		
		l1 = new ListPolynomial1Coefficients<LispNumberField>(
				LispNumberField.ONE);
		r1 = RationalFunction1Coefficients.valueOf(l1, poly);
		return new LispRationalFunction1(r1, symbol);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public LispExact1Function divide(LispExact1Function x) {
		if(!symbol.equals(x.symbol)) {
			throw new IllegalArgumentException();
		} else if(x instanceof LispPolynomial1) {
			return new LispPolynomial1(
					poly.divide(((LispPolynomial1)x).poly),
					symbol);
		} else if(x instanceof LispRationalFunction1) {
			return LispExact1Function.toRational(this).divide(x);
		} else {
			throw new IllegalArgumentException();
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int r = Hashes.INIT;
		
		r = Hashes.A * (r + symbol.hashCode());
		r = Hashes.A * (r + poly.hashCode());
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispPolynomial1) {
			LispPolynomial1 p = (LispPolynomial1)obj;
			
			return symbol.equals(p.symbol) && poly.equals(p.poly);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return poly.toString(symbol.getName());
	}
	
}
