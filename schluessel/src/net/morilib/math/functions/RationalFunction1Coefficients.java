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

import net.morilib.lang.Hashes;
import net.morilib.lang.algebra.FieldElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/03
 */
public class RationalFunction1Coefficients<C extends FieldElement<C>>
implements FieldElement<RationalFunction1Coefficients<C>> {

	//
	private Polynomial1Coefficients<C> numerator, denominator;
	
	//
	private RationalFunction1Coefficients(
			Polynomial1Coefficients<C> n,
			Polynomial1Coefficients<C> d) {
		numerator = n;
		denominator = d;
	}
	
	/**
	 * 
	 * @param <C>
	 * @param n
	 * @param d
	 * @return
	 */
	public static<C extends FieldElement<C>>
	RationalFunction1Coefficients<C> valueOf(
			Polynomial1Coefficients<C> n,
			Polynomial1Coefficients<C> d) {
		Polynomial1Coefficients<C> gcd;
		if(d.isZero()) {
			throw new ArithmeticException();
		}
		
		gcd = Polynomial1s.gcd(n, d);
		return new RationalFunction1Coefficients<C>(
				n.divide(gcd),
				d.divide(gcd));
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return numerator.isUnit() && denominator.isUnit();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return numerator.isZero();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public RationalFunction1Coefficients<C> negate() {
		return new RationalFunction1Coefficients<C>(
				numerator.negate(), denominator);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public RationalFunction1Coefficients<C> subtract(
			RationalFunction1Coefficients<C> x) {
		Polynomial1Coefficients<C> x1, x2;
		
		x1 = numerator.multiply(x.denominator);
		x2 = x.numerator.multiply(denominator);
		return valueOf(x1.subtract(x2),
				denominator.multiply(denominator));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public RationalFunction1Coefficients<C> add(
			RationalFunction1Coefficients<C> x) {
		Polynomial1Coefficients<C> x1, x2;
		
		x1 = numerator.multiply(x.denominator);
		x2 = x.numerator.multiply(denominator);
		return valueOf(x1.add(x2),
				denominator.multiply(denominator));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public RationalFunction1Coefficients<C> multiply(int n) {
		return new RationalFunction1Coefficients<C>(
				numerator.multiply(n), denominator);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public RationalFunction1Coefficients<C> multiply(
			RationalFunction1Coefficients<C> x) {
		return valueOf(
				numerator.multiply(x.numerator),
				denominator.multiply(denominator));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public RationalFunction1Coefficients<C> power(int n) {
		return new RationalFunction1Coefficients<C>(
				numerator.power(n),
				denominator.power(n));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public RationalFunction1Coefficients<C> invert() {
		if(isZero()) {
			throw new ArithmeticException();
		}
		return new RationalFunction1Coefficients<C>(
				denominator, numerator);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public RationalFunction1Coefficients<C> divide(
			RationalFunction1Coefficients<C> x) {
		return valueOf(
				numerator.multiply(x.denominator),
				denominator.multiply(numerator));
	}

	/**
	 * @return the numerator
	 */
	public Polynomial1Coefficients<C> getNumerator() {
		return numerator;
	}

	/**
	 * @return the denominator
	 */
	public Polynomial1Coefficients<C> getDenominator() {
		return denominator;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int r = Hashes.INIT;
		
		if(isZero()) {
			return 72;
		}
		r = Hashes.A * (r + numerator.hashCode());
		r = Hashes.A * (r + denominator.hashCode());
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof RationalFunction1Coefficients) {
			RationalFunction1Coefficients<?> x;
			
			x = (RationalFunction1Coefficients<?>)obj;
			return (isZero() && x.isZero() ||
					(numerator.equals(x.numerator) &&
							denominator.equals(x.denominator)));
		}
		return false;
	}

	/**
	 * 
	 * @param var
	 * @return
	 */
	public String toString(String var) {
		if(isZero()) {
			return "0";
		} else if(denominator.isUnit()) {
			return numerator.toString(var);
		}
		return ("(" + numerator.toString(var) + ") /" +
				"(" + denominator.toString(var) + ")");
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return toString("x");
	}

}
