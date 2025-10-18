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
package net.morilib.math.series;

import java.util.Iterator;

import net.morilib.lang.number.Rational;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public abstract class ContinuedFractionSeries
implements Iterator<Rational> {

	/**
	 * 
	 */
	protected Rational d  = Rational.ZERO;

	/**
	 * 
	 */
	protected Rational dh = Rational.ONE;

	/**
	 * 
	 */
	protected Rational h  = Rational.ONE;

	/**
	 * 
	 */
	protected int n;

	/**
	 * 
	 * @return
	 */
	protected abstract Rational getB0();

	/**
	 * 
	 * @return
	 */
	protected abstract Rational getA1();

	/**
	 * 
	 * @return
	 */
	protected abstract Rational getB1();

	/**
	 * 
	 * @param n
	 * @return
	 */
	protected abstract Rational getAn(int n);

	/**
	 * 
	 * @param n
	 * @return
	 */
	protected abstract Rational getBn(int n);

	/**
	 * 
	 * @return
	 */
	protected Rational init() {
		Rational b0, a1, b1;

		b0 = getB0();  a1 = getA1();  b1 = getB1();
		this.d  = Rational.ONE.divide(b1);
		this.dh = a1.divide(b1);
		this.h  = b0.add(dh);
		this.n  = 1;
		return h;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		return true;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	public Rational next() {
		Rational a, b;

		a  = getAn(n);  b = getBn(n);
		d  = Rational.ONE.divide(b.add(a.multiply(d)));
		dh = b.multiply(d).subtract(Rational.ONE).multiply(dh);
		h  = h.add(dh);
		n++;
		return h;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}

}
