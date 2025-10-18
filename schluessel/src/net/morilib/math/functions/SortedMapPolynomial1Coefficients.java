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

import java.util.Iterator;
import java.util.SortedMap;
import java.util.TreeMap;

import net.morilib.lang.algebra.FieldElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/02
 */
public class SortedMapPolynomial1Coefficients
<C extends FieldElement<C>>
extends Polynomial1Coefficients<C>
implements java.io.Serializable {
	
	//
	private static final long serialVersionUID = 3911167163889106012L;
	
	//
	private SortedMap<Integer, C> coefficients;
	
	/**
	 * 
	 * @param coefficients
	 */
	public SortedMapPolynomial1Coefficients(
			SortedMap<Integer, C> coefficients) {
		this.coefficients = new TreeMap<Integer, C>(coefficients);
	}
	
	//
	private SortedMapPolynomial1Coefficients() {
		coefficients = new TreeMap<Integer, C>();
	}
	
	//
	private SortedMapPolynomial1Coefficients(
			SortedMapPolynomial1Coefficients<C> d) {
		coefficients = new TreeMap<Integer, C>(d.coefficients);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.math.functions.Polynomial1Coefficients#getCoefficient(int)
	 */
	@Override
	public C getCoefficient(int n) {
		return coefficients.get(n);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.functions.Polynomial1Coefficients#setCoefficient(int, net.morilib.lang.algebra.FieldElement)
	 */
	@Override
	protected void setCoefficient(int n, C v) {
		if(v.isZero()) {
			coefficients.remove(n);
		} else {
			coefficients.put(n, v);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.functions.Polynomial1Coefficients#indexIterator()
	 */
	@Override
	protected Iterator<Integer> indexIterator() {
		return coefficients.keySet().iterator();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.functions.Polynomial1Coefficients#deg()
	 */
	@Override
	public int deg() {
		return coefficients.isEmpty() ?
				-1 : coefficients.lastKey();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.functions.Polynomial1Coefficients#newInstance(int)
	 */
	@Override
	protected Polynomial1Coefficients<C> newInstance(int size) {
		return new SortedMapPolynomial1Coefficients<C>();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.functions.Polynomial1Coefficients#duplicate()
	 */
	@Override
	public Polynomial1Coefficients<C> duplicate() {
		return new SortedMapPolynomial1Coefficients<C>(this);
	}

}
