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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.morilib.lang.algebra.FieldElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/02
 */
public class ListPolynomial1Coefficients<C extends FieldElement<C>>
extends Polynomial1Coefficients<C>
implements java.io.Serializable {

	//
	private static final long serialVersionUID = 7431581179968261833L;
	
	//
	private List<C> coefficients;
	
	/**
	 * 
	 * @param c
	 */
	public ListPolynomial1Coefficients(C c) {
		coefficients = new ArrayList<C>();
		coefficients.add(c);
	}
	
	//
	private ListPolynomial1Coefficients(int n) {
		coefficients = new ArrayList<C>(n);
	}
	
	//
	private ListPolynomial1Coefficients(
			ListPolynomial1Coefficients<C> d) {
		coefficients = new ArrayList<C>(d.coefficients);
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
			coefficients.set(n, null);
		} else {
			coefficients.set(n, v);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.functions.Polynomial1Coefficients#indexIterator()
	 */
	@Override
	protected Iterator<Integer> indexIterator() {
		final int si;
		int s0 = 0;
		
		while(s0 < coefficients.size() &&
				coefficients.get(s0) == null) {
			s0++;
		}
		si = s0;
		
		return new Iterator<Integer>() {

			private int i = si;
			
			public boolean hasNext() {
				return i < coefficients.size();
			}

			public Integer next() {
				int ri = i;
				
				while(i < coefficients.size() &&
						coefficients.get(i) == null) {
					i++;
				}
				return ri;
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.functions.Polynomial1Coefficients#deg()
	 */
	@Override
	public int deg() {
		int s0 = coefficients.size() - 1;
		
		while(s0 >= 0 && coefficients.get(s0) == null) {
			s0--;
		}
		return s0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.functions.Polynomial1Coefficients#newInstance(int)
	 */
	@Override
	protected Polynomial1Coefficients<C> newInstance(int size) {
		if(size < 0) {
			return new ListPolynomial1Coefficients<C>(0);
		} else {
			return new ListPolynomial1Coefficients<C>(size + 1);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.functions.Polynomial1Coefficients#duplicate()
	 */
	@Override
	public Polynomial1Coefficients<C> duplicate() {
		return new ListPolynomial1Coefficients<C>(this);
	}

}
