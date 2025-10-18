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
package net.morilib.lisp.math.sequence;

import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMath;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/12
 */
public class Lisp2OrderLRS extends AbstractLispRealSequence {

	/**
	 * 
	 */
	public Lisp2OrderLRS FIBONACCI = new Lisp2OrderLRS(LispInteger.ONE,
			LispInteger.ONE, LispInteger.ONE, LispInteger.ONE);

	//
	private static final LispReal TWO = LispInteger.valueOf(2);

	//
	private LispComplex[] roots;
	private LispNumber    a, b;

	/**
	 * 
	 * @param p
	 * @param q
	 * @param a1
	 * @param a2
	 */
	public Lisp2OrderLRS(LispReal p, LispReal q,
			LispReal a1, LispReal a2) {
		LispNumber ab, aq;

		roots = LispMath.solveQuadratic(LispInteger.ONE,
				p.negate(), q.negate());
		if(roots.length == 2) {
			ab = (LispComplex)roots[0].sub(roots[1]);
			a  = a2.sub(a1.mul(roots[1])).div(roots[0].mul(ab));
			b  = roots[0].mul(a1).sub(a2).div(roots[1].mul(ab));
		} else {
			aq = roots[0].mul(roots[0]);
			a  = roots[0].mul(a1).mul(TWO).div(aq);
			b  = a2.sub(a1).div(aq);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#get(int)
	 */
	public LispReal get(int i) {
		if(i < 1) {
			throw new IndexOutOfBoundsException();
		} else if(roots.length == 2) {
			return a.mul(roots[0].pow(i)).add(
					b.mul(roots[1].pow(i))).getReal();
		} else {
			return a.mul(roots[0].pow(i)).add(b.mul(
					roots[0].pow(i).mul(
							LispInteger.valueOf(i)))).getReal();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#limit()
	 */
	public LispReal limit() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispSequence#isFinite()
	 */
	public boolean isFinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispSequence#size()
	 */
	public int size() {
		return -1;
	}

}
