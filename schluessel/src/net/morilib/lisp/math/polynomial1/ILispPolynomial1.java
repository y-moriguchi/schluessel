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
package net.morilib.lisp.math.polynomial1;

import net.morilib.lisp.LispNumber;
import net.morilib.lisp.math.algebra.ILispAlgebraicInteger;
import net.morilib.lisp.math.algebra.ILispNumberEqual;
import net.morilib.lisp.math.analysis.ILispDifferenciatable1;
import net.morilib.lisp.math.analysis.ILispFunction1;
import net.morilib.lisp.math.analysis.ILispIntegrable1;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/24
 */
public interface ILispPolynomial1
extends ILispAlgebraicInteger<ILispPolynomial1>,
ILispDifferenciatable1<ILispPolynomial1>, ILispFunction1,
ILispIntegrable1, ILispNumberEqual<ILispPolynomial1> {

	/**
	 * 
	 * @return
	 */
	public boolean isZero();

	/**
	 * 
	 * @param n
	 * @return
	 */
	public LispNumber coefficient(int n);

	/**
	 * 
	 * @return
	 */
	public int degree();

	/**
	 * 
	 * @param x
	 * @return
	 */
	public LispNumber substitute(LispNumber x);

	/**
	 * 
	 * @param a
	 * @return
	 */
	public ILispPolynomial1 mul(LispNumber a);

	/**
	 * 
	 * @param a
	 * @return
	 */
	public ILispPolynomial1 div(LispNumber a);

	/**
	 * 
	 * @param b
	 * @return
	 */
	public ILispPolynomial1 add(ILispPolynomial1 b);

	/**
	 * 
	 * @param b
	 * @return
	 */
	public ILispPolynomial1 sub(ILispPolynomial1 b);

	/**
	 * 
	 * @return
	 */
	public ILispPolynomial1 uminus();

	/**
	 * 
	 * @param b
	 * @return
	 */
	public ILispPolynomial1 mul(ILispPolynomial1 b);

	/**
	 * 
	 * @param b
	 * @return
	 */
	public ILispPolynomial1[] divAndRemainder(ILispPolynomial1 b);

	/**
	 * 
	 * @return
	 */
	public ILispPolynomial1 differenciate();

	/**
	 * 
	 * @param constant
	 * @return
	 */
	public ILispPolynomial1 integrate(LispNumber constant);

}
