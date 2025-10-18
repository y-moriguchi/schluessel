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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/03
 */
public class PolynomialMul extends PolynomialOperator1 {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.PolynomialOperator1#initValue()
	 */
	@Override
	protected LispPolynomial initValue() {
		return LispPolynomial.ONE;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.PolynomialOperator1#calculate(net.morilib.lisp.math.LispPolynomial, net.morilib.lisp.math.LispPolynomial)
	 */
	@Override
	protected LispPolynomial calculate(
			LispPolynomial o1, LispPolynomial o2) {
		return new LispPolynomial(
				o1.getPoly().multiply(o2.getPoly()));
	}

}
