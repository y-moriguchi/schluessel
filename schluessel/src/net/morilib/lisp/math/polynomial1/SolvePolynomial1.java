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

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.math.polynomial1.DoublePolynomial1;
import net.morilib.math.polynomial1.Polynomial1Solver;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/07
 */
public class SolvePolynomial1 extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		ILispPolynomial1 p;
		DoublePolynomial1 dp;
		double[] cef, rs;
		ConsListBuilder b = new ConsListBuilder();

		if(!(c1a instanceof ILispPolynomial1)) {
			throw mesg.getError("err.math.require.poly1", c1a);
		} else if((p = (ILispPolynomial1)c1a).degree() >= 2) {
			cef = new double[((ILispPolynomial1)c1a).degree() + 1];
			for(int i = 0; i <= p.degree(); i++) {
				cef[i] = SubrUtils.getDouble(p.coefficient(i), mesg);
			}
			dp = new DoublePolynomial1(cef);
			rs = Polynomial1Solver.solveAll(dp);

			for(double d : rs) {
				b.append(new LispDouble(d));
			}
			return b.get();
		} else if(p.degree() == 1) {
			return b.append(p.coefficient(1).div(
					p.coefficient(0)).uminus()).get();
//		} else if(p.isZero()) {
//			return LispBoolean.FALSE;
//		} else {
//			return Nil.NIL;
		} else {
			return LispBoolean.FALSE;
		}
	}

}
