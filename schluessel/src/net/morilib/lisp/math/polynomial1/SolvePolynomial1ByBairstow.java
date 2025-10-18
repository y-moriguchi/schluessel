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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.math.polynomial1.DoublePolynomial1;
import net.morilib.math.polynomial1.Polynomial1Utils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/07
 */
public class SolvePolynomial1ByBairstow extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		double epsilon = SubrUtils.nextDouble(itr, 1e-05);
		int iterate = SubrUtils.nextSmallInt(itr, 1000, mesg);
		ILispPolynomial1 p;
		DoublePolynomial1 dp;
		double[] cef;
		double[][] rs;
		ConsListBuilder b;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(!(c1a instanceof ILispPolynomial1)) {
			throw mesg.getError("err.math.require.poly1", c1a);
		} else if((p = (ILispPolynomial1)c1a).degree() >= 1) {
			cef = new double[((ILispPolynomial1)c1a).degree() + 1];
			for(int i = 0; i <= p.degree(); i++) {
				cef[i] = SubrUtils.getDouble(p.coefficient(i), mesg);
			}
			dp = new DoublePolynomial1(cef);
			rs = Polynomial1Utils.solveByBairstow1(
					dp, epsilon, iterate);

			if(rs == null) {
				// an invalid equation or not converged
				return LispBoolean.FALSE;
			}
			b = new ConsListBuilder();
			for(double[] d : rs) {
				b.append(LispComplex.newComplex(d[0], d[1]));
			}
			return b.get();
//		} else if(p.isZero()) {
//			return LispBoolean.FALSE;
//		} else {
//			return Nil.NIL;
		} else {
			return LispBoolean.FALSE;
		}
	}

}
