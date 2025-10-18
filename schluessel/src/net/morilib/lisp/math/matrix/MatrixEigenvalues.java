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
package net.morilib.lisp.math.matrix;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.math.polynomial1.Polynomial1Solver;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/08
 */
public class MatrixEigenvalues extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		ILispMatrix m;
		LispNumber l;
		double[] cef, eig;
		ConsListBuilder b;

		if(!(c1a instanceof ILispMatrix)) {
			throw mesg.getError("err.matrix.require.numbermatrix",
					c1a);
		} else if(!(m = (ILispMatrix)c1a).isSquare()) {
			throw mesg.getError("err.matrix.require.squarematrix",
					c1a);
		} else {
			cef = new double[m.rowSize() + 1];
			for(int i = 0; i < m.rowSize(); i++) {
				l = LispMatrices.determinantAllMinor(
						m, m.rowSize() - i);
				if(!l.isReal()) {
					throw mesg.getError("err.required.real", l);
				}
				cef[i] = l.getRealDouble();
				if((m.rowSize() - i) % 2 == 1)  cef[i] = -cef[i];
			}
			cef[m.rowSize()] = 1.0;
			eig = Polynomial1Solver.solveAll(cef);

			b = new ConsListBuilder();
			for(int i = 0; i < eig.length; i++) {
				b.append(new LispDouble(eig[i]));
			}
			return b.get();
		}
	}

}
