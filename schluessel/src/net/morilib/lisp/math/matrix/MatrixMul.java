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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public class MatrixMul extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		ILispMatrix m;
		Datum r = SubrUtils.nextIf(itr, mesg, body);
		Datum x;

		if(r instanceof LispNumber) {
			x = SubrUtils.nextIf(itr, mesg, body);
			SubrUtils.checkTerminated(itr, body, mesg);
			if(x instanceof ILispMatrix) {
				return (Datum)((ILispMatrix)x).mul(
						(LispNumber)r);
			} else if(r instanceof ILispNumberVector) {
				return (Datum)((ILispNumberVector)x).mul(
						(LispNumber)r);
			} else {
				throw mesg.getError("err.matrix.require.numbermatrix",
						x);
			}
		} else if(r instanceof ILispMatrix) {
			m = (ILispMatrix)r;
			while(itr.hasNext()) {
				x = itr.next();
				if(x instanceof ILispMatrix) {
					try {
						m = m.mul((ILispMatrix)x);
					} catch (LispMatrixException e) {
						throw mesg.getError(
								"err.matrix.require.multipliable", x);
					}
				} else if(x instanceof ILispNumberVector) {
					SubrUtils.checkTerminated(itr, body, mesg);
					try {
						return (Datum)m.mul((ILispNumberVector)x);
					} catch (LispMatrixException e) {
						throw mesg.getError(
								"err.matrix.require.multipliable", x);
					}
				} else {
					throw mesg.getError(
							"err.matrix.require.numbermatrix", x);
				}
			}
			SubrUtils.checkTerminated(itr, body, mesg);
			return (Datum)m;
		} else {
			throw mesg.getError("err.matrix.require.numbermatrix",
					r);
		}
	}

}
