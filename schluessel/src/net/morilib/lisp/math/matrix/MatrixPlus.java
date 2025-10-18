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
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public class MatrixPlus extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		ILispMatrix r = null;

		while(itr.hasNext()) {
			Datum d = itr.next();

			if(d instanceof ILispMatrix) {
				if(r == null) {
					r = (LispMatrix)d;
				} else {
					try {
						r = r.add((ILispMatrix)d);
					} catch (LispMatrixException e) {
						throw mesg.getError(
								"err.matrix.require.samesize", d);
					}
				}
			} else {
				throw mesg.getError("err.matrix.require.numbermatrix",
						d);
			}
		}

		SubrUtils.checkTerminated(itr, body, mesg);
		if(r == null) {
			throw mesg.getError("err.argument", body);
		}
		return (Datum)r;
	}

}
