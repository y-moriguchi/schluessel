/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.subr;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;
import net.morilib.lisp.math.matrix.LispMatrixException;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class MathOperator1<A> extends Subr {

	/**
	 * 
	 * @return
	 */
	protected abstract LispNumber initValue();

	/**
	 * 
	 * @param o1
	 * @param o2
	 * @return
	 */
	protected abstract A calculate(A o1, A o2);

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@SuppressWarnings({ "unchecked" })
	public Datum eval(
			Datum body,
			Environment env,
			LispMessage mesg) {
		A res = null;

		Datum li = body;
		while(true) {
			if(li instanceof Nil) {
				return (res != null) ? (Datum)res : initValue();
			} else if(li instanceof Cons) {
				Cons c2 = (Cons)li;
				Datum c2a = c2.getCar();

				if(res == null) {
					try {
						res = (A)c2a;
					} catch(ClassCastException e) {
						throw mesg.getError(
								"err.require.number", c2a);
					}
				} else {
					try {
						res = calculate(res, (A)c2a);
					} catch(ClassCastException e) {
						throw mesg.getError(
								"err.math.require.sametype", c2a);
					} catch(LispMatrixException e) {
						throw mesg.getError(
								"err.math.require.sametype", c2a);
					}
				}
				li = c2.getCdr();
			} else {
				throw mesg.getError("err.list");
			}
		}
	}

}
