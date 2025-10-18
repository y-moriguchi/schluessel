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
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.Nil;
import net.morilib.lisp.math.algebra.ILispMultipliable;
import net.morilib.lisp.math.algebra.ILispScalarMultipliable;
import net.morilib.lisp.math.matrix.LispMatrixException;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class Mul extends MathOperator1<ILispMultipliable<?>> {

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	protected ILispMultipliable calculate(ILispMultipliable o1,
			ILispMultipliable o2) {
		return (ILispMultipliable)o1.mul(o2);
	}

	@Override
	protected LispNumber initValue() {
		return LispInteger.ONE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@SuppressWarnings("rawtypes")
	public Datum eval(
			Datum body,
			Environment env,
			LispMessage mesg) {
		ILispMultipliable res = null;
		ILispScalarMultipliable re2 = null;

		Datum li = body;
		while(true) {
			if(li instanceof Nil) {
				if(re2 == null) {
					return (res != null) ? (Datum)res : initValue();
				} else {
					return (Datum)re2;
				}
			} else if(li instanceof Cons) {
				Cons c2 = (Cons)li;
				Datum c2a = c2.getCar();

				if(!(c2a instanceof ILispMultipliable ||
						c2a instanceof ILispScalarMultipliable ||
						c2a instanceof LispNumber)) {
					throw mesg.getError(
							"err.require.number", c2a);
				} else if(res == null && re2 == null) {
					if(c2a instanceof ILispMultipliable) {
						res = (ILispMultipliable)c2a;
					} else {
						re2 = (ILispScalarMultipliable)c2a;
					}
				} else if(re2 != null && c2a instanceof LispNumber) {
					re2 = re2.mul((LispNumber)c2a);
				} else if(res != null &&
						res instanceof LispNumber &&
						c2a instanceof ILispScalarMultipliable) {
					try {
						re2 = ((ILispScalarMultipliable)c2a).mul(
								(LispNumber)res);
						res = null;
					} catch(ClassCastException e) {
						throw mesg.getError(
								"err.math.invalid.scalar", c2a);
					} catch(IllegalArgumentException e) {
						throw mesg.getError(
								"err.math.invalid.scalar", c2a);
					}
				} else {
					try {
						res = calculate(res, (ILispMultipliable)c2a);
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
