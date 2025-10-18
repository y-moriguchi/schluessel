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
import net.morilib.lisp.LispArithmeticException;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;
import net.morilib.lisp.math.algebra.ILispNegatable;
import net.morilib.lisp.math.algebra.ILispSubtractable;
import net.morilib.lisp.math.matrix.LispMatrixException;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class Sub extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public Datum eval(
			Datum body,
			Environment env,
			LispMessage mesg) {
		if(body instanceof Cons) {
			Cons  c1  = (Cons)body;
			Datum c1a = c1.getCar();

			Datum li = c1.getCdr();
			if(li.isNil()) {
				if(c1a instanceof ILispNegatable) {
					return (Datum)((ILispNegatable)c1a).uminus();
				} else {
					throw mesg.getError("err.math.require.negatable");
				}
			} else {
				ILispSubtractable res;

				if(c1a instanceof ILispSubtractable) {
					res = (ILispSubtractable)c1a;
				} else {
					throw mesg.getError(
							"err.math.require.subtractable");
				}

				while(true) {
					if(li.isNil()) {
						return (Datum)res;
					} else if(li instanceof Cons) {
						Cons  c2  = (Cons)li;
						Datum c2a = c2.getCar();

						if(c2a instanceof ILispSubtractable) {
							try {
								res = res.sub((ILispSubtractable)c2a);
							} catch(LispArithmeticException e) {
								throw mesg.getError(e.getErrorCode());
							} catch(ClassCastException e) {
								throw mesg.getError(
										"err.math.require.sametype",
										c2a);
							} catch(LispMatrixException e) {
								throw mesg.getError(
										"err.math.require.sametype",
										c2a);
							}
							li = c2.getCdr();
						} else {
							throw mesg.getError(
									"err.math.require.subtractable",
									c2a);
						}
					} else {
						throw mesg.getError("err.list");
					}
				}
			}
		} else if(body instanceof Nil) {
			throw mesg.getError("err.argument", symbolName);
		} else {
			throw mesg.getError("err.require.number", body);
		}
	}

}
