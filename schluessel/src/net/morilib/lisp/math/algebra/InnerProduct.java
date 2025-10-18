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
package net.morilib.lisp.math.algebra;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.math.matrix.LispMatrixException;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/16
 */
public class InnerProduct extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		if(!(c1a instanceof ILispInnerProduct)) {
			throw mesg.getError("err.math.require.innerproduct", c1a);
		} else if(!(c2a instanceof ILispInnerProduct)) {
			throw mesg.getError("err.math.require.innerproduct", c2a);
		} else {
			try {
				return ((ILispInnerProduct)c1a).innerProduct(
						(ILispInnerProduct)c2a);
			} catch(ClassCastException e) {
				throw mesg.getError("err.math.mismatch", c2a);
			} catch(LispMatrixException e) {
				throw mesg.getError("err.math.mismatch", c2a);
			}
		}
	}

}
