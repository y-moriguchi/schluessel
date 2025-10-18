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
package net.morilib.lisp.math;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.math.algebra.ILispMultipliable;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/11/18
 */
public class Square extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		if(c1a instanceof LispNumber) {
			return ((LispNumber)c1a).mul((LispNumber)c1a);
		} else if(c1a instanceof ILispSquareIdempotence) {
			return c1a;
		} else if(c1a instanceof ILispSquareable) {
			return ((ILispSquareable)c1a).square();
		} else if(c1a instanceof ILispMultipliable) {
			return (Datum)((ILispMultipliable)c1a).mul(
					(ILispMultipliable)c1a);
		} else {
			throw mesg.getError("err.math.require.squareable", c1a);
		}
	}

}
