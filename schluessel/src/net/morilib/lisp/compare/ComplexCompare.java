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
package net.morilib.lisp.compare;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/20
 */
public class ComplexCompare extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		LispInteger res;

		if(!(c1a instanceof LispComplex)) {
			throw mesg.getError("err.require.complex", c1a);
		} else if(!(c2a instanceof LispComplex)) {
			throw mesg.getError("err.require.complex", c2a);
		} else if((res = SRFI67.compareReal(c1a.getReal(),
				c2a.getReal())).signum() != 0) {
			return res;
		} else {
			return SRFI67.compareReal(c1a.getImag(), c2a.getImag());
		}
	}

}
