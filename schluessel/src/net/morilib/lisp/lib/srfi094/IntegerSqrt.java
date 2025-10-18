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
package net.morilib.lisp.lib.srfi094;

import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.math.Math2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/06/29
 */
public class IntegerSqrt extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		if(c1a instanceof LispInteger) {
			BigInteger x = ((LispInteger)c1a).getBigInteger();
			BigInteger r;

			if(x.signum() < 0) {
				throw mesg.getError("err.srfi94.outofdomain", c1a);
			} else if((r = Math2.sqrtExact(x)).signum() < 0) {
				throw mesg.getError("err.srfi94.resultnotinteger");
			} else {
				return LispInteger.valueOf(r);
			}
		} else {
			throw mesg.getError("err.require.int", c1a);
		}
	}

}
