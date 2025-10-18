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

import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMath;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.lib.srfi094.SRFI94;
import net.morilib.lisp.math.algebra.ILispRemainder;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/02
 */
public class Quotient extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Environment env, LispMessage mesg) {
		if((c1a instanceof LispReal) && (c2a instanceof LispReal)) {
			LispReal r1 = (LispReal)c1a;
			LispReal r2 = (LispReal)c2a;

			if(r2.signum() == 0) {
				throw mesg.getError("err.divbyzero");
			} else if(r1.isInteger() && r2.isInteger()) {
				BigInteger a = r1.getBigInteger();
				BigInteger b = r2.getBigInteger();

				if(r1.isExact() && r2.isExact()) {
					return LispInteger.valueOf(a.divide(b));
				} else if(SRFI94.isSafe(env)) {
					throw mesg.getError("err.srfi94.outofdomain");
				} else {
					return new LispDouble(a.divide(b).doubleValue());
				}
			} else if(!SRFI94.isSafe(env) &&
					!r1.isInfinity() && !r1.isNaN() &&
					!r2.isInfinity() && !r2.isNaN()) {
				return LispMath.quo(r1, r2);
			} else if(!r1.isInteger()) {
				throw mesg.getError("err.srfi94.outofdomain", r1);
			} else {
				throw mesg.getError("err.srfi94.outofdomain", r2);
			}
		} else if(c1a instanceof ILispRemainder &&
				c2a instanceof ILispRemainder) {
			try {
				return (Datum)((ILispRemainder)c1a).divAndRemainder(
						(ILispRemainder)c2a)[0];
			} catch(ClassCastException e) {
				throw mesg.getError("err.math.require.sametype");
			}
		} else if(!(c1a instanceof LispReal)) {
			throw mesg.getError("err.require.real", c1a);
		} else {
			throw mesg.getError("err.require.real", c2a);
		}
	}

}
