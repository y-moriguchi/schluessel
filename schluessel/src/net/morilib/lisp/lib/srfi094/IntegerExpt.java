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
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.math.Math2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/06/30
 */
public class IntegerExpt extends BinaryArgs {

	//
	private static final BigInteger LIMIT_INT =
		BigInteger.valueOf(Integer.MAX_VALUE);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		if(!(c1a instanceof LispInteger)) {
			throw mesg.getError("err.require.int", c1a);
		} else if(!(c2a instanceof LispInteger)) {
			throw mesg.getError("err.require.int", c2a);
		} else {
			BigInteger b1 = c1a.getBigInteger();
			BigInteger b2 = c2a.getBigInteger();

			if(b2.signum() < 0) {
				throw mesg.getError("err.srfi94.outofdomain", c2a);
			} else if(b2.signum() == 0) {
				return LispInteger.ZERO;
			} else if(b1.signum() == 0) {
				return LispInteger.ZERO;
			} else if(b1.equals(BigInteger.ONE)) {
				return LispInteger.ONE;
			} else if(b2.compareTo(LIMIT_INT) <= 0) {
				return LispInteger.valueOf(b1.pow(b2.intValue()));
			} else {
				return LispInteger.valueOf(Math2.pow(b1, b2));
			}
		}
	}

}
