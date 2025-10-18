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
package net.morilib.lisp.r6rs.subr;

import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.subr.BinaryArgs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/02
 */
public class Div0AndMod0 extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Environment env, LispMessage mesg) {
		LispReal div, mod;

		if((c1a instanceof LispReal) && (c2a instanceof LispReal)) {
			LispReal r1 = (LispReal)c1a;
			LispReal r2 = (LispReal)c2a;

			if(r2.signum() == 0) {
				throw mesg.getError("err.divbyzero");
			} else if(r1.isInteger() && r2.isInteger()) {
				BigInteger a = r1.getBigInteger();
				BigInteger b = r2.getBigInteger();

				div = LispInteger.valueOf(a.divide(b));
				mod = LispInteger.valueOf(a.remainder(b));
				if(!(r1.isExact() && r2.isExact())) {
					div = div.toInexact();
					mod = mod.toInexact();
				}
			} else if(!r1.isInteger()) {
				throw mesg.getError("err.require.int", r1);
			} else {
				throw mesg.getError("err.require.int", r2);
			}
		} else if(!(c1a instanceof LispReal)) {
			throw mesg.getError("err.require.real", c1a);
		} else {
			throw mesg.getError("err.require.real", c2a);
		}
		return MultiValues.newValues(div, mod);
	}

}
