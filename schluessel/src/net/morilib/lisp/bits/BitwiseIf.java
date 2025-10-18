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
package net.morilib.lisp.bits;

import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.TernaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/03
 */
public class BitwiseIf extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		if(!(c1a instanceof LispInteger)) {
			throw mesg.getError("err.require.int", c1a);
		} else if(!(c2a instanceof LispInteger)) {
			throw mesg.getError("err.require.int", c2a);
		} else if(!(c3a instanceof LispInteger)) {
			throw mesg.getError("err.require.int", c3a);
		} else {
			BigInteger m = c1a.getBigInteger();
			BigInteger a = c2a.getBigInteger();
			BigInteger b = c3a.getBigInteger();

			return LispInteger.valueOf(a.and(m).or(b.andNot(m)));
		}
	}

}
