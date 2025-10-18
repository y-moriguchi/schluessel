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
import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/03
 */
public class CopyBitField extends QuaternaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Environment env, LispMessage mesg) {
		int s = SubrUtils.getSmallInt(c3a, mesg);
		int e = SubrUtils.getSmallInt(c4a, mesg);

		if(s < 0) {
			throw mesg.getError("err.require.int.nonnegative", c3a);
		} else if(e < 0) {
			throw mesg.getError("err.require.int.nonnegative", c4a);
		} else if(s > e) {
			throw mesg.getError("err.srfi60.index.invalid");
		} else if(s == e) {
			return LispInteger.ZERO;
		} else if(!(c1a instanceof LispInteger)) {
			throw mesg.getError("err.require.int", c1a);
		} else if(!(c2a instanceof LispInteger)) {
			throw mesg.getError("err.require.int", c2a);
		} else {
			BigInteger b = c1a.getBigInteger();
			BigInteger c = c2a.getBigInteger();
			BigInteger m;

			m = BigInteger.valueOf(-1).shiftLeft(e - s);
			m = m.not().shiftLeft(s);
			return LispInteger.valueOf(b.andNot(m).or(c.and(m)));
		}
	}

}
