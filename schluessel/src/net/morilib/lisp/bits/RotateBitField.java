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
public class RotateBitField extends QuaternaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.QuaternaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Environment env, LispMessage mesg) {
		int c = SubrUtils.getSmallInt(c2a, mesg);
		int s = SubrUtils.getSmallInt(c3a, mesg);
		int e = SubrUtils.getSmallInt(c4a, mesg);

		if(s < 0) {
			throw mesg.getError("err.require.int.nonnegative", c3a);
		} else if(e < 0) {
			throw mesg.getError("err.require.int.nonnegative", c4a);
		} else if(s > e) {
			throw mesg.getError("err.srfi60.index.invalid");
		} else if(s == e) {
			return c1a;
		} else if(c1a instanceof LispInteger) {
			BigInteger b = c1a.getBigInteger();
			BigInteger m = BigInteger.valueOf(-1);
			BigInteger t = m;
			BigInteger d, f;

			if((c = c % (e - s)) < 0) {
				c += (e - s);
			}
			m = m.shiftLeft(e - s).not().shiftLeft(s);
			f = b.and(m);
			if(c == 0) {
				return c1a;
			} else {
				t = t.shiftLeft(c).not().shiftLeft(e - c);
				d = f.and(t).shiftRight(e - s - c);
				f = f.shiftLeft(c).or(d);
//			} else {
//				t = t.shiftLeft(-c).not().shiftLeft(s);
//				v = t.shiftLeft(e - s + c).not().shiftLeft(s - c);
//				d = f.and(v).shiftLeft(-c);
//				f = f.and(t).shiftRight(e - s + c).or(d);
			}
			return LispInteger.valueOf(b.andNot(m).or(f.and(m)));
		} else {
			throw mesg.getError("err.require.int", c1a);
		}
	}

}
