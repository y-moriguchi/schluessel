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
import java.util.Arrays;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/03
 */
public class ReverseBitField extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		int s = SubrUtils.getSmallInt(c2a, mesg);
		int e = SubrUtils.getSmallInt(c3a, mesg);

		if(s < 0) {
			throw mesg.getError("err.require.int.nonnegative", c2a);
		} else if(e < 0) {
			throw mesg.getError("err.require.int.nonnegative", c3a);
		} else if(s > e) {
			throw mesg.getError("err.srfi60.index.invalid");
		} else if(!(c1a instanceof LispInteger)) {
			throw mesg.getError("err.require.int", c1a);
		} else if(e - s <= 1) {
			return c1a;
		} else {
			byte[] b = c1a.getBigInteger().toByteArray();
			byte[] r;
			int ll, fs;

			ll = (e >> 3) + 1;
			if(ll < b.length) {
				ll = b.length;
			}
			r  = new byte[ll];
			fs = ll - b.length;
			if(c1a.getBigInteger().signum() >= 0) {
				Arrays.fill(r, (byte)0);
			} else {
				Arrays.fill(r, (byte)-1);
			}
			System.arraycopy(b, 0, r, fs, b.length);

			for(int i = s; i < (s + e) / 2; i++) {
				int js = b.length - (i >> 3) - 1;
				int ms = (1 << (i & 0x7));
				int je = b.length - ((e - (i - s) - 1) >> 3) - 1;
				int me = (1 << ((e - (i - s) - 1) & 0x7));
				boolean os, oe;

				os = ((js < 0) ? (b[0] & 0x80) : b[js] & ms) != 0;
				oe = ((je < 0) ? (b[0] & 0x80) : b[je] & me) != 0;
				if(oe) {
					r[js + fs] |= ms;
				} else {
					r[js + fs] &= ~ms;
				}
				if(os) {
					r[je + fs] |= me;
				} else {
					r[je + fs] &= ~me;
				}
			}
			return LispInteger.valueOf(new BigInteger(r));
		}
	}

}
