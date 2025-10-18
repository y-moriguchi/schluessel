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

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.BitUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/03
 */
public class Logcount extends UnaryArgs {

	//
	private static final byte[] POS_CNT;
	private static final byte[] NEG_CNT;

	//
	static {
		POS_CNT = new byte[256];
		NEG_CNT = new byte[256];

		for(int i = -128; i < 128; i++) {
			int j = i & 0xff;

			POS_CNT[j] = (byte)BitUtils.countBit(j);
			NEG_CNT[j] = (byte)(8 - POS_CNT[j]);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		if(c1a instanceof LispInteger) {
			LispInteger n = (LispInteger)c1a;
			byte[] ctbl = (n.signum() >= 0) ? POS_CNT : NEG_CNT;
			byte[] b = n.getBigInteger().toByteArray();
			long r = 0;

			for(int i = 0; i < b.length; i++) {
				r += ctbl[(int)b[i] & 0xff];
			}
			return LispInteger.valueOf(r);
		} else {
			throw mesg.getError("err.require.int", c1a);
		}
	}

}
