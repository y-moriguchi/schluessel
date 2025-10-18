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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/03
 */
public class IntegerToList extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum n = SubrUtils.nextIf(itr, mesg, body);

		if(n instanceof LispInteger) {
			BigInteger b = n.getBigInteger();
			int len = SubrUtils.nextSmallInt(itr, b.bitLength(), mesg);
			ConsListBuilder r = new ConsListBuilder();

			if(len < 0) {
				throw mesg.getError("err.require.int.nonnegative",
						"" + len);
			}
			SubrUtils.checkTerminated(itr, body, mesg);
			for(int i = 0; i < len; i++) {
				r.append(LispBoolean.getInstance(b.testBit(i)));
			}
			return r.get();
		} else {
			throw mesg.getError("err.require.int", n);
		}
	}

}
