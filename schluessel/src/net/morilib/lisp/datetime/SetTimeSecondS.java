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
package net.morilib.lisp.datetime;

import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/07
 */
public class SetTimeSecondS extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		long sec;

		if(!(c1a instanceof LispTime)) {
			throw mesg.getError("err.srfi19.require.time", c1a);
		} else if(!(c2a instanceof LispNumber)) {
			throw mesg.getError("err.require.int", c2a);
		}

		if(((LispNumber)c2a).isInteger()) {
			BigInteger bi = c2a.getBigInteger();

			if(bi.compareTo(LispTime.MAX_LONG) > 0) {
				sec = Long.MAX_VALUE;
			} else if(bi.compareTo(LispTime.MIN_LONG) < 0) {
				sec = Long.MIN_VALUE;
			} else {
				sec = bi.longValue();
			}
		} else {
			throw mesg.getError("err.require.int", c2a);
		}
		((LispTime)c1a).setSecond(sec);
		return Undef.UNDEF;
	}

}
