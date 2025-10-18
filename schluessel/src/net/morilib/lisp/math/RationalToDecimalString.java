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
package net.morilib.lisp.math;

import java.math.BigInteger;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.math.Math2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/31
 */
public class RationalToDecimalString extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		int radix = SubrUtils.nextSmallInt(itr, 10, mesg);
		BigInteger[] nd = SubrUtils.getRational(c1a, mesg);

		if(radix < 2 || radix > 36) {
			throw mesg.getError("err.radix.invalid", radix + "");
		}
		return new LispString(Math2.toDecimalString(
				nd[0], nd[1], radix));
	}

}
