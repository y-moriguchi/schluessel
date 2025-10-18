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
package net.morilib.lisp.r6rs.bytevector;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.QuinaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/18
 */
public class BytevectorCopyS extends QuinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.QuinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Datum c5a, Environment env, LispMessage mesg) {
		LispBytevector vs =
				LispBytevector.datumToBytevector(c1a, mesg);
		int ss = SubrUtils.getSmallInt(c2a, mesg);
		LispBytevector vd =
				LispBytevector.datumToBytevector(c3a, mesg);
		int sd = SubrUtils.getSmallInt(c4a, mesg);
		int ln = SubrUtils.getSmallInt(c5a, mesg);

		if(ln < 0) {
			throw mesg.getError("err.require.int.nonnegative", c5a);
		} else if(ss < 0 || ss + ln > vs.length()) {
			throw mesg.getError("err.range.invalid", c2a);
		} else if(sd < 0 || sd + ln > vd.length()) {
			throw mesg.getError("err.range.invalid", c4a);
		}
		vs.copy(ss, vd, sd, ln);
		return Undef.UNDEF;
	}

}
