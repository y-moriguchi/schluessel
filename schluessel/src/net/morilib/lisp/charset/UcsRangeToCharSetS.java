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
package net.morilib.lisp.charset;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.range.integer.IntInterval;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/10
 */
public class UcsRangeToCharSetS extends QuaternaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.QuaternaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Environment env, LispMessage mesg) {
		if(c4a instanceof LispCharSet) {
			int st = SubrUtils.getSmallInt(c1a, mesg);
			int ed = SubrUtils.getSmallInt(c2a, mesg);
			LispCharSet s = (LispCharSet)c4a;

			if(st < 0) {
				throw mesg.getError("err.require.int.nonegative", c1a);
			} else if(ed < 0) {
				throw mesg.getError("err.require.int.nonegative", c2a);
			} else if(st > ed) {
				throw mesg.getError("err.range.invalid");
			}
			s.charset = s.charset.join(new IntInterval(st, ed));
			return s;
		} else {
			throw mesg.getError("err.charset.require.charset", c4a);
		}
	}

}
