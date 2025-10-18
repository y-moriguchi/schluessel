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
package net.morilib.lisp.compare;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/20
 */
public class ListCompareAsVector extends ListCompare {

	//
	/*package*/ Datum comparelist(Datum x, Datum y, Datum p, Datum e,
			Datum h, Datum t, Environment env, LispMessage mesg) {
		int sx = 0, sy = 0;
		LispInteger cp;

		for(Datum x0 = x; !SRFI67.callIsNull(e, x0, env, mesg); sx++) {
			x0 = SRFI67.callCdr(t, x0, env, mesg);
		}

		for(Datum y0 = y; !SRFI67.callIsNull(e, y0, env, mesg); sy++) {
			y0 = SRFI67.callCdr(t, y0, env, mesg);
		}

		if(sx > sy) {
			return SRFI67.GREATER;
		} else if(sx < sy) {
			return SRFI67.LESS;
		} else {
			for(int i = 0; i < sx; i++) {
				Datum xa = SRFI67.callCar(h, x, env, mesg);
				Datum ya = SRFI67.callCar(h, y, env, mesg);

				cp = SRFI67.callCompare(p, xa, ya, env, mesg);
				if(cp.signum() != 0) {
					return cp;
				} else {
					x = SRFI67.callCdr(t, x, env, mesg);
					y = SRFI67.callCdr(t, y, env, mesg);
				}
			}
			return SRFI67.EQUAL;
		}
	}

}
