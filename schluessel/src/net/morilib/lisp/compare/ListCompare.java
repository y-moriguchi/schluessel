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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/20
 */
public class ListCompare extends Subr {

	//
	/*package*/ Datum comparelist(Datum x, Datum y, Datum p, Datum e,
			Datum h, Datum t, Environment env, LispMessage mesg) {
		boolean bx, by;
		LispInteger cp;

		while(!(bx = SRFI67.callIsNull(e, x, env, mesg)) &
				!(by = SRFI67.callIsNull(e, y, env, mesg))) {
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
		return bx ? (by ? SRFI67.GREATER : SRFI67.EQUAL) : SRFI67.LESS;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum x1 = SubrUtils.nextIf(itr, mesg, body);
		Datum p = null, x, y, e = null, h = null, t = null;

		if(x1 instanceof Procedure) {
			p = x1;
			x = SubrUtils.nextIf(itr, mesg, body);
			y = SubrUtils.nextIf(itr, mesg, body);
			if((e = Iterators.nextIf(itr)) != null) {
				h = SubrUtils.nextIf(itr, mesg, body);
				t = SubrUtils.nextIf(itr, mesg, body);
			}
		} else {
			x = x1;
			y = SubrUtils.nextIf(itr, mesg, body);
			if((e = Iterators.nextIf(itr)) != null) {
				h = SubrUtils.nextIf(itr, mesg, body);
				t = SubrUtils.nextIf(itr, mesg, body);
			}
		}
		SubrUtils.checkTerminated(itr, body, mesg);
		return comparelist(x, y, p, e, h, t, env, mesg);
	}

}
