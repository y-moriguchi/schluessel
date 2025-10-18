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
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/20
 */
public class VectorCompare extends Subr {

	//
	/*package*/ Datum comparevec(ILispVector x, ILispVector y, Datum p,
			Datum s, Datum r, Environment env, LispMessage mesg) {
		int sx = SRFI67.callSize(s, x, env, mesg);
		int sy = SRFI67.callSize(s, y, env, mesg);

		if(sx < sy) {
			return SRFI67.LESS;
		} else if(sx > sy) {
			return SRFI67.GREATER;
		} else {
			for(int i = 0; i < sx; i++) {
				Datum x0 = SRFI67.callRef(r, x, i, env, mesg);
				Datum y0 = SRFI67.callRef(r, y, i, env, mesg);
				LispInteger c;

				c = SRFI67.callCompare(p, x0, y0, env, mesg);
				if(c.signum() != 0) {
					return c;
				}
			}
			return SRFI67.EQUAL;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum x1 = SubrUtils.nextIf(itr, mesg, body);
		Datum p = null, x, y, s = null, r = null;

		if(x1 instanceof Procedure) {
			p = x1;
			x = SubrUtils.nextIf(itr, mesg, body);
			y = SubrUtils.nextIf(itr, mesg, body);
			if((s = Iterators.nextIf(itr)) != null) {
				r = SubrUtils.nextIf(itr, mesg, body);
			}
		} else {
			x = x1;
			y = SubrUtils.nextIf(itr, mesg, body);
			if((s = Iterators.nextIf(itr)) != null) {
				r = SubrUtils.nextIf(itr, mesg, body);
			}
		}
		SubrUtils.checkTerminated(itr, body, mesg);

		if(!(x instanceof ILispVector)) {
			throw mesg.getError("err.require.vector", x);
		} else if(!(y instanceof LispVector)) {
			throw mesg.getError("err.require.vector", y);
		}
		return comparevec((ILispVector)x, (ILispVector)y,
				p, s, r, env, mesg);
	}

}
