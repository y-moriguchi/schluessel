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

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/23
 */
public class PairCompare extends Subr {

	//
	private LispInteger cmppair(Datum x, Datum y, Datum p,
			Environment env, LispMessage mesg) {
		if(x.isNil()) {
			return y.isNil() ? SRFI67.EQUAL : SRFI67.LESS;
		} else if(x instanceof Cons) {
			if(y.isNil()) {
				return SRFI67.GREATER;
			} else if(y instanceof Cons) {
				return SRFI67.callCompare(p, ((Cons)x).getCar(),
						((Cons)y).getCar(), env, mesg);
			} else {
				return SRFI67.LESS;
			}
		} else {
			if(y.isNil() || y instanceof Cons) {
				return SRFI67.GREATER;
			} else {
				return SRFI67.callCompare(p, x, y, env, mesg);
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum p = SubrUtils.nextIf(itr, mesg, body);
		Datum q = SubrUtils.nextIf(itr, mesg, body);
		Datum x, y;

		if(p instanceof Procedure && q instanceof Procedure) {
			// 4-ary
			x = SubrUtils.nextIf(itr, mesg, body);
			y = SubrUtils.nextIf(itr, mesg, body);
			if(!(x instanceof Cons)) {
				throw mesg.getError("err.require.pair", x);
			} else if(!(y instanceof Cons)) {
				throw mesg.getError("err.require.pair", y);
			} else {
				LispInteger c;
				Cons cx = (Cons)x;
				Cons cy = (Cons)y;

				c = SRFI67.callCompare(p, cx.getCar(), cy.getCar(),
						env, mesg);
				if(c.signum() == 0) {
					return SRFI67.callCompare(q, cx.getCdr(),
							cy.getCdr(), env, mesg);
				} else {
					return c;
				}
			}
		} else {
			// 2 or 3-ary
			ConsIterator ix, iy;
			LispInteger c;

			if(p instanceof Procedure) {
				// 3-ary
				x = q;
				y = SubrUtils.nextIf(itr, mesg, body);
			} else {
				// 2-ary
				x = p;
				y = q;
				p = null;
			}

			ix = new ConsIterator(x);
			iy = new ConsIterator(y);
			while(ix.hasNext() && iy.hasNext()) {
				c = cmppair(ix.next(), iy.next(), p, env, mesg);
				if(c.signum() != 0) {
					return c;
				}
			}
			return cmppair(ix.rest(), iy.rest(), p, env, mesg);
		}
	}

}
