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
package net.morilib.lisp.r6rs.flonum;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/31
 */
public abstract class AbstractFlonumOp2 extends Subr {

	/**
	 * 
	 * @param r
	 * @param s
	 * @return
	 */
	protected abstract double op2(double r, double s, LispMessage m);

	/**
	 * 
	 * @param x
	 * @return
	 */
	protected abstract double op1(double x, LispMessage m);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		double r = FlonumUtils.nextFlonum(itr, body, mesg), s;

		if(itr.hasNext()) {
			while(itr.hasNext()) {
				s = FlonumUtils.nextFlonum(itr, body, mesg);
				if(Double.isNaN(s))  return FlonumUtils.flonum(s);
				r = op2(r, s, mesg);
			}
		} else {
			r = op1(r, mesg);
		}
		SubrUtils.checkTerminated(itr, body, mesg);
		return FlonumUtils.flonum(r);
	}

}
