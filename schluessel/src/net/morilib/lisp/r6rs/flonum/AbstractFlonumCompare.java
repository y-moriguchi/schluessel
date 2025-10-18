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
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/31
 */
public abstract class AbstractFlonumCompare extends Subr {

	/**
	 * 
	 * @param r
	 * @param s
	 * @return
	 */
	protected abstract boolean compare(double r, double s);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		boolean t = true;
		double r = 0, s;

		while(itr.hasNext()) {
			s = FlonumUtils.nextFlonum(itr, body, mesg);
			if(t) {
				t = false;
			} else if(!compare(r, s)) {
				return LispBoolean.FALSE;
			}
			r = s;
		}
		return LispBoolean.TRUE;
	}

}
