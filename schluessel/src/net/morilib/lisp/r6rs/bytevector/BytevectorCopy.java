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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/18
 */
public class BytevectorCopy extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		LispBytevector v;
		Datum d1 = SubrUtils.nextIf(itr, mesg, body);
		Datum d2 = Iterators.nextIf(itr);
		Datum d3 = Iterators.nextIf(itr);
		int s, e;

		SubrUtils.checkTerminated(itr, body, mesg);
		v = LispBytevector.datumToBytevector(d1, mesg);
		s = (d2 == null) ? 0 : SubrUtils.getSmallInt(d2, mesg);
		e = (d3 == null) ?
				v.length() : SubrUtils.getSmallInt(d3, mesg);
		if(s < 0 || s >= v.length()) {
			throw mesg.getError("err.range.invalid", d2);
		} else if(e < 0 || e > v.length()) {
			throw mesg.getError("err.range.invalid", d3);
		} else if(e < s) {
			throw mesg.getError("err.range.invalid");
		}
		return v.duplicate(s, e);
	}

}
