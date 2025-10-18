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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.range.integer.IntRange;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/10
 */
public class CharSetDiffPlusIntersection extends Subr {

	//
	/*package*/ Datum e2(LispCharSet cs1, LispCharSet cs2,
			ConsIterator itr, LispMessage mesg) {
		Datum d;
		IntRange rs1, rs2;
		LispCharSet rx;

		rs1 = cs1.charset;
		rs2 = cs2.charset;
		while(itr.hasNext()) {
			d = itr.next();
			if(d instanceof LispCharSet) {
				rx  = (LispCharSet)d;
				rs2 = rs2.join(rx.charset);
			} else {
				throw mesg.getError("err.charset.require.charset", d);
			}
		}
		return MultiValues.newValues(
				new LispCharSet(rs1.difference(rs2)),
				new LispCharSet(rs1.meet(rs2)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum cs1 = SubrUtils.nextIf(itr, mesg, body);
		Datum cs2 = SubrUtils.nextIf(itr, mesg, body);

		if(!(cs1 instanceof LispCharSet)) {
			throw mesg.getError("err.charset.require.charset", cs1);
		} else if(!(cs2 instanceof LispCharSet)) {
			throw mesg.getError("err.charset.require.charset", cs2);
		} else {
			return e2((LispCharSet)cs1, (LispCharSet)cs2, itr, mesg);
		}
	}

}
