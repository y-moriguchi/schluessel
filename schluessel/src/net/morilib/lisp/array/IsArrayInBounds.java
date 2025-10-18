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
package net.morilib.lisp.array;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/23
 */
public class IsArrayInBounds extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum a  = SubrUtils.nextIf(itr, mesg, body);
		IntegerVector bin = new IntegerArrayVector();

		while(itr.hasNext()) {
			int b = SubrUtils.nextSmallInt(itr, mesg, body);

			if(b < 0) {
				throw mesg.getError("err.srfi25.arraysize.invalid",
						LispInteger.valueOf(b));
			}
			bin.add(b);
		}

		SubrUtils.checkTerminated(itr, body, mesg);
		if(!(a instanceof ILispArray)) {
			throw mesg.getError("err.srfi25.require.array", a);
		} else {
			ILispArray aa = (ILispArray)a;

			if(aa.rank() != bin.size()) {
				return LispBoolean.FALSE;
			} else {
				for(int i = 0; i < bin.size(); i++) {
					if(bin.getInt(i) < aa.startIndex(i) ||
							bin.getInt(i) >= aa.endIndex(i)) {
						return LispBoolean.FALSE;
					}
				}
				return LispBoolean.TRUE;
			}
		}
	}

}
