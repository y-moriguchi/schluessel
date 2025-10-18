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
package net.morilib.lisp.unicode;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/25
 */
public class IntegerToDigit extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		int dg = SubrUtils.nextSmallInt(itr, mesg, body);
		int rd = SubrUtils.nextSmallInt(itr, 10, mesg);

		SubrUtils.checkTerminated(itr, body, mesg);
		if(rd <= 0) {
			throw mesg.getError("err.radix.invalid", rd + "");
		} else if(rd <= 10) {
			if(dg < 0) {
				return LispBoolean.FALSE;
			} else if(dg < rd) {
				return LispCharacter.valueOf(dg + '0');
			} else {
				return LispBoolean.FALSE;
			}
		} else if(rd <= 36) {
			if(dg < 0) {
				return LispBoolean.FALSE;
			} else if(dg < 10) {
				return LispCharacter.valueOf(dg + '0');
			} else if(dg < rd) {
				return LispCharacter.valueOf(dg + 'a' - 10);
			} else {
				return LispBoolean.FALSE;
			}
		} else {
			throw mesg.getError("err.radix.invalid", rd + "");
		}
	}

}
