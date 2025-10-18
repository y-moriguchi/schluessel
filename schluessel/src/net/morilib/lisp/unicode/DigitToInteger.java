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
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/25
 */
public class DigitToInteger extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		int ch = SubrUtils.nextCharacter(itr, mesg, body);
		int rd = SubrUtils.nextSmallInt(itr, 10, mesg);

		SubrUtils.checkTerminated(itr, body, mesg);
		if(rd <= 0) {
			throw mesg.getError("err.radix.invalid", rd + "");
		} else if(rd <= 10) {
			if(ch >= '0' && ch < '0' + rd) {
				return LispInteger.valueOf(ch - '0');
			} else {
				return LispBoolean.FALSE;
			}
		} else if(rd <= 36) {
			if(ch >= '0' && ch < '0' + rd) {
				return LispInteger.valueOf(ch - '0');
			} else if(ch >= 'A' && ch < 'A' + rd) {
				return LispInteger.valueOf(ch - 'A' + 10);
			} else if(ch >= 'a' && ch < 'a' + rd) {
				return LispInteger.valueOf(ch - 'a' + 10);
			} else {
				return LispBoolean.FALSE;
			}
		} else {
			throw mesg.getError("err.radix.invalid", rd + "");
		}
	}

}
