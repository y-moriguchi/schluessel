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
package net.morilib.lisp.calendar.era;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.datetime.LispDate;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/28
 */
public class DaysFromNewYearEpoch extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		int d;

		if(!(c1a instanceof LispEra)) {
			throw mesg.getError("err.calendar.require.cjkvera", c1a);
		} else if(!(c2a instanceof LispEra)) {
			throw mesg.getError("err.srfi19.require.date", c2a);
		} else {
			d = ((LispEra)c1a).era.getDaysFromNewYearEpoch(
					((LispDate)c2a).getDate());
			return (d < 0) ?
					LispBoolean.FALSE : LispInteger.valueOf(d);
		}
	}

}
