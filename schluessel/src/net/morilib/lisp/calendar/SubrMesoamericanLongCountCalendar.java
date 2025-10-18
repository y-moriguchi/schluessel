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
package net.morilib.lisp.calendar;

import net.morilib.calendar.MesoamericanLongCountCalendar;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.QuinaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public class SubrMesoamericanLongCountCalendar extends QuinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Datum c5a, Environment env, LispMessage mesg) {
		int y1 = SubrUtils.getSmallInt(c1a, mesg);
		int y2 = SubrUtils.getSmallInt(c2a, mesg);
		int y3 = SubrUtils.getSmallInt(c3a, mesg);
		int y4 = SubrUtils.getSmallInt(c4a, mesg);
		int y5 = SubrUtils.getSmallInt(c5a, mesg);

		try {
			return new LispDay(new MesoamericanLongCountCalendar(
					y1, y2, y3, y4, y5));
		} catch(IllegalArgumentException e) {
			throw mesg.getError("err.calendar.ymd.invalid");
		}
	}

}
