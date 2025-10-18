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

import net.morilib.calendar.cjkv.CJKVEra;
import net.morilib.calendar.cjkv.JapaneseEras;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.datetime.LispDate;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/11/08
 */
public class DateToJapaneseEra extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum da = Iterators.nextIf(itr);
		java.util.Date d;
		CJKVEra e;

		if(da == null) {
			d = new java.util.Date();
		} else if(da instanceof LispDate) {
			d = ((LispDate)da).getDate();
		} else {
			throw mesg.getError("err.srfi19.require.date", da);
		}
		e = JapaneseEras.getEra(d);
		return (e != null) ? new LispEra(e) : LispBoolean.FALSE;
	}

}
