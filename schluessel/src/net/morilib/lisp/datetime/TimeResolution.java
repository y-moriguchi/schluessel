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
package net.morilib.lisp.datetime;

import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/09
 */
public class TimeResolution extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		LispTime.TimeType tt;

		if(l.size() == 0) {
			tt = LispTime.TimeType.TIME_UTC;
		} else if(l.size() == 1) {
			tt = LispTime.SYM_TO_TYPE.get(l.get(0));

			if(tt == null) {
				throw mesg.getError(
						"err.srfi19.invalidtimetype", l.get(0));
			}
		} else {
			throw mesg.getError("err.argument", body);
		}
		return LispInteger.valueOf(LispTime.getTimeResolution(tt));
	}

}
