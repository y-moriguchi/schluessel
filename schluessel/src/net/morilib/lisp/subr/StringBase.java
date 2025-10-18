/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.subr;

import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/09
 */
public abstract class StringBase extends Subr {

	/**
	 * 
	 * @param c1a
	 * @param env
	 * @param mesg
	 * @return
	 */
	protected abstract Datum execute(
			Datum c1a, Environment env, LispMessage mesg);

	/**
	 * 
	 * @param s
	 * @param b
	 * @param e
	 * @return
	 */
	protected abstract Datum execute(
			String s, int b, int e, LispMessage mesg);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		String s;
		int b, e;

		if(l.size() == 1) {
			return execute(l.get(0), env, mesg);
		} else if(l.size() == 2) {
			s = SubrUtils.getString(l.get(0), mesg);
			b = SubrUtils.getSmallInt(l.get(1), mesg);
			e = s.length();
			if(b >= s.length()) {
				throw mesg.getError("err.string.outofrange", l.get(1));
			}
		} else if(l.size() == 3) {
			s = SubrUtils.getString(l.get(0), mesg);
			b = SubrUtils.getSmallInt(l.get(1), mesg);
			e = SubrUtils.getSmallInt(l.get(2), mesg);
			if(b >= s.length()) {
				throw mesg.getError("err.string.outofrange", l.get(1));
			} else if(e > s.length()) {
				throw mesg.getError("err.string.outofrange", l.get(2));
			} else if(b > e) {
				throw mesg.getError("err.range.invalid");
			}
		} else {
			throw mesg.getError("err.argument", body);
		}
		return execute(s, b, e, mesg);
	}

}
