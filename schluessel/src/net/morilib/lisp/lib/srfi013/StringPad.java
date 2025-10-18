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
package net.morilib.lisp.lib.srfi013;

import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/09
 */
public class StringPad extends Subr {

	/**
	 * @param s
	 * @param le
	 * @param ch
	 * @param b
	 * @param e
	 * @param mesg
	 * @return
	 */
	protected Datum execute(
			String s, int le, int ch, int b, int e,
			LispMessage mesg) {
		if(le <= e - b) {
			return new LispString(s.substring(e - le, e));
		} else {
			StringBuilder bf = new StringBuilder();

			for(int i = e - b; i < le; i++) {
				bf.append((char)ch);
			}
			bf.append(s.substring(b, e));
			return new LispString(bf.toString());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		String s;
		int le, b, e, ch;

		if(l.size() == 2) {
			s  = SubrUtils.getString(l.get(0), mesg);
			ch = ' ';
			b  = 0;
			e  = s.length();
		} else if(l.size() == 3) {
			s  = SubrUtils.getString(l.get(0), mesg);
			ch = SubrUtils.getCharacter(l.get(2), mesg);
			b  = 0;
			e  = s.length();
		} else if(l.size() == 4) {
			s  = SubrUtils.getString(l.get(0), mesg);
			ch = SubrUtils.getCharacter(l.get(2), mesg);
			b  = SubrUtils.getSmallInt(l.get(3), mesg);
			e  = s.length();
			if(b >= s.length()) {
				throw mesg.getError("err.string.outofrange", l.get(3));
			}
		} else if(l.size() == 5) {
			s  = SubrUtils.getString(l.get(0), mesg);
			ch = SubrUtils.getCharacter(l.get(2), mesg);
			b  = SubrUtils.getSmallInt(l.get(3), mesg);
			e  = SubrUtils.getSmallInt(l.get(4), mesg);
			if(b >= s.length()) {
				throw mesg.getError("err.string.outofrange", l.get(3));
			} else if(e > s.length()) {
				throw mesg.getError("err.string.outofrange", l.get(4));
			} else if(b > e) {
				throw mesg.getError("err.range.invalid");
			}
		} else {
			throw mesg.getError("err.argument", body);
		}
		le = SubrUtils.getSmallInt(l.get(1), mesg);
		return execute(s, le, ch, b, e, mesg);
	}

}
