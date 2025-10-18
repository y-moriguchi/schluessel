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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.charset.LispCharSet;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/13
 */
public class StringTokenize extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		String s = SubrUtils.nextString(itr, mesg, body);
		Datum d2 = Iterators.nextIf(itr, LispCharSet.GRAPHIC);
		int b = SubrUtils.nextSmallInt(itr, 0, mesg);
		int e = SubrUtils.nextSmallInt(itr, s.length(), mesg);

		if(b < 0 || b >= s.length()) {
			throw mesg.getError("err.string.outofrange", "" + b);
		} else if(e < 0 || e > s.length()) {
			throw mesg.getError("err.string.outofrange", "" + e);
		} else if(e < b) {
			throw mesg.getError("err.range.invalid");
		} else if(d2 instanceof LispCharSet) {
			ConsListBuilder cb = new ConsListBuilder();
			StringBuilder bf = new StringBuilder();
			LispCharSet cs = (LispCharSet)d2;
			int stat = 0;

			for(int i = b; i < e; i++) {
				char ch = s.charAt(i);

				if(cs.contains(ch)) {
					bf.append(ch);
					stat = 1;
				} else if(stat == 1) {
					cb.append(new LispString(bf.toString()));
					bf = new StringBuilder();
					stat = 0;
				}
			}

			if(stat == 1) {
				cb.append(new LispString(bf.toString()));
			}
			return cb.get();
		} else {
			throw mesg.getError("err.srfi14.require.charset", d2);
		}
	}

}
