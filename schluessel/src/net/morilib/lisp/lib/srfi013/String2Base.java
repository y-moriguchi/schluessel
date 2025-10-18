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
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/10
 */
public abstract class String2Base extends Subr {

	/**
	 * @param s
	 * @param t
	 * @param bs
	 * @param es
	 * @param bt
	 * @param et
	 * @param mesg
	 * @return
	 */
	protected abstract Datum execute(
			String s1, String s2, int b1, int e1, int b2, int e2,
			LispMessage mesg);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		String s = SubrUtils.nextString(itr, mesg, body);
		String t = SubrUtils.nextString(itr, mesg, body);
		int bs = 0, es = s.length();
		int bt = 0, et = t.length();

		bs = SubrUtils.nextSmallInt(itr, 0, mesg);
		es = SubrUtils.nextSmallInt(itr, s.length(), mesg);
		bt = SubrUtils.nextSmallInt(itr, 0, mesg);
		et = SubrUtils.nextSmallInt(itr, t.length(), mesg);
		SubrUtils.checkTerminated(itr, body, mesg);

		try {
			return execute(s, t, bs, es, bt, et, mesg);
		} catch(StringIndexOutOfBoundsException e) {
			throw mesg.getError(
					"err.string.outofrange", e.getMessage());
		} catch(ArrayIndexOutOfBoundsException e) {
			throw mesg.getError(
					"err.string.outofrange", e.getMessage());
		} catch(IndexOutOfBoundsException e) {
			throw mesg.getError("err.range.invalid");
		}
	}

}
