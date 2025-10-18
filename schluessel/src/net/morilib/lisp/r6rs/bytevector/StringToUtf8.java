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
package net.morilib.lisp.r6rs.bytevector;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.io.UTF8;
import net.morilib.util.string.StringIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/18
 */
public class StringToUtf8 extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		StringIterator str;
		ConsIterator itr = new ConsIterator(body);
		String s = SubrUtils.nextString(itr, mesg, body);
		int f = SubrUtils.nextSmallInt(itr, 0, mesg);
		int t = SubrUtils.nextSmallInt(itr, s.length(), mesg);

		SubrUtils.checkTerminated(itr, body, mesg);
		if(f < 0 || f > s.length()) {
			throw mesg.getError("err.range.invalid", f);
		} else if(t < 0 || t > s.length()) {
			throw mesg.getError("err.range.invalid", t);
		} else if(t < f) {
			throw mesg.getError("err.range.invalid");
		}

		str = new StringIterator(s);
		while(str.hasNext()) {
			try {
				UTF8.write(bos, str.next());
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		return new LispBytevector(bos.toByteArray());
	}

}
