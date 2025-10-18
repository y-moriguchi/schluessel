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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Endianness2;
import net.morilib.util.Iterators;
import net.morilib.util.string.StringIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/18
 */
public class StringToUtf32 extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		StringIterator str;
		Endianness2 e;
		ConsIterator itr = new ConsIterator(body);
		String s = SubrUtils.nextString(itr, mesg, body);
		Datum d2 = Iterators.nextIf(itr);
		Datum d3 = Iterators.nextIf(itr);
		Datum d4 = Iterators.nextIf(itr);
		byte[] a = new byte[s.length() * 4];
		int f, t;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(d2 == null) {
			e = Endianness2.BIG;
			f = 0;
			t = s.length();
		} else if(!(d2 instanceof Symbol)) {
			throw mesg.getError("err.r6rs.require.endianness", d2);
		} else if(d3 == null) {
			e = SynEndianness.getEndianness(d2, mesg);
			f = 0;
			t = s.length();
		} else if(d4 == null) {
			e = SynEndianness.getEndianness(d2, mesg);
			f = SubrUtils.getSmallInt(d3, mesg);
			t = s.length();
		} else {
			e = SynEndianness.getEndianness(d2, mesg);
			f = SubrUtils.getSmallInt(d3, mesg);
			t = SubrUtils.getSmallInt(d4, mesg);
		}

		if(f < 0 || f > s.length()) {
			throw mesg.getError("err.range.invalid", f);
		} else if(t < 0 || t > s.length()) {
			throw mesg.getError("err.range.invalid", t);
		} else if(t < f) {
			throw mesg.getError("err.range.invalid");
		}

		str = new StringIterator(s);
		for(int k = 0; str.hasNext(); k += 4) {
			e.write(a, k, 4, str.next());
		}
		return new LispBytevector(a);
	}

}
