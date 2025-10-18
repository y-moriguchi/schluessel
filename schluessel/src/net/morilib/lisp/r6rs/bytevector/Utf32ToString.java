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
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Endianness2;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/18
 */
public class Utf32ToString extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		StringBuilder b = new StringBuilder();
		Endianness2 e;
		ConsIterator itr = new ConsIterator(body);
		LispBytevector v = LispBytevector.datumToBytevector(
				SubrUtils.nextIf(itr, mesg, body), mesg);
		Datum d2 = SubrUtils.nextIf(itr, mesg, body);
		Datum d3 = Iterators.nextIf(itr);
//		Datum d4 = Iterators.nextIf(itr);
//		Datum d5 = Iterators.nextIf(itr);
		int f, t;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(!(d2 instanceof Symbol)) {
			throw mesg.getError("err.r6rs.require.endianness", d2);
		}
		e = SynEndianness.getEndianness(d2, mesg);

//		if(f < 0 || f >= v.length()) {
//			throw mesg.getError("err.range.invalid", f);
//		} else if(t < 0 || t > v.length()) {
//			throw mesg.getError("err.range.invalid", t);
//		} else if(t < f) {
//			throw mesg.getError("err.range.invalid");
//		} else if((t - f) % 4 != 0) {
//			throw mesg.getError("err.r6rs.range.mustbemultiple");
//		}
		f = 0;  t = v.length();
		if((t - f) % 4 != 0) {
			throw mesg.getError("err.r6rs.range.mustbemultiple");
		}

		if(d3 != null && d3.isTrue()) {
			// do nothing
		} else if(t - f >= 4) {
			switch((int)Endianness2.BIG.read(v.vector, 0, 4)) {
			case 0x0000feff:  e = Endianness2.BIG;     f += 4;  break;
			case 0xfffe0000:  e = Endianness2.LITTLE;  f += 4;  break;
			default:  break;
			}
		}

		for(int k = f; k < t; k += 4) {
			b.appendCodePoint((int)e.read(v.vector, k, 4));
		}
		return new LispString(b.toString());
	}

}
