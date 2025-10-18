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
package net.morilib.lisp.charset;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.range.integer.IntRange;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/10
 */
public class CharSetXor extends Subr {

	//
	/*package*/ Datum e2(ConsIterator itr, LispMessage mesg) {
		IntRange r = IntRange.O;
		Datum d;

		while(itr.hasNext()) {
			d = itr.next();
			if(d instanceof LispCharSet) {
				r = r.xor(((LispCharSet)d).charset);
			} else {
				throw mesg.getError("err.charset.require.charset", d);
			}
		}
		return new LispCharSet(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		return e2(new ConsIterator(body), mesg);
	}

}
