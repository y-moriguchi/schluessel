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
package net.morilib.lisp.math.polynomial1;

import java.util.ArrayList;
import java.util.List;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/25
 */
public class Polynomial1 extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		List<LispNumber> l = new ArrayList<LispNumber>();
		LispNumber[] r;
		Datum x;

		while(itr.hasNext()) {
			x = itr.next();
			if(x instanceof LispNumber) {
				l.add((LispNumber)x);
			} else {
				throw mesg.getError("err.require.number", x);
			}
		}
		SubrUtils.checkTerminated(itr, body, mesg);

		if(l.size() == 0 || (l.size() == 1 && l.get(0).isZero())) {
			return LispPolynomial1.ZERO;
		} else {
			r = new LispNumber[l.size()];
			for(int i = l.size() - 1; i >= 0; i--) {
				r[i] = l.get(l.size() - 1 - i);
			}
			return new LispPolynomial1(r);
		}
	}

}
