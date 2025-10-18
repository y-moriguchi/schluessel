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
package net.morilib.lisp.math.stat;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.Subr;
import net.morilib.lisp.math.ILispQuantity;
import net.morilib.lisp.math.ILispQuantityFactory;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/14
 */
public class ArithmetricMean extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		ILispQuantityFactory f = null;
		ILispQuantity q;
		LispReal r = LispInteger.ZERO;
		Datum d;
		int n;

		for(n = 0; itr.hasNext(); n++) {
			if(!((d = itr.next()) instanceof ILispQuantity)) {
				throw mesg.getError("err.stat.require.quantity", d);
			} else if(f == null) {
				f = (q = (ILispQuantity)d).factory();
				r = q.getReal();
			} else if(!f.equals((q = (ILispQuantity)d).factory())) {
				throw mesg.getError("err.stat.require.samequantity",
						d);
			} else {
				r = r.add(q.getReal());
			}
		}

		if(f == null) {
			return LispInteger.ZERO;
		} else {
			return (Datum)f.getInstance(
					r.divide(LispInteger.valueOf(n)));
		}
	}

}
