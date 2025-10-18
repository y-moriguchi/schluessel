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
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class NumberToString extends Subr {

	//
	private Datum process(Datum d, int radix, LispMessage mesg) {
		if(d instanceof LispNumber) {
			try {
				return ((LispNumber)d).toLispString(radix);
			} catch(IllegalArgumentException e) {
				throw mesg.getError("err.notsupported.radix10");
			}
		} else {
			throw mesg.getError("err.require.number", d);
		}
	}

	//
	private Datum process(LispNumber d, int radix, int precision,
			LispMessage mesg) {
		try {
			return ((LispNumber)d).toLispString(radix, precision);
		} catch(IllegalArgumentException e) {
			throw mesg.getError("err.notsupported.radix10");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);

		if(lst.size() == 1) {
			return process(lst.get(0), 10, mesg);
		} else if(lst.size() == 2) {
			Datum d = lst.get(1);

			if(d instanceof LispNumber) {
				LispNumber n = (LispNumber)d;

				if(!n.isInteger()) {
					throw mesg.getError("err.require.integer", n);
				} else {
					int ii = SubrUtils.getSmallInt(n, mesg);

					if(ii < 2 || ii > 16) {
						throw mesg.getError("err.radix.invalid", d);
					} else {
						return process(lst.get(0), ii, mesg);
					}
				}
			} else {
				throw mesg.getError("err.require.integer", d);
			}
		} else if(lst.size() == 3) {
			LispNumber n = SubrUtils.getNumber(lst.get(0), mesg);
			int ii = SubrUtils.getSmallInt(lst.get(1), mesg);
			int p  = SubrUtils.getSmallInt(lst.get(2), mesg);

			if(ii < 2 || ii > 16) {
				throw mesg.getError("err.radix.invalid", lst.get(1));
			} else if(p < 0) {
				throw mesg.getError("err.require.int.nonnegative",
						lst.get(2));
			} else {
				return process(n, ii, p, mesg);
			}
		} else {
			throw mesg.getError("err.argument", symbolName);
		}
	}

}
