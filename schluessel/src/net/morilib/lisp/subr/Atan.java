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
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispMath;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class Atan extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);

		if(lst.size() == 1) {
			Datum d = lst.get(0);

			if(d instanceof LispComplex) {
				return LispMath.atan((LispComplex)d);
			} else {
				throw mesg.getError("err.require.complex", d);
			}
		} else if(lst.size() == 2) {
			Datum d1 = lst.get(0);
			Datum d2 = lst.get(1);

			if(!(d1 instanceof LispComplex)) {
				throw mesg.getError("err.require.real", d1);
			} else if(!(d2 instanceof LispComplex)) {
				throw mesg.getError("err.require.real", d2);
			} else {
				LispComplex n1 = (LispComplex)d1;
				LispComplex n2 = (LispComplex)d2;

				if(n1.isReal() && n2.isReal()) {
					return new LispDouble(Math.atan2(
							n1.getRealDouble(), n2.getRealDouble()));
				} else if(!n1.isReal()) {
					throw mesg.getError("err.require.real", n1);
				} else {
					throw mesg.getError("err.require.real", n2);
				}
			}
		} else {
			throw mesg.getError("err.argument", "atan");
		}
	}

}
