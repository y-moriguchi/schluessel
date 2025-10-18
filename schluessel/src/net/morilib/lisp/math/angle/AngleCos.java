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
package net.morilib.lisp.math.angle;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/16
 */
public class AngleCos extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		LispReal x = SubrUtils.nextReal(itr, mesg, body);
		LispReal y = SubrUtils.nextReal(itr, null);

		if(y == null) {
			if(x.isRational() && x.signum() > 0) {
				return (Datum)LispAngleTrigonometric.cos(
						LispInteger.valueOf(x.getDenominator()),
						LispInteger.valueOf(x.getNumerator()));
			} else {
				return new LispRadian(Math.acos(x.doubleValue()));
			}
		} else {
			if(!x.isExact() || !y.isExact() ||
					x.signum() <= 0 || y.signum() <= 0) {
				return new LispRadian(
						Math.acos(x.doubleValue() / y.doubleValue()));
			} else {
				return (Datum)LispAngleTrigonometric.cos(y, x);
			}
		}
	}

}
