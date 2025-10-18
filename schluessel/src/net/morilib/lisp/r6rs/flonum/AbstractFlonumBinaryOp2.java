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
package net.morilib.lisp.r6rs.flonum;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/31
 */
public abstract class AbstractFlonumBinaryOp2 extends BinaryArgs {

	/**
	 * 
	 * @param r
	 * @param s
	 * @return
	 */
	protected abstract double op21(double r, double s, LispMessage m);

	/**
	 * 
	 * @param r
	 * @param s
	 * @return
	 */
	protected abstract double op22(double r, double s, LispMessage m);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		Datum a = FlonumUtils.flonum(op21(
				FlonumUtils.getFlonum(c1a, mesg),
				FlonumUtils.getFlonum(c2a, mesg), mesg));
		Datum b = FlonumUtils.flonum(op22(
				FlonumUtils.getFlonum(c1a, mesg),
				FlonumUtils.getFlonum(c2a, mesg), mesg));

		return MultiValues.newValues(a, b);
	}

}
