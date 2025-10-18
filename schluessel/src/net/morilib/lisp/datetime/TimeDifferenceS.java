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
package net.morilib.lisp.datetime;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/09
 */
public class TimeDifferenceS extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		LispTime t1, t2;

		if(!(c1a instanceof LispTime)) {
			throw mesg.getError("err.srfi19.require.time", c1a);
		} else if(!(c2a instanceof LispTime)) {
			throw mesg.getError("err.srfi19.require.time", c2a);
		}

		t1 = (LispTime)c1a;
		t2 = (LispTime)c2a;
		if(!t1.getTimeType().equals(t2.getTimeType())) {
			throw mesg.getError("err.srfi19.differenttimetype");
		}
		t1.subtractAssign(t2);
		return t1;
	}

}
