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
package net.morilib.lisp.compare;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/23
 */
public class KthLargest extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, final Environment env,
			final LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		final Datum p = SubrUtils.nextIf(itr, mesg, body);
		int k = SubrUtils.nextSmallInt(itr, mesg, body);
		List<Datum> l = LispUtils.consToList(itr.rest(), mesg);
		Datum[] a = l.toArray(new Datum[0]);

		if(a.length == 0) {
			throw mesg.getError("err.argument", body);
		}

		Arrays.sort(a, new Comparator<Datum>() {

			public int compare(Datum o1, Datum o2) {
				return SRFI67.callCompare(p, o1, o2, env,
						mesg).getInt();
			}

		});
		k = k % a.length;
		return a[(k < 0) ? k + a.length : k];
	}

}
