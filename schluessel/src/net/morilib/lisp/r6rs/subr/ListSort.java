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
package net.morilib.lisp.r6rs.subr;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public class ListSort extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a,
			final Environment env, final LispMessage mesg) {
		final Procedure p = SubrUtils.getProcedure(c1a, mesg);
		List<Datum> d = LispUtils.consToList(c2a, mesg);
		Comparator<Datum> cmp = new Comparator<Datum>() {

			public int compare(Datum o1, Datum o2) {
				if(Scheme.callva(p, env, mesg, o1, o2).isTrue()) {
					return -1;
				} else if(Scheme.callva(p, env, mesg,
						o2, o1).isTrue()) {
					return 1;
				} else {
					return 0;
				}
			}

		};

		Collections.sort(d, cmp);
		return LispUtils.listToCons(d);
	}

}
