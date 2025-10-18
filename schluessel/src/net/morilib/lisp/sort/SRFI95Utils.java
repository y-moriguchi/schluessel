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
package net.morilib.lisp.sort;

import java.util.Comparator;
import java.util.Iterator;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public final class SRFI95Utils {

	/**
	 * 
	 * @param cmp
	 * @param key
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static Comparator<Datum> getCmp(final Procedure cmp,
			final Procedure key,
			final Environment env,
			final LispMessage mesg) {
		if(key == null) {
			return new Comparator<Datum>() {

				public int compare(Datum o1, Datum o2) {
					if(Scheme.callva(cmp, env, mesg,
							o1, o2).isTrue()) {
						return -1;
					} else if(Scheme.callva(cmp, env, mesg,
							o2, o1).isTrue()) {
						return 1;
					} else {
						return 0;
					}
				}

			};
		} else {
			return new Comparator<Datum>() {

				public int compare(Datum d1, Datum d2) {
					Datum o1 = Scheme.callva(key, env, mesg, d1);
					Datum o2 = Scheme.callva(key, env, mesg, d2);

					if(Scheme.callva(cmp, env, mesg,
							o1, o2).isTrue()) {
						return -1;
					} else if(Scheme.callva(cmp, env, mesg,
							o2, o1).isTrue()) {
						return 1;
					} else {
						return 0;
					}
				}

			};
		}
	}

	/**
	 * 
	 * @param itr
	 * @param body
	 * @param mesg
	 * @return
	 */
	public static SRFI95Sequence<?> next(Iterator<Datum> itr,
			Datum body, LispMessage mesg) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d instanceof SRFI95Sequence) {
			return (SRFI95Sequence<?>)d;
		} else {
			throw mesg.getError("err.srfi95.require.sequence", d);
		}
	}

}
