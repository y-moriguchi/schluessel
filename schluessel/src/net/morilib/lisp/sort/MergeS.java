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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public class MergeS extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		SRFI95Sequence s1 = SRFI95Utils.next(itr, body, mesg);
		SRFI95Sequence s2 = SRFI95Utils.next(itr, body, mesg);
		Procedure les = SubrUtils.nextProcedure(itr, mesg, body);
		Procedure key = SubrUtils.nextProcedureOptional(itr, mesg);

		try {
			s1.mergeS(s2, SRFI95Utils.getCmp(les, key, env, mesg));
			return (Datum)s1;
		} catch(ClassCastException e) {
			throw mesg.getError("err.srfi95.sequence.typemismatch");
		}
	}

}
