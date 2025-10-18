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
package net.morilib.lisp.r6rs.hash;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.collection.hash.LispHash.AbstractMakeHashTable;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/21
 */
public class MakeHashtable extends AbstractMakeHashTable {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Procedure e = SubrUtils.nextProcedure(itr, mesg, body);
		Procedure h = SubrUtils.nextProcedure(itr, mesg, body);
		Datum d1 = Iterators.nextIf(itr);
		int k;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(d1 == null) {
			return makeHash((Datum)e, (Datum)h, env, mesg);
		} else {
			k = SubrUtils.getNonnegativeSmallInt(d1, mesg);
			return makeHash((Datum)e, (Datum)h, k, env, mesg);
		}
	}

}
