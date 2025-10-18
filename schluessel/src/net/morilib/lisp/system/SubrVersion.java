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
package net.morilib.lisp.system;

import net.morilib.lang.system.Version;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/24
 */
public class SubrVersion extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		IntegerVector v = new IntegerArrayVector();
		int n;

		while(itr.hasNext()) {
			n = SubrUtils.getSmallInt(itr.next(), mesg);
			v.add(n);
		}
		SubrUtils.checkTerminated(itr, body, mesg);
		return new LispVersion(new Version(v.toIntArray()));
	}

}
