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
package net.morilib.lisp.condition;

import java.util.HashSet;
import java.util.Set;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/03
 */
public class MakeConditionType extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		String id = SubrUtils.getSymbolName(c1a, mesg);
		Set<String> st = new HashSet<String>();
		ConsIterator itr;

		if(c2a instanceof LispConditionType) {
			itr = new ConsIterator(c3a);
			while(itr.hasNext()) {
				st.add(SubrUtils.getSymbolName(itr.next(), mesg));
			}
			SubrUtils.checkTerminated(itr, c3a, mesg);
			return LispConditionType.newInstance(
					id, (LispConditionType)c2a, st);
		} else {
			throw mesg.getError(
					"err.condition.require.conditiontype", c2a);
		}
	}

}
