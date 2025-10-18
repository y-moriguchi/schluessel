/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.lib.srfi013;

import java.util.List;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Nil;
import net.morilib.lisp.subr.UnaryArgs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/09
 */
public class ReverseListToString extends UnaryArgs {

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		if(c1a != Nil.NIL && !(c1a instanceof Cons)) {
			throw mesg.getError("err.list", c1a);
		}

		List<Datum> lst = LispUtils.consToList(c1a, mesg);
		StringBuilder bld = new StringBuilder();

		for(int i = lst.size() - 1; i >= 0; i--) {
			if(lst.get(i) instanceof LispCharacter) {
				bld.append(((LispCharacter)lst.get(i)).getCharacter());
			} else {
				throw mesg.getError("err.require.char", lst.get(i));
			}
		}
		return new LispString(bld.toString());
	}

}
