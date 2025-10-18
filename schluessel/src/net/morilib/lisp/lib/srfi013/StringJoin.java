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
package net.morilib.lisp.lib.srfi013;

import java.util.List;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/09
 */
public class StringJoin extends Subr {

	//
	private static final Datum INFIX = Symbol.getSymbol("infix");
	private static final Datum STRICT_INFIX =
		Symbol.getSymbol("strict-infix");
	private static final Datum SUFFIX = Symbol.getSymbol("suffix");
	private static final Datum PREFIX = Symbol.getSymbol("prefix");

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		String dlm = " ";
		Datum  grm = INFIX;
		ConsIterator itr;
		StringBuilder b = new StringBuilder();

		if(l.size() < 1 || l.size() > 3) {
			throw mesg.getError("err.argument", body);
		} else if(l.size() == 2) {
			dlm = SubrUtils.getString(l.get(1), mesg);
		} else if(l.size() == 3) {
			dlm = SubrUtils.getString(l.get(1), mesg);
			grm = l.get(2);
		}
		itr = new ConsIterator(l.get(0));

		if(!itr.hasNext() && grm.equals(STRICT_INFIX)) {
			throw mesg.getError("err.require.list.notnull", l.get(0));
		} else {
			for(int i = 0; itr.hasNext(); i++) {
				String s = SubrUtils.getString(itr.next(), mesg);

				if(grm.equals(SUFFIX)) {
					b.append(s).append(dlm);
				} else if(grm.equals(PREFIX)) {
					b.append(dlm).append(s);
				} else if(!grm.equals(INFIX) &&
						!grm.equals(STRICT_INFIX)) {
					throw mesg.getError("err.symbol.invalid", grm);
				} else if(i > 0) {
					b.append(dlm).append(s);
				} else {
					b.append(s);
				}
			}
			return new LispString(b.toString());
		}
	}

}
