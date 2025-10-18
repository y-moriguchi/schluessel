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
package net.morilib.lisp.text;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.Keyword;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/03
 */
public class HtmlElement extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		LispHtmlElement elm;

		if(symbolName.startsWith("html:")) {
			elm = new LispHtmlElement();
			elm.name = symbolName.replaceFirst("html:", "");
		} else {
			return Nil.NIL;
		}

		while(itr.hasNext()) {
			Datum d = itr.next();

			if(d instanceof Keyword) {
				if(itr.hasNext()) {
					elm.attrs.put(((Keyword)d).getName(),
							LispUtils.print(itr.next()));
				} else {
					elm.content = Nil.NIL;
				}
			} else if(itr.hasNext()) {
				elm.content = new Cons(d, itr.rest());
			} else {
				elm.content = d;
				break;
			}
		}
		SubrUtils.checkTerminated(itr, body, mesg);
		return elm;
	}

}
