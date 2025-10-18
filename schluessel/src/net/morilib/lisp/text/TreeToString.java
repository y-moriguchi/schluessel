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

import java.util.Map;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;
import net.morilib.util.Strings;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/03
 */
public class TreeToString extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum tr, Environment env,
			LispMessage mesg) {
		Stack2<Datum> stk = new ArrayListStack<Datum>();
		StringBuilder b = new StringBuilder();

		stk.push(tr);
		while(!stk.isEmpty()) {
			Datum d = stk.pop();

			if(d instanceof Cons) {
				stk.push(((Cons)d).getCdr());
				stk.push(((Cons)d).getCar());
			} else if(d instanceof LispHtmlElement) {
				LispHtmlElement h = (LispHtmlElement)d;

				b.append("<");
				b.append(h.name);
				for(Map.Entry<String, String> e : h.attrs.entrySet()) {
					String s = e.getValue();

					b.append(" ");
					b.append(e.getKey());
					s = s.replaceAll("\"", "&quot;");
					b.append("=\"");
					b.append(s);
					b.append("\"");
				}
				b.append(">");

				stk.push(new LispString("</" + h.name + "\n>"));
				if(h.content instanceof LispHtmlElement) {
					stk.push(h.content);
				} else {
					stk.push(new LispString(Strings.escapeHtml(
							LispUtils.print(h.content))));
				}
			} else if(!d.isNil()) {
				b.append(LispUtils.print(d));
			}
		}
		b.append("\n");
		return new LispString(b.toString());
	}

}
