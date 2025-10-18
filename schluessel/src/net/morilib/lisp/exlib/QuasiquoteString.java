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
package net.morilib.lisp.exlib;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/01
 */
public class QuasiquoteString extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		StringBuilder b = new StringBuilder();
		StringBuilder t = new StringBuilder();
		String s = SubrUtils.getString(c1a, mesg);
		Scheme e = new Scheme(env, mesg);
		int stat = 0, pc = 0;

		for(int p = 0;; p++) {
			int c = (p < s.length()) ? s.charAt(p) : -1;

			outer: switch(stat) {
			case 0:
				switch(c) {
				case ',':
					stat = 1;
					break outer;
				case '\\':
					stat = 5;
					break outer;
				case -1:
					return new LispString(b.toString());
				default:
					b.append((char)c);
					break outer;
				}
			case 1:
				t = new StringBuilder();
				if(c == '|') {
					stat = 3;
					break;
				} else if(c == '(') {
					t.append((char)c);
					pc = 1;
					stat = 4;
					break;
				} else if(c == -1) {
					b.append(',');
					return new LispString(b.toString());
				}
				stat = 2;
				// go to next
			case 2:
				if(Character.isWhitespace(c) || c == '(' || c == ')' ||
						c == '#' || c == -1) {
					Datum d = env.findDatum(
							Symbol.getSymbol(t.toString()));

					if(d != null) {
						b.append(LispUtils.print(d));
					}
					stat = 0;
					if(c == -1) {
						return new LispString(b.toString());
					}
				} else {
					t.append((char)c);
				}
				break;
			case 3:
				if(c == '|') {
					Datum d = env.findDatum(
							Symbol.getSymbol(t.toString()));

					if(d != null) {
						b.append(LispUtils.print(d));
					}
					stat = 0;
				} else if(c == -1) {
					throw mesg.getError(
							"err.quasiquotestring.invalid", s);
				} else {
					t.append((char)c);
				}
				break;
			case 4:
				if(c == '(') {
					t.append((char)c);
					pc++;
				} else if(c == ')') {
					t.append((char)c);
					if(--pc == 0) {
						b.append(LispUtils.print(
								e.exec(t.toString())));
						stat = 0;
					}
				} else if(c == -1) {
					throw mesg.getError(
							"err.quasiquotestring.invalid", s);
				} else {
					t.append((char)c);
				}
				break;
			case 5:
				if(c == -1) {
					b.append('\\');
					return new LispString(b.toString());
				} else {
					b.append((char)c);
					stat = 0;
				}
				break;
			default:
				throw new RuntimeException();
			}
		}
	}

}
