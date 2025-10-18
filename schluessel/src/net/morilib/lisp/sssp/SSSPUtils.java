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
package net.morilib.lisp.sssp;

import java.io.IOException;
import java.io.Reader;

import net.morilib.lisp.Scheme;
import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/27
 */
public class SSSPUtils {

	//
	private static final int BUF_SIZE = 1024;

	//
	private static StringBuilder write(Scheme sch, StringBuilder b,
			int c) {
		b.append((char)c);
		if(b.length() == BUF_SIZE) {
			sch.exec("(display \"" + b.toString() + "\" out)");
			return new StringBuilder();
		} else {
			return b;
		}
	}

	//
	private static void flush(Scheme sch, StringBuilder b) {
		if(b.length() > 0) {
			sch.exec("(display \"" + b.toString() + "\" out)");
		}
	}

	/**
	 * 
	 * @param rd
	 * @param wr
	 * @param sch
	 * @throws IOException
	 */
	public static void parseSssp(Reader rd,
			Scheme sch) throws IOException {
		Stack2<Integer> stk = new ArrayListStack<Integer>();
		StringBuilder b, bbk = null;
		int state = 0, c, dpt = -1;
		boolean rte = false, txt = false;

		b = new StringBuilder("");
		while((c = rd.read()) >= 0) {
			switch(state) {
			case 0:
				if(c == '<') {
					state = 20;
				} else if(stk.isEmpty()) {
					write(sch, b, c);
				} else if(!txt) {
					b.append("(display \"");
					b.append((char)c);
					txt = true;
				} else {
					b.append((char)c);
				}
				break;
			case 20:
				if(c == '%') {
					state = 21;
				} else if(stk.isEmpty()) {
					write(sch, b, '<');
					write(sch, b, c);
					state = 0;
				} else if(!txt) {
					b.append("(display \"<");
					b.append((char)c);
					txt = true;
					state = 0;
				} else {
					b.append('<');
					b.append((char)c);
					state = 0;
				}
				break;
			case 21:
				if(stk.isEmpty()) {
					flush(sch, b);
					b = new StringBuilder("");
				} else if(txt) {
					b.append("\" out)");
					txt = false;
				}

				if(c == '=') {
					rte = true;
					dpt = stk.size();
					bbk = b;
					b   = new StringBuilder();
					state = 3;
					break;
				} else {
					rte = false;
					state = 3;
				}
				/* next */
			case 3:
				if(c == '%') {
					state = 4;
				} else if(c == '\"') {
					state = 301;
					b.append((char)c);
				} else if(c == '#') {
					state = 311;
					b.append((char)c);
				} else if(c == '(') {
					stk.push((int)')');
					b.append((char)c);
				} else if(c == '[') {
					stk.push((int)']');
					b.append((char)c);
				} else if(c == ')' || c == ']') {
					if(stk.isEmpty()) {
						throw new SsspException();
					} else if(stk.pop() != c) {
						throw new SsspException();
					}
					b.append((char)c);
				} else {
					b.append((char)c);
				}
				break;
			case 301:
				if(c == '\\') {
					state = 302;
				} else if(c == '\"') {
					state = 3;
				}
				b.append((char)c);
				break;
			case 302:
				state = 301;
				b.append((char)c);
				break;
			case 311:
				if(c == '\\') {
					state = 312;
				} else if(c == '/') {
					state = 313;
				} else if(c == '[') {
					state = 314;
				} else if(c == 'R') {
					state = 315;
				} else {
					state = 3;
				}
				b.append((char)c);
				break;
			case 312:
				state = 3;
				b.append((char)c);
				break;
			case 313:
				if(c == '/') {
					state = 3;
				}
				b.append((char)c);
				break;
			case 314:
				if(c == ']') {
					state = 3;
				}
				b.append((char)c);
				break;
			case 315:
				if(c == '[' || c == '(') {
					state = 316;
				} else {
					state = 3;
				}
				b.append((char)c);
				break;
			case 316:
				if(c == ']' || c == ')') {
					state = 3;
				}
				b.append((char)c);
				break;
			case 4:
				if(c == '>') {
					if(rte) {
						if(dpt != stk.size()) {
							throw new SsspException();
						} else if(stk.isEmpty()) {
							sch.exec("(display " + b.toString() +
									" out)");
							b = new StringBuilder("");
						} else {
							bbk.append("(display ");
							bbk.append(b);
							bbk.append(" out)");
							b = bbk;
						}
					} else if(stk.isEmpty()) {
						sch.exec(b.toString());
						b = new StringBuilder("");
					}
					state = 0;
				} else {
					b.append('%');
					b.append(c);
					state = 3;
				}
				break;
			default:
				throw new RuntimeException();
			}
		}

		switch(state) {
		case 0:   flush(sch, b);  break;
		case 20:
			b.append('<');
			flush(sch, b);  break;
		default:  throw new SsspException();
		}
	}

}
