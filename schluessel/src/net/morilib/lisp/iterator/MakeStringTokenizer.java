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
package net.morilib.lisp.iterator;

import java.util.NoSuchElementException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.charset.LispCharSet;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/29
 */
public class MakeStringTokenizer extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		final String s = SubrUtils.getString(c1a, mesg);
		final LispCharSet cs;
		final int[] p = new int[2];

		if(c2a instanceof LispCharSet) {
			cs = (LispCharSet)c2a;
		} else if(c2a instanceof LispString) {
			cs = new LispCharSet(c2a.getString());
		} else {
			throw mesg.getError("err.srfi14.require.charset", c2a);
		}

		return new LispIteratorDatum() {

			private String nxt = "";
			private int _g() {
				char c1;

				if(p[0] >= s.length()) {
					return -1;
				} else if(Character.isHighSurrogate(
						(c1 = s.charAt(p[0]++)))) {
					return Character.toCodePoint(c1, s.charAt(p[0]++));
				} else {
					return c1;
				}
			}

			private String _s() {
				StringBuilder b = new StringBuilder();

				if(p[1] < 0)  return (nxt = null);
				while(p[0] < s.length() && !cs.contains(p[1] = _g())) {
					b.appendCodePoint(p[1]);
				}
				if(p[0] >= s.length())  p[1] = -1;
				while(p[0] < s.length() && cs.contains(p[1] = _g()));

				if(nxt.equals("") &&
						b.length() == 0 && p[0] >= s.length()) {
					nxt = null;
				} else {
					p[0] -= (p[1] < 0) ?
							0 : (p[1] > Character.MAX_VALUE) ? 2 : 1;
					nxt = b.toString();
				}
				return nxt;
			}

			public boolean isTerminated() {
				if(nxt != null && nxt.equals("")) {
					_s();
				}
				return nxt == null;
			}

			public ILispIterator next() {
				if(nxt.equals(""))  _s();
				if(isTerminated()) {
					throw new NoSuchElementException();
				}
				_s();
				return this;
			}

			public Datum getCurrentDatum() {
				if(nxt.equals(""))  _s();
				if(isTerminated()) {
					throw new NoSuchElementException();
				}
				return new LispString(nxt);
			}

			@Override
			public void toDisplayString(StringBuilder buf) {
				buf.append("#<string-tokenizer>");
			}

		};
	}

}
