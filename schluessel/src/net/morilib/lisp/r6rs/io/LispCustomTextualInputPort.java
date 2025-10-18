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
package net.morilib.lisp.r6rs.io;

import java.io.IOException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Parser;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.ReadUnreadable;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.io.UTF16;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public class LispCustomTextualInputPort extends LispCustomInputPort
implements ILispTextualInputPort, ReadUnreadable {

	//
	private Procedure read;
	private Parser parser;
	private int unread;

	/**
	 * @param id
	 * @param rd
	 * @param ps
	 * @param sp
	 * @param cl
	 */
	public LispCustomTextualInputPort(Environment env, LispMessage mes,
			String id, Procedure rd, Procedure ps, Procedure sp,
			Procedure cl) {
		super(env, mes, id, ps, sp, cl);
		this.read = rd;
		this.parser = new Parser(this);
		this.unread = -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getChar()
	 */
	public int getChar() throws IOException {
		if(unread >= 0) {
			lookahead = unread;
			return unread;
		} else if(lookahead < 0) {
			return -1;
		} else {
			LispString b = new LispString(" ");
			Datum d = Scheme.callva(read, env, mesg,
					b, LispInteger.ZERO, LispInteger.ONE);
			int bts = SubrUtils.getSmallInt(d, mesg);
			int r = lookahead;

			if(bts > 0) {
				lookahead = b.get(0).getCharacterCodePoint();
				return r;
			} else {
				return lookahead = -1;
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#lookaheadChar()
	 */
	public int lookaheadChar() throws IOException {
		return (unread >= 0) ? unread : lookahead;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getString(int)
	 */
	public String getString(int n) throws IOException {
		int[] r = new int[n];

		return (getChars(r, 0, n) >= 0) ? UTF16.toString(r) : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getChars(int[], int, int)
	 */
	public int getChars(int[] buf,
			int start, int end) throws IOException {
		if(lookahead < 0) {
			return -1;
		} else {
			LispString b;
			Datum d;
			int bts, t;
			int[] r;

			t = (unread < 0) ? 0 : 1;
			b = new LispString(UTF16.toString(buf));
			d = Scheme.callva(read, env, mesg,
					b, LispInteger.valueOf(start),
					LispInteger.valueOf(end - start - t));
			bts = SubrUtils.getSmallInt(d, mesg);

			if(bts > 0) {
				r = UTF16.getInts(b.getString());
				System.arraycopy(r, start,
						buf, start + 1 + t, bts - 1 - t);
				if(t > 0) {
					r[start]     = unread;
					r[start + 1] = lookahead;
					unread = -1;
				} else {
					r[start]     = lookahead;
				}
				lookahead = r[start + bts - 1 - t];
				return bts;
			} else if(t > 0) {
				buf[start]     = unread;
				buf[start + 1] = lookahead;
				unread = lookahead = -1;
				return 2;
			} else {
				buf[start] = lookahead;
				lookahead = -1;
				return 1;
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getStringAll()
	 */
	public String getStringAll() throws IOException {
		StringBuilder b = new StringBuilder();
		int[] c = new int[1024];
		int l;

		while((l = getChars(c, 0, 1024)) >= 0) {
			b.append(UTF16.toString(c, 0, l));
		}
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getLine()
	 */
	public String getLine() throws IOException {
		StringBuilder b = new StringBuilder();

		for(int c = 0; (c = getChar()) >= 0; c++) {
			if(c == '\n') {
				break;
			} else {
				b.appendCodePoint(c);
			}
		}
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getDatum()
	 */
	public Datum getDatum() throws IOException {
		return parser.parse() ? parser.getDatum() : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ReadUnreadable#getc()
	 */
	public int getc() throws IOException {
		return getChar();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ReadUnreadable#unread(int)
	 */
	public void unread(int c) {
		unread = c;
	}

}
