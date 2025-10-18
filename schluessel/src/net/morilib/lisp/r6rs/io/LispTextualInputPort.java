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

import java.io.File;
import java.io.IOException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNotSupportedException;
import net.morilib.lisp.Parser;
import net.morilib.lisp.ReadUnreadable;
import net.morilib.lisp.r6rs.io.transcd.InputTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispBufferMode;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LookaheadInputTranscoder;
import net.morilib.util.io.UTF16;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/02
 */
public class LispTextualInputPort extends Datum2
implements ILispTextualInputPort, ReadUnreadable {

	//
	private ILispTranscoder tf;
	private LookaheadInputTranscoder tr;
	private Parser parser;
	private int unread = -1;

	/**
	 * 
	 * @param file
	 * @param opts
	 * @param bufmode
	 * @param tf
	 * @param mesg
	 * @throws IOException
	 */
	public LispTextualInputPort(File file, LispFileOptions opts,
			LispBufferMode bufmode, ILispTranscoder tf,
			LispMessage mesg) throws IOException {
		this.tf = tf;
		this.tr = new LookaheadInputTranscoder(
				tf.newInput(opts.openForInput(file, mesg)));
		parser = new Parser(this);
	}

	/**
	 * @param tr2
	 * @param c2a
	 */
	public LispTextualInputPort(InputTranscoder tr,
			ILispTranscoder tf) {
		this.tf = tf;
		this.tr = new LookaheadInputTranscoder(tr);
		parser = new Parser(this);
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		tr.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getChar()
	 */
	public int getChar() throws IOException {
		int r;

		if(unread < 0) {
			return tr.read();
		} else {
			r = unread;
			unread = -1;
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#lookaheadChar()
	 */
	public int lookaheadChar() throws IOException {
		return (unread < 0) ? tr.lookahead() : unread;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getChars(int)
	 */
	public String getString(int n) throws IOException {
		int[] b = new int[n];
		int l, u;

		if(unread >= 0) {
			u = 1;
			b[0] = unread;
			unread = -1;
		} else {
			u = 0;
		}

		if((l = tr.read(b, u, b.length - u)) < 0) {
			return (u > 0) ? UTF16.toString(b, 0, u) : null;
		} else if(l + u < b.length) {
			return UTF16.toString(b, 0, l + u);
		} else {
			return UTF16.toString(b);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getChars(char[], int, int)
	 */
	public int getChars(int[] buf,
			int start, int end) throws IOException {
		int u;

		if(unread >= 0) {
			u = 1;
			buf[start] = unread;
			unread = -1;
		} else {
			u = 0;
		}
		return tr.read(buf, start + u, end - start - u);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getStringAll()
	 */
	public String getStringAll() throws IOException {
		StringBuilder b = new StringBuilder();
		int[] c = new int[1024];
		int l;

		if(unread >= 0) {
			b.appendCodePoint(unread);
			unread = -1;
		}

		while((l = tr.read(c)) >= 0) {
			b.append(UTF16.toString(c, 0, l));
		}
		return b.length() != 0 ? b.toString() : null;
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
		return b.length() != 0 ? b.toString() : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getDatum()
	 */
	public Datum getDatum() throws IOException {
		if(parser.isEOF()) {
			return null;
		} else {
			parser.clear();
			parser.parse();
			return parser.isEOF() ? null : parser.getDatum();
		}
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

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getTranscoder()
	 */
	public ILispTranscoder getTranscoder() {
		return tf;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSInputPort#isPortEof()
	 */
	public boolean isPortEof() throws IOException {
		return tr.lookahead() < 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#supportsPosition()
	 */
	public boolean hasPortPosition() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getPortPosition()
	 */
	public Datum getPortPosition() {
		throw new LispNotSupportedException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#supportsSetPosition()
	 */
	public boolean hasSetPortPosition() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#setPortPosition(net.morilib.lisp.Datum)
	 */
	public void setPortPosition(Datum pos) {
		throw new LispNotSupportedException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<textual-input-port>");
	}

}
