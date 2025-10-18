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
package net.morilib.lisp.net;

import java.io.IOException;
import java.net.Socket;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispNotSupportedException;
import net.morilib.lisp.r6rs.io.ILispTextualInputPort;
import net.morilib.lisp.r6rs.io.ILispTextualOutputPort;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispBufferMode;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/22
 */
public class LispTcpTextualInputOutputPort extends Datum2
implements ILispTextualInputPort, ILispTextualOutputPort {

	//
	private Socket sok;
	private ILispTextualInputPort  inp;
	private ILispTextualOutputPort oup;

	//
	LispTcpTextualInputOutputPort(Socket sok,
			ILispTextualInputPort inp, ILispTextualOutputPort oup) {
		this.sok = sok;
		this.inp = inp;
		this.oup = oup;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getTranscoder()
	 */
	@Override
	public ILispTranscoder getTranscoder() {
		return inp.getTranscoder();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#hasPortPosition()
	 */
	@Override
	public boolean hasPortPosition() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getPortPosition()
	 */
	@Override
	public Datum getPortPosition() {
		throw new LispNotSupportedException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#hasSetPortPosition()
	 */
	@Override
	public boolean hasSetPortPosition() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#setPortPosition(net.morilib.lisp.Datum)
	 */
	@Override
	public void setPortPosition(Datum pos) {
		throw new LispNotSupportedException();
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	@Override
	public void close() throws IOException {
		sok.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSInputPort#isPortEof()
	 */
	@Override
	public boolean isPortEof() throws IOException {
		return inp.isPortEof();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSOutputPort#flush()
	 */
	@Override
	public void flush() throws IOException {
		oup.flush();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSOutputPort#getBufferMode()
	 */
	@Override
	public LispBufferMode getBufferMode() {
		return oup.getBufferMode();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putChar(int)
	 */
	@Override
	public void putChar(int c) throws IOException {
		oup.putChar(c);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putString(java.lang.String)
	 */
	@Override
	public void putString(String s) throws IOException {
		oup.putString(s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putString(java.lang.String, int)
	 */
	@Override
	public void putString(String s, int start) throws IOException {
		oup.putString(s, start);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putString(java.lang.String, int, int)
	 */
	@Override
	public void putString(String s,
			int start, int end) throws IOException {
		oup.putString(s, start, end);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putDatum(net.morilib.lisp.Datum)
	 */
	@Override
	public void putDatum(Datum d) throws IOException {
		oup.putDatum(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getChar()
	 */
	@Override
	public int getChar() throws IOException {
		return inp.getChar();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#lookaheadChar()
	 */
	@Override
	public int lookaheadChar() throws IOException {
		return inp.lookaheadChar();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getString(int)
	 */
	@Override
	public String getString(int n) throws IOException {
		return inp.getString(n);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getChars(int[], int, int)
	 */
	@Override
	public int getChars(int[] buf,
			int start, int end) throws IOException {
		return inp.getChars(buf, start, end);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getStringAll()
	 */
	@Override
	public String getStringAll() throws IOException {
		return inp.getStringAll();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getLine()
	 */
	@Override
	public String getLine() throws IOException {
		return inp.getLine();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualInputPort#getDatum()
	 */
	@Override
	public Datum getDatum() throws IOException {
		return inp.getDatum();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<tcp-textual-input/output-port>");
	}

}
