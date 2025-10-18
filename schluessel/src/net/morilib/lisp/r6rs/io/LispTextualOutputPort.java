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
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispNotSupportedException;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispBufferMode;
import net.morilib.lisp.r6rs.io.transcd.OutputTranscoder;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/02
 */
public class LispTextualOutputPort extends Datum2
implements ILispTextualOutputPort {

	//
	private ILispTranscoder tf;
	private OutputTranscoder tr;
	private LispBufferMode bm;

	/**
	 * @param tr2
	 * @param c2a
	 */
	public LispTextualOutputPort(OutputTranscoder tr,
			ILispTranscoder tf, LispBufferMode bm) {
		this.tf = tf;
		this.tr = tr;
		this.bm = bm;
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		tr.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putChar(int)
	 */
	public void putChar(int c) throws IOException {
		tr.write(c);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putString(java.lang.String)
	 */
	public void putString(String s) throws IOException {
		tr.write(s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putString(java.lang.String, int)
	 */
	public void putString(String s, int start) throws IOException {
		tr.write(s, start, s.length() - start);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putString(java.lang.String, int, int)
	 */
	public void putString(String s,
			int start, int end) throws IOException {
		tr.write(s, start, end - start);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putDatum(net.morilib.lisp.Datum)
	 */
	public void putDatum(Datum d) throws IOException {
		tr.write(LispUtils.print(d));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getTranscoder()
	 */
	public ILispTranscoder getTranscoder() {
		return tf;
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
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSOutputPort#flush()
	 */
	public void flush() throws IOException {
		tr.flush();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSOutputPort#getBufferMode()
	 */
	public LispBufferMode getBufferMode() {
		return bm;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<textual-output-port>");
	}

}
