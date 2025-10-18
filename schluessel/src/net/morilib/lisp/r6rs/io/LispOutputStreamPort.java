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
import java.io.OutputStream;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispNotSupportedException;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispBufferMode;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/02
 */
public class LispOutputStreamPort extends Datum2
implements ILispBinaryOutputPort {

	//
	private OutputStream ous;
	private LispBufferMode bm;

	/**
	 * 
	 * @param ous
	 * @param mode
	 */
	public LispOutputStreamPort(OutputStream ous,
			LispBufferMode bm) {
		this.ous = bm.getOutputStream(ous);
		this.bm  = bm;
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		ous.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putByte(int)
	 */
	public void putByte(int b) throws IOException {
		ous.write(b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[])
	 */
	public void putBytes(byte[] src) throws IOException {
		ous.write(src);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[], int)
	 */
	public void putBytes(byte[] src, int start) throws IOException {
		ous.write(src, start, src.length - start);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[], int, int)
	 */
	public void putBytes(byte[] src,
			int start, int end) throws IOException {
		ous.write(src, start, end - start);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getTranscoder()
	 */
	public ILispTranscoder getTranscoder() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#getStream()
	 */
	public OutputStream getOutputStream() {
		return ous;
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
		ous.flush();
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
		buf.append("#<binary-output-port>");
	}

}
