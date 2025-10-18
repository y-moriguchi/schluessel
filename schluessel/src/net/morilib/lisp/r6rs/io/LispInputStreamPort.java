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

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispNotSupportedException;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.util.io.LookaheadInputStream;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/02
 */
public class LispInputStreamPort extends Datum2
implements ILispBinaryInputPort {

	//
	private LookaheadInputStream ins;

	/**
	 * 
	 * @param ins
	 */
	public LispInputStreamPort(InputStream ins) {
		this.ins = new LookaheadInputStream(
				new BufferedInputStream(ins));
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		ins.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getByte()
	 */
	public int getByte() throws IOException {
		return ins.read();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#lookaheadByte()
	 */
	public int lookaheadByte() throws IOException {
		return ins.lookahead();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytes(int)
	 */
	public byte[] getBytes(int n) throws IOException {
		byte[] b = new byte[n], r;
		int l;

		if((l = ins.read(b)) < 0) {
			return null;
		} else if(l < b.length) {
			r = new byte[l];
			System.arraycopy(b, 0, r, 0, l);
			return r;
		} else {
			return b;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytes(byte[], int, int)
	 */
	public int getBytes(byte[] buf,
			int start, int end) throws IOException {
		return ins.read(buf, start, end - start);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getByteSome()
	 */
	public byte[] getBytesSome() throws IOException {
		return getBytes(1024);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getByteAll()
	 */
	public byte[] getBytesAll() throws IOException {
		ByteArrayOutputStream b = new ByteArrayOutputStream();
		byte[] c = new byte[1024], r;
		int l;

		while((l = ins.read(c)) >= 0) {
			b.write(c, 0, l);
		}
		return (r = b.toByteArray()).length == 0 ? null : r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getTranscoder()
	 */
	public ILispTranscoder getTranscoder() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getStream()
	 */
	public InputStream getInputStream() {
		return ins;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSInputPort#isPortEof()
	 */
	public boolean isPortEof() throws IOException {
		return ins.lookahead() < 0;
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
		buf.append("#<binary-port>");
	}

}
