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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.util.Bytes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/07
 */
public class LispByteArrayBinaryInputPort extends Datum2
implements ILispBinaryInputPort {

	//
	private byte[] buf;
	private int ptr;

	/**
	 * @param bytes
	 */
	public LispByteArrayBinaryInputPort(byte[] bytes) {
		buf = new byte[bytes.length];
		ptr = 0;
		System.arraycopy(bytes, 0, buf, 0, buf.length);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getTranscoder()
	 */
	public ILispTranscoder getTranscoder() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#hasPortPosition()
	 */
	public boolean hasPortPosition() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getPortPosition()
	 */
	public Datum getPortPosition() {
		return LispInteger.valueOf(ptr);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#hasSetPortPosition()
	 */
	public boolean hasSetPortPosition() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#setPortPosition(net.morilib.lisp.Datum)
	 */
	public void setPortPosition(
			Datum pos) throws InvalidPositionException {
		int p;

		if(!(pos instanceof LispSmallInt)) {
			throw new InvalidPositionException();
		} else if((p = pos.getInt()) < 0 || p > buf.length) {
			throw new InvalidPositionException();
		} else {
			ptr = p;
		}
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		// do nothing
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSInputPort#isPortEof()
	 */
	public boolean isPortEof() {
		return ptr >= buf.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getByte()
	 */
	public int getByte() throws IOException {
		return (ptr < buf.length) ? Bytes.ubyteToInt(buf[ptr++]) : -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#lookaheadByte()
	 */
	public int lookaheadByte() throws IOException {
		return (ptr < buf.length) ? Bytes.ubyteToInt(buf[ptr]) : -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytes(int)
	 */
	public byte[] getBytes(int n) throws IOException {
		if(ptr < buf.length) {
			int l = Math.min(n, buf.length - ptr);
			byte[] r = new byte[l];
	
			System.arraycopy(buf, ptr, r, 0, l);
			ptr += l;
			return r;
		} else {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytes(byte[], int, int)
	 */
	public int getBytes(byte[] b,
			int start, int end) throws IOException {
		if(ptr < buf.length) {
			int l = Math.min(end - start, buf.length - ptr);
	
			System.arraycopy(buf, ptr, b, start, l);
			ptr += l;
			return l;
		} else {
			return -1;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytesSome()
	 */
	public byte[] getBytesSome() throws IOException {
		return getBytes(72);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytesAll()
	 */
	public byte[] getBytesAll() throws IOException {
		if(ptr < buf.length) {
			int l = buf.length - ptr;
			byte[] r = new byte[l];
	
			System.arraycopy(buf, ptr, r, 0, l);
			ptr += l;
			return r;
		} else {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getStream()
	 */
	public InputStream getInputStream() {
		return new ByteArrayInputStream(buf, ptr, buf.length - ptr);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<bytevector-binary-input-stream>");
	}

}
