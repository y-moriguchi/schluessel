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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispBufferMode;
import net.morilib.util.primitive.ByteArrayVector;
import net.morilib.util.primitive.ByteVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/07
 */
public class LispByteArrayBinaryOutputPort extends Datum2
implements ILispBinaryOutputPort {

	//
	private ByteVector buf;
	private int ptr;

	/**
	 * 
	 */
	public LispByteArrayBinaryOutputPort() {
		buf = new ByteArrayVector();
		ptr = 0;
	}

	/**
	 * 
	 * @param bytes
	 */
	public LispByteArrayBinaryOutputPort(byte[] bytes) {
		buf = new ByteArrayVector(bytes);
		ptr = 0;
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
		} else if((p = pos.getInt()) < 0 || p > buf.size()) {
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
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSOutputPort#flush()
	 */
	public void flush() throws IOException {
		// do nothing
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSOutputPort#getBufferMode()
	 */
	public LispBufferMode getBufferMode() {
		return LispBufferMode.NONE;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putByte(int)
	 */
	public void putByte(int b) throws IOException {
		buf.add(ptr++, b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[])
	 */
	public void putBytes(byte[] src) throws IOException {
		buf.addAllByte(ptr, src);
		ptr += src.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[], int)
	 */
	public void putBytes(byte[] src, int start) throws IOException {
		putBytes(src, start, src.length);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[], int, int)
	 */
	public void putBytes(byte[] src, int start, int end) throws IOException {
		byte[] b = new byte[end - start];

		System.arraycopy(src, start, b, 0, b.length);
		buf.addAllByte(ptr, b);
		ptr += b.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#getStream()
	 */
	public OutputStream getOutputStream() {
		ByteArrayOutputStream ous = new ByteArrayOutputStream();

		ous.write(buf.toByteArray(), 0, ptr);
		return ous;
	}

	/**
	 * @return
	 */
	public byte[] toByteArray() {
		return buf.toByteArray();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<bytevector-binary-output-port>");
	}

}
