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
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispNotSupportedException;
import net.morilib.lisp.r6rs.io.ILispBinaryInputPort;
import net.morilib.lisp.r6rs.io.ILispBinaryOutputPort;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispBufferMode;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/22
 */
public class LispTcpBinaryInputOutputPort extends Datum2
implements ILispBinaryInputPort, ILispBinaryOutputPort {

	//
	private Socket sok;
	private ILispBinaryInputPort  inp;
	private ILispBinaryOutputPort oup;

	//
	LispTcpBinaryInputOutputPort(Socket sok,
			ILispBinaryInputPort inp, ILispBinaryOutputPort oup) {
		this.sok = sok;
		this.inp = inp;
		this.oup = oup;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getTranscoder()
	 */
	@Override
	public ILispTranscoder getTranscoder() {
		return null;
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
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putByte(int)
	 */
	@Override
	public void putByte(int b) throws IOException {
		oup.putByte(b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[])
	 */
	@Override
	public void putBytes(byte[] src) throws IOException {
		oup.putBytes(src);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[], int)
	 */
	@Override
	public void putBytes(byte[] src, int start) throws IOException {
		oup.putBytes(src, start);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[], int, int)
	 */
	@Override
	public void putBytes(byte[] src,
			int start, int end) throws IOException {
		oup.putBytes(src, start, end);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#getOutputStream()
	 */
	@Override
	public OutputStream getOutputStream() {
		return oup.getOutputStream();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getByte()
	 */
	@Override
	public int getByte() throws IOException {
		return inp.getByte();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#lookaheadByte()
	 */
	@Override
	public int lookaheadByte() throws IOException {
		return inp.lookaheadByte();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytes(int)
	 */
	@Override
	public byte[] getBytes(int n) throws IOException {
		return inp.getBytes(n);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytes(byte[], int, int)
	 */
	@Override
	public int getBytes(byte[] buf,
			int start, int end) throws IOException {
		return inp.getBytes(buf, start, end);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytesSome()
	 */
	@Override
	public byte[] getBytesSome() throws IOException {
		return inp.getBytesSome();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytesAll()
	 */
	@Override
	public byte[] getBytesAll() throws IOException {
		return inp.getBytesAll();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getInputStream()
	 */
	@Override
	public InputStream getInputStream() {
		return inp.getInputStream();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<tcp-binary-input/output-port>");
	}

}
