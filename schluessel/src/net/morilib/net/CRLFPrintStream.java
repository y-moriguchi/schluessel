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
package net.morilib.net;

import java.io.IOException;
import java.io.OutputStream;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/18
 */
public class CRLFPrintStream extends OutputStream {

	//
	private static final byte[] CRLF =
		new byte[] { (byte)'\r', (byte)'\n' };

	//
	private OutputStream ous;

	/**
	 * @param pr
	 */
	public CRLFPrintStream(OutputStream pr) {
		this.ous = pr;
	}

	/* (non-Javadoc)
	 * @see java.io.OutputStream#write(int)
	 */
	@Override
	public void write(int b) throws IOException {
		ous.write(b);
	}

	/* (non-Javadoc)
	 * @see java.io.OutputStream#write(byte[])
	 */
	@Override
	public void write(byte[] b) throws IOException {
		ous.write(b);
	}

	/* (non-Javadoc)
	 * @see java.io.OutputStream#write(byte[], int, int)
	 */
	@Override
	public void write(byte[] b, int off, int len) throws IOException {
		ous.write(b, off, len);
	}

	/* (non-Javadoc)
	 * @see java.io.OutputStream#flush()
	 */
	@Override
	public void flush() throws IOException {
		ous.flush();
	}

	/* (non-Javadoc)
	 * @see java.io.OutputStream#close()
	 */
	@Override
	public void close() throws IOException {
		ous.close();
	}

	/**
	 * 
	 * @param s
	 * @throws IOException
	 */
	public void print(String s) throws IOException {
		ous.write(s.getBytes());
	}

	/**
	 * 
	 * @param s
	 * @throws IOException
	 */
	public void println(String s) throws IOException {
		ous.write(s.getBytes());
		ous.write(CRLF);
	}

}
