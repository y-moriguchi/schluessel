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
package net.morilib.util.io;

import java.io.IOException;
import java.io.InputStream;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/02
 */
public class LookaheadInputStream extends InputStream {

	//
	private InputStream ins;
	private int lookahead = -72;

	/**
	 * @param bufferedInputStream
	 */
	public LookaheadInputStream(InputStream ins) {
		this.ins = ins;
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#read()
	 */
	@Override
	public int read() throws IOException {
		int r;

		if(lookahead >= 0 || lookahead == -72) {
			r = lookahead;
			lookahead = ins.read();
			return r;
		} else {
			return lookahead;
		}
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#read(byte[])
	 */
	@Override
	public int read(byte[] b) throws IOException {
		return read(b, 0, b.length);
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#read(byte[], int, int)
	 */
	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		int r;

		if(lookahead == -72) {
			if((r = ins.read(b, off, len)) < 0) {
				return lookahead = -1;
			} else {
				lookahead = ins.read();
				return r;
			}
		} else if(lookahead < 0) {
			return -1;
		} else if((r = ins.read(b, off + 1, len - 1)) > 0) {
			b[off] = (byte)lookahead;
			lookahead = ins.read();
			return r + 1;
		} else if(r == 0) {
			return 0;
		} else {
			b[off] = (byte)lookahead;
			lookahead = r;
			return 1;
		}
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#skip(long)
	 */
	@Override
	public long skip(long n) throws IOException {
		long r;

		if(lookahead == -72) {
			if((r = ins.skip(n)) < 0) {
				return lookahead = -1;
			} else {
				lookahead = ins.read();
				return r;
			}
		} else if(lookahead < 0) {
			return 0;
		} else if((r = ins.skip(n - 1)) > 0) {
			lookahead = ins.read();
			return r + 1;
		} else {
			lookahead = -1;
			return 1;
		}
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#available()
	 */
	@Override
	public int available() throws IOException {
		return ins.available();
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#close()
	 */
	@Override
	public void close() throws IOException {
		ins.close();
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#mark(int)
	 */
	@Override
	public synchronized void mark(int readlimit) {
		// do nothing
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#reset()
	 */
	@Override
	public synchronized void reset() throws IOException {
		throw new IOException();
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#markSupported()
	 */
	@Override
	public boolean markSupported() {
		return false;
	}

	/**
	 * @return the lookahead
	 * @throws IOException 
	 */
	public int lookahead() throws IOException {
		if(lookahead == -72) {
			lookahead = ins.read();
		}
		return lookahead;
	}

}
