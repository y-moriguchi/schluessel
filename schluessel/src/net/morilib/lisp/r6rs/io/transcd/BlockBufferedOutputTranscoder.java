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
package net.morilib.lisp.r6rs.io.transcd;

import java.io.IOException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class BlockBufferedOutputTranscoder
extends AbstractOutputTranscoder {

	/**
	 * 
	 */
	public static final int DEFAULT_BUFSIZE = 4096;

	/**
	 * 
	 */
	protected OutputTranscoder tr;

	/**
	 * 
	 */
	protected int[] buf;

	/**
	 * 
	 */
	protected int   count;

	/**
	 * 
	 * @param tr
	 * @param bufsize
	 */
	public BlockBufferedOutputTranscoder(OutputTranscoder tr,
			int bufsize) {
		this.tr = tr;
		this.buf = new int[bufsize];
		this.count = 0;
	}

	/**
	 * 
	 * @param tr
	 */
	public BlockBufferedOutputTranscoder(OutputTranscoder tr) {
		this(tr, DEFAULT_BUFSIZE);
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		flush();
		tr.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(int)
	 */
	public void write(int c) throws IOException {
		buf[count++] = c;
		if(count >= buf.length) {
			flush();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(int[], int, int)
	 */
	public void write(int[] b, int off, int len) throws IOException {
		int c2 = len, cp;

		if(count + c2 < buf.length) {
			System.arraycopy(b, off, buf, count, len);
			count += len;
		} else {
			tr.write(buf, 0, count);
			tr.write(b, off, buf.length - count);
			cp = buf.length - count;
			for(; c2 - cp >= buf.length; cp += buf.length) {
				tr.write(b, off + cp, buf.length);
			}
			System.arraycopy(b, off + cp, buf, 0, c2 - cp);
			count = c2 - cp;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#flush()
	 */
	public void flush() throws IOException {
		tr.write(buf, 0, count);
		count = 0;
	}

}
