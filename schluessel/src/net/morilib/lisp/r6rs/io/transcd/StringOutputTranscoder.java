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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class StringOutputTranscoder implements OutputTranscoder {

	//
	private StringBuilder buf;

	/**
	 * 
	 */
	public StringOutputTranscoder() {
		buf = new StringBuilder();
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() {
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(int)
	 */
	public void write(int c) {
		buf.appendCodePoint(c);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(java.lang.String)
	 */
	public void write(String b) {
		buf.append(b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(java.lang.String, int, int)
	 */
	public void write(String b, int off, int len) {
		buf.append(b.substring(off, off + len));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(int[])
	 */
	public void write(int[] b) {
		write(b, 0, b.length);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(int[], int, int)
	 */
	public void write(int[] b, int off, int len) {
		for(int i = off; i < off + len; i++) {
			buf.appendCodePoint(b[i]);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#flush()
	 */
	public void flush() {
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return buf.toString();
	}

}
