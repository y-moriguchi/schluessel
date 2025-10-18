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

import net.morilib.util.io.UTF16;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/02
 */
public class NewlineOutputTranscoder implements OutputTranscoder {

	//
	private OutputTranscoder tr;
	private LispEolStyle eol;

	/**
	 * @param latin1OutputTranscoder
	 * @param eol2
	 */
	public NewlineOutputTranscoder(OutputTranscoder tr,
			LispEolStyle eol) {
		this.tr  = tr;
		this.eol = eol;
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		tr.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(int)
	 */
	public void write(int c) throws IOException {
		if(c == '\n') {
			tr.write(eol.newline);
		} else {
			tr.write(c);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(java.lang.String)
	 */
	public void write(String b) throws IOException {
		write(UTF16.getInts(b));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(java.lang.String, int, int)
	 */
	public void write(String b, int off, int len) throws IOException {
		write(UTF16.getInts(b), off, len);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(int[])
	 */
	public void write(int[] b) throws IOException {
		write(b, 0, b.length);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(int[], int, int)
	 */
	public void write(int[] b, int off, int len) throws IOException {
		for(int i = off; i < off + len; i++) {
			write(b[i]);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#flush()
	 */
	public void flush() throws IOException {
		tr.flush();
	}

}
