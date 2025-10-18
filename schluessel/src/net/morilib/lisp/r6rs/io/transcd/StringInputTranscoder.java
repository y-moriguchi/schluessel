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
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public class StringInputTranscoder implements InputTranscoder {

	//
	private int[] str;
	private int ptr;

	/**
	 * 
	 * @param str
	 */
	public StringInputTranscoder(String str) {
		this.str = UTF16.getInts(str);
		this.ptr = 0;
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.InputTranscoder#read()
	 */
	public int read() throws IOException {
		return (ptr < str.length) ? str[ptr++] : -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.InputTranscoder#read(int[])
	 */
	public int read(int[] b) throws IOException {
		return read(b, 0, b.length);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.InputTranscoder#read(int[], int, int)
	 */
	public int read(int[] b, int off, int len) throws IOException {
		int l;

		if(ptr < str.length) {
			l = Math.min(len, str.length - ptr);
			System.arraycopy(str, ptr, b, off, l);
			ptr += l;
			return l;
		} else {
			return -1;
		}
	}

}
