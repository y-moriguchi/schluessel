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
public class NewlineInputTranscoder implements InputTranscoder {

	//
	private InputTranscoder tr;
	private int[] newline;
	private int   ptr, rdptr, code;
	private transient int pushbk;

	/**
	 * 
	 * @param tr
	 * @param newline
	 * @param code
	 */
	public NewlineInputTranscoder(InputTranscoder tr,
			LispEolStyle newline, int code) {
		if(tr == null || newline == null) {
			throw new NullPointerException();
		} else if(newline.newline == null) {
			throw new IllegalArgumentException();
		} else if(newline.newline.length == 0) {
			throw new IllegalArgumentException();
		}
		this.tr      = tr;
		this.newline = newline.newline;
		this.ptr     = 0;
		this.rdptr   = newline.newline.length - 1;
		this.code    = code;
		this.pushbk  = -1;
	}

	public int read() throws IOException {
		int c;

		if(rdptr < ptr) {
			if(rdptr + 1 == ptr) {
				ptr = 0;
			}
			return newline[rdptr++];
		} else if(pushbk >= 0) {
			c = pushbk;
			pushbk = -1;
			return c;
		} else {
			if((c = tr.read()) == newline[ptr]) {
				if(++ptr == newline.length) {
					ptr   = 0;
					rdptr = newline.length - 1;
					return code;
				} else {
					return read();
				}
			} else if(ptr > 0) {
				pushbk = c;
				rdptr = 0;
				return read();
			} else {
				return c;
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.InputTranscoder#read(int[])
	 */
	public int read(int[] b) throws IOException {
		return read(b, 0, b.length);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.InputTranscoder#read(int[], int, int)
	 */
	public int read(int[] b, int off, int len) throws IOException {
		int i = off, c = -1;

		if(len <= 0) {
			throw new IllegalArgumentException();
		} else {
			for(; i < off + len; i++) {
				if((c = read()) < 0) {
					break;
				} else {
					b[i] = c;
				}
			}
			return (i == off && c < 0) ? -1 : i - off;
		}
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		tr.close();
	}

}
