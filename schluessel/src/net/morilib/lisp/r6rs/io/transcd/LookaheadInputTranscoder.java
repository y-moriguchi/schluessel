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
 * @author MORIGUCHI, Yuichiro 2012/01/02
 */
public class LookaheadInputTranscoder implements InputTranscoder {

	//
	private InputTranscoder tr;
	private int lookahead = -72;

	/**
	 * @param newInput
	 */
	public LookaheadInputTranscoder(InputTranscoder tr) {
		this.tr = tr;
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		tr.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.InputTranscoder#read()
	 */
	public int read() throws IOException {
		int r;

		if(lookahead == -72) {
			r = tr.read();
			lookahead = tr.read();
			return r;
		} else if(lookahead >= 0) {
			r = lookahead;
			lookahead = tr.read();
			return r;
		} else {
			return lookahead;
		}
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
		int r;

		if(lookahead == -72) {
			if((r = tr.read(b, off, len)) > 0) {
				lookahead = tr.read();
				return r;
			} else {
				return lookahead = -1;
			}
		} else if(lookahead < 0) {
			return -1;
		} else if((r = tr.read(b, off + 1, len - 1)) >= 0) {
			b[off] = lookahead;
			lookahead = tr.read();
			return r + 1;
		} else if(r == 0) {
			return 0;
		} else {
			b[off] = lookahead;
			lookahead = r;
			return 1;
		}
	}

	/**
	 * @return the lookahead
	 */
	public int lookahead() throws IOException {
		if(lookahead == -72) {
			lookahead = tr.read();
		}
		return lookahead;
	}

}
