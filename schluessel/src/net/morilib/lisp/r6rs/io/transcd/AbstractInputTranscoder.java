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
public abstract class AbstractInputTranscoder
implements InputTranscoder {

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
			for(; i < off + len && (c = read()) >= 0; i++) {
				b[i] = c;
			}
			return (i == off && c < 0) ? -1 : i - off;
		}
	}

}
