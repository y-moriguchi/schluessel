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
import java.io.InputStream;

import net.morilib.util.io.UTF8;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class UTF8InputTranscoder extends AbstractInputTranscoder {

	//
	private InputStream ins;
	private LispErrorHandlingMode mode;

	/**
	 * @param ins2
	 * @param mode2
	 */
	public UTF8InputTranscoder(InputStream ins,
			LispErrorHandlingMode mode) {
		this.ins  = ins;
		this.mode = mode;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.InputTranscoder#read()
	 */
	public int read() throws IOException {
		while(true) {
			int r = UTF8.read(ins), s;

			if(r < 0) {
				return -1;
			} else if(Character.isDefined(r)) {
				return r;
			} else if((s = mode.ifRead(r, 0xfffd)) >= 0) {
				return s;
			}
		}
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		ins.close();
	}

}
