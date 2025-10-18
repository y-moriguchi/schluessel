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
import java.io.PushbackInputStream;

import net.morilib.util.io.UTF16;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class UTF16InputTranscoder extends AbstractInputTranscoder {

	//
	private PushbackInputStream ins;
	private LispErrorHandlingMode mode;
	private boolean endian;

	/**
	 * 
	 * @param ins
	 * @param mode
	 * @throws IOException
	 */
	public UTF16InputTranscoder(InputStream ins,
			LispErrorHandlingMode mode) throws IOException {
		int c1, c2, c;

		this.ins  = new PushbackInputStream(ins, 2);
		this.mode = mode;
		c1 = ins.read();
		c2 = ins.read();
		c  = (c1 << 8) | c2;
		if(c == 0xfeff) {
			endian = UTF16.BIG_ENDIAN;
		} else if(c == 0xfffe) {
			endian = UTF16.LITTLE_ENDIAN;
		} else {
			endian = UTF16.BIG_ENDIAN;
			this.ins.unread(c2);
			this.ins.unread(c1);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.InputTranscoder#read()
	 */
	public int read() throws IOException {
		while(true) {
			int r = UTF16.read(ins, endian), s;
	
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
