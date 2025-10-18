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
import java.io.OutputStream;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class Latin1OutputTranscoder extends AbstractOutputTranscoder {

	//
	private OutputStream ous;
	private LispErrorHandlingMode mode;

	/**
	 * @param ous2
	 * @param mode2
	 */
	public Latin1OutputTranscoder(OutputStream ous,
			LispErrorHandlingMode mode) {
		this.ous  = ous;
		this.mode = mode;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#write(int)
	 */
	public void write(int c) throws IOException {
		int r;

		if(c >= 0 && c <= 0xff) {
			ous.write(c);
		} else if((r = mode.ifWrite(c, '?')) >= 0) {
			ous.write(r);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.OutputTranscoder#flush()
	 */
	public void flush() throws IOException {
		ous.flush();
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		ous.close();
	}

}
