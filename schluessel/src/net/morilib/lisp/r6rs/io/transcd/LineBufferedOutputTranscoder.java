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

import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class LineBufferedOutputTranscoder
extends AbstractOutputTranscoder {

	//
	private OutputTranscoder tr;
	private IntegerVector b = new IntegerArrayVector();

	/**
	 * @param tr2
	 */
	public LineBufferedOutputTranscoder(OutputTranscoder tr) {
		this.tr = tr;
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		flush();
		tr.close();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.OutputTranscoder#write(int)
	 */
	public void write(int c) throws IOException {
		b.add(c);
		if(c == '\n') {
			flush();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.OutputTranscoder#flush()
	 */
	public void flush() throws IOException {
		tr.write(b.toIntArray());
		b.clear();
	}

}
