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
package net.morilib.util.io;

import java.io.IOException;
import java.io.Reader;
import java.util.Stack;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/23
 */
public class IncludeReader extends Reader {

	//
	private Stack<Reader> readers;

	/**
	 * 
	 * @param reader
	 */
	public IncludeReader(Reader reader) {
		readers = new Stack<Reader>();
		readers.push(reader);
	}

	/* (non-Javadoc)
	 * @see java.io.Reader#close()
	 */
	@Override
	public void close() throws IOException {
		while(!readers.isEmpty()) {
			readers.pop().close();
		}
	}

	/* (non-Javadoc)
	 * @see java.io.Reader#read(char[], int, int)
	 */
	@Override
	public int read(char[] cbuf, int off, int len) throws IOException {
		int bt = 0;

		while(!readers.isEmpty() &&
				(bt += readers.peek()
						.read(cbuf, off + bt, len - bt)) < len) {
			readers.pop().close();
		}
		return bt;
	}

	/**
	 * 
	 * @param rd
	 */
	public void include(Reader rd) {
		readers.push(rd);
	}

}
