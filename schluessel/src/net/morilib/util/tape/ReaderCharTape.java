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
package net.morilib.util.tape;

import java.io.IOException;
import java.io.Reader;

import net.morilib.util.IntRingBuffer;

public class ReaderCharTape implements CharTape {
	
	//
	private Reader reader;
	private IntRingBuffer buffer;
	
	
	public ReaderCharTape(Reader reader, int size) {
		this.reader = reader;
		this.buffer = new IntRingBuffer(size + 1);
		this.buffer.put(-1);
	}
	
	
	public int readc() {
		return buffer.isEmpty() ? 0 : buffer.get(1);
	}

	
	public boolean writec(int c) {
		throw new UnsupportedOperationException();
	}

	
	public boolean moveLeft() {
		if(buffer.isEmpty()) {
			return false;
		} else {
			buffer.remove();
			return true;
		}
	}

	public boolean moveRight() {
		try {
			int res;
			
			if(buffer.peek() == -1) {
				res = reader.read();
				buffer.add(res);
				buffer.put(-1);
			} else {
				res = buffer.peek();
				buffer.add(res);
			}
			return true;
		} catch(IOException e) {
			throw new TapeException(e);
		}
	}
	

	public Integer read() {
		return readc();
	}

	
	public boolean write(Integer symbol) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#mark()
	 */
	public int mark() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#back()
	 */
	public int back() {
		throw new UnsupportedOperationException();
	}

}
