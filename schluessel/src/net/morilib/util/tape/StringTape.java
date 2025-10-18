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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/02
 */
public class StringTape implements CharTape {

	//
	private CharSequence str;
	private int ptr = 0;
	private int mark = 0;

	/**
	 * 
	 * @param str
	 */
	public StringTape(CharSequence str) {
		this.str = str;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#read()
	 */
	public Integer read() {
		return readc();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#write(java.lang.Object)
	 */
	public boolean write(Integer symbol) {
		return writec(symbol);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#moveRight()
	 */
	public boolean moveRight() {
		if(ptr < str.length()) {
			ptr++;
			return true;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#moveLeft()
	 */
	public boolean moveLeft() {
		if(ptr > 0) {
			ptr--;
			return true;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.CharTape#readc()
	 */
	public int readc() {
		return (ptr >= 0 && ptr < str.length()) ? str.charAt(ptr) : -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.CharTape#writec(int)
	 */
	public boolean writec(int c) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#mark()
	 */
	public int mark() {
		return mark = ptr;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#back()
	 */
	public int back() {
		return ptr = mark;
	}

}
