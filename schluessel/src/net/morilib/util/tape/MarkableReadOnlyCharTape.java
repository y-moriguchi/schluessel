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

import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/16
 */
public class MarkableReadOnlyCharTape implements CharTape {

	//
	private CharTape tape;
	private IntegerVector buf = null;
	private int ptr = -1;
	private int ptrall = 0;
	private boolean reachRightEdge = false;

	/**
	 * 
	 * @param tape
	 */
	public MarkableReadOnlyCharTape(CharTape tape) {
		this.tape = tape;
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
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#moveRight()
	 */
	public boolean moveRight() {
		int c;

		if(reachRightEdge) {
			return false;
		} else if(ptr < 0) {
			c = tape.read();
			if(!tape.moveRight()) {
				reachRightEdge = true;
				ptrall++;
				return false;
			} else if(buf != null) {
				buf.add(c);
			}
		} else {
			if(++ptr >= buf.size()) {
				tape.moveRight();
				ptr = -1;
			}
		}
		ptrall++;
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#moveLeft()
	 */
	public boolean moveLeft() {
		if(reachRightEdge) {
			reachRightEdge = false;
		}

		if(ptr < 0) {
			if(buf != null) {
				ptr = buf.size() - 1;
			} else {
				return false;
			}
		} else {
			if(--ptr < 0) {
				ptr = 0;
				return false;
			}
		}
		ptrall--;
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#mark()
	 */
	public int mark() {
		buf = new IntegerArrayVector();
		ptr = -1;
		return ptrall;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.Tape#back()
	 */
	public int back() {
		if(buf != null) {
			ptrall -= buf.size() - 1 - ((ptr < 0) ? 0 : ptr);
			if(reachRightEdge) {
				ptrall--;
			}
			ptr = 0;
		}
		return ptrall;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.CharTape#readc()
	 */
	public int readc() {
		int c;

		if(ptr < 0 || ptr >= buf.size()) {
			if((c = tape.readc()) >= 0) {
				buf.add(c);
				return c;
			} else {
				reachRightEdge = true;
				return -1;
			}
		} else {
			return (ptr < 0) ? tape.readc() : buf.getInt(ptr);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.tape.CharTape#writec(int)
	 */
	public boolean writec(int c) {
		throw new UnsupportedOperationException();
	}

}
