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
package net.morilib.util.string;

import java.util.NoSuchElementException;

import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/15
 */
public class StringIterator implements IntegerIterator {

	//
	private String s;
	private int p = 0;

	/**
	 * 
	 * @param string
	 */
	public StringIterator(String string) {
		this.s = string;
	}

	//
	private boolean hasNext(int q) {
		return q < s.length();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.iterator.CharacterIterator#hasNext()
	 */
	@Override
	public boolean hasNext() {
		return p < s.length();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.iterator.CharacterIterator#next()
	 */
	@Override
	public int next() {
		char ch, cg;

		if(!hasNext()) {
			throw new NoSuchElementException();
		} else if(!Character.isHighSurrogate(ch = s.charAt(p++))) {
			return ch;
		} else if(hasNext() &&
				Character.isSurrogatePair(ch, cg = s.charAt(p++))) {
			return Character.toCodePoint(ch, cg);
		} else {
			return '?';
		}
	}

	/**
	 * 
	 * @return
	 */
	public int peek() {
		char ch, cg;
		int q = p;

		if(!hasNext(q)) {
			throw new NoSuchElementException();
		} else if(!Character.isHighSurrogate(ch = s.charAt(q++))) {
			return ch;
		} else if(hasNext(q) &&
				Character.isSurrogatePair(ch, cg = s.charAt(q++))) {
			return Character.toCodePoint(ch, cg);
		} else {
			return '?';
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.iterator.CharacterIterator#remove()
	 */
	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}

}
