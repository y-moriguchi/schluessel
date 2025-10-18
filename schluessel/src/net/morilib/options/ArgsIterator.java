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
package net.morilib.options;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/15
 */
class ArgsIterator implements Iterator<String> {

	//
	private static final String END_OPT = "--";
	private static final String LONG_OPT = "--";

	//
	private String[] args;
	private int pos = 0;

	//
	ArgsIterator(String[] args) {
		this.args = new String[args.length];
		System.arraycopy(args, 0, this.args, 0, args.length);
	}

	//
	public boolean hasNext() {
		return pos < args.length && !args[pos].equals(END_OPT);
	}

	//
	public String next() {
		if(!hasNext())  throw new NoSuchElementException();
		return args[pos++];
	}

	//
	public void remove() {
		throw new UnsupportedOperationException();
	}

	//
	String peek() {
		if(!hasNext())  throw new NoSuchElementException();
		return args[pos];
	}

	//
	ArgsIterator forward() {
		if(!hasNext())  throw new NoSuchElementException();
		pos++;
		return this;
	}

	//
	ArgsIterator forwardShort() {
		if(!hasNext()) {
			throw new NoSuchElementException();
		} else if(args[pos].indexOf(LONG_OPT) == 0) {
			pos++;
		} else if(args[pos].length() > 2) {
			args[pos] = (args[pos].substring(0, 1) +
					args[pos].substring(2));
		} else {
			pos++;
		}
		return this;
	}

	//
	boolean isDelim(int optchar) {
		return (!hasNext() ||
				isShortOption(optchar) || isLongOption(optchar));
	}

	//
	boolean isShortOption(int optchar) {
		return (hasNext() &&
				args[pos].length() > 1 &&
				args[pos].charAt(0) == optchar &&
				args[pos].charAt(1) != optchar);
	}

	//
	boolean isLongOption(int optchar) {
		return (hasNext() &&
				args[pos].length() > 2 &&
				args[pos].charAt(0) == optchar &&
				args[pos].charAt(1) == optchar);
	}

	//
	boolean isOption(int optchar) {
		return isShortOption(optchar) || isLongOption(optchar);
	}

	//
	String[] operands() {
		String[] r;

		if(pos < args.length && args[pos].equals(END_OPT))  pos++;
		r = new String[args.length - pos];
		System.arraycopy(args, pos, r, 0, r.length);
		return r;
	}

}
