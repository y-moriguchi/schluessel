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

import net.morilib.lang.Hashes;
import net.morilib.util.IntMath;
import net.morilib.util.Pointer;
import net.morilib.util.PointerMismatchException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/27
 */
public class StringPointer implements Pointer<String, Character> {

	//
	private String string;
	private int ptr;

	/**
	 * 
	 * @param str
	 * @param ptr
	 */
	public StringPointer(String str, int ptr) {
		if(str == null) {
			throw new NullPointerException();
		}
		this.string = str;
		this.ptr    = ptr;
	}

	/**
	 * 
	 * @param str
	 */
	public StringPointer(String str) {
		this(str, 0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#inclement()
	 */
	public StringPointer inclement() {
		ptr++;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#declement()
	 */
	public StringPointer declement() {
		ptr--;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#add(int)
	 */
	public StringPointer add(int x) {
		ptr += x;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#isValid()
	 */
	public boolean isValid() {
		return ptr >= 0 && ptr < string.length();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#isValid()
	 */
	public boolean isValid(int x) {
		return (ptr + x) >= 0 && (ptr + x) < string.length();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#refer()
	 */
	public Character refer() {
		if(!isValid()) {
			throw new StringIndexOutOfBoundsException();
		}
		return string.charAt(ptr);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#referSafe()
	 */
	public Character referSafe() {
		return isValid() ? string.charAt(ptr) : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#indexOf(int)
	 */
	public Character indexOf(int x) {
		if(!isValid(x)) {
			throw new StringIndexOutOfBoundsException();
		}
		return string.charAt(ptr + x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#indexOfSafe(int)
	 */
	public Character indexOfSafe(int x) {
		return isValid() ? string.charAt(ptr + x) : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#subtract(net.morilib.util.Pointer)
	 */
	public int subtract(Pointer<String, Character> p) {
		if(!(p instanceof StringPointer)) {
			throw new PointerMismatchException();
		} else if(string != ((StringPointer)p).string) {
			throw new PointerMismatchException();
		}
		return ptr - ((StringPointer)p).ptr;
	}

	/**
	 * 
	 * @return
	 */
	public int charAt() {
		if(!isValid()) {
			throw new StringIndexOutOfBoundsException();
		}
		return string.charAt(ptr);
	}

	/**
	 * 
	 * @return
	 */
	public int charAtSafe() {
		return isValid() ? string.charAt(ptr) : -1;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public int charAt(int x) {
		if(!isValid(x)) {
			throw new StringIndexOutOfBoundsException();
		}
		return string.charAt(ptr + x);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public int charAtSafe(int x) {
		return isValid(x) ? string.charAt(ptr + x) : -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#getIndex()
	 */
	public int getIndex() {
		return ptr;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	public StringPointer clone() {
		return new StringPointer(string, ptr);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#overwrite(net.morilib.util.Pointer)
	 */
	public StringPointer overwrite(Pointer<String, Character> p) {
		if(!(p instanceof StringPointer)) {
			throw new PointerMismatchException();
		} else if(string != ((StringPointer)p).string) {
			throw new PointerMismatchException();
		}
		ptr = ((StringPointer)p).ptr;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#getReferent()
	 */
	public String getReferent() {
		return string;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#moveStart()
	 */
	public StringPointer moveStart() {
		ptr = 0;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Pointer#moveEnd()
	 */
	public StringPointer moveEnd() {
		ptr = string.length() - 1;
		return this;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = (r * Hashes.A) + string.hashCode();
		r = (r * Hashes.A) + ptr;
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object p) {
		if(!(p instanceof StringPointer)) {
			return false;
		} else if(string != ((StringPointer)p).string) {
			return false;
		}
		return ptr == ((StringPointer)p).ptr;
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Pointer<String, Character> p) {
		if(!(p instanceof StringPointer)) {
			throw new PointerMismatchException();
		} else if(string != ((StringPointer)p).string) {
			throw new PointerMismatchException();
		}
		return IntMath.compareTo(ptr, ((StringPointer)p).ptr);
	}

}
