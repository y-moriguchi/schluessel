/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp;

import java.math.BigInteger;

import net.morilib.lisp.sos.LispType;
import net.morilib.util.Inclementor;
import net.morilib.util.InclementorBoundsException;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class LispCharacter extends Atom
implements Comparable<LispCharacter>,
Inclementor<LispCharacter>, JavaObjective {

	/**
	 * 
	 */
	public static final LispCharacter NEWLINE =
		new LispCharacter('\n');

	/**
	 * 
	 */
	public static final LispCharacter SPACE = new LispCharacter(' ');

	/**
	 * 
	 */
	public static final LispCharacter NUL =
		new LispCharacter('\u0000');

	/**
	 * 
	 */
	public static final LispCharacter ALERM =
		new LispCharacter('\u0007');

	/**
	 * 
	 */
	public static final LispCharacter BACKSPACE =
		new LispCharacter('\u0008');

	/**
	 * 
	 */
	public static final LispCharacter TAB =
		new LispCharacter('\u0009');

	/**
	 * 
	 */
	public static final LispCharacter LINEFEED =
		new LispCharacter('\n');

	/**
	 * 
	 */
	public static final LispCharacter VTAB =
		new LispCharacter('\u000b');

	/**
	 * 
	 */
	public static final LispCharacter PAGE =
		new LispCharacter('\u000c');

	/**
	 * 
	 */
	public static final LispCharacter RETURN =
		new LispCharacter('\r');

	/**
	 * 
	 */
	public static final LispCharacter ESC =
		new LispCharacter('\u001b');

	/**
	 * 
	 */
	public static final LispCharacter DELETE =
		new LispCharacter('\u007F');

	//
	private int character;

	/**
	 * 
	 * @param ch
	 */
	public LispCharacter(int ch) {
		if(ch < 0) {
			throw new IllegalArgumentException();
		}
		character = ch;
	}

	//
	private static String getName(int ch) {
		if(ch == '\n') {
			return "newline";
		} else if(ch == ' ') {
			return "space";
		} else if(ch == '\u0000') {
			return "nul";
		} else if(ch == '\u0007') {
			return "alarm";
		} else if(ch == '\u0008') {
			return "backspace";
		} else if(ch == '\u0009') {
			return "tab";
		} else if(ch == '\u000b') {
			return "vtab";
		} else if(ch == '\u000c') {
			return "page";
		} else if(ch == '\r') {
			return "return";
		} else if(ch == '\u001b') {
			return "esc";
		} else if(ch == '\u007f') {
			return "delete";
		} else if(ch > Character.MAX_VALUE) {
			return "?";
		} else {
			return Character.toString((char)ch);
		}
	}

	/**
	 * 
	 * @param name
	 * @return
	 */
	public static LispCharacter getByName(String name) {
		if("newline".equals(name)) {
			return NEWLINE;
		} else if("space".equals(name)) {
			return SPACE;
		} else if("nul".equals(name)) {
			return NUL;
		} else if("alarm".equals(name)) {
			return ALERM;
		} else if("backspace".equals(name)) {
			return BACKSPACE;
		} else if("tab".equals(name)) {
			return TAB;
		} else if("linefeed".equals(name)) {
			return LINEFEED;
		} else if("vtab".equals(name)) {
			return VTAB;
		} else if("page".equals(name)) {
			return PAGE;
		} else if("return".equals(name)) {
			return RETURN;
		} else if("esc".equals(name)) {
			return ESC;
		} else if("delete".equals(name)) {
			return DELETE;
		} else {
			return null;
		}
	}

	/**
	 * @param charAt
	 * @return
	 */
	public static LispCharacter valueOf(int ch) {
		return new LispCharacter(ch);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getCharacter()
	 */
	public char getCharacter() {
		return (char)character;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getCharacterCodePoint()
	 */
	public int getCharacterCodePoint() {
		return character;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	@Override
	public String getResult() {
		return "#\\" + getName(character);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	@Override
	public String print() {
		if(character > Character.MAX_VALUE) {
			return "?";
		} else {
			return Character.toString((char)character);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toLispString()
	 */
	@Override
	public LispString toLispString() {
		return new LispString("#\\" + getName(character));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTypeCharacter()
	 */
	public boolean isTypeCharacter() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(LispCharacter o) {
		return (character < o.character) ? -1 :
			((character > o.character) ? 1 : 0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#equalIncliment(net.morilib.util.Inclementor)
	 */
	public boolean equalIncliment(Inclementor<?> i) {
		if(i instanceof LispCharacter) {
			return character == ((LispCharacter)i).character;
		} else {
			return character == i.toInt();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#equalInt(int)
	 */
	public boolean equalInt(int i) {
		return character == i;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#getObject()
	 */
	public LispCharacter getObject() {
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#isZero()
	 */
	public boolean isZero() {
		return character == 0;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#suc()
	 */
	public Inclementor<LispCharacter> suc() {
		if(character >= Character.MAX_VALUE) {
			throw new InclementorBoundsException();
		}
		return new LispCharacter((char)(character + 1));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#suc(int)
	 */
	public Inclementor<LispCharacter> suc(int step) {
		if((character + step) >= Character.MAX_VALUE) {
			throw new InclementorBoundsException();
		}
		return new LispCharacter((char)(character + 1));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#toInt()
	 */
	public int toInt() {
		return character;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#hasNext()
	 */
	public boolean hasNext() {
		return character < Character.MAX_VALUE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#getZero()
	 */
	public Inclementor<LispCharacter> getZero() {
		return new LispCharacter((char)0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#equalInt(java.math.BigInteger)
	 */
	public boolean equalInt(BigInteger i) {
		return i.equals(BigInteger.valueOf(character));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	public LispType getType() {
		return LispType.CHAR;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toObject()
	 */
	public Object toObject() {
		if(character > Character.MAX_VALUE) {
			return Integer.valueOf(character);
		} else {
			return Character.valueOf((char)character);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object d) {
		if(d instanceof LispCharacter) {
			return character == ((LispCharacter)d).character;
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int l = 17;

		l = 37 * l + (int)character;
		return l;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		if(character > Character.MAX_VALUE) {
			return "?";
		} else {
			return Character.toString((char)character);
		}
	}

}
