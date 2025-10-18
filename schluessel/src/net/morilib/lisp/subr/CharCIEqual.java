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
package net.morilib.lisp.subr;

import net.morilib.lisp.LispCharacter;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class CharCIEqual extends CharCompare {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.CharCompare#compare(net.morilib.lisp.LispCharacter, net.morilib.lisp.LispCharacter)
	 */
	@Override
	protected boolean compare(LispCharacter c1a, LispCharacter c2a) {
//		char ch1 = Character.toUpperCase(c1a.getCharacter());
//		char ch2 = Character.toUpperCase(c2a.getCharacter());
		int ch1 = SubrUtils.toFoldCase(c1a.getCharacterCodePoint());
		int ch2 = SubrUtils.toFoldCase(c2a.getCharacterCodePoint());

//		ch1 = Character.toLowerCase(ch1);
//		ch2 = Character.toLowerCase(ch2);
		return ch1 == ch2;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Subr#toString()
	 */
	public String toString() {
		return "Subr:char-ci=?";
	}

}
