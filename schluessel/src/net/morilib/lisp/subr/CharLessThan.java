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

public class CharLessThan extends CharCompare {

	@Override
	protected boolean compare(LispCharacter c1a, LispCharacter c2a) {
		return c1a.getCharacter() < c2a.getCharacter();
	}
	
	
	public String toString() {
		return "Subr:char<?";
	}
	
}
