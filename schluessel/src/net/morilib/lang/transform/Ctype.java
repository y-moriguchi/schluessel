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
package net.morilib.lang.transform;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/23
 */
public final class Ctype {
	
	//
	private Ctype() { }
	
	/**
	 * 
	 */
	public static final CharacterIndicator
	JAVA_IDENTIFIER_PART = new CharacterIndicator() {

		public boolean ki(char x) {
			return Character.isJavaIdentifierPart(x);
		}
		
	};
	
	/**
	 * 
	 */
	public static final CharacterIndicator
	JAVA_IDENTIFIER_START = new CharacterIndicator() {

		public boolean ki(char x) {
			return Character.isJavaIdentifierStart(x);
		}
		
	};
	
}
