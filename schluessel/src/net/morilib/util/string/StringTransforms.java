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

import net.morilib.lang.transform.CharacterIndicator;
import net.morilib.lang.transform.Ctype;
import net.morilib.lang.transform.Transform;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/23
 */
public final class StringTransforms {
	
	//
	private StringTransforms() { }
	
	/**
	 * 
	 */
	public static final Transform<String>
	HYPHEN_TO_CAMEL = new Transform<String>() {
		
		public String f(String s) {
			CharacterIndicator ind = Ctype.JAVA_IDENTIFIER_START;
			StringBuilder b = new StringBuilder();
			boolean hyphen = false;
			
			for(int i = 0; i < s.length(); i++) {
				char c = s.charAt(i);
				
				if(hyphen) {
					if(Character.isLowerCase(c)) {
						b.append(Character.toUpperCase(c));
					} else if(ind.ki(c)) {
						b.append(c);
					} else {
						throw new InvalidStringException();
					}
					hyphen = false;
				} else if(c == '-') {
					hyphen = true;
				} else if(ind.ki(c)) {
					b.append(c);
				} else {
					throw new InvalidStringException();
				}
				ind = Ctype.JAVA_IDENTIFIER_PART;
			}
			return b.toString();
		}
		
	};
	
	/**
	 * 
	 */
	public static final Transform<String>
	CAMEL_TO_HYPHEN = new Transform<String>() {
		
		public String f(String s) {
			CharacterIndicator ind = Ctype.JAVA_IDENTIFIER_START;
			StringBuilder b = new StringBuilder();
			
			for(int i = 0; i < s.length(); i++) {
				char c = s.charAt(i);
				
				if(c > 0 && Character.isUpperCase(c)) {
					b.append('-').append(c);
				} else if(ind.ki(c)) {
					b.append(c);
				} else {
					throw new InvalidStringException();
				}
				ind = Ctype.JAVA_IDENTIFIER_PART;
			}
			return b.toString();
		}
		
	};
	
	/**
	 * 
	 */
	public static final Transform<String>
	CAPITALIZE_FIRST = new Transform<String>() {
		
		public String f(String s) {
			StringBuilder b = new StringBuilder();
			
			if(s.length() == 0) {
				// do nothing
			} else if(Character.isLowerCase(s.charAt(0))) {
				b.append(Character.toUpperCase(s.charAt(0)));
				b.append(s, 1, s.length());
			} else {
				return s;
			}
			return b.toString();
		}
		
	};
	
}
