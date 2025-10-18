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
package net.morilib.parser;

import java.util.HashMap;
import java.util.Map;

import net.morilib.util.Tokenizer;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/09
 */
public class QuoteLexer implements Tokenizer<Object> {
	
	//
	private Map<Character, Enum<?>> map;
	
	/**
	 * 
	 */
	public static final Object END = new Object();
	
	//
	/*package*/ static final Object EMPTY       = new Object();
	/*package*/ static final Object COMMA       = new Object();
	/*package*/ static final Object EQUAL       = new Object();
	private static final int _INIT = 0;
	private static final int _UNQT = 1;
	private static final int _QUOT = 2;
	private static final int _CNMA = 3;
	private static final int _ENDS = 5;
	
	//
	private String str;
	private int    ptr = 0;
	private int    delim;
	private int    quote;
	private int    state = _INIT;
	
	/**
	 * 
	 * @param c
	 * @param map
	 */
	public QuoteLexer(
			char c, Map<Character, ? extends Enum<?>> map) {
		this.quote = c;
		this.map   = new HashMap<Character, Enum<?>>(map);
	}
	
	/**
	 * 
	 * @param map
	 */
	public QuoteLexer(Map<Character, ? extends Enum<?>> map) {
		this('\"', map);
	}
	
	/**
	 * 
	 * @param str
	 */
	public void setString(String str) {
		this.str   = str;
		this.ptr   = 0;
		this.state = _INIT;
	}
	
	//
	private int getchar() {
		return (ptr < str.length()) ? str.charAt(ptr++) : -1;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.Tokenizer#getToken()
	 */
	public Object getToken() {
		StringBuilder b = new StringBuilder();
		int c;
		
		while(true) {
			switch(state) {
			case _INIT:
				c = getchar();
				if(c < 0) {
					state = _ENDS;
					return b.toString();
				} else if(c == quote) {
					state = _QUOT;
				} else if(map.containsKey((char)c)) {
					state = _CNMA;
					delim = c;
					return b.toString();
				} else {
					b.append((char)c);
				}
				break;
			case _UNQT:
				c = getchar();
				if(c < 0) {
					state = _INIT;
					return b.toString();
				} else if(map.containsKey((char)c)) {
					state = _CNMA;
					delim = c;
					return b.toString();
				} else {
					b.append((char)c);
				}
				break;
			case _QUOT:
				c = getchar();
				if(c < 0) {
					throw new RuntimeException();
				} else if(c == quote) {
					int d = getchar();
					
					if(d < 0) {
						state = _ENDS;
						return b.toString();
					} else if(map.containsKey((char)c)) {
						state = _CNMA;
						delim = c;
						return b.toString();
					} else if(d == '\"') {
						b.append((char)d);
					} else {
						throw new RuntimeException();
					}
				} else {
					b.append((char)c);
				}
				break;
			case _CNMA:
				state = _INIT;
				return map.get((char)delim);
			case _ENDS:
				state = _INIT;
				return END;
			}
		}
	}
	
}
