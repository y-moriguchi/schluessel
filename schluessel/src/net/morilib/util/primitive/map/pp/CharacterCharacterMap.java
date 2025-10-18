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
package net.morilib.util.primitive.map.pp;

import java.util.Map;
import java.util.Set;

import net.morilib.util.primitive.CharacterSet;
import net.morilib.util.primitive.CharacterCollection;
import net.morilib.util.primitive.map.op.CharacterValueMap;
import net.morilib.util.primitive.map.po.CharacterMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface CharacterCharacterMap
extends Map<Character, Character>,
CharacterMap<Character>,
CharacterValueMap<Character> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public char getKey();
		
		public char getValue();
		
		public char setValue(char v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Character getElement(char k);
	
	public Character get(int k);
	
	public char f(char k);
	
	//public CharacterCharacterMap go(char k)   // %SAME_DOMAIN%
	
	public CharacterSet keySet();
	
	public Character putElement(char k, char v);
	
	public void putAllElement(CharacterCharacterMap map);
	
	public Character removeElement(char k);
	
	public Character remove(int k);
	
	public CharacterCollection values();
	
	public boolean isTotal();
	
}
