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
import net.morilib.util.primitive.ByteCollection;
import net.morilib.util.primitive.map.op.ByteValueMap;
import net.morilib.util.primitive.map.po.CharacterMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface CharacterByteMap
extends Map<Character, Byte>,
CharacterMap<Byte>,
ByteValueMap<Character> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public char getKey();
		
		public byte getValue();
		
		public byte setValue(byte v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Byte getElement(char k);
	
	public Byte get(int k);
	
	public byte f(char k);
	
	//public CharacterByteMap go(char k)   // %SAME_DOMAIN%
	
	public CharacterSet keySet();
	
	public Byte putElement(char k, byte v);
	
	public void putAllElement(CharacterByteMap map);
	
	public Byte removeElement(char k);
	
	public Byte remove(int k);
	
	public ByteCollection values();
	
	public boolean isTotal();
	
}
