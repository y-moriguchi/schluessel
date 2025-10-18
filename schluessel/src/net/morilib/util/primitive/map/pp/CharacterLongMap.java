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
import net.morilib.util.primitive.LongCollection;
import net.morilib.util.primitive.map.op.LongValueMap;
import net.morilib.util.primitive.map.po.CharacterMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface CharacterLongMap
extends Map<Character, Long>,
CharacterMap<Long>,
LongValueMap<Character> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public char getKey();
		
		public long getValue();
		
		public long setValue(long v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Long getElement(char k);
	
	public Long get(int k);
	
	public long f(char k);
	
	//public CharacterLongMap go(char k)   // %SAME_DOMAIN%
	
	public CharacterSet keySet();
	
	public Long putElement(char k, long v);
	
	public void putAllElement(CharacterLongMap map);
	
	public Long removeElement(char k);
	
	public Long remove(int k);
	
	public LongCollection values();
	
	public boolean isTotal();
	
}
