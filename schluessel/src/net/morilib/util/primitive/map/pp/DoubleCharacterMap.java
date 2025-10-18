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

import net.morilib.util.primitive.DoubleSet;
import net.morilib.util.primitive.CharacterCollection;
import net.morilib.util.primitive.map.op.CharacterValueMap;
import net.morilib.util.primitive.map.po.DoubleMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface DoubleCharacterMap
extends Map<Double, Character>,
DoubleMap<Character>,
CharacterValueMap<Double> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public double getKey();
		
		public char getValue();
		
		public char setValue(char v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Character getElement(double k);
	
	public Character get(int k);
	
	public char f(double k);
	
	//public DoubleCharacterMap go(double k)   // %SAME_DOMAIN%
	
	public DoubleSet keySet();
	
	public Character putElement(double k, char v);
	
	public void putAllElement(DoubleCharacterMap map);
	
	public Character removeElement(double k);
	
	public Character remove(int k);
	
	public CharacterCollection values();
	
	public boolean isTotal();
	
}
