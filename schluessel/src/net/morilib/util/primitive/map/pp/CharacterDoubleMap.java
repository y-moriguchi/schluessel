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
import net.morilib.util.primitive.DoubleCollection;
import net.morilib.util.primitive.map.op.DoubleValueMap;
import net.morilib.util.primitive.map.po.CharacterMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface CharacterDoubleMap
extends Map<Character, Double>,
CharacterMap<Double>,
DoubleValueMap<Character> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public char getKey();
		
		public double getValue();
		
		public double setValue(double v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Double getElement(char k);
	
	public Double get(int k);
	
	public double f(char k);
	
	//public CharacterDoubleMap go(char k)   // %SAME_DOMAIN%
	
	public CharacterSet keySet();
	
	public Double putElement(char k, double v);
	
	public void putAllElement(CharacterDoubleMap map);
	
	public Double removeElement(char k);
	
	public Double remove(int k);
	
	public DoubleCollection values();
	
	public boolean isTotal();
	
}
