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

import net.morilib.util.primitive.IntegerSet;
import net.morilib.util.primitive.IntegerCollection;
import net.morilib.util.primitive.map.op.IntegerValueMap;
import net.morilib.util.primitive.map.po.IntegerMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface IntegerIntegerMap
extends Map<Integer, Integer>,
IntegerMap<Integer>,
IntegerValueMap<Integer> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public int getKey();
		
		public int getValue();
		
		public int setValue(int v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Integer getElement(int k);
	
	public Integer get(int k);
	
	public int f(int k);
	
	//public IntegerIntegerMap go(int k)   // %SAME_DOMAIN%
	
	public IntegerSet keySet();
	
	public Integer putElement(int k, int v);
	
	public void putAllElement(IntegerIntegerMap map);
	
	public Integer removeElement(int k);
	
	public Integer remove(int k);
	
	public IntegerCollection values();
	
	public boolean isTotal();
	
}
