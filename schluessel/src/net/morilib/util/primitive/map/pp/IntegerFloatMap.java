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
import net.morilib.util.primitive.FloatCollection;
import net.morilib.util.primitive.map.op.FloatValueMap;
import net.morilib.util.primitive.map.po.IntegerMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface IntegerFloatMap
extends Map<Integer, Float>,
IntegerMap<Float>,
FloatValueMap<Integer> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public int getKey();
		
		public float getValue();
		
		public float setValue(float v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Float getElement(int k);
	
	public Float get(int k);
	
	public float f(int k);
	
	//public IntegerFloatMap go(int k)   // %SAME_DOMAIN%
	
	public IntegerSet keySet();
	
	public Float putElement(int k, float v);
	
	public void putAllElement(IntegerFloatMap map);
	
	public Float removeElement(int k);
	
	public Float remove(int k);
	
	public FloatCollection values();
	
	public boolean isTotal();
	
}
