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

import net.morilib.util.primitive.FloatSet;
import net.morilib.util.primitive.IntegerCollection;
import net.morilib.util.primitive.map.op.IntegerValueMap;
import net.morilib.util.primitive.map.po.FloatMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface FloatIntegerMap
extends Map<Float, Integer>,
FloatMap<Integer>,
IntegerValueMap<Float> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public float getKey();
		
		public int getValue();
		
		public int setValue(int v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Integer getElement(float k);
	
	public Integer get(int k);
	
	public int f(float k);
	
	//public FloatIntegerMap go(float k)   // %SAME_DOMAIN%
	
	public FloatSet keySet();
	
	public Integer putElement(float k, int v);
	
	public void putAllElement(FloatIntegerMap map);
	
	public Integer removeElement(float k);
	
	public Integer remove(int k);
	
	public IntegerCollection values();
	
	public boolean isTotal();
	
}
