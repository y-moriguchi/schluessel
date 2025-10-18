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

import net.morilib.util.primitive.LongSet;
import net.morilib.util.primitive.FloatCollection;
import net.morilib.util.primitive.map.op.FloatValueMap;
import net.morilib.util.primitive.map.po.LongMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface LongFloatMap
extends Map<Long, Float>,
LongMap<Float>,
FloatValueMap<Long> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public long getKey();
		
		public float getValue();
		
		public float setValue(float v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Float getElement(long k);
	
	public Float get(int k);
	
	public float f(long k);
	
	//public LongFloatMap go(long k)   // %SAME_DOMAIN%
	
	public LongSet keySet();
	
	public Float putElement(long k, float v);
	
	public void putAllElement(LongFloatMap map);
	
	public Float removeElement(long k);
	
	public Float remove(int k);
	
	public FloatCollection values();
	
	public boolean isTotal();
	
}
