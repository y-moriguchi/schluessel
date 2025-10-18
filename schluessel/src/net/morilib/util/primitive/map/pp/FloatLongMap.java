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
import net.morilib.util.primitive.LongCollection;
import net.morilib.util.primitive.map.op.LongValueMap;
import net.morilib.util.primitive.map.po.FloatMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface FloatLongMap
extends Map<Float, Long>,
FloatMap<Long>,
LongValueMap<Float> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public float getKey();
		
		public long getValue();
		
		public long setValue(long v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Long getElement(float k);
	
	public Long get(int k);
	
	public long f(float k);
	
	//public FloatLongMap go(float k)   // %SAME_DOMAIN%
	
	public FloatSet keySet();
	
	public Long putElement(float k, long v);
	
	public void putAllElement(FloatLongMap map);
	
	public Long removeElement(float k);
	
	public Long remove(int k);
	
	public LongCollection values();
	
	public boolean isTotal();
	
}
