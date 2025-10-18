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
import net.morilib.util.primitive.ShortCollection;
import net.morilib.util.primitive.map.op.ShortValueMap;
import net.morilib.util.primitive.map.po.FloatMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface FloatShortMap
extends Map<Float, Short>,
FloatMap<Short>,
ShortValueMap<Float> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public float getKey();
		
		public short getValue();
		
		public short setValue(short v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Short getElement(float k);
	
	public Short get(int k);
	
	public short f(float k);
	
	//public FloatShortMap go(float k)   // %SAME_DOMAIN%
	
	public FloatSet keySet();
	
	public Short putElement(float k, short v);
	
	public void putAllElement(FloatShortMap map);
	
	public Short removeElement(float k);
	
	public Short remove(int k);
	
	public ShortCollection values();
	
	public boolean isTotal();
	
}
