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
import net.morilib.util.primitive.ShortCollection;
import net.morilib.util.primitive.map.op.ShortValueMap;
import net.morilib.util.primitive.map.po.LongMap;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface LongShortMap
extends Map<Long, Short>,
LongMap<Short>,
ShortValueMap<Long> {
	
	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		
		public long getKey();
		
		
		public short getValue();
		
		
		public short setValue(short v);
		
	}
	
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	
	public Short getElement(long k);
	
	
	public Short get(int k);
	
	
	public short f(long k);
	
	
	//public LongShortMap go(long k)   // %SAME_DOMAIN%
	
	
	public LongSet keySet();
	
	
	public Short putElement(long k, short v);
	
	
	public void putAllElement(LongShortMap map);
	
	
	public Short removeElement(long k);
	
	
	public Short remove(int k);
	
	
	public ShortCollection values();
	
	
	public boolean isTotal();
	
}
