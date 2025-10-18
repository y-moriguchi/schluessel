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
import net.morilib.util.primitive.LongCollection;
import net.morilib.util.primitive.map.op.LongValueMap;
import net.morilib.util.primitive.map.po.IntegerMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface IntegerLongMap
extends Map<Integer, Long>,
IntegerMap<Long>,
LongValueMap<Integer> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public int getKey();
		
		public long getValue();
		
		public long setValue(long v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Long getElement(int k);
	
	public Long get(int k);
	
	public long f(int k);
	
	//public IntegerLongMap go(int k)   // %SAME_DOMAIN%
	
	public IntegerSet keySet();
	
	public Long putElement(int k, long v);
	
	public void putAllElement(IntegerLongMap map);
	
	public Long removeElement(int k);
	
	public Long remove(int k);
	
	public LongCollection values();
	
	public boolean isTotal();
	
}
