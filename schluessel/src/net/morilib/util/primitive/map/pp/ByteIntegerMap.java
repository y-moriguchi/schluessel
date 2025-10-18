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

import net.morilib.util.primitive.ByteSet;
import net.morilib.util.primitive.IntegerCollection;
import net.morilib.util.primitive.map.op.IntegerValueMap;
import net.morilib.util.primitive.map.po.ByteMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface ByteIntegerMap
extends Map<Byte, Integer>,
ByteMap<Integer>,
IntegerValueMap<Byte> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public byte getKey();
		
		public int getValue();
		
		public int setValue(int v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Integer getElement(byte k);
	
	public Integer get(int k);
	
	public int f(byte k);
	
	//public ByteIntegerMap go(byte k)   // %SAME_DOMAIN%
	
	public ByteSet keySet();
	
	public Integer putElement(byte k, int v);
	
	public void putAllElement(ByteIntegerMap map);
	
	public Integer removeElement(byte k);
	
	public Integer remove(int k);
	
	public IntegerCollection values();
	
	public boolean isTotal();
	
}
