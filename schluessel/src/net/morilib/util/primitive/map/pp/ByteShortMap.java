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
import net.morilib.util.primitive.ShortCollection;
import net.morilib.util.primitive.map.op.ShortValueMap;
import net.morilib.util.primitive.map.po.ByteMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface ByteShortMap
extends Map<Byte, Short>,
ByteMap<Short>,
ShortValueMap<Byte> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public byte getKey();
		
		public short getValue();
		
		public short setValue(short v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Short getElement(byte k);
	
	public Short get(int k);
	
	public short f(byte k);
	
	//public ByteShortMap go(byte k)   // %SAME_DOMAIN%
	
	public ByteSet keySet();
	
	public Short putElement(byte k, short v);
	
	public void putAllElement(ByteShortMap map);
	
	public Short removeElement(byte k);
	
	public Short remove(int k);
	
	public ShortCollection values();
	
	public boolean isTotal();
	
}
