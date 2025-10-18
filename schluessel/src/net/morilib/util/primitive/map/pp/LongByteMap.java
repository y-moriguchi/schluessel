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
import net.morilib.util.primitive.ByteCollection;
import net.morilib.util.primitive.map.op.ByteValueMap;
import net.morilib.util.primitive.map.po.LongMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface LongByteMap
extends Map<Long, Byte>,
LongMap<Byte>,
ByteValueMap<Long> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public long getKey();
		
		public byte getValue();
		
		public byte setValue(byte v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Byte getElement(long k);
	
	public Byte get(int k);
	
	public byte f(long k);
	
	//public LongByteMap go(long k)   // %SAME_DOMAIN%
	
	public LongSet keySet();
	
	public Byte putElement(long k, byte v);
	
	public void putAllElement(LongByteMap map);
	
	public Byte removeElement(long k);
	
	public Byte remove(int k);
	
	public ByteCollection values();
	
	public boolean isTotal();
	
}
