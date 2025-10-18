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
import net.morilib.util.primitive.ByteCollection;
import net.morilib.util.primitive.map.op.ByteValueMap;
import net.morilib.util.primitive.map.po.FloatMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface FloatByteMap
extends Map<Float, Byte>,
FloatMap<Byte>,
ByteValueMap<Float> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public float getKey();
		
		public byte getValue();
		
		public byte setValue(byte v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Byte getElement(float k);
	
	public Byte get(int k);
	
	public byte f(float k);
	
	//public FloatByteMap go(float k)   // %SAME_DOMAIN%
	
	public FloatSet keySet();
	
	public Byte putElement(float k, byte v);
	
	public void putAllElement(FloatByteMap map);
	
	public Byte removeElement(float k);
	
	public Byte remove(int k);
	
	public ByteCollection values();
	
	public boolean isTotal();
	
}
