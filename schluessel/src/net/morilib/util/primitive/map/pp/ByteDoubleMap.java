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
import net.morilib.util.primitive.DoubleCollection;
import net.morilib.util.primitive.map.op.DoubleValueMap;
import net.morilib.util.primitive.map.po.ByteMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface ByteDoubleMap
extends Map<Byte, Double>,
ByteMap<Double>,
DoubleValueMap<Byte> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public byte getKey();
		
		public double getValue();
		
		public double setValue(double v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Double getElement(byte k);
	
	public Double get(int k);
	
	public double f(byte k);
	
	//public ByteDoubleMap go(byte k)   // %SAME_DOMAIN%
	
	public ByteSet keySet();
	
	public Double putElement(byte k, double v);
	
	public void putAllElement(ByteDoubleMap map);
	
	public Double removeElement(byte k);
	
	public Double remove(int k);
	
	public DoubleCollection values();
	
	public boolean isTotal();
	
}
