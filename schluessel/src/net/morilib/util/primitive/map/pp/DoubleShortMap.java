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

import net.morilib.util.primitive.DoubleSet;
import net.morilib.util.primitive.ShortCollection;
import net.morilib.util.primitive.map.op.ShortValueMap;
import net.morilib.util.primitive.map.po.DoubleMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface DoubleShortMap
extends Map<Double, Short>,
DoubleMap<Short>,
ShortValueMap<Double> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public double getKey();
		
		public short getValue();
		
		public short setValue(short v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Short getElement(double k);
	
	public Short get(int k);
	
	public short f(double k);
	
	//public DoubleShortMap go(double k)   // %SAME_DOMAIN%
	
	public DoubleSet keySet();
	
	public Short putElement(double k, short v);
	
	public void putAllElement(DoubleShortMap map);
	
	public Short removeElement(double k);
	
	public Short remove(int k);
	
	public ShortCollection values();
	
	public boolean isTotal();
	
}
