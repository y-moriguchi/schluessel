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
import net.morilib.util.primitive.IntegerCollection;
import net.morilib.util.primitive.map.op.IntegerValueMap;
import net.morilib.util.primitive.map.po.DoubleMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface DoubleIntegerMap
extends Map<Double, Integer>,
DoubleMap<Integer>,
IntegerValueMap<Double> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public double getKey();
		
		public int getValue();
		
		public int setValue(int v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Integer getElement(double k);
	
	public Integer get(int k);
	
	public int f(double k);
	
	//public DoubleIntegerMap go(double k)   // %SAME_DOMAIN%
	
	public DoubleSet keySet();
	
	public Integer putElement(double k, int v);
	
	public void putAllElement(DoubleIntegerMap map);
	
	public Integer removeElement(double k);
	
	public Integer remove(int k);
	
	public IntegerCollection values();
	
	public boolean isTotal();
	
}
