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
import net.morilib.util.primitive.FloatCollection;
import net.morilib.util.primitive.map.op.FloatValueMap;
import net.morilib.util.primitive.map.po.DoubleMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface DoubleFloatMap
extends Map<Double, Float>,
DoubleMap<Float>,
FloatValueMap<Double> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public double getKey();
		
		public float getValue();
		
		public float setValue(float v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Float getElement(double k);
	
	public Float get(int k);
	
	public float f(double k);
	
	//public DoubleFloatMap go(double k)   // %SAME_DOMAIN%
	
	public DoubleSet keySet();
	
	public Float putElement(double k, float v);
	
	public void putAllElement(DoubleFloatMap map);
	
	public Float removeElement(double k);
	
	public Float remove(int k);
	
	public FloatCollection values();
	
	public boolean isTotal();
	
}
