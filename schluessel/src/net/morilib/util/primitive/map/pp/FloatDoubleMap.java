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
import net.morilib.util.primitive.DoubleCollection;
import net.morilib.util.primitive.map.op.DoubleValueMap;
import net.morilib.util.primitive.map.po.FloatMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface FloatDoubleMap
extends Map<Float, Double>,
FloatMap<Double>,
DoubleValueMap<Float> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntry {
		
		public float getKey();
		
		public double getValue();
		
		public double setValue(double v);
		
	}
	
	public Set<PrimitiveEntry> primitiveEntrySet();
	
	public Double getElement(float k);
	
	public Double get(int k);
	
	public double f(float k);
	
	//public FloatDoubleMap go(float k)   // %SAME_DOMAIN%
	
	public FloatSet keySet();
	
	public Double putElement(float k, double v);
	
	public void putAllElement(FloatDoubleMap map);
	
	public Double removeElement(float k);
	
	public Double remove(int k);
	
	public DoubleCollection values();
	
	public boolean isTotal();
	
}
