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
package net.morilib.util.primitive.map.op;

import java.util.Map;
import java.util.Set;

import net.morilib.util.primitive.DoubleCollection;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface DoubleValueMap<K> extends Map<K, Double> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntryV<K> {
		
		public K getKey();
		
		public double getValue();
		
		public double setValue(double v);
		
	}
	
	public void clear();
	
	public boolean containsKey(Object k);
	
	public boolean containsValueElement(double v);
	
	public boolean containsValue(int v);
	
	public Set<PrimitiveEntryV<K>> doubleValueEntrySet();
	
	public double getElement(Object k);
	
	public Double get(Object k);
	
	public boolean isEmpty();
	
	public Set<K> keySet();
	
	public Double put(K k, double v);
	
	public double putElement(K k, double v);
	
	public void putAllElement(DoubleValueMap<K> map);
	
	public Double remove(Object k);
	
	public int size();
	
	public DoubleCollection doubleValues();
	
}
