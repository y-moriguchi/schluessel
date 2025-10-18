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
package net.morilib.util.primitive.map.po;

import java.util.Map;
import java.util.Set;

import net.morilib.util.primitive.DoubleSet;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface DoubleMap<V> extends Map<Double, V> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntryK<V> {
		
		public double getKey();
		
		public V getValue();
		
		public V setValue(V v);
		
	}
	
	public boolean containsKey(int k);
	
	public boolean containsKeyElement(double k);
	
	public Set<PrimitiveEntryK<V>> doubleKeyEntrySet();
	
	public V get(int k);
	
	public V getElement(double k);
	
	public DoubleSet doubleKeySet();
	
	public V put(int k, V v);
	
	public V putElement(double k, V v);
	
	public void putAllElement(DoubleMap<V> map);
	
	public V removeElement(double k);
	
	public V remove(int k);
	
	public boolean isTotal();
	
}
