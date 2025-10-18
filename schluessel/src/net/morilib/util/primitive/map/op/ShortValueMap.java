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

import net.morilib.util.primitive.ShortCollection;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface ShortValueMap<K> extends Map<K, Short> {
	
	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntryV<K> {
		
		
		public K getKey();
		
		
		public short getValue();
		
		
		public short setValue(short v);
		
	}
	
	
	public void clear();
	
	
	public boolean containsKey(Object k);
	
	
	public boolean containsValueElement(short v);
	
	
	public boolean containsValue(int v);
	
	
	public Set<PrimitiveEntryV<K>> shortValueEntrySet();
	
	
	public short getElement(Object k);
	
	
	public Short get(Object k);
	
	
	public boolean isEmpty();
	
	
	public Set<K> keySet();
	
	
	public Short put(K k, short v);
	
	
	public short putElement(K k, short v);
	
	
	public void putAllElement(ShortValueMap<K> map);
	
	
	public Short remove(Object k);
	
	
	public int size();
	
	
	public ShortCollection shortValues();
	
}
