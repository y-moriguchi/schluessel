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

import net.morilib.util.primitive.ByteSet;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public interface ByteMap<V> extends Map<Byte, V> {
	
	/**
	 * 
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/16
	 */
	public static interface PrimitiveEntryK<V> {
		
		public byte getKey();
		
		public V getValue();
		
		public V setValue(V v);
		
	}
	
	public boolean containsKey(int k);
	
	public boolean containsKeyElement(byte k);
	
	public Set<PrimitiveEntryK<V>> byteKeyEntrySet();
	
	public V get(int k);
	
	public V getElement(byte k);
	
	public ByteSet byteKeySet();
	
	public V put(int k, V v);
	
	public V putElement(byte k, V v);
	
	public void putAllElement(ByteMap<V> map);
	
	public V removeElement(byte k);
	
	public V remove(int k);
	
	public boolean isTotal();
	
}
