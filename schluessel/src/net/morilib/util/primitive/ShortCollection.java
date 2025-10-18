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
package net.morilib.util.primitive;

import java.util.Collection;

import net.morilib.util.primitive.iterator.ShortIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface ShortCollection extends Collection<Short> {
	
	
	public boolean addShort(short v);
	
	
	public boolean add(int v);
	
	
	public boolean addAllShort(ShortCollection a);
	
	
	public boolean addAllShort(ShortCollection... as);
	
	
	public boolean addAllShort(
			Collection<? extends ShortCollection> as);
	
	
	public void clear();
	
	
	public boolean containsShort(short v);
	
	
	public boolean contains(int v);
	
	
	public boolean containsAllShort(ShortCollection a);
	
	
	public boolean isEmpty();
	
	
	public ShortIterator shortIterator();
	
	
	public boolean removeShort(short v);
	
	
	public boolean removeElement(int v);
	
	
	public boolean removeAllShort(ShortCollection a);
	
	
	public boolean retainAllShort(ShortCollection a);
	
	
	public int size();
	
	
	public short[] toShortArray();
	
	
	public short[] toShortArray(short[] a);
	
	
	public boolean isInfinite();
	
	
	public ShortSet toSet();
	
}
