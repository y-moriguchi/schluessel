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

import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface IntegerCollection extends Collection<Integer> {
	
	public boolean addInt(int v);
	
	public boolean add(int v);
	
	public boolean addAllInt(IntegerCollection a);
	
	public boolean addAllInt(IntegerCollection... as);
	
	public boolean addAllInt(
			Collection<? extends IntegerCollection> as);
	
	public void clear();
	
	public boolean containsInt(int v);
	
	public boolean contains(int v);
	
	public boolean containsAllInt(IntegerCollection a);
	
	public boolean isEmpty();
	
	public IntegerIterator intIterator();
	
	public boolean removeInt(int v);
	
	public boolean removeElement(int v);
	
	public boolean removeAllInt(IntegerCollection a);
	
	public boolean retainAllInt(IntegerCollection a);
	
	public int size();
	
	public int[] toIntArray();
	
	public int[] toIntArray(int[] a);
	
	public boolean isInfinite();
	
	public IntegerSet toSet();
	
}
