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

import net.morilib.util.primitive.iterator.LongIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface LongCollection extends Collection<Long> {
	
	public boolean addLong(long v);
	
	public boolean add(int v);
	
	public boolean addAllLong(LongCollection a);
	
	public boolean addAllLong(LongCollection... as);
	
	public boolean addAllLong(
			Collection<? extends LongCollection> as);
	
	public void clear();
	
	public boolean containsLong(long v);
	
	public boolean contains(int v);
	
	public boolean containsAllLong(LongCollection a);
	
	public boolean isEmpty();
	
	public LongIterator longIterator();
	
	public boolean removeLong(long v);
	
	public boolean removeElement(int v);
	
	public boolean removeAllLong(LongCollection a);
	
	public boolean retainAllLong(LongCollection a);
	
	public int size();
	
	public long[] toLongArray();
	
	public long[] toLongArray(long[] a);
	
	public boolean isInfinite();
	
	public LongSet toSet();
	
}
