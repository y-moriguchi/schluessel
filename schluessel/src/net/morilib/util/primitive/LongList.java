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

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface LongList extends LongCollection {
	
	public void addLong(int index, long v);
	
	public void add(int index, int v);
	
	public boolean addAllLong(int index, LongCollection a);
	
	public long first();
	
	public long getLong(int index);
	
	public int indexOfLong(long v);
	
	public int indexOf(int v);
	
	public long removeAt(int index);
	
	public LongList rest();
	
	public LongList rest(int index);
	
	public long setLong(int index, long v);
	
	public long set(int index, int v);
	
}
