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
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface ShortList extends ShortCollection {
	
	
	public void addShort(int index, short v);
	
	
	public void add(int index, int v);
	
	
	public boolean addAllShort(int index, ShortCollection a);
	
	
	public short first();
	
	
	public short getShort(int index);
	
	
	public int indexOfShort(short v);
	
	
	public int indexOf(int v);
	
	
	public short removeAt(int index);
	
	
	public ShortList rest();
	
	
	public ShortList rest(int index);
	
	
	public short setShort(int index, short v);
	
	
	public short set(int index, int v);
	
}
