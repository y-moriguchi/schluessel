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
public interface IntegerList extends IntegerCollection {
	
	public void addInt(int index, int v);
	
	public void add(int index, int v);
	
	public boolean addAllInt(int index, IntegerCollection a);
	
	public int first();
	
	public int getInt(int index);
	
	public int indexOfInt(int v);
	
	public int indexOf(int v);
	
	public int removeAt(int index);
	
	public IntegerList rest();
	
	public IntegerList rest(int index);
	
	public int setInt(int index, int v);
	
	public int set(int index, int v);
	
}
