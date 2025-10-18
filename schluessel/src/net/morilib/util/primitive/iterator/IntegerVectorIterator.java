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
package net.morilib.util.primitive.iterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface IntegerVectorIterator extends IntegerIterator {
	
	public void addInt(int v);
	
	public void add(int v);
	
	public boolean hasPrevious();
	
	public int nextIndex();
	
	public int previous();
	
	public int previousIndex();
	
	public void setInt(int v);
	
	public void set(int v);
	
}
