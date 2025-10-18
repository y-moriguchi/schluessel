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
public interface CharacterList extends CharacterCollection {
	
	public void addChar(int index, char v);
	
	public void add(int index, int v);
	
	public boolean addAllChar(int index, CharacterCollection a);
	
	public char first();
	
	public char getChar(int index);
	
	public int indexOfChar(char v);
	
	public int indexOf(int v);
	
	public char removeAt(int index);
	
	public CharacterList rest();
	
	public CharacterList rest(int index);
	
	public char setChar(int index, char v);
	
	public char set(int index, int v);
	
}
