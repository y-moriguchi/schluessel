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

import net.morilib.util.primitive.iterator.CharacterIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface CharacterCollection extends Collection<Character> {
	
	public boolean addChar(char v);
	
	public boolean add(int v);
	
	public boolean addAllChar(CharacterCollection a);
	
	public boolean addAllChar(CharacterCollection... as);
	
	public boolean addAllChar(
			Collection<? extends CharacterCollection> as);
	
	public void clear();
	
	public boolean containsChar(char v);
	
	public boolean contains(int v);
	
	public boolean containsAllChar(CharacterCollection a);
	
	public boolean isEmpty();
	
	public CharacterIterator charIterator();
	
	public boolean removeChar(char v);
	
	public boolean removeElement(int v);
	
	public boolean removeAllChar(CharacterCollection a);
	
	public boolean retainAllChar(CharacterCollection a);
	
	public int size();
	
	public char[] toCharArray();
	
	public char[] toCharArray(char[] a);
	
	public boolean isInfinite();
	
	public CharacterSet toSet();
	
}
