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

import net.morilib.util.primitive.iterator.ByteIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface ByteCollection extends Collection<Byte> {
	
	public boolean addByte(byte v);
	
	public boolean add(int v);
	
	public boolean addAllByte(ByteCollection a);
	
	public boolean addAllByte(ByteCollection... as);
	
	public boolean addAllByte(
			Collection<? extends ByteCollection> as);
	
	public void clear();
	
	public boolean containsByte(byte v);
	
	public boolean contains(int v);
	
	public boolean containsAllByte(ByteCollection a);
	
	public boolean isEmpty();
	
	public ByteIterator byteIterator();
	
	public boolean removeByte(byte v);
	
	public boolean removeElement(int v);
	
	public boolean removeAllByte(ByteCollection a);
	
	public boolean retainAllByte(ByteCollection a);
	
	public int size();
	
	public byte[] toByteArray();
	
	public byte[] toByteArray(byte[] a);
	
	public boolean isInfinite();
	
	public ByteSet toSet();
	
}
