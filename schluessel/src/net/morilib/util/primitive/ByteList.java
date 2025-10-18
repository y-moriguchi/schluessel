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
public interface ByteList extends ByteCollection {
	
	public void addByte(int index, byte v);
	
	public void add(int index, int v);
	
	public boolean addAllByte(int index, ByteCollection a);
	
	public byte first();
	
	public byte getByte(int index);
	
	public int indexOfByte(byte v);
	
	public int indexOf(int v);
	
	public byte removeAt(int index);
	
	public ByteList rest();
	
	public ByteList rest(int index);
	
	public byte setByte(int index, byte v);
	
	public byte set(int index, int v);
	
}
