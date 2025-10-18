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

import java.util.List;

import net.morilib.util.primitive.iterator.ByteVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface ByteVector extends List<Byte>, ByteList {

	public int lastIndexOfByte(byte v);

	public ByteVectorIterator byteVectorIterator();

	public ByteVectorIterator byteVectorIterator(int index);

	public ByteVector subVector(int start, int end);

	public boolean addAllByte(byte[] b);

	public boolean addAllByte(int index, byte[] b);

}
