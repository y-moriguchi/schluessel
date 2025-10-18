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

import net.morilib.util.primitive.iterator.LongVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface LongVector extends List<Long>, LongList {

	public int lastIndexOfLong(long v);

	public LongVectorIterator longVectorIterator();

	public LongVectorIterator longVectorIterator(int index);

	public LongVector subVector(int start, int end);

	public boolean addAllLong(long[] b);

	public boolean addAllLong(int index, long[] b);

}
