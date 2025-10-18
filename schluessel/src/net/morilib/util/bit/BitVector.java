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
package net.morilib.util.bit;

import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/24
 */
public interface BitVector extends BitCollection, List<Boolean> {

	/**
	 * 
	 * @param index
	 * @param x
	 * @return 
	 */
	public boolean add(int index, boolean x);

	/**
	 * 
	 * @param index
	 * @param col
	 * @return 
	 */
	public boolean addAllBoolean(int index, BitCollection col);

	/**
	 * 
	 * @param index
	 * @return
	 */
	public boolean getBoolean(int index);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public int indexOf(boolean x);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public int lastIndexOf(boolean x);

	/**
	 * 
	 * @return
	 */
	public BitVectorIterator bitVectorIterator();

	/**
	 * 
	 * @param index
	 * @return
	 */
	public BitVectorIterator bitVectorIterator(int index);

	/**
	 * 
	 * @param index
	 * @return
	 */
	public boolean removeAt(int index);

	/**
	 * 
	 * @param index
	 * @param x
	 * @return
	 */
	public boolean set(int index, boolean x);

	/**
	 * 
	 * @param start
	 * @param end
	 * @return
	 */
	public BitVector subVector(int start, int end);

	/**
	 * 
	 */
	public void negate();

}
