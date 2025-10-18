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
package net.morilib.util.iterator;

/**
 * Implementation of an Object[] array.
 * Object[]型の配列である.
 * 
 * @author MORIGUCHI, Yuichiro 2005/01/02
 */
public class ArrayIterator<E> extends IndexIterator<E> {
	
	//
	private E[] array;
	
	/**
	 * constructs an iterator of the given array.
	 * <p>与えられた配列のIteratorを生成する.
	 * 
	 * @param objs  an array to be iterated
	 */
	public ArrayIterator(E... objs) {
		super(0, objs.length);
		array = objs;
	}
	
	/**
	 * constructs an iterator of the given array.
	 * <p>与えられた配列のbeginからendまでを反復するIteratorを生成する.
	 * 
	 * @param objs   an array to be iterated
	 * @param begin  the starting index 
	 * @param end    the ending index
	 */
	public ArrayIterator(int begin, int end, E... objs) {
		super(begin, end);
		array = objs;
	}
	
	/**
	 * constructs an iterator of the given array.
	 * <p>与えられた配列のindexの長さを反復するIteratorを生成する.
	 * 
	 * @param objs    an array to be iterated
	 * @param length  length to be iterated
	 */
	public ArrayIterator(int length, E... objs) {
		super(length, 0, objs.length);
		array = objs;
	}
	
	/**
	 * constructs an iterator of the given object.
	 * <p>与えられた配列のbeginからendまでを反復するIteratorを生成する.
	 * Iteratorの添え字はindexから開始する.
	 * 
	 * @param objs
	 * @param begin
	 * @param end
	 */
	public ArrayIterator(int index, int begin, int end, E... objs) {
		super(index, begin, end);
		array = objs;
	}
	
	/* (non-Javadoc)
	 * @see org.usei.data.iterator.IndexIterator#get(int)
	 */
	protected E get(int index) {
		if(!isInRange(index)) {
			throw new IndexOutOfBoundsException("" + index);
		}
		return array[index];
	}

	/* (non-Javadoc)
	 * @see org.usei.data.iterator.IndexIterator#set(int, java.lang.Object)
	 */
	protected void set(int index, E o) {
		throw new UnsupportedOperationException();
	}

}
