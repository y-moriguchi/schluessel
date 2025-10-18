/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.util;

import java.util.AbstractList;
import java.util.Collection;
import java.util.Iterator;

/**
 * Implementation of array as a List.
 * <p>Listとしての配列の実装である.
 * 
 * @author MORIGUCHI, Yuichiro 2006/04/29
 */
public class ObjectArray<E> extends AbstractList<E> {
	
	//
	private Object[] array;
	
	/**
	 * creates a new ObjectArray.
	 * <p>新しくObjectArrayを生成する.
	 * 
	 * @param size  配列のサイズ
	 */
	public ObjectArray(int size) {
		array = new Object[size];
	}
	
	/**
	 * creates a new ObjectArray.
	 * <p>新しくObjectArrayを生成する.
	 * 
	 * @param array  コピー元の配列
	 */
	public ObjectArray(E[] array) {
		this.array = (Object[])array.clone();
	}
	
	/**
	 * creates a new ObjectArray.
	 * <p>新しくObjectArrayを生成する.
	 * 
	 * @param col  コピー元のコレクション
	 */
	public ObjectArray(Collection<E> col) {
		this(col.size());
		
		Iterator<E> iter = col.iterator();
		for(int i = 0; iter.hasNext(); i++) {
			array[i] = iter.next();
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#get(int)
	 */
	@SuppressWarnings("unchecked")
	public E get(int index) {
		if(index < 0 || index >= array.length) {
			throw new IndexOutOfBoundsException("" + index);
		}
		return (E)array[index];
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#size()
	 */
	public int size() {
		return array.length;
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, E element) {
		throw new UnsupportedOperationException();
	}
	
	/* (non-Javadoc)
	 * @see java.util.Collection#add(java.lang.Object)
	 */
	public boolean add(E o) {
		throw new UnsupportedOperationException();
	}
	
	/* (non-Javadoc)
	 * @see java.util.List#addAll(int, java.util.Collection)
	 */
	public boolean addAll(int index, Collection<? extends E> c) {
		throw new UnsupportedOperationException();
	}
	
	/* (non-Javadoc)
	 * @see java.util.Collection#clear()
	 */
	public void clear() {
		throw new UnsupportedOperationException();
	}
	
	/* (non-Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public E remove(int index) {
		throw new UnsupportedOperationException();
	}
	
	/* (non-Javadoc)
	 * @see java.util.AbstractList#removeRange(int, int)
	 */
	protected void removeRange(int fromIndex, int toIndex) {
		throw new UnsupportedOperationException();
	}
	
	/* (non-Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public E set(int index, Object element) {
		if(index < 0 || index >= size()) {
			throw new IndexOutOfBoundsException("" + index);
		}
		
		E res = (E)array[index];
		array[index] = element;
		return res;
	}
	
}
