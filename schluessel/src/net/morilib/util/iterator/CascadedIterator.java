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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Implementation of the iterator which cascades
 * iterators of some collections.
 * <p>いくつかのコレクションのIteratorを連結したIteratorの実装である.
 * 
 * @author MORIGUCHI, Yuichiro 2005/01/01
 */
public final class CascadedIterator<E> implements Iterator<E> {
	
	//
	private List<Iterator<E>> iterators;
	private int ptr = 0;
	
	/**
	 * constructs an iterator which cascades all elements of
	 * the given array.
	 * <p>配列にある全ての要素を連結したIteratorを生成する.
	 * 連結の順番は配列の順番と同一である.
	 * 
	 * @param cols  an array of iterators
	 */
	public CascadedIterator(Collection<Iterator<E>> cols) {
		iterators = new ArrayList<Iterator<E>>();
		for(Iterator<E> i : cols) {
			iterators.add(i);
		}
	}
	
	/**
	 * @param cacheIterator
	 * @param iterator
	 */
	public CascadedIterator(Iterator<E> iter1, Iterator<E> iter2) {
		iterators = new ArrayList<Iterator<E>>();
		iterators.add(iter1);
		iterators.add(iter2);
	}

	/**
	 * removes the element which is pointed by this iterator.
	 * <p>Iteratorが指し示す要素を削除する.
	 * 
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
		if(ptr >= iterators.size()) {
			throw new NoSuchElementException();
		}
		iterators.get(ptr).remove();
		//hasNext();   // skip
	}

	/**
	 * returns true if this iterator has a next element.
	 * <p>Iteratorが次の要素を持つときにtrueを得る.
	 * 
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		while(ptr < iterators.size() && !iterators.get(ptr).hasNext()) {
			ptr++;
		}
		return ptr < iterators.size();
	}

	/**
	 * gets the value pointed by this iterator and forwards the pointer of
	 * this.
	 * <p>Iteratorにより示されている値を得て、ポインタを進める.
	 * 
	 * @see java.util.Iterator#next()
	 */
	public E next() {
		if(!hasNext()) {
			throw new NoSuchElementException();
		}
		return iterators.get(ptr).next();
	}

}
