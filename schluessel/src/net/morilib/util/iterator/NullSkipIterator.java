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

import java.util.Iterator;
import java.util.NoSuchElementException;

import net.morilib.util.Iterators;

/**
 * This is a wrapper iterator for skipping nulls.
 * <p>nullをスキップするラッパーiteratorである.
 * ラップしたIteratorの現在の要素がnullのときは、
 * その要素をスキップする.
 * 
 * @author MORIGUCHI, Yuichiro 2005/05/01
 */
public final class NullSkipIterator<E> implements Iterator<E> {
	
	//
	private Iterator<E> skipiter;
	private E          _next;
	
	/**
	 * creates a new NullSkipIterator.
	 * <p>NullSkipIteratorを生成する.
	 * 
	 * @param iter  an iterator to be wrapped
	 */
	public NullSkipIterator(Iterator<E> iter) {
		skipiter = iter;
		_next = Iterators.nextNotNull(iter);
	}

	/**
	 * removes the element which is pointed by this iterator.
	 * <p>Iteratorが指し示す要素を削除する.
	 * このメソッドはサポートされていない.
	 * 
	 * @see java.util.Iterator#remove()
	 * @throws UnsupportedOperationException
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}

	/**
	 * returns true if this iterator has a next element.
	 * <p>Iteratorが次の要素を持つときにtrueを得る.
	 * 
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		return _next != null;
	}

	/**
	 * gets the value pointed by this iterator and forwards the pointer of
	 * this.
	 * <p>Iteratorにより示されている値を得て、ポインタを進める.
	 * 
	 * @see java.util.Iterator#next()
	 */
	public E next() {
		if(_next == null) {
			throw new NoSuchElementException();
		}
		
		E res = _next;
		_next = Iterators.nextNotNull(skipiter);
		return res;
	}

}
