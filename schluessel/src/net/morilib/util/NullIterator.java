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

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * An iterator which has no elements.
 * <p>要素をもたないIteratorです。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public final class NullIterator<E> implements Iterator<E> {
	
	private NullIterator() {
		// do nothing
	}
	
	private static final NullIterator<Object> NULL =
		new NullIterator<Object>();
	
	/**
	 * gets a NullIterator.
	 * <p>NullIteratorを取得します。
	 */
	@SuppressWarnings("unchecked")
	public static <T> NullIterator<T> getInstance() {
		return (NullIterator<T>)NULL;
	}
	
	/**
	 * always returns false.
	 * <p>常にfalseを返します。
	 * 
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		return false;
	}

	/**
	 * always throws NoSuchElementException.
	 * <p>常にNoSuchElementExceptionをスローします。
	 * 
	 * @see java.util.Iterator#next()
	 */
	public E next() {
		throw new NoSuchElementException();
	}

	/**
	 * does nothing.
	 * <p>何もしません。
	 * 
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
		// do nothing
	}

}
