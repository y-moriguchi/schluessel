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

import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

/**
 * An implementation of Set which does nothing for update.
 * <p>更新について何もしないSetの実装です。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public final class NullSet<E> extends AbstractSet<E> {
	
	private NullSet() {
		// do nothing
	}
	
	private static final Set<Object> NULL = new NullSet<Object>();
	
	/**
	 * gets an instance of NullIterator.
	 * <p>NullIteratorを取得します。
	 */
	@SuppressWarnings("unchecked")
	public static <T> Set<T> getInstance() {
		return (NullSet<T>)NULL;
	}
	
	/**
	 * does nothing.
	 * <p>何もしません。
	 * 
	 * @return always returns false
	 * @see java.util.AbstractCollection#add(java.lang.Object)
	 */
	public boolean add(E o) {
		return false;
	}

	/**
	 * does nothing.
	 * <p>何もしません。
	 * 
	 * @return always returns false
	 * @see java.util.AbstractCollection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends E> c) {
		return false;
	}

	/**
	 * does nothing.
	 * <p>何もしません。
	 * 
	 * @see java.util.AbstractCollection#clear()
	 */
	public void clear() {
		// do nothing
	}

	/**
	 * always returns false.
	 * <p>常にfalseを返します。
	 * 
	 * @see java.util.AbstractCollection#contains(java.lang.Object)
	 */
	public boolean contains(Object o) {
		return false;
	}

	/**
	 * always returns false.
	 * <p>常にfalseを返します。
	 * 
	 * @see java.util.AbstractCollection#containsAll(java.util.Collection)
	 */
	public boolean containsAll(Collection<?> c) {
		return false;
	}

	/**
	 * always returns true.
	 * <p>常にtrueを返します。
	 * 
	 * @see java.util.AbstractCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return true;
	}

	/**
	 * always returns this instance.
	 * <p>常にこのインスタンスを返します。
	 * 
	 * @see java.util.AbstractCollection#iterator()
	 */
	public Iterator<E> iterator() {
		return NullIterator.getInstance();
	}

	/**
	 * does nothing.
	 * <p>何もしません。
	 * 
	 * @return always returns false
	 * @see java.util.AbstractCollection#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		return false;
	}

	/**
	 * does nothing.
	 * <p>何もしません。
	 * 
	 * @return always returns false
	 * @see java.util.AbstractSet#removeAll(java.util.Collection)
	 */
	public boolean removeAll(Collection<?> c) {
		return false;
	}

	/**
	 * does nothing.
	 * <p>何もしません。
	 * 
	 * @return always returns false
	 * @see java.util.AbstractCollection#retainAll(java.util.Collection)
	 */
	public boolean retainAll(Collection<?> c) {
		return false;
	}

	/**
	 * always returns 0.
	 * <p>常に0を返します。
	 * 
	 * @see java.util.AbstractCollection#size()
	 */
	public int size() {
		return 0;
	}

}
