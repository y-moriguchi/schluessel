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
package net.morilib.util.set;

import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

import net.morilib.util.iterator.CascadedIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/05
 */
public abstract class CachedSet<E> extends AbstractSet<E> {
	
	//
	/*package*/ Set<E> second = null;
	
	/**
	 * 
	 * @return
	 */
	protected abstract Set<E> createSet();
	
	/**
	 * 
	 * @return
	 */
	protected abstract Iterator<E> cacheIterator();
	
	/**
	 * 
	 * @return
	 */
	protected abstract int cacheSize();
	
	/**
	 * 
	 * @return
	 */
	protected abstract boolean isCacheFull();
	
	/**
	 * 
	 * @param e
	 * @return
	 */
	protected abstract int hitCache(Object e);
	
	/**
	 * 
	 * @param i
	 */
	protected abstract E getCache(int i);
	
	/**
	 * 
	 * @param e
	 */
	protected abstract void addCache(E e);
	
	/**
	 * 
	 * @param i
	 */
	protected abstract void removeCache(int i);
	
	/**
	 * 
	 * @param i
	 * @param e
	 */
	protected abstract void setCache(int i, E e);
	
	/**
	 * 
	 */
	protected abstract void clearCache();
	
	/**
	 * 
	 * @param cacheSize
	 */
	protected CachedSet() { }
	
	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#iterator()
	 */
	@Override
	public Iterator<E> iterator() {
		if(second != null) {
			return new CascadedIterator<E>(
					cacheIterator(), second.iterator());
		} else {
			return cacheIterator();
		}
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#size()
	 */
	@Override
	public int size() {
		return cacheSize() + second.size();
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return cacheSize() == 0 && second.isEmpty();
	}

	//
	private boolean containsWithoutCache(Object o) {
		return hitCache(o) >= 0 || second.contains(o);
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#contains(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public boolean contains(Object o) {
		int ch = hitCache(o);
		
		if(ch >= 0) {
			return true;
		} else if(second.contains(o)) {
			second.remove(o);
			second.add(getCache(0));
			setCache(0, (E)o);
			return true;
		} else {
			return false;
		}
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		for(Object e : c) {
			if(!containsWithoutCache(e)) {
				return false;
			}
		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#add(java.lang.Object)
	 */
	@Override
	public boolean add(E e) {
		if(hitCache(e) >= 0) {
			return false;
		} else if(isCacheFull()) {
			if(second == null) {
				second = createSet();
			}
			return second.add(e);
		} else {
			addCache(e);
			return true;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#remove(java.lang.Object)
	 */
	@Override
	public boolean remove(Object o) {
		int ch = hitCache(o);
		
		if(ch >= 0) {
			removeCache(ch);
			return true;
		} else if(second != null) {
			return second.remove(o);
		} else {
			return false;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#clear()
	 */
	@Override
	public void clear() {
		clearCache();
		if(second != null) {
			second.clear();
		}
	}

}
