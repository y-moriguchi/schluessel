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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/03
 */
public class DecoratedIterator<E> implements Iterator<E> {
	
	/**
	 * 
	 */
	protected Iterator<E> wrapee;
	
	//
	private E currentValue;
	
	/**
	 * 
	 * @param w
	 */
	public DecoratedIterator(Iterator<E> w) {
		this.wrapee = w;
	}
	
	/* (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		return wrapee.hasNext();
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	public E next() {
		return (currentValue = next());
	}
	
	/**
	 * 
	 * @param value
	 */
	protected void removeElement(Object value) {
		wrapee.remove();
	}
	
	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	public final void remove() {
		if(currentValue == null) {
			throw new IllegalStateException();
		}
		removeElement(currentValue);
	}

}
