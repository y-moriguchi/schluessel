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
 * @author MORIGUCHI, Yuichiro 2011/01/01
 */
public class SummedIterator
<E extends Comparable<E>> implements Iterator<E> {

	//
	private Iterator<E> iterator1, iterator2;
	private E val1, val2;
	
	/**
	 * 
	 * @param iterator1
	 * @param iterator2
	 */
	public SummedIterator(
			Iterator<E> iterator1, Iterator<E> iterator2) {
		this.iterator1 = iterator1;
		this.iterator2 = iterator2;
		val1 = iterator1.hasNext() ? iterator1.next() : null;
		val2 = iterator2.hasNext() ? iterator2.next() : null;
	}
	
	//
	private int compareTo(E val1, E val2) {
		if(val1 == null) {
			return 1;
		} else if(val2 == null) {
			return -1;
		} else {
			return val1.compareTo(val2);
		}
	}
	
	/* (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		return val1 != null || val2 != null;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	public E next() {
		E res;
		
		if(compareTo(val1, val2) < 0) {
			res = val1;
			val1 = iterator1.hasNext() ? iterator1.next() : null;
		} else if(compareTo(val1, val2) > 0) {
			res = val2;
			val2 = iterator2.hasNext() ? iterator2.next() : null;
		} else {
			res = val1;
			val1 = iterator1.hasNext() ? iterator1.next() : null;
			val2 = iterator2.hasNext() ? iterator2.next() : null;
		}
		return res;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
		throw new IllegalArgumentException();
	}

}
