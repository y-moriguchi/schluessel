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
package net.morilib.util.collection;

import java.util.AbstractCollection;
import java.util.Collection;
import java.util.Iterator;

import net.morilib.util.Objects;
import net.morilib.util.SimpleMap;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/15
 */
public class MappedCollection<F, E> extends AbstractCollection<E> {
	
	//
	private Collection<? extends F> wrapee;
	private SimpleMap<F, E> composite;

	/**
	 * @param map
	 */
	public MappedCollection(
			Collection<? extends F> col, SimpleMap<F, E> map) {
		this.wrapee    = col;
		this.composite = map;
	}

	/* (non-Javadoc)
	 * @see java.util.Set#size()
	 */
	@Override
	public int size() {
		return wrapee.size();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return wrapee.isEmpty();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#contains(java.lang.Object)
	 */
	@Override
	public boolean contains(Object o) {
		for(F f : wrapee) {
			if(Objects.equals(composite.map(f), o)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Set#iterator()
	 */
	@Override
	public Iterator<E> iterator() {
		return new Iterator<E>() {
			
			//
			private Iterator<? extends F> wrp = wrapee.iterator();

			public boolean hasNext() {
				return wrp.hasNext();
			}

			public E next() {
				return composite.map(wrp.next());
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		};
	}

}
