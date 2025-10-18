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
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

import net.morilib.util.Objects;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/21
 */
public class NestedSet<E> extends AbstractSet<E> {
	
	//
	private Set<E> composite;
	private E elem;
	
	//
	private NestedSet(E elem, Set<E> composite) {
		this.elem      = elem;
		this.composite = composite;
	}
	
	/**
	 * 
	 * @param elem
	 * @param composite
	 * @return
	 */
	public static<E> Set<E> compose(E elem, Set<E> composite) {
		return composite.contains(elem) ?
				composite : new NestedSet<E>(elem, composite);
	}
	
	/*
	 * (non-Javadoc)
	 * @see java.util.AbstractCollection#contains(java.lang.Object)
	 */
	@Override
	public boolean contains(Object o) {
		if(Objects.equals(elem, o)) {
			return true;
		} else if(composite != null) {
			return composite.contains(o);
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#iterator()
	 */
	@Override
	public Iterator<E> iterator() {
		return new Iterator<E>() {
			
			//
			private Set<E> c = NestedSet.this;
			private Iterator<E> it = null;
			
			public boolean hasNext() {
				return (c instanceof NestedSet || 
						(it != null && it.hasNext()));
			}

			public E next() {
				if(!hasNext()) {
					throw new NoSuchElementException();
				} else if(c instanceof NestedSet) {
					E res = ((NestedSet<E>)c).elem;
					
					c = ((NestedSet<E>)c).composite;
					if(!(c instanceof NestedSet)) {
						it = c.iterator();
					}
					return res;
				} else {
					return it.next();
				}
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#size()
	 */
	@Override
	public int size() {
		Set<E> c = this;
		int res = 0;
		
		while(c instanceof NestedSet) {
			res++;
			c = ((NestedSet<E>)c).composite;
		}
		return res + c.size();
	}

}
