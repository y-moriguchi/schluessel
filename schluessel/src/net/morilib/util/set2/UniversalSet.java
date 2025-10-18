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
package net.morilib.util.set2;

import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

import net.morilib.lang.algebra.AbstractBooleanAlgebra;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/29
 */
public abstract class UniversalSet
<E, S extends BooleanAlgebricSet<E, S>>
extends AbstractBooleanAlgebra<S> implements Set<E> {
	
	
	protected abstract Set<E> getUniversalSet();
	
	/* (non-Javadoc)
	 * @see java.util.Set#size()
	 */
	@Override
	public final int size() {
		return getUniversalSet().size();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#isEmpty()
	 */
	@Override
	public final boolean isEmpty() {
		return getUniversalSet().isEmpty();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#contains(java.lang.Object)
	 */
	@Override
	public final boolean contains(Object o) {
		return getUniversalSet().contains(o);
	}

	/* (non-Javadoc)
	 * @see java.util.Set#iterator()
	 */
	@Override
	public final Iterator<E> iterator() {
		return Iterators.unmodifiable(getUniversalSet().iterator());
	}

	/* (non-Javadoc)
	 * @see java.util.Set#toArray()
	 */
	@Override
	public final Object[] toArray() {
		return getUniversalSet().toArray();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#toArray(T[])
	 */
	@Override
	public final <T> T[] toArray(T[] a) {
		return getUniversalSet().toArray(a);
	}

	/* (non-Javadoc)
	 * @see java.util.Set#add(java.lang.Object)
	 */
	@Override
	public final boolean add(E e) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#remove(java.lang.Object)
	 */
	@Override
	public final boolean remove(Object o) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#containsAll(java.util.Collection)
	 */
	@Override
	public final boolean containsAll(Collection<?> c) {
		return getUniversalSet().containsAll(c);
	}

	/* (non-Javadoc)
	 * @see java.util.Set#addAll(java.util.Collection)
	 */
	@Override
	public final boolean addAll(Collection<? extends E> c) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#retainAll(java.util.Collection)
	 */
	@Override
	public final boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#removeAll(java.util.Collection)
	 */
	@Override
	public final boolean removeAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#clear()
	 */
	@Override
	public final void clear() {
		throw new UnsupportedOperationException();
	}
	
}
