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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/29
 */
/*package*/ final class BooleanAlgebricJavaOneSet<E>
extends BooleanAlgebricJavaSet<E> implements java.io.Serializable {
	
	//
	private static final long serialVersionUID = 4078708809239260690L;
	
	
	/*package*/ BooleanAlgebricJavaOneSet(UniversalJavaSet<E> uni) {
		super(uni);
	}
	
	/* (non-Javadoc)
	 * @see java.util.Set#size()
	 */
	@Override
	public int size() {
		return universalSet().size();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return universalSet().isEmpty();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#contains(java.lang.Object)
	 */
	@Override
	public boolean contains(Object o) {
		return universalSet().contains(o);
	}

	/* (non-Javadoc)
	 * @see java.util.Set#iterator()
	 */
	@Override
	public Iterator<E> iterator() {
		return universalSet().iterator();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#toArray()
	 */
	@Override
	public Object[] toArray() {
		return universalSet().toArray();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#toArray(T[])
	 */
	@Override
	public <T> T[] toArray(T[] a) {
		return universalSet().toArray(a);
	}

	/* (non-Javadoc)
	 * @see java.util.Set#add(java.lang.Object)
	 */
	@Override
	public boolean add(E e) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#remove(java.lang.Object)
	 */
	@Override
	public boolean remove(Object o) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#containsAll(java.util.Collection)
	 */
	@Override
	public boolean containsAll(Collection<?> c) {
		return universalSet().containsAll(c);
	}

	/* (non-Javadoc)
	 * @see java.util.Set#addAll(java.util.Collection)
	 */
	@Override
	public boolean addAll(Collection<? extends E> c) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#retainAll(java.util.Collection)
	 */
	@Override
	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#removeAll(java.util.Collection)
	 */
	@Override
	public boolean removeAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see java.util.Set#clear()
	 */
	@Override
	public void clear() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#join(net.morilib.lang.algebra.BooleanElement)
	 */
	@Override
	public BooleanAlgebricJavaSet<E> join(BooleanAlgebricJavaSet<E> x) {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#meet(net.morilib.lang.algebra.BooleanElement)
	 */
	@Override
	public BooleanAlgebricJavaSet<E> meet(BooleanAlgebricJavaSet<E> x) {
		return x;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#complement()
	 */
	@Override
	public BooleanAlgebricJavaSet<E> complement() {
		return universalSet().get0();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#is0()
	 */
	@Override
	public boolean is0() {
		return universalSet().isEmpty();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#is1()
	 */
	@Override
	public boolean is1() {
		return true;
	}

}
