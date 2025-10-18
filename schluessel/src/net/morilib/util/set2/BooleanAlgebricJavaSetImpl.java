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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/29
 */
/*package*/ class BooleanAlgebricJavaSetImpl<E>
extends BooleanAlgebricJavaSet<E> {
	
	//
	private Set<E> wrapee;
	
	/**
	 * @param uni
	 */
	/*package*/ BooleanAlgebricJavaSetImpl(
			UniversalJavaSet<E> uni, Set<E> c) {
		super(uni);
		this.wrapee = c;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#iterator()
	 */
	@Override
	public Iterator<E> iterator() {
		return wrapee.iterator();
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#size()
	 */
	@Override
	public int size() {
		return wrapee.size();
	}

	
	@Override
	public boolean add(E e) {
		if(!universalSet().contains(e)) {
			throw new InvalidElementException();
		}
		return wrapee.add(e);
	}

	
	@Override
	public boolean addAll(Collection<? extends E> c) {
		if(!universalSet().containsAll(c)) {
			throw new InvalidElementException();
		}
		return wrapee.addAll(c);
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		return wrapee.removeAll(c);
	}

	@Override
	public boolean isEmpty() {
		return wrapee.isEmpty();
	}

	@Override
	public boolean contains(Object o) {
		return wrapee.contains(o);
	}

	@Override
	public boolean remove(Object o) {
		return wrapee.remove(o);
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		return wrapee.containsAll(c);
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		return wrapee.retainAll(c);
	}

	@Override
	public void clear() {
		wrapee.clear();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#join(net.morilib.lang.algebra.BooleanElement)
	 */
	@Override
	public BooleanAlgebricJavaSet<E> join(BooleanAlgebricJavaSet<E> x) {
		Set<E> s = universalSet().getFactory().clone(x);
		
		s.addAll(x);
		return new BooleanAlgebricJavaSetImpl<E>(universalSet(), s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#meet(net.morilib.lang.algebra.BooleanElement)
	 */
	@Override
	public BooleanAlgebricJavaSet<E> meet(BooleanAlgebricJavaSet<E> x) {
		Set<E> s = universalSet().getFactory().clone(x);
		
		s.retainAll(x);
		return new BooleanAlgebricJavaSetImpl<E>(universalSet(), s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#complement()
	 */
	@Override
	public BooleanAlgebricJavaSet<E> complement() {
		Set<E> s = universalSet().getFactory().clone(universalSet());
		
		s.removeAll(this);
		return new BooleanAlgebricJavaSetImpl<E>(universalSet(), s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#is0()
	 */
	@Override
	public boolean is0() {
		return isEmpty();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#is1()
	 */
	@Override
	public boolean is1() {
		return equals(universalSet());
	}

}
