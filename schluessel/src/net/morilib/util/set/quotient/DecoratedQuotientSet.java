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
package net.morilib.util.set.quotient;

import java.util.Collection;

import net.morilib.lang.Equivalence;
import net.morilib.util.SimpleMap;
import net.morilib.util.collection.MappedCollection;
import net.morilib.util.datafactory.SetFactory;
import net.morilib.util.set.AbstractDecoratedSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/24
 */
public class DecoratedQuotientSet<E, Q>
extends AbstractDecoratedSet<Q> implements QuotientSet<E, Q> {

	/**
	 * 
	 */
	protected Equivalence<E, Q> equivalence;
	
	/**
	 * @param towrap
	 */
	protected DecoratedQuotientSet(
			SetFactory factory,
			Equivalence<E, Q> equivalence) {
		super(factory.<Q>newInstance());
		this.equivalence = equivalence;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.quotient.QuotientSet#containsClass(java.lang.Object)
	 */
	public boolean containsClass(E o) {
		return contains(equivalence.classify(o));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.quotient.QuotientSet#addClass(java.lang.Object)
	 */
	public boolean addClass(E e) {
		return add(equivalence.classify(e));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.quotient.QuotientSet#containsAllClass(java.util.Collection)
	 */
	public boolean containsAllClass(Collection<? extends E> c) {
		return containsAll(new MappedCollection<E, Object>(c,
				new SimpleMap<E, Object>() {

					public Object map(E e) {
						return equivalence.classify(e);
					}
			
		}));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set.quotient.QuotientSet#addAllClass(java.util.Collection)
	 */
	public boolean addAllClass(Collection<? extends E> c) {
		return addAll(new MappedCollection<E, Q>(c,
				new SimpleMap<E, Q>() {

					public Q map(E e) {
						return equivalence.classify(e);
					}
			
		}));
	}

}
