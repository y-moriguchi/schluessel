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
package net.morilib.lang.proposition;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/05
 */
public class SetContains
extends OneVariableProposition<SetContains> {
	
	/**
	 * 
	 */
	public static final Proposition<SetContains> EMPTY =
		new SetContains(false, Collections.emptySet());
	
	/**
	 * 
	 */
	public static final Proposition<SetContains> UNIVERSE =
		LogicalNot.newInstance(EMPTY);
	
	//
	private Set<Object> elements;
	
	//
	private SetContains(boolean dummy, Set<Object> elements) {
		this.elements = elements;
	}
	
	/**
	 * 
	 * @return
	 */
	public static Proposition<SetContains> getEmpty() {
		return (Proposition<SetContains>)EMPTY;
	}
	
	/**
	 * 
	 * @param elements
	 */
	public SetContains(Collection<Object> elements) {
		this.elements = new HashSet<Object>(elements);
	}
	
	/**
	 * 
	 * @param ts
	 */
	public SetContains(Object... ts) {
		this(Arrays.asList(ts));
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is1(java.lang.Object)
	 */
	@Override
	public boolean is1(Object var1) {
		return elements.contains(var1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#implies(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean implies(Proposition<SetContains> p) {
		if(p instanceof SetContains) {
			SetContains s = (SetContains)p;
			
			return elements.containsAll(s.elements);
		}
		return p.isImplied(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isImplied(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isImplied(Proposition<SetContains> p) {
		if(p instanceof SetContains) {
			SetContains s = (SetContains)p;
			
			return s.elements.containsAll(elements);
		}
		return p.implies(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isIndependent(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isIndependent(Proposition<SetContains> p) {
		for(Object e : elements) {
			if(p.is1(e)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isEquivalent(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isEqualTo(Proposition<SetContains> p) {
		if(p instanceof SetContains) {
			SetContains s = (SetContains)p;
			
			return elements.equals(s.elements);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isFalse()
	 */
	@Override
	public boolean isFalse() {
		return elements.isEmpty();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isTrue()
	 */
	@Override
	public boolean isTrue() {
		return false;
	}
	
	/**
	 * 
	 */
	public String toString() {
		return elements.toString();
	}

}
