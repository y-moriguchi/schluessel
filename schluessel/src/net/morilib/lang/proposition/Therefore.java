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

import net.morilib.lang.composite.Traverse;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/05
 */
public final class Therefore<S extends Proposition<S>>
implements Proposition<S> {
	
	//
	@Traverse
	/*package*/ Proposition<S> assumption, conclusion;
	
	//
	/*package*/ Therefore(Proposition<S> a, Proposition<S> c) {
		this.assumption = a;
		this.conclusion = c;
	}
	
	/**
	 * 
	 * @param assumption
	 * @param conclusion
	 * @return
	 */
	public static<S extends Proposition<S>>
	Proposition<S> newInstance(
			Proposition<S> assumption,
			Proposition<S> conclusion) {
		return new Therefore<S>(assumption, conclusion);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is(java.lang.Object[])
	 */
	@Override
	public boolean is(Object... variables) {
		return assumption.is(variables) ?
				conclusion.is(variables) : true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is1(java.lang.Object)
	 */
	@Override
	public boolean is1(Object var1) {
		return assumption.is(var1) ? conclusion.is(var1) : true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is2(java.lang.Object, java.lang.Object)
	 */
	@Override
	public boolean is2(Object var1, Object var2) {
		return assumption.is(var1, var2) ?
				conclusion.is(var1, var2) : true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isEquivalent(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isEqualTo(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
		if(p instanceof Therefore) {
			Therefore<S> t = (Therefore<S>)p;
			
			return (assumption.isEqualTo(t.assumption) &&
					conclusion.isEqualTo(t.conclusion));
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isTrueIf(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean implies(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
		return (assumption.isIndependent(p) ||
				(assumption.implies(p) && conclusion.implies(p)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isImplied(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isImplied(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
		return isIndependent(LogicalNot.newInstance(p)) ;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isIndependent(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isIndependent(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
		return assumption.implies(p) && conclusion.isIndependent(p);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isFalse()
	 */
	@Override
	public boolean isFalse() {
		return assumption.isTrue() && conclusion.isFalse();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isTrue()
	 */
	@Override
	public boolean isTrue() {
		return assumption.isFalse() || conclusion.isTrue();
	}
	
	/**
	 * 
	 */
	public String toString() {
		return ("(" + assumption.toString() + " -> " +
				conclusion.toString() + ")");
	}

}
