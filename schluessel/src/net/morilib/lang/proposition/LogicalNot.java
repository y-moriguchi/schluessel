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
public final class LogicalNot<S extends Proposition<S>>
implements Proposition<S> {
	
	//
	@Traverse
	/*package*/ Proposition<S> proposition1;
	
	//
	/*package*/ LogicalNot(Proposition<S> a) {
		this.proposition1 = a;
	}
	
	/**
	 * 
	 * @param <Object>
	 * @param <S>
	 * @param proposition
	 * @return
	 */
	public static<S extends Proposition<S>>
	Proposition<S> newInstance(
			Proposition<S> proposition) {
		if(proposition instanceof LogicalAnd) {
			return ((LogicalAnd<S>)proposition).negation;
		} else if(proposition instanceof LogicalOr) {
			return ((LogicalOr<S>)proposition).negation;
		} else if(proposition instanceof LogicalNot) {
			return ((LogicalNot<S>)proposition).proposition1;
		}
		return new LogicalNot<S>(proposition);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is(java.lang.Object[])
	 */
	@Override
	public boolean is(Object... variables) {
		return !proposition1.is(variables);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is1(java.lang.Object)
	 */
	@Override
	public boolean is1(Object var1) {
		return !proposition1.is(var1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is2(java.lang.Object, java.lang.Object)
	 */
	@Override
	public boolean is2(Object var1, Object var2) {
		return !proposition1.is(var1, var2);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isEquivalent(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isEqualTo(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
		if(p instanceof LogicalNot) {
			return proposition1.isEqualTo(
					((LogicalNot<S>)p).proposition1);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#implies(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean implies(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
		return proposition1.isIndependent(p);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isImplied(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isImplied(Proposition<S> p) {
//		if(p == null) {
//			throw new NullPointerException();
//		} else if(p instanceof LogicalNot) {
//			return proposition1.implies(
//					((LogicalNot<S>)p).proposition1);
//		}
		return newInstance(p).implies(proposition1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isIndependent(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isIndependent(Proposition<S> p) {
		if(p == null) {
			throw new NullPointerException();
		}
		return proposition1.implies(p);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isFalse()
	 */
	@Override
	public boolean isFalse() {
		return proposition1.isTrue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isTrue()
	 */
	@Override
	public boolean isTrue() {
		return proposition1.isFalse();
	}
	
	/**
	 * 
	 */
	public String toString() {
		return "(not " + proposition1.toString() + ")";
	}
	
}
