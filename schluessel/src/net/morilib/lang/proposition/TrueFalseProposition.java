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

public class TrueFalseProposition
implements Proposition<TrueFalseProposition> {
	
	//
	private boolean value;
	
	//
	private TrueFalseProposition(boolean value) {
		this.value = value;
	}
	
	/**
	 * 
	 */
	public static final TrueFalseProposition TRUE =
		new TrueFalseProposition(true);
	
	/**
	 * 
	 */
	public static final TrueFalseProposition FALSE =
		new TrueFalseProposition(false);
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is(java.lang.Object[])
	 */
	@Override
	public boolean is(Object... variables) {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is1(java.lang.Object)
	 */
	@Override
	public boolean is1(Object var1) {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is2(java.lang.Object, java.lang.Object)
	 */
	@Override
	public boolean is2(Object var1, Object var2) {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#implies(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean implies(Proposition<TrueFalseProposition> p) {
		return value && p.isFalse();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isImplied(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isImplied(Proposition<TrueFalseProposition> p) {
		return !value && p.isTrue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isIndependent(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isIndependent(Proposition<TrueFalseProposition> p) {
		return !value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isEqualTo(net.morilib.lang.proposition.Proposition)
	 */
	@Override
	public boolean isEqualTo(Proposition<TrueFalseProposition> p) {
		return (value && p.isTrue()) || (!value && p.isFalse());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isFalse()
	 */
	@Override
	public boolean isFalse() {
		return !value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#isTrue()
	 */
	@Override
	public boolean isTrue() {
		return value;
	}
	
}