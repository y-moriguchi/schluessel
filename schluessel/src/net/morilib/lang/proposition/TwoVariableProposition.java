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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/05
 */
public abstract class
TwoVariableProposition<S extends Proposition<S>>
implements Proposition<S> {
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is(java.lang.Object[])
	 */
	@Override
	public final boolean is(Object... variables) {
		if(variables == null) {
			throw new NullPointerException();
		} else if(variables.length < 2) {
			throw new IllegalArgumentException();
		}
		return is2(variables[0], variables[1]);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is2(java.lang.Object, java.lang.Object)
	 */
	@Override
	public final boolean is1(Object var1) {
		throw new IllegalArgumentException();
	}

}
