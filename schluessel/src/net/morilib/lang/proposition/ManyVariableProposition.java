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

import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/05
 */
public abstract class
ManyVariableProposition<S extends Proposition<S>>
implements Proposition<S> {
	
	//
	/*package*/ static<S extends Proposition<S>>
	boolean isEquivalent(
			List<Proposition<S>> l1, List<Proposition<S>> l2) {
		if(l1.size() == l2.size()) {
			for(int i = 0; i < l1.size(); i++) {
				if(!l1.get(i).isEqualTo(l2.get(i))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is1(java.lang.Object)
	 */
	@Override
	public final boolean is1(Object var1) {
		return is(var1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.proposition.Proposition#is2(java.lang.Object, java.lang.Object)
	 */
	@Override
	public final boolean is2(Object var1, Object var2) {
		return is(var1, var2);
	}

}
