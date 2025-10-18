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

import java.util.AbstractSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/29
 */
public abstract class BooleanAlgebricJavaSet<E>
extends AbstractSet<E>
implements BooleanAlgebricSet<E, BooleanAlgebricJavaSet<E>> {
	
	//
//	private static final long serialVersionUID = -7383065471384117297L;
	
	//
	private final UniversalJavaSet<E> uni;
	
	
	/*package*/ BooleanAlgebricJavaSet(UniversalJavaSet<E> uni) {
		this.uni = uni;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.set2.BooleanAlgebricSet#universalSet()
	 */
	@Override
	public final UniversalJavaSet<E> universalSet() {
		return uni;
	}

}
