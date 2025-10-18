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
package net.morilib.lisp.math;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.math.algebra.ILispMultipliable;
import net.morilib.math.ArrayIntSymmetricGroup;
import net.morilib.math.IntPermutation;
import net.morilib.math.IntSymmetricGroup;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/24
 */
public class LispPermutation extends Datum2
implements ILispMultipliable<LispPermutation> {

	//
	/*package*/ IntPermutation perm;

	//
	/*package*/ LispPermutation(IntPermutation perm) {
		this.perm = perm;
	}

	/**
	 * 
	 * @param is
	 */
	public LispPermutation(int... is) {
		IntSymmetricGroup group;

		group = ArrayIntSymmetricGroup.getInstance(is.length);
		perm  = group.newElement(is);
	}

	/**
	 * 
	 * @param i
	 * @return
	 */
	public int get(int i) {
		return perm.get(i);
	}

	/**
	 * 
	 * @return
	 */
	public LispPermutation invert() {
		return new LispPermutation(perm.invert());
	}

	/**
	 * @return
	 */
	public int size() {
		return perm.getCardinal();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public LispPermutation mul(LispPermutation y) {
		return new LispPermutation(perm.map(y.perm));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<permutation");
		for(int i = 0; i < perm.getCardinal(); i++) {
			buf.append(" ").append(perm.get(i));
		}
		buf.append(">");
	}

}
