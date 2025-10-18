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
package net.morilib.lang.algebra;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/28
 */
public abstract class AbstractBooleanAlgebra
<A extends BooleanElement<A>>
implements BooleanAlgebra<A> {

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#join(net.morilib.lang.algebra.BooleanElement, net.morilib.lang.algebra.BooleanElement)
	 */
	public A join(A x, A y) {
		return (x != null && y != null) ? x.join(y) : null;
	}

	/*
	 * @see net.morilib.lang.algebra.BooleanAlgebra#join(A[])
	 */
	public A join(A... as) {
		A res;
		
		if(as == null) {
			throw new NullPointerException();
		} else if(as.length == 0) {
			return get1();
		}
		
		res = as[0];
		for(int i = 1; i < as.length; i++) {
			A a = as[i];
			
			if(a == null) {
				return null;
			} else if(a.is1()) {
				return get1();
			}
			res = res.join(a);
		}
		return res;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#meet(net.morilib.lang.algebra.BooleanElement, net.morilib.lang.algebra.BooleanElement)
	 */
	public A meet(A x, A y) {
		return (x != null && y != null) ? x.meet(y) : null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#meet(A[])
	 */
	public A meet(A... as) {
		A res;
		
		if(as == null) {
			throw new NullPointerException();
		} else if(as.length == 0) {
			return get0();
		}
		
		res = as[0];
		for(int i = 1; i < as.length; i++) {
			A a = as[i];
			
			if(a == null) {
				return null;
			} else if(a.is0()) {
				return get0();
			}
			res = res.meet(a);
		}
		return res;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#complement(net.morilib.lang.algebra.BooleanElement)
	 */
	public A complement(A x) {
		return (x != null) ? x.complement() : null;
	}
	
}
