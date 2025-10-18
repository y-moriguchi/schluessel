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
package net.morilib.lang.number;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/30
 */
public abstract class AbstractNumericalRing
<C extends NumericalRingElement<C>>
implements NumericalRing<C> {

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Ring#add(net.morilib.lang.algebra.RingElement, net.morilib.lang.algebra.RingElement)
	 */
	public C add(C x, C y) {
		return x.add(y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Ring#subtract(net.morilib.lang.algebra.RingElement, net.morilib.lang.algebra.RingElement)
	 */
	public C subtract(C x, C y) {
		return x.subtract(y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Ring#multiply(net.morilib.lang.algebra.RingElement, net.morilib.lang.algebra.RingElement)
	 */
	public C multiply(C x, C y) {
		return x.multiply(y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Ring#negate(net.morilib.lang.algebra.RingElement)
	 */
	public C negate(C x) {
		return x.negate();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(byte)
	 */
	public C valueOf(byte v) {
		return valueOf((int)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRing#valueOf(short)
	 */
	public C valueOf(short v) {
		return valueOf((int)v);
	}

}
