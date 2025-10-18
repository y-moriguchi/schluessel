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
package net.morilib.lisp.math.order;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.math.algebra.ILispNumberEqual;
import net.morilib.lisp.math.algebra.ILispRing;
import net.morilib.math.order.OrderNotation;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/16
 */
public class LispOrderNotation extends Datum2
implements ILispRing<LispOrderNotation>,
ILispNumberEqual<LispOrderNotation> {

	//
	OrderNotation order;

	/**
	 * 
	 * @param order
	 */
	public LispOrderNotation(OrderNotation order) {
		this.order = order;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public LispOrderNotation add(LispOrderNotation y) {
		return new LispOrderNotation(order.add(y.order));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispSubtractable#sub(net.morilib.lisp.math.algebra.ILispSubtractable)
	 */
	public LispOrderNotation sub(LispOrderNotation y) {
		return new LispOrderNotation(order.subtract(y.order));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNegatable#uminus()
	 */
	public LispOrderNotation uminus() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public LispOrderNotation mul(LispOrderNotation y) {
		return new LispOrderNotation(order.multiply(y.order));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNumberEqual#isEqualTo(java.lang.Object)
	 */
	@Override
	public boolean isEqualTo(LispOrderNotation x) {
		return order.equals(x.order);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return order.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		return (o instanceof LispOrderNotation &&
				order.equals(((LispOrderNotation)o).order));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append(order);
	}

}
