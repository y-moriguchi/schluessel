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
package net.morilib.math.order;

import java.util.Set;
import java.util.TreeSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/16
 */
public class FactorialOrder extends OrderElement {

	//
	private int order;

	//
	FactorialOrder(int n) {
		order = n;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.order.OrderElement#level()
	 */
	@Override
	public int level() {
		return FACT_LV;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.order.OrderElement#multiply(net.morilib.math.order.OrderElement)
	 */
	@Override
	public OrderElement multiply(OrderElement o) {
		return compareTo(o) < 0 ? o : this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.order.OrderElement#log()
	 */
	@Override
	public Set<OrderElement> log() {
		TreeSet<OrderElement> r = new TreeSet<OrderElement>();

		r.add(new PolynomialOrder(1));
		r.add(new LogarithmicOrder(1, 1));
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.order.OrderElement#compareLevel(net.morilib.math.order.OrderElement)
	 */
	@Override
	public int compareLevel(OrderElement b) {
		return compareTo(b);
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(OrderElement o) {
		if(level() < o.level()) {
			return -1;
		} else if(level() > o.level()) {
			return 1;
		} else if(order < ((ExponentialOrder)o).order) {
			return -1;
		} else if(order > ((ExponentialOrder)o).order) {
			return 1;
		} else {
			return 0;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.order.OrderElement#isConstant()
	 */
	@Override
	public boolean isConstant() {
		return order == 0;
	}

	public String toString() {
		StringBuilder b = new StringBuilder();

		if(order == 0) {
			return "1";
		} else {
			if(order != 1) {
				b.append("(n!)^").append(order);
			} else {
				b.append("n!");
			}
			return b.toString();
		}
	}

}
