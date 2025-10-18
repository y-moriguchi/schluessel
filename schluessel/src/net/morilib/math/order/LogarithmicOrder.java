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

import java.util.Collections;
import java.util.Set;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/11
 */
class LogarithmicOrder extends OrderElement {

	//
	int order, level;

	//
	LogarithmicOrder(int order, int level) {
		if(level <= 0)  throw new IllegalArgumentException();
		this.order = order;
		this.level = level;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.order.AbstractOrderNotation#level()
	 */
	public int level() {
		return LOG_LV;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.order.OrderNotation#multiply(net.morilib.math.order.OrderNotation)
	 */
	public OrderElement multiply(OrderElement o) {
		if(level != ((LogarithmicOrder)o).level) {
			throw new IllegalArgumentException();
		}
		return new LogarithmicOrder(
				order + ((LogarithmicOrder)o).order, level);
	}

//	/* (non-Javadoc)
//	 * @see net.morilib.math.order.OrderNotation#divide(net.morilib.math.order.OrderNotation)
//	 */
//	public OrderElement divide(OrderElement o) {
//		if(level != ((LogarithmicOrder)o).level) {
//			throw new IllegalArgumentException();
//		}
//		return new LogarithmicOrder(
//				order - ((LogarithmicOrder)o).order, level);
//	}
//
//	@Override
//	public OrderElement invert() {
//		return new LogarithmicOrder(-order, level);
//	}

	@Override
	public Set<OrderElement> log() {
		return Collections.<OrderElement>singleton(
				new LogarithmicOrder(1, level + 1));
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.order.OrderElement#isSameLevel(net.morilib.math.order.OrderElement)
	 */
	@Override
	public int compareLevel(OrderElement o) {
		if(level() < o.level()) {
			return -1;
		} else if(level() > o.level()) {
			return 1;
		} else if(level < ((LogarithmicOrder)o).level) {
			return 1;
		} else if(level > ((LogarithmicOrder)o).level) {
			return -1;
		} else if(order < ((LogarithmicOrder)o).order) {
			return -1;
		} else if(order > ((LogarithmicOrder)o).order) {
			return 1;
		} else {
			return 0;
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(OrderElement o) {
		if(level() < o.level()) {
			return -1;
		} else if(level() > o.level()) {
			return 1;
		} else if(level < ((LogarithmicOrder)o).level) {
			return 1;
		} else if(level > ((LogarithmicOrder)o).level) {
			return -1;
		} else if(order < ((LogarithmicOrder)o).order) {
			return -1;
		} else if(order > ((LogarithmicOrder)o).order) {
			return 1;
		} else {
			return 0;
		}
	}

	@Override
	public boolean isConstant() {
		return order == 0;
	}

	public String toString() {
		StringBuilder b = new StringBuilder();

		b.append("(");
		for(int i = 0; i < level; i++)  b.append("log ");
		b.append("n)");
		if(order != 1)  b.append("^").append(order);
		return b.toString();
	}

}
