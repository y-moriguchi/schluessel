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

import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/11
 */
public class OrderNotation implements Comparable<OrderNotation> {

	/**
	 * 
	 */
	public static final OrderNotation CONSTANT = new OrderNotation();

	//
	private SortedSet<OrderElement> elements;

	//
	OrderNotation() {
		this.elements = new TreeSet<OrderElement>();
	}

	//
	OrderNotation(OrderElement e) {
		this.elements = new TreeSet<OrderElement>();
		elements.add(e);
	}

	//
	OrderNotation(SortedSet<OrderElement> es) {
		this.elements = es;
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static final OrderNotation polynomial(int n) {
		if(n != 0) {
			return new OrderNotation(new PolynomialOrder(n));
		} else {
			return CONSTANT;
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static final OrderNotation exp(int n) {
		if(n != 0) {
			return new OrderNotation(new ExponentialOrder(n));
		} else {
			return CONSTANT;
		}
	}

	/**
	 * 
	 * @param level
	 * @param n
	 * @return
	 */
	public static final OrderNotation log(int level, int n) {
		if(level < 0) {
			throw new IllegalArgumentException();
		} else if(n == 0) {
			return CONSTANT;
		} else if(level == 0) {
			return polynomial(n);
		} else {
			return new OrderNotation(new LogarithmicOrder(n, level));
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static final OrderNotation factorial(int n) {
		if(n != 0) {
			return new OrderNotation(new FactorialOrder(n));
		} else {
			return CONSTANT;
		}
	}

	//
	private static<T> T next(Iterator<T> i) {
		return i.hasNext() ? i.next() : null;
	}

	//
	private OrderNotation reduce() {
//		if(elements.isEmpty()) {
//			return this;
//		} else if(elements.last().level() == OrderElement.EXP_LV) {
//			return new OrderNotation(elements.last());
//		} else {
//			return this;
//		}
		return this;
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public OrderNotation add(OrderNotation o) {
		return compareTo(o) > 0 ? this : o;
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public OrderNotation subtract(OrderNotation o) {
		return compareTo(o) > 0 ? this : o;
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public OrderNotation multiply(OrderNotation o) {
		SortedSet<OrderElement> r = new TreeSet<OrderElement>();
		Iterator<OrderElement> i, j;
		OrderElement a, b, c;
		int x;

		i = elements.iterator();
		j = o.elements.iterator();
		a = b = null;
		while(a != null || i.hasNext() || b != null || j.hasNext()) {
			if(!i.hasNext() && a != null) {
				r.add(a);  a = null;
			} else if(!j.hasNext() && b != null) {
				r.add(b);  b = null;
			} else {
				if(a == null && i.hasNext())  a = next(i);
				if(b == null && j.hasNext())  b = next(j);
				if((x = a.compareLevel(b)) < 0) {
					r.add(a);  a = next(i);
				} else if(x > 0) {
					r.add(b);  b = next(j);
				} else {
					if(!(c = a.multiply(b)).isConstant())  r.add(c);
					a = next(i);  b = next(j);
				}
			}
		}
		return new OrderNotation(r).reduce();
	}

//	/**
//	 * 
//	 * @param o
//	 * @return
//	 */
//	public OrderNotation divide(OrderNotation o) {
//		SortedSet<OrderElement> r = new TreeSet<OrderElement>();
//		Iterator<OrderElement> i, j;
//		OrderElement a, b, c;
//		int x;
//
//		i = elements.iterator();
//		j = o.elements.iterator();
//		a = b = null;
//		while(a != null || i.hasNext() || b != null || j.hasNext()) {
//			if(!i.hasNext() && a != null) {
//				r.add(a);  a = null;
//			} else if(!j.hasNext() && b != null) {
//				r.add(b.invert());  b = null;
//			} else {
//				if(a == null && i.hasNext())  a = next(i);
//				if(b == null && j.hasNext())  b = next(j);
//				if((x = a.compareLevel(b)) < 0) {
//					r.add(a);  a = next(i);
//				} else if(x > 0) {
//					r.add(b.invert());  b = next(j);
//				} else {
//					if(!(c = a.divide(b)).isConstant())  r.add(c);
//					a = next(i);  b = next(j);
//				}
//			}
//		}
//		return new OrderNotation(r).reduce();
//	}
//
//	/**
//	 * 
//	 * @return
//	 */
//	public OrderNotation invert() {
//		SortedSet<OrderElement> r = new TreeSet<OrderElement>();
//
//		for(OrderElement e : elements) {
//			r.add(e.invert());
//		}
//		return new OrderNotation(r).reduce();
//	}

	public OrderNotation log() {
		if(elements.isEmpty()) {
			throw new ArithmeticException();
		}
		return new OrderNotation(new TreeSet<OrderElement>(
				elements.last().log()));
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(OrderNotation o) {
		return elements.last().compareTo(o.elements.last());
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return elements.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		return (o instanceof OrderNotation &&
				compareTo((OrderNotation)o) == 0);
	}

	public String toString() {
		StringBuilder b = new StringBuilder("O(");
		String dlm = "";

		if(elements.isEmpty()) {
			b.append("1");
		} else {
			for(OrderElement e : elements) {
				b.append(dlm).append(e);
				dlm = "*";
			}
		}
		return b.append(")").toString();
	}

}
