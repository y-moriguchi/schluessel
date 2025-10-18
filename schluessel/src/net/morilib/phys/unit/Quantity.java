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
package net.morilib.phys.unit;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.morilib.lang.Hashes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/05
 */
public final class Quantity implements java.io.Serializable {

	/**
	 * 
	 */
	public static final Quantity ZERO =
		new Quantity(0.0, Unit.NONDIMENSION);

	//
	private static final Pattern PTN = Pattern.compile(
			"(-?([0-9]+|[0-9]*\\.[0-9]+)?([eE]-?[0-9]+)?)(.*)");

	//
	private double value;
	private Unit unit;

	/**
	 * 
	 * @param sys
	 * @param s
	 * @return
	 */
	public static Quantity parse(UnitSystem sys, String s) {
		Matcher m = PTN.matcher(s);
		String s1, s2;
		Quantity q;

		if(!m.matches())  return null;
		s1 = m.group(1);
		if((s2 = m.group(4)).equals("")) {
			return new Quantity(Double.parseDouble(s1),
					Unit.NONDIMENSION);
		} else {
			if((q = Units.parse(sys, s2)) != null) {
				return q.multiply(Double.parseDouble(s1));
			} else {
				return null;
			}
		}
	}

	/**
	 * 
	 * @param value
	 * @param unit
	 */
	public Quantity(double value, Unit unit) {
		this.value = value;
		this.unit  = unit;
	}

	//
	Quantity(String u) {
		this.value = 1.0;
		this.unit  = Unit.getUnit(u, 1);
	}

	/**
	 * @return the value
	 */
	public double getValue() {
		return value;
	}

	/**
	 * @return the unit
	 */
	public Unit getUnit() {
		return unit;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Quantity multiply(double x) {
		return new Quantity(value * x, unit);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Quantity divide(double x) {
		return new Quantity(value / x, unit);
	}

	/**
	 * 
	 * @param q
	 * @return
	 */
	public Quantity add(Quantity q) {
		if(q == null) {
			throw new NullPointerException();
		} else if(!unit.equals(q.unit)) {
			throw new ArithmeticException();
		}
		return new Quantity(value + q.value, unit);
	}

	/**
	 * 
	 * @param q
	 * @return
	 */
	public Quantity subtract(Quantity q) {
		if(q == null) {
			throw new NullPointerException();
		} else if(!unit.equals(q.unit)) {
			throw new ArithmeticException();
		}
		return new Quantity(value - q.value, unit);
	}

	/**
	 * 
	 * @param q
	 * @return
	 */
	public Quantity multiply(Quantity q) {
		if(q == null) {
			throw new NullPointerException();
		}
		return new Quantity(value * q.value, unit.multiply(q.unit));
	}

	/**
	 * 
	 * @param q
	 * @return
	 */
	public Quantity divide(Quantity q) {
		if(q == null) {
			throw new NullPointerException();
		} else if(q.value == 0.0) {
			throw new ArithmeticException();
		}
		return new Quantity(value / q.value, unit.divide(q.unit));
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public Quantity pow(int n) {
		if(n == 0) {
			return ZERO;
		} else if(n == 1) {
			return this;
		} else {
			return new Quantity(Math.pow(value, n), unit.pow(n));
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * ((int)Double.doubleToLongBits(value) + r);
		r = Hashes.A * (unit.hashCode() + r);
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		return ((o instanceof Quantity) &&
				value == ((Quantity)o).value &&
				unit.equals(((Quantity)o).unit));
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return value + unit.toString();
	}

}
