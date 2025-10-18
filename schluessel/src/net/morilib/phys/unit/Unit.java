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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public final class Unit implements java.io.Serializable {

	/**
	 * 
	 */
	public static final Unit NONDIMENSION = new Unit(false);

	//
	private Map<String, Integer> units;

	//
	private Unit(boolean dummy) {
		units = Collections.emptyMap();
	}

	//
	private Unit(String u, int n) {
		units = new HashMap<String, Integer>();
		units.put(u, Integer.valueOf(n));
	}

	//
	Unit(Map<String, Integer> units) {
		this.units = units;
	}

	/**
	 * 
	 * @param u
	 * @param n
	 * @return
	 */
	public static Unit getUnit(String u, int n) {
		return (n == 0) ? NONDIMENSION : new Unit(u, n);
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public int getIndex(String s) {
		Integer i = units.get(s);

		return (i != null) ? i.intValue() : 0;
	}

	/**
	 * 
	 * @return
	 */
	public Map<String, Integer> getIndices() {
		return Collections.unmodifiableMap(units);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isBaseUnit() {
		String s;

		if(units.size() != 1) {
			return false;
		} else {
			s = units.keySet().iterator().next();
			return units.get(s).intValue() == 1;
		}
	}

	/**
	 * 
	 * @return
	 */
	public boolean isNondimension() {
		return units.isEmpty();
	}

	//
	/*package*/ boolean isSingleDerivedUnit() {
		return units.size() == 1;
	}

	//
	/*package*/ int getSingleDerivedIndex() {
		String s;

		if(units.size() != 1) {
			return 0;
		} else {
			s = units.keySet().iterator().next();
			return units.get(s).intValue();
		}
	}

	/**
	 * 
	 * @param u
	 * @return
	 */
	public Unit multiply(Unit u) {
		Map<String, Integer> r;

		r = new HashMap<String, Integer>(units);
		Units.multiply(r, u.units);
		return r.isEmpty() ? NONDIMENSION : new Unit(r);
	}

	/**
	 * 
	 * @param u
	 * @return
	 */
	public Unit divide(Unit u) {
		Map<String, Integer> r;

		r = new HashMap<String, Integer>(units);
		Units.divide(r, u.units);
		return r.isEmpty() ? NONDIMENSION : new Unit(r);
	}

	/**
	 * 
	 * @return
	 */
	public Unit invert() {
		return pow(-1);
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public Unit pow(int n) {
		Map<String, Integer> r;

		if(n == 0) {
			return NONDIMENSION;
		} else if(n == 1) {
			return this;
		} else {
			r = new HashMap<String, Integer>(units);
			Units.pow(r, n);
			return new Unit(r);
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return units.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		return ((o instanceof Unit) &&
				units.equals(((Unit)o).units));
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();
		String dlm = "";

		for(String s : units.keySet()) {
			b.append(dlm).append(s).append("^").append(units.get(s));
			dlm = "*";
		}
		return b.toString();
	}

}
