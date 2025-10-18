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

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public class UnitSystem implements java.io.Serializable {

	//
	private static final String NUMS = "0123456789.";

	//
	Map<String, Quantity> units = new HashMap<String, Quantity>();
	Map<String, String> bases = new HashMap<String, String>();

	/**
	 * 
	 * @param un
	 * @param ins
	 * @throws IOException
	 */
	protected static void load(UnitSystem un,
			InputStream ins) throws IOException {
		Properties pp = new Properties();
		Quantity q;
		String s, t;

		pp.load(ins);
		for(Object o : pp.keySet()) {
			s = o + "";
			t = pp.getProperty(s);
			if(NUMS.indexOf(t.charAt(0)) >= 0) {
				// do nothing
			} else if(t.indexOf('*') >= 0 ||
					t.indexOf('^') >= 0 || t.indexOf('/') >= 0) {
				// do nothing
			} else {
				un.units.put(s, new Quantity(t));
				un.bases.put(t, s);
			}
		}

		for(Object o : pp.keySet()) {
			s = o + "";
			t = pp.getProperty(s);
			if(NUMS.indexOf(t.charAt(0)) >= 0) {
				// do nothing
			} else if(t.indexOf('*') >= 0 ||
					t.indexOf('^') >= 0 || t.indexOf('/') >= 0) {
				if((q = Units.parse(un, t)) != null) {
					un.units.put(s, q);
				}
			}
		}

		for(Object o : pp.keySet()) {
			s = o + "";
			t = pp.getProperty(s);
			if(NUMS.indexOf(t.charAt(0)) >= 0) {
				if((q = Quantity.parse(un, t)) != null) {
					un.units.put(s, q);
				}
			}
		}
	}

	/**
	 * 
	 * @param ins
	 * @return
	 */
	public static UnitSystem load(InputStream ins) {
		UnitSystem un = new UnitSystem();

		try {
			load(un, ins);
			return un;
		} catch (IOException e) {
			return null;
		}
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static UnitSystem loadResource(String s) {
		InputStream ins;

		ins = UnitSystem.class.getResourceAsStream(s);
		return (ins == null) ? null : load(ins);
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static UnitSystem loadFile(String s) {
		InputStream ins;

		try {
			ins = new FileInputStream(s);
			return (ins == null) ? null : load(ins);
		} catch (FileNotFoundException e) {
			return null;
		}
	}

	//
	UnitSystem() { }

	/**
	 * 
	 * @param s
	 * @return
	 */
	public Unit getUnit(String s) {
		return units.get(s).getUnit();
	}

	/**
	 * 
	 * @param u
	 * @return
	 */
	public double getCoefficient(String s) {
		return units.get(s).getValue();
	}

	/**
	 * 
	 * @param u
	 * @return
	 */
	public Quantity getQuantity(String s) {
		return units.get(s);
	}

	/**
	 * 
	 * @param u
	 * @return
	 */
	public String getDescription(Unit u) {
		String s = null, t = null;
		int d = Integer.MAX_VALUE, f;

		for(Map.Entry<String, Quantity> e : units.entrySet()) {
			if(e.getValue().getValue() != 1.0) {
				// do nothing
			} else if(e.getValue().getUnit().equals(u)) {
				return e.getKey();
			} else if(e.getValue().getUnit().isSingleDerivedUnit()) {
				f = e.getValue().getUnit().getSingleDerivedIndex();
				if(Math.abs(f) < d) {
					s = e.getKey();
					t = e.getValue().getUnit().toString();
					d = Math.abs(f);
				}
			}
		}
		return (s != null) ? s + "*" + t : u.toString();
	}

}
