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

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/05
 */
public class SIUnitSystem extends UnitSystem {

	/**
	 * 
	 */
	public static final SIUnitSystem SI;

	//
	static {
		InputStream ins;
		SIUnitSystem un = new SIUnitSystem();

		ins = UnitSystem.class.getResourceAsStream(
				"/net/morilib/phys/unit/SIsystem.properties");
		try {
			load(un, ins);
			SI = un;
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	//
	private SIUnitSystem() {}

	//
	private String _g(String t) {
		String s = bases.get(t);

		return s.equals("g") ? "kg" : s;
	}

	/**
	 * 
	 * @param u
	 * @return
	 */
	public String getDescription(Unit u) {
		StringBuilder b;
		String s = null, t = null;
		double ms;
		Unit v;
		int d = Integer.MAX_VALUE, f, g = 0;

		if(u.isNondimension()) {
			return "";
		} else if(u.isBaseUnit()) {
			t = u.getIndices().keySet().iterator().next();
			return _g(t);
		} else if(u.isSingleDerivedUnit()) {
			t = u.getIndices().keySet().iterator().next();
			g = u.getIndices().values().iterator().next();
			return _g(t) + "^" + g;
		}

		for(Map.Entry<String, Quantity> e : units.entrySet()) {
			ms = Math.pow(1000,
					e.getValue().getUnit().getIndex("mass"));
			if(e.getValue().getValue() != ms) {
				// do nothing
			} else if(e.getValue().getUnit().equals(u)) {
				return e.getKey();
			} else if((v = u.divide(
					e.getValue().getUnit())).isSingleDerivedUnit()) {
				f = v.getSingleDerivedIndex();
				if(f != 0 && Math.abs(f) < d) {
					s = e.getKey();
					t = v.getIndices().keySet().iterator().next();
					d = Math.abs(f);
					g = f;
				}
			}
		}

		if(s != null) {
			if(g != 1) {
				return s + "*" + _g(t) + "^" + g;
			} else {
				return s + "*" + _g(t);
			}
		} else {
			b = new StringBuilder();
			t = "";
			for(String z : u.getIndices().keySet()) {
				b.append(t).append(_g(z));
				if(u.getIndex(z) != 1) {
					b.append("^").append(u.getIndex(z));
				}
				t = "*";
			}
			return b.toString();
		}
	}

}
