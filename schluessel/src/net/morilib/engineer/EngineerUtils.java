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
package net.morilib.engineer;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.morilib.util.Maps;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public final class EngineerUtils {

	/**
	 * 
	 */
	public static final double EXA   = 1000000000000000000.0;

	/**
	 * 
	 */
	public static final double PETA  = 1000000000000000.0;

	/**
	 * 
	 */
	public static final double TERA  = 1000000000000.0;

	/**
	 * 
	 */
	public static final double GIGA  = 1000000000.0;

	/**
	 * 
	 */
	public static final double MEGA  = 1000000.0;

	/**
	 * 
	 */
	public static final double KILO  = 1000.0;

	/**
	 * 
	 */
	public static final double HECTO = 100.0;

	/**
	 * 
	 */
	public static final double DECA  = 10.0;

	/**
	 * 
	 */
	public static final double DECI  = 0.1;

	/**
	 * 
	 */
	public static final double CENTI = 0.01;

	/**
	 * 
	 */
	public static final double MILLI = 0.001;

	/**
	 * 
	 */
	public static final double MICRO = 0.000001;

	/**
	 * 
	 */
	public static final double NANO  = 0.000000001;

	/**
	 * 
	 */
	public static final double PICO  = 0.000000000001;

	/**
	 * 
	 */
	public static final double FEMTO = 0.000000000000001;

	/**
	 * 
	 */
	public static final double ATTO  = 0.000000000000000001;

	/**
	 * 
	 */
	public static final Map<String, Double>
	SI_PREFIX = Maps.initHashMap(new Object[][] {
			{ "E", 	EXA },
			{ "P", 	PETA },
			{ "T", 	TERA },
			{ "G", 	GIGA },
			{ "M", 	MEGA },
			{ "k", 	KILO },
			{ "h", 	HECTO },
			{ "da",	DECA },
			{ "d", 	DECI },
			{ "c", 	CENTI },
			{ "m", 	MILLI },
			{ "μ", 	MICRO },
			{ "u", 	MICRO },
			{ "n", 	NANO },
			{ "p", 	PICO },
			{ "f", 	FEMTO },
			{ "a", 	ATTO }
	});

	//
	private EngineerUtils() { }

	/**
	 * 
	 * @param val
	 * @param digit
	 * @return
	 */
	public static String toSIString(double val, int digit) {
		double v2 = Math.abs(val);
		double ks = 0.0;
		String si;

		if(v2 == 0.0) {
			return "0";
		} else if(v2 >= EXA) {
			si = null;
		} else if(v2 >= PETA) {
			si = "P";
			ks = PETA;
		} else if(v2 >= TERA) {
			si = "T";
			ks = TERA;
		} else if(v2 >= GIGA) {
			si = "G";
			ks = GIGA;
		} else if(v2 >= MEGA) {
			si = "M";
			ks = MEGA;
		} else if(v2 >= KILO) {
			si = "k";
			ks = KILO;
		} else if(v2 >= 1.0) {
			si = "";
			ks = 1.0;
		} else if(v2 >= MILLI) {
			si = "m";
			ks = MILLI;
		} else if(v2 >= MICRO) {
			si = "u";
			ks = MICRO;
		} else if(v2 >= NANO) {
			si = "n";
			ks = NANO;
		} else if(v2 >= PICO) {
			si = "p";
			ks = PICO;
		} else if(v2 >= FEMTO) {
			si = "f";
			ks = FEMTO;
		} else {
			si = null;
		}

		if(si == null) {
			BigDecimal d1 = BigDecimal.valueOf(val);

			d1 = d1.setScale(digit, RoundingMode.HALF_EVEN);
			return d1.toEngineeringString();
		} else {
			String fmts = "##0";
			DecimalFormat fmt;

			if(v2 >= 100 * ks) {
				for(int i = 0; i < digit - 3; i++) {
					if(i == 0) {
						fmts += ".";
					}
					fmts += "0";
				}
			} else if(v2 >= 10 * ks) {
				fmts += ".";
				for(int i = 0; i < digit - 2; i++) {
					fmts += "0";
				}
			} else {
				fmts += ".";
				for(int i = 0; i < digit - 1; i++) {
					fmts += "0";
				}
			}
			fmt = new DecimalFormat(fmts);
			return fmt.format(val / ks) + si;
		}
	}

	/**
	 * 
	 * @param si
	 * @return
	 */
	public static double parseSIDouble(String si) {
		Pattern p = Pattern.compile("^([0-9eE.]*)([PTGMkmunpf]?)$");
		Matcher m = p.matcher(si);
		String  num, prf;
		double  res;

		if(!m.matches()) {
			throw new NumberFormatException(si);
		}
		num = m.group(1);
		prf = m.group(2);

		res = Double.parseDouble(num);
		if(SI_PREFIX.containsKey(prf)) {
			res *= SI_PREFIX.get(prf);
		}
		return res;
	}

}
