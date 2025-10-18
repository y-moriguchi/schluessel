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

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Numbers {
	
	private Numbers() { }
	
	//
	private static Integer2 toInteger2(Number n) {
		Integer2 d;
		
		if(n instanceof Integer || n instanceof Short ||
				n instanceof Byte || n instanceof Long) {
			d = Integer2.valueOf(n.longValue());
		} else if(n instanceof Double || n instanceof Float) {
			Rational r = Rational.valueOf(n.doubleValue());
			
			if(!r.isInteger()) {
				return null;
			}
			d = r.getIntegerPart();
		} else if(n instanceof BigDecimal) {
			Rational r = Rational.valueOf((BigDecimal)n);
			
			if(!r.isInteger()) {
				return null;
			}
			d = r.getIntegerPart();
		} else if(n instanceof BigInteger) {
			d = Integer2.valueOf((BigInteger)n);
		} else {
			return null;
		}
		return d;
	}
	
	
	public static boolean equalTo(Number n, long i) {
		Integer2 d = toInteger2(n);
		
		if(d != null) {
			return d.inLongRange() && (i == d.toLong());
		} else {
			return false;
		}
	}
	
	
	public static boolean between(Number n, long f, long t) {
		Integer2 d = toInteger2(n);
		
		if(d != null && d.inLongRange()) {
			return (d.toLong() >= f) && (d.toLong() <= t);
		} else {
			return false;
		}
	}
	
	
	public static int compare(double a, double b) {
		return (a < b) ? -1 : ((a > b) ? 1 : 0);
	}
	
}
