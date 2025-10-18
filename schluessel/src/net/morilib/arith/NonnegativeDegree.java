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
package net.morilib.arith;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/09/16
 */
public class NonnegativeDegree extends Number
implements Comparable<NonnegativeDegree> {

	/**
	 * 
	 */
	public static NonnegativeDegree ZERO =
			new NonnegativeDegree(0, true);

	//
	private static Pattern PARSE = Pattern.compile(
			"^([0-9]+°)?([0-9]+′)?([0-9]+(\\.[0-9]+)?″)?$");

	//
	private static int SECOND_BIT_NUM = 8;
	private static int MINUTE_BIT_NUM = 8 + 6;
	private static int DEGREE_BIT_NUM = 8 + 12;
	private static int MAX_DEGREES = 2048;
	private static int MAX_SECONDS = 2048 * 60 * 60;
	private static int MAX_I = MAX_DEGREES * 60 * 60 * 100;
	private static int DECIMAL_PT = 100;

	//
	private int value;

	//
	NonnegativeDegree(int x, boolean d) {
		this.value = x;
	}

	/**
	 * 
	 * @param x
	 */
	public NonnegativeDegree(int x) {
		int r = x;

		if(r >= MAX_I) {
			throw new IllegalArgumentException();
		} else if(r < 0) {
			throw new IllegalArgumentException();
		}
		value  = (x % 10);  x /= 10;
		value |= (x % 10) << 4;  x /= 10;
		value |= (x % 60) << SECOND_BIT_NUM;  x /= 60;
		value |= (x % 60) << MINUTE_BIT_NUM;  x /= 60;
		value |= x << DEGREE_BIT_NUM;
	}

	/**
	 * 
	 * @param degree
	 * @param minutes
	 * @param seconds
	 * @param undersec
	 */
	public NonnegativeDegree(int degree, int minutes, int seconds,
			int undersec) {
		if(degree < 0 || degree >= MAX_DEGREES) {
			throw new IllegalArgumentException();
		} else if(minutes < 0 || minutes >= 60) {
			throw new IllegalArgumentException();
		} else if(seconds < 0 || seconds >= 60) {
			throw new IllegalArgumentException();
		} else if(undersec < 0 || undersec >= 100) {
			throw new IllegalArgumentException();
		}
		value  = (undersec % 10);
		value |= (undersec / 10) << 4;
		value |= seconds << SECOND_BIT_NUM;
		value |= minutes << MINUTE_BIT_NUM;
		value |= degree << DEGREE_BIT_NUM;
	}

	/**
	 * 
	 * @param degree
	 */
	public NonnegativeDegree(double seconds) {
		double x = seconds;
		int d, m, s, u;

		if(seconds < 0) {
			throw new IllegalArgumentException();
		} else if(seconds >= MAX_SECONDS) {
			throw new IllegalArgumentException();
		}
		s = (int)x;  x = (x - s) * 100;
		u = (int)x;
		d = s / 3600;  s = s % 3600;
		m = s / 60;    s = s % 60;
		value  = (u % 10);
		value |= (u / 10) << 4;
		value |= s << SECOND_BIT_NUM;
		value |= m << MINUTE_BIT_NUM;
		value |= d << DEGREE_BIT_NUM;
	}

	//
	private static boolean emp(String s) {
		return s == null || s.equals("");
	}

	/**
	 * 
	 * @param string
	 * @return
	 */
	public static NonnegativeDegree parse(String string) {
		Matcher e = PARSE.matcher(string);
		String[] p;
		String z1, z2, z3;
		int d, m, s, u;

		if(string.equals("") || !e.matches()) {
			throw new NumberFormatException("syntax error");
		} else {
			d = emp(z1 = e.group(1)) ?
					0 : Integer.parseInt(z1.replaceFirst("°", ""));
			m = emp(z2 = e.group(2)) ?
					0 : Integer.parseInt(z2.replaceFirst("′", ""));
			if(emp(z3 = e.group(3))) {
				s = u = 0;
			} else if((z3 = z3.replaceFirst("″", ""))
					.matches("^[0-9]+$")) {
				s = Integer.parseInt(z3);
				u = 0;
			} else {
				p = (z3 = z3 + "0").split("\\.");
				s = Integer.parseInt(p[0]);
				u = Integer.parseInt(p[1].substring(0, 2));
			}

			try {
				return new NonnegativeDegree(d, m, s, u);
			} catch(IllegalArgumentException e1) {
				throw new NumberFormatException();
			}
		}
	}

	/**
	 * 
	 * @param x
	 * @param carry
	 * @return
	 */
	public NonnegativeDegree add(
			NonnegativeDegree x, int[] carry) {
		int a, b, c = 0, r = 0, i = 0;

		// undersec
		for(; i < SECOND_BIT_NUM; i += 4) {
			a = (value   & (0xf << i)) >> i;
			b = (x.value & (0xf << i)) >> i;
			c = a + b + c;
			r = r | ((c % 10) << i);
			c = c / 10;
		}

		// seconds, minutes
		for(; i < DEGREE_BIT_NUM; i += 6) {
			a = (value   & (0x3f << i)) >> i;
			b = (x.value & (0x3f << i)) >> i;
			c = a + b + c;
			r = r | ((c % 60) << i);
			c = c / 60;
		}

		// degree
		a = value   >> DEGREE_BIT_NUM;
		b = x.value >> DEGREE_BIT_NUM;
		c = a + b + c;
		r = r | ((c % MAX_DEGREES) << DEGREE_BIT_NUM);
		c = c / MAX_DEGREES;
		if(carry != null && carry.length > 0) {
			carry[0] = c;
		}
		return new NonnegativeDegree(r, true);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public NonnegativeDegree add(NonnegativeDegree x) {
		return add(x, null);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public NonnegativeDegree subtract(NonnegativeDegree x) {
		int a, b, c = 0, r = 0, i = 0;

		if(compareTo(x) < 0) {
			throw new ArithmeticException("the result is negative");
		}

		// undersec
		for(; i < SECOND_BIT_NUM; i += 4) {
			a = (value   & (0xf << i)) >> i;
			b = (x.value & (0xf << i)) >> i;
			c = a - b + c;
			if(c < 0) {
				r = r | ((c + 10) << i);
				c = -1;
			} else {
				r = r | (c << i);
				c = 0;
			}
		}

		// seconds, minutes
		for(; i < DEGREE_BIT_NUM; i += 6) {
			a = (value   & (0x3f << i)) >> i;
			b = (x.value & (0x3f << i)) >> i;
			c = a - b + c;
			if(c < 0) {
				r = r | ((c + 60) << i);
				c = -1;
			} else {
				r = r | (c << i);
				c = 0;
			}
		}

		// degree
		a = value   >> DEGREE_BIT_NUM;
		b = x.value >> DEGREE_BIT_NUM;
		c = a - b + c;
		if(c < 0) {
			r = r | ((c + MAX_DEGREES) << i);
			c = -1;
		} else {
			r = r | (c << i);
			c = 0;
		}
		return new NonnegativeDegree(r, true);
	}

	/**
	 * 
	 * @return
	 */
	public int toCentiSecond() {
		int r;

		r = (value >> DEGREE_BIT_NUM);
		r = r * 60 + ((value >> MINUTE_BIT_NUM) & 0x3f);
		r = r * 60 + ((value >> SECOND_BIT_NUM) & 0x3f);
		r = r * 10 + ((value >> 4) & 0x0f);
		r = r * 10 + (value & 0x0f);
		return r;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public NonnegativeDegree multiply(
			NonnegativeDegree x, int[] carry) {
		long a = toCentiSecond(), b = x.toCentiSecond(), c;

		c = a * b / DECIMAL_PT;
		if(carry != null && carry.length > 0) {
			carry[0] = (int)(c / MAX_I);
		}
		return new NonnegativeDegree((int)(c % MAX_I));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public NonnegativeDegree multiply(NonnegativeDegree x) {
		return multiply(x, null);
	}

	/**
	 * 
	 * @param x
	 * @param carry
	 * @return
	 */
	public NonnegativeDegree divide(
			NonnegativeDegree x, int[] carry) {
		long a = toCentiSecond(), b = x.toCentiSecond(), c;

		if(x.signum() == 0) {
			throw new ArithmeticException("divide by zero");
		}
		c = a * DECIMAL_PT / b;
		if(carry != null && carry.length > 0) {
			carry[0] = (int)(c / MAX_I);
		}
		return new NonnegativeDegree((int)(c % MAX_I));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public NonnegativeDegree divide(NonnegativeDegree x) {
		return divide(x, null);
	}

	/**
	 * 
	 * @return
	 */
	public NonnegativeDegree invert() {
		long a = toCentiSecond(), c;

		if(signum() == 0) {
			throw new ArithmeticException("divide by zero");
		}
		c = DECIMAL_PT * DECIMAL_PT / a;
		return new NonnegativeDegree((int)c);
	}

	/**
	 * 
	 * @return
	 */
	public NonnegativeDegree remainder(NonnegativeDegree x) {
		long a = toCentiSecond(), b = x.toCentiSecond(), c;

		if(x.signum() == 0) {
			throw new ArithmeticException("divide by zero");
		}
		c = a % b;
		return new NonnegativeDegree((int)c);
	}

	/**
	 * 
	 * @return
	 */
	public int signum() {
		return value > 0 ? 1 : 0;
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#intValue()
	 */
	@Override
	public int intValue() {
		return toCentiSecond() / DECIMAL_PT;
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#longValue()
	 */
	@Override
	public long longValue() {
		return toCentiSecond() / DECIMAL_PT;
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#floatValue()
	 */
	@Override
	public float floatValue() {
		return (float)doubleValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#doubleValue()
	 */
	@Override
	public double doubleValue() {
		double r = 0.0;

		r = (value >> DEGREE_BIT_NUM);
		r = r * 60 + ((value >> MINUTE_BIT_NUM) & 0x3f);
		r = r * 60 + ((value >> SECOND_BIT_NUM) & 0x3f);
		r = r      + ((value >> 4) & 0x0f) * 0.1;
		r = r      + (value & 0x0f) * 0.01;
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(NonnegativeDegree o) {
		return (value < o.value) ? -1 : (value > o.value) ?
				1 : 0;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return value;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof NonnegativeDegree) {
			return value == ((NonnegativeDegree)o).value;
		}
		return false;
	}

	/*(non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();
		int f = 0;

		b.append(value >> DEGREE_BIT_NUM).append("°");
		b.append((value >> MINUTE_BIT_NUM) & 0x3f).append("′");
		b.append((value >> SECOND_BIT_NUM) & 0x3f);
		f = ((value >> 4) & 0x0f) * 10 + (value & 0x0f);
		if(f != 0) {
			b.append(".").append(f).append("″");
		} else {
			b.append("″");
		}
		return b.toString();
	}

}
