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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/09/22
 */
public class Signum3 implements Comparable<Signum3> {

	/**
	 * 
	 */
	public static final Signum3 POSITIVE = new Signum3(1);

	/**
	 * 
	 */
	public static final Signum3 NEGATIVE = new Signum3(-1);

	/**
	 * 
	 */
	public static final Signum3 ZERO = new Signum3(0);

	//
	private int signum;

	//
	private Signum3(int signum) {
		this.signum = signum;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static Signum3 toSignum(int x) {
		return (x > 0) ? POSITIVE : (x < 0) ? NEGATIVE : ZERO;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static Signum3 toSignum(long x) {
		return (x > 0) ? POSITIVE : (x < 0) ? NEGATIVE : ZERO;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Signum3 multiply(Signum3 x) {
		if(signum == 0 || x.signum == 0) {
			return ZERO;
		} else if(signum > 0 ^ x.signum > 0) {
			return NEGATIVE;
		} else {
			return POSITIVE;
		}
	}

	/**
	 * 
	 * @return
	 */
	public int signum() {
		return signum;
	}

	/**
	 * 
	 * @return
	 */
	public Signum3 negate() {
		switch(signum) {
		case  1:  return NEGATIVE;
		case  0:  return ZERO;
		case -1:  return POSITIVE;
		default:  throw new RuntimeException();
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public int multiply(int x) {
		switch(signum) {
		case  1:  return x;
		case  0:  return 0;
		case -1:  return -x;
		default:  throw new RuntimeException();
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double multiply(double x) {
		switch(signum) {
		case  1:  return x;
		case  0:  return 0;
		case -1:  return -x;
		default:  throw new RuntimeException();
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(Signum3 o) {
		return (signum < o.signum) ? -1 : (signum > o.signum) ?
				1 : 0;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return (signum > 0) ? "+" : (signum < 0) ? "-" : "0";
	}

}
