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
public class Signum2 implements Comparable<Signum2> {

	/**
	 * 
	 */
	public static final Signum2 POSITIVE = new Signum2(true);

	/**
	 * 
	 */
	public static final Signum2 NEGATIVE = new Signum2(false);

	//
	private boolean signum;

	//
	private Signum2(boolean signum) {
		this.signum = signum;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static Signum2 toSignum(int x) {
		if(x == 0) {
			throw new IllegalArgumentException();
		}
		return (x > 0) ? POSITIVE : NEGATIVE;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static Signum2 toSignum(long x) {
		if(x == 0) {
			throw new IllegalArgumentException();
		}
		return (x > 0) ? POSITIVE : NEGATIVE;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Signum2 multiply(Signum2 x) {
		return (signum != x.signum) ? NEGATIVE : POSITIVE;
	}

	/**
	 * 
	 * @return
	 */
	public int signum() {
		return signum ? 1 : -1;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public long multiply(long x) {
		return signum ? x : -x;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public int multiply(int x) {
		return signum ? x : -x;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double multiply(double x) {
		return signum ? x : -x;
	}

	/**
	 * 
	 * @return
	 */
	public Signum2 negate() {
		return signum ? NEGATIVE : POSITIVE;
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(Signum2 o) {
		return (signum == o.signum) ?
				0 : (signum && !o.signum) ? 1 : -1;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return signum ? "+" : "-";
	}

}
